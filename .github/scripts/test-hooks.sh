#!/bin/bash
set -euo pipefail

###############################################################################
# Unit tests for board-status-updater.sh and ai-confidence-recorder.sh hooks
###############################################################################

PASS=0
FAIL=0

pass() {
  PASS=$((PASS+1))
  echo "  PASS: $1"
}

fail() {
  FAIL=$((FAIL+1))
  echo "  FAIL: $1"
}

###############################################################################
# Section 1 - XML Parsing (T1-T9)
# Tests the actual <github-project> XML format used by hooks:
#   board-status-updater.sh, ai-confidence-recorder.sh, board-transition.sh
###############################################################################
echo "=== Section 1: XML Parsing ==="

TMPDIR_TEST=$(mktemp -d)
CLAUDE_MD="$TMPDIR_TEST/CLAUDE.md"

cat > "$CLAUDE_MD" <<'XMLEOF'
## Task Management
<github-project id="PVT_test123" url="https://github.com/users/testuser/projects/4">
  <field name="Status" id="PVTSSF_status001">
    <option name="Todo" id="98456001"/>
    <option name="In Progress" id="98456002"/>
    <option name="Review" id="98456003"/>
    <option name="Done" id="98456004"/>
  </field>
  <field name="Priority" id="PVTSSF_priority001">
    <option name="P0" id="77700001"/>
    <option name="P1" id="77700002"/>
    <option name="P2" id="77700003"/>
  </field>
  <field name="AI-Confidence" id="PVTF_confidence001"/>
  <field name="Turns-Used" id="PVTF_turns001"/>
</github-project>

Some other content here.
XMLEOF

# T1: grep finds <github-project tag
if grep -q '<github-project' "$CLAUDE_MD"; then
  pass "T1: grep finds <github-project> tag"
else
  fail "T1: grep finds <github-project> tag"
fi

# T2: Parse PROJECT_ID (same pattern as board-status-updater.sh line 49)
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' "$CLAUDE_MD" | head -1)
if [[ "$PROJECT_ID" == "PVT_test123" ]]; then
  pass "T2: Parse PROJECT_ID = $PROJECT_ID"
else
  fail "T2: Parse PROJECT_ID expected PVT_test123, got '$PROJECT_ID'"
fi

# T3: Parse PROJECT_NUMBER from URL (same pattern as board-status-updater.sh line 99)
PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' "$CLAUDE_MD" | head -1)
if [[ "$PROJECT_NUMBER" == "4" ]]; then
  pass "T3: Parse PROJECT_NUMBER = $PROJECT_NUMBER"
else
  fail "T3: Parse PROJECT_NUMBER expected 4, got '$PROJECT_NUMBER'"
fi

# T4: Parse OWNER from URL (same pattern as board-status-updater.sh line 90)
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' "$CLAUDE_MD" | head -1)
if [[ "$OWNER" == "testuser" ]]; then
  pass "T4: Parse OWNER = $OWNER"
else
  fail "T4: Parse OWNER expected testuser, got '$OWNER'"
fi

# T5: Parse STATUS_FIELD_ID (same pattern as board-status-updater.sh line 53)
STATUS_FIELD_ID=$(grep -A1 'name="Status"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
if [[ "$STATUS_FIELD_ID" == "PVTSSF_status001" ]]; then
  pass "T5: Parse STATUS_FIELD_ID = $STATUS_FIELD_ID"
else
  fail "T5: Parse STATUS_FIELD_ID expected PVTSSF_status001, got '$STATUS_FIELD_ID'"
fi

# T6: Parse Review option ID (same pattern as board-status-updater.sh line 64-65)
REVIEW_ID=$(grep -B0 -A0 'name="Review"' "$CLAUDE_MD" \
  | grep -oP '<option name="Review" id="\K[^"]+' | head -1)
if [[ "$REVIEW_ID" == "98456003" ]]; then
  pass "T6: Parse Review option ID = $REVIEW_ID"
else
  fail "T6: Parse Review option ID expected 98456003, got '$REVIEW_ID'"
fi

# T7: Parse In Progress option ID (same pattern as board-status-updater.sh line 64-65)
IN_PROGRESS_ID=$(grep -B0 -A0 'name="In Progress"' "$CLAUDE_MD" \
  | grep -oP '<option name="In Progress" id="\K[^"]+' | head -1)
if [[ "$IN_PROGRESS_ID" == "98456002" ]]; then
  pass "T7: Parse In Progress option ID = $IN_PROGRESS_ID"
else
  fail "T7: Parse In Progress option ID expected 98456002, got '$IN_PROGRESS_ID'"
fi

# T8: Parse AI-Confidence field ID
AI_CONFIDENCE_ID=$(grep 'name="AI-Confidence"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
if [[ "$AI_CONFIDENCE_ID" == "PVTF_confidence001" ]]; then
  pass "T8: Parse AI-Confidence field ID = $AI_CONFIDENCE_ID"
else
  fail "T8: Parse AI-Confidence field ID expected PVTF_confidence001, got '$AI_CONFIDENCE_ID'"
fi

# T9: Parse Turns-Used field ID
TURNS_USED_ID=$(grep 'name="Turns-Used"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
if [[ "$TURNS_USED_ID" == "PVTF_turns001" ]]; then
  pass "T9: Parse Turns-Used field ID = $TURNS_USED_ID"
else
  fail "T9: Parse Turns-Used field ID expected PVTF_turns001, got '$TURNS_USED_ID'"
fi

rm -rf "$TMPDIR_TEST"

###############################################################################
# Section 2 - Trigger Detection (T10-T13)
###############################################################################
echo ""
echo "=== Section 2: Trigger Detection ==="

# T10: "gh pr create --title ..." matches *"gh pr create"*
CMD10="gh pr create --title \"Add auth feature\""
if [[ "$CMD10" == *"gh pr create"* ]]; then
  pass "T10: 'gh pr create' trigger detected"
else
  fail "T10: 'gh pr create' trigger not detected"
fi

# T11: "git push -u origin feature/14" matches *"git push"*
CMD11="git push -u origin feature/14"
if [[ "$CMD11" == *"git push"* ]]; then
  pass "T11: 'git push' trigger detected"
else
  fail "T11: 'git push' trigger not detected"
fi

# T12: "git status" does NOT match either trigger
CMD12="git status"
if [[ "$CMD12" != *"gh pr create"* && "$CMD12" != *"git push"* ]]; then
  pass "T12: 'git status' correctly not a trigger"
else
  fail "T12: 'git status' incorrectly matched a trigger"
fi

# T13: "! [rejected] main -> main" contains "rejected"
OUTPUT13="! [rejected] main -> main (fetch first)"
if [[ "$OUTPUT13" == *"rejected"* ]]; then
  pass "T13: 'rejected' detected in push output"
else
  fail "T13: 'rejected' not detected in push output"
fi

###############################################################################
# Section 3 - Branch Issue Extraction (T14-T18)
###############################################################################
echo ""
echo "=== Section 3: Branch Issue Extraction ==="

extract_issue_number() {
  local branch="$1"
  if [[ "$branch" =~ (^|[/_-])([0-9]+)(-|$) ]]; then
    echo "${BASH_REMATCH[2]}"
  else
    echo ""
  fi
}

# T14: "feature/12-add-auth" → 12
RESULT14=$(extract_issue_number "feature/12-add-auth")
if [[ "$RESULT14" == "12" ]]; then
  pass "T14: feature/12-add-auth → $RESULT14"
else
  fail "T14: feature/12-add-auth expected 12, got '$RESULT14'"
fi

# T15: "GH-5-fix-bug" → 5
RESULT15=$(extract_issue_number "GH-5-fix-bug")
if [[ "$RESULT15" == "5" ]]; then
  pass "T15: GH-5-fix-bug → $RESULT15"
else
  fail "T15: GH-5-fix-bug expected 5, got '$RESULT15'"
fi

# T16: "issue-123-feature" → 123
RESULT16=$(extract_issue_number "issue-123-feature")
if [[ "$RESULT16" == "123" ]]; then
  pass "T16: issue-123-feature → $RESULT16"
else
  fail "T16: issue-123-feature expected 123, got '$RESULT16'"
fi

# T17: "main" → empty
RESULT17=$(extract_issue_number "main")
if [[ "$RESULT17" == "" ]]; then
  pass "T17: main → (empty)"
else
  fail "T17: main expected empty, got '$RESULT17'"
fi

# T18: "feature/14-test-hook" → 14
RESULT18=$(extract_issue_number "feature/14-test-hook")
if [[ "$RESULT18" == "14" ]]; then
  pass "T18: feature/14-test-hook → $RESULT18"
else
  fail "T18: feature/14-test-hook expected 14, got '$RESULT18'"
fi

###############################################################################
# Section 4 - AI-Confidence Estimation (T19-T24)
###############################################################################
echo ""
echo "=== Section 4: AI-Confidence Estimation ==="

estimate_confidence() {
  local turns=$1
  if [[ $turns -le 3 ]]; then
    echo 85
  elif [[ $turns -le 7 ]]; then
    echo 70
  elif [[ $turns -le 15 ]]; then
    echo 55
  else
    echo 40
  fi
}

# T19: 1 turn → 85
RESULT19=$(estimate_confidence 1)
if [[ "$RESULT19" == "85" ]]; then
  pass "T19: 1 turn → confidence $RESULT19"
else
  fail "T19: 1 turn expected 85, got '$RESULT19'"
fi

# T20: 3 turns → 85
RESULT20=$(estimate_confidence 3)
if [[ "$RESULT20" == "85" ]]; then
  pass "T20: 3 turns → confidence $RESULT20"
else
  fail "T20: 3 turns expected 85, got '$RESULT20'"
fi

# T21: 5 turns → 70
RESULT21=$(estimate_confidence 5)
if [[ "$RESULT21" == "70" ]]; then
  pass "T21: 5 turns → confidence $RESULT21"
else
  fail "T21: 5 turns expected 70, got '$RESULT21'"
fi

# T22: 7 turns → 70
RESULT22=$(estimate_confidence 7)
if [[ "$RESULT22" == "70" ]]; then
  pass "T22: 7 turns → confidence $RESULT22"
else
  fail "T22: 7 turns expected 70, got '$RESULT22'"
fi

# T23: 10 turns → 55
RESULT23=$(estimate_confidence 10)
if [[ "$RESULT23" == "55" ]]; then
  pass "T23: 10 turns → confidence $RESULT23"
else
  fail "T23: 10 turns expected 55, got '$RESULT23'"
fi

# T24: 20 turns → 40
RESULT24=$(estimate_confidence 20)
if [[ "$RESULT24" == "40" ]]; then
  pass "T24: 20 turns → confidence $RESULT24"
else
  fail "T24: 20 turns expected 40, got '$RESULT24'"
fi

###############################################################################
# Section 5 - Mock Stdin Integration (T25-T27)
###############################################################################
echo ""
echo "=== Section 5: Mock Stdin Integration ==="

# T25: jq parses gh pr create command from JSON stdin
STDIN25='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"gh pr create --title \"feat: add auth\" --body \"Implements #12\""}}'
CMD25=$(echo "$STDIN25" | jq -r '.tool_input.command')
if [[ "$CMD25" == *"gh pr create"* ]]; then
  pass "T25: jq parses gh pr create command from stdin"
else
  fail "T25: jq failed to parse gh pr create command, got '$CMD25'"
fi

# T26: jq parses pr output URL from JSON stdin
STDIN26='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"gh pr create --title test"},"tool_output":{"stdout":"https://github.com/testuser/repo/pull/12\n","stderr":""}}'
PR_URL=$(echo "$STDIN26" | jq -r '.tool_output.stdout' | tr -d '\n')
if [[ "$PR_URL" == "https://github.com/testuser/repo/pull/12" ]]; then
  pass "T26: jq parses pr output URL from stdin"
else
  fail "T26: jq failed to parse pr URL, got '$PR_URL'"
fi

# T27: jq parses git push command from JSON stdin
STDIN27='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push -u origin feature/14-test-hook"}}'
CMD27=$(echo "$STDIN27" | jq -r '.tool_input.command')
if [[ "$CMD27" == *"git push"* ]]; then
  pass "T27: jq parses git push command from stdin"
else
  fail "T27: jq failed to parse git push command, got '$CMD27'"
fi

###############################################################################
# Section 6 - Squad XML Parsing (T28-T32)
# Tests Squad-specific XML attributes and fields
###############################################################################
echo ""
echo "=== Section 6: Squad XML Parsing ==="

TMPDIR_SQUAD=$(mktemp -d)
CLAUDE_MD_SQUAD="$TMPDIR_SQUAD/CLAUDE.md"

cat > "$CLAUDE_MD_SQUAD" <<'XMLEOF'
## Task Management
<github-project id="PVT_squad123" url="https://github.com/orgs/testorg/projects/7" scale="squad">
  <field name="Status" id="PVTSSF_status002">
    <option name="Triage" id="99001001"/>
    <option name="Backlog" id="99001002"/>
    <option name="Ready" id="99001003"/>
    <option name="In Progress" id="99001004"/>
    <option name="Review" id="99001005"/>
    <option name="Done" id="99001006"/>
  </field>
  <field name="Component" id="PVTSSF_comp001">
    <option name="Pod-A" id="88001001"/>
    <option name="Pod-B" id="88001002"/>
    <option name="Pod-C" id="88001003"/>
  </field>
  <field name="Agent-Assigned" id="PVTSSF_agent001">
    <option name="AI" id="77001001"/>
    <option name="Human" id="77001002"/>
    <option name="Pair" id="77001003"/>
  </field>
  <field name="MTTV-Hours" id="PVTF_mttv001"/>
  <field name="Rework-Count" id="PVTF_rework001"/>
  <field name="Sprint-Goal" id="PVTF_goal001"/>
  <field name="Blocked-By" id="PVTF_blocked001"/>
</github-project>
XMLEOF

# T28: Parse scale="squad" attribute
SCALE28=$(grep -oP '<github-project[^>]*scale="\K[^"]+' "$CLAUDE_MD_SQUAD" | head -1)
if [[ "$SCALE28" == "squad" ]]; then
  pass "T28: Parse scale attribute = $SCALE28"
else
  fail "T28: Parse scale attribute expected squad, got '$SCALE28'"
fi

# T29: Squad has 6 status options
STATUS_COUNT29=$(grep -c '<option name=.*id=' "$CLAUDE_MD_SQUAD" | head -1)
# Count only Status field options (6 options)
STATUS_OPTIONS29=$(sed -n '/<field name="Status"/,/<\/field>/p' "$CLAUDE_MD_SQUAD" | grep -c '<option')
if [[ "$STATUS_OPTIONS29" == "6" ]]; then
  pass "T29: Squad Status has $STATUS_OPTIONS29 options"
else
  fail "T29: Squad Status expected 6 options, got '$STATUS_OPTIONS29'"
fi

# T30: Parse Component field ID
COMPONENT_ID30=$(grep 'name="Component"' "$CLAUDE_MD_SQUAD" | grep -oP 'id="\K[^"]+' | head -1)
if [[ "$COMPONENT_ID30" == "PVTSSF_comp001" ]]; then
  pass "T30: Parse Component field ID = $COMPONENT_ID30"
else
  fail "T30: Parse Component field ID expected PVTSSF_comp001, got '$COMPONENT_ID30'"
fi

# T31: Parse Agent-Assigned field ID
AGENT_ID31=$(grep 'name="Agent-Assigned"' "$CLAUDE_MD_SQUAD" | grep -oP 'id="\K[^"]+' | head -1)
if [[ "$AGENT_ID31" == "PVTSSF_agent001" ]]; then
  pass "T31: Parse Agent-Assigned field ID = $AGENT_ID31"
else
  fail "T31: Parse Agent-Assigned field ID expected PVTSSF_agent001, got '$AGENT_ID31'"
fi

# T32: Parse Triage option ID (Squad-specific first status)
TRIAGE_ID32=$(grep -B0 -A0 'name="Triage"' "$CLAUDE_MD_SQUAD" \
  | grep -oP '<option name="Triage" id="\K[^"]+' | head -1)
if [[ "$TRIAGE_ID32" == "99001001" ]]; then
  pass "T32: Parse Triage option ID = $TRIAGE_ID32"
else
  fail "T32: Parse Triage option ID expected 99001001, got '$TRIAGE_ID32'"
fi

rm -rf "$TMPDIR_SQUAD"

###############################################################################
# Section 7 - Scale Fallback & Status Options (T33-T37)
# Tests backward compatibility and get_status_options logic
###############################################################################
echo ""
echo "=== Section 7: Scale Fallback & Status Options ==="

# T33: scale attribute missing → fallback to "solo"
TMPDIR_FALLBACK=$(mktemp -d)
CLAUDE_MD_FALLBACK="$TMPDIR_FALLBACK/CLAUDE.md"
cat > "$CLAUDE_MD_FALLBACK" <<'XMLEOF'
## Task Management
<github-project id="PVT_legacy123" url="https://github.com/users/testuser/projects/5">
  <field name="Status" id="PVTSSF_status003"/>
</github-project>
XMLEOF

SCALE33=$(grep -oP '<github-project[^>]*scale="\K[^"]+' "$CLAUDE_MD_FALLBACK" 2>/dev/null || echo "solo")
if [[ "$SCALE33" == "solo" ]]; then
  pass "T33: Missing scale attribute → fallback to '$SCALE33'"
else
  fail "T33: Missing scale attribute expected solo fallback, got '$SCALE33'"
fi

# T34: scale attribute present → correctly parsed (not fallback)
SCALE34=$(grep -oP '<github-project[^>]*scale="\K[^"]+' "$CLAUDE_MD_SQUAD" 2>/dev/null || echo "solo")
# $CLAUDE_MD_SQUAD was already cleaned up, use inline test
SCALE34_INLINE=$(echo '<github-project id="PVT_x" url="..." scale="pod">' \
  | grep -oP 'scale="\K[^"]+' || echo "solo")
if [[ "$SCALE34_INLINE" == "pod" ]]; then
  pass "T34: Present scale attribute parsed = $SCALE34_INLINE (not fallback)"
else
  fail "T34: Present scale attribute expected pod, got '$SCALE34_INLINE'"
fi

rm -rf "$TMPDIR_FALLBACK"

# T35: get_status_options squad → 6 comma-separated statuses
get_status_options() {
  case "$1" in
    solo|pod)    echo "Todo,In Progress,Review,Done" ;;
    squad)       echo "Triage,Backlog,Ready,In Progress,Review,Done" ;;
    enterprise)  echo "Triage,Backlog,Ready,In Progress,In CI,Review,Done" ;;
  esac
}

SQUAD_OPTS=$(get_status_options squad)
IFS=',' read -ra SQUAD_ARR <<< "$SQUAD_OPTS"
if [[ "${#SQUAD_ARR[@]}" == "6" ]]; then
  pass "T35: get_status_options squad → ${#SQUAD_ARR[@]} statuses"
else
  fail "T35: get_status_options squad expected 6 statuses, got ${#SQUAD_ARR[@]}"
fi

# T36: Squad first status is Triage (not Todo)
if [[ "${SQUAD_ARR[0]}" == "Triage" ]]; then
  pass "T36: Squad first status = ${SQUAD_ARR[0]} (not Todo)"
else
  fail "T36: Squad first status expected Triage, got '${SQUAD_ARR[0]}'"
fi

# T37: Solo/Pod has 4 statuses, starts with Todo
SOLO_OPTS=$(get_status_options solo)
IFS=',' read -ra SOLO_ARR <<< "$SOLO_OPTS"
if [[ "${#SOLO_ARR[@]}" == "4" && "${SOLO_ARR[0]}" == "Todo" ]]; then
  pass "T37: get_status_options solo → ${#SOLO_ARR[@]} statuses, first=${SOLO_ARR[0]}"
else
  fail "T37: get_status_options solo expected 4 statuses starting with Todo, got ${#SOLO_ARR[@]}/${SOLO_ARR[0]}"
fi

###############################################################################
# Section 8 - board-transition.sh Argument Validation (T38-T45)
# Tests the regex and validation at board-transition.sh L78-79
###############################################################################
echo ""
echo "=== Section 8: board-transition.sh Argument Validation ==="

# Validate issue_number: must match ^[0-9]+$
validate_issue_number() {
  [[ "$1" =~ ^[0-9]+$ ]] && echo "accept" || echo "reject"
}

# T38: "42" → accept
if [[ $(validate_issue_number "42") == "accept" ]]; then
  pass "T38: issue_number '42' accepted"
else
  fail "T38: issue_number '42' should be accepted"
fi

# T39: "abc" → reject
if [[ $(validate_issue_number "abc") == "reject" ]]; then
  pass "T39: issue_number 'abc' rejected"
else
  fail "T39: issue_number 'abc' should be rejected"
fi

# T40: "" → reject
if [[ $(validate_issue_number "") == "reject" ]]; then
  pass "T40: issue_number '' rejected"
else
  fail "T40: issue_number '' should be rejected"
fi

# T41: "-5" → reject
if [[ $(validate_issue_number "-5") == "reject" ]]; then
  pass "T41: issue_number '-5' rejected"
else
  fail "T41: issue_number '-5' should be rejected"
fi

# T42: "3.14" → reject
if [[ $(validate_issue_number "3.14") == "reject" ]]; then
  pass "T42: issue_number '3.14' rejected"
else
  fail "T42: issue_number '3.14' should be rejected"
fi

# T43: "99999" → accept
if [[ $(validate_issue_number "99999") == "accept" ]]; then
  pass "T43: issue_number '99999' accepted"
else
  fail "T43: issue_number '99999' should be accepted"
fi

# T44: target_status "" → reject (must not be empty)
validate_target_status() {
  [[ -z "$1" ]] && echo "reject" || echo "accept"
}

if [[ $(validate_target_status "") == "reject" ]]; then
  pass "T44: target_status '' rejected"
else
  fail "T44: target_status '' should be rejected"
fi

# T45: target_status "In Progress" → accept (spaces allowed)
if [[ $(validate_target_status "In Progress") == "accept" ]]; then
  pass "T45: target_status 'In Progress' accepted"
else
  fail "T45: target_status 'In Progress' should be accepted"
fi

###############################################################################
# Section 9 - Status Name → Option ID Resolution (T46-T51)
# Tests the pattern at board-transition.sh L105-111
###############################################################################
echo ""
echo "=== Section 9: Status Name → Option ID Resolution ==="

TMPDIR_S9=$(mktemp -d)
CLAUDE_MD_S9="$TMPDIR_S9/CLAUDE.md"
cat > "$CLAUDE_MD_S9" <<'XMLEOF'
## Task Management
<github-project id="PVT_test123" url="https://github.com/users/testuser/projects/4">
  <field name="Status" id="PVTSSF_status001">
    <option name="Todo" id="98456001"/>
    <option name="In Progress" id="98456002"/>
    <option name="Review" id="98456003"/>
    <option name="Done" id="98456004"/>
  </field>
  <field name="Priority" id="PVTSSF_priority001">
    <option name="P0" id="77700001"/>
    <option name="P1" id="77700002"/>
    <option name="P2" id="77700003"/>
  </field>
</github-project>
XMLEOF

resolve_option_id() {
  local target="$1"
  local file="$2"
  grep -B0 -A0 "name=\"$target\"" "$file" 2>/dev/null \
    | grep -oP '<option name="'"$target"'" id="\K[^"]+' 2>/dev/null | head -1 || true
}

# T46: "Done" → 98456004
RESULT46=$(resolve_option_id "Done" "$CLAUDE_MD_S9")
if [[ "$RESULT46" == "98456004" ]]; then
  pass "T46: 'Done' → $RESULT46"
else
  fail "T46: 'Done' expected 98456004, got '$RESULT46'"
fi

# T47: "In Progress" (space) → 98456002
RESULT47=$(resolve_option_id "In Progress" "$CLAUDE_MD_S9")
if [[ "$RESULT47" == "98456002" ]]; then
  pass "T47: 'In Progress' → $RESULT47"
else
  fail "T47: 'In Progress' expected 98456002, got '$RESULT47'"
fi

# T48: "Blocked" (non-existent) → empty
RESULT48=$(resolve_option_id "Blocked" "$CLAUDE_MD_S9")
if [[ -z "$RESULT48" ]]; then
  pass "T48: 'Blocked' → (empty, not found)"
else
  fail "T48: 'Blocked' expected empty, got '$RESULT48'"
fi

# T49: Available options listed on mismatch
AVAILABLE49=$(grep -oP '<option name="\K[^"]+' "$CLAUDE_MD_S9" 2>/dev/null | sort -u | paste -sd ', ' -)
if [[ "$AVAILABLE49" == *"Done"* && "$AVAILABLE49" == *"Todo"* ]]; then
  pass "T49: Available options listed: $AVAILABLE49"
else
  fail "T49: Available options expected to contain Done and Todo, got '$AVAILABLE49'"
fi

# T50: "todo" (lowercase) → empty (case-sensitive)
RESULT50=$(resolve_option_id "todo" "$CLAUDE_MD_S9")
if [[ -z "$RESULT50" ]]; then
  pass "T50: 'todo' (lowercase) → (empty, case-sensitive)"
else
  fail "T50: 'todo' expected empty, got '$RESULT50'"
fi

# T51: Priority "P0" → 77700001
RESULT51=$(resolve_option_id "P0" "$CLAUDE_MD_S9")
if [[ "$RESULT51" == "77700001" ]]; then
  pass "T51: 'P0' → $RESULT51"
else
  fail "T51: 'P0' expected 77700001, got '$RESULT51'"
fi

rm -rf "$TMPDIR_S9"

###############################################################################
# Section 10 - Idempotency Check (T52-T55)
# Tests the logic at board-transition.sh L147
###############################################################################
echo ""
echo "=== Section 10: Idempotency Check ==="

check_idempotent() {
  local current="$1"
  local target="$2"
  if [[ "$current" == "$target" ]]; then
    echo "skip"
  else
    echo "continue"
  fi
}

# T52: current="Review" target="Review" → skip
if [[ $(check_idempotent "Review" "Review") == "skip" ]]; then
  pass "T52: Review→Review → skip (idempotent)"
else
  fail "T52: Review→Review should skip"
fi

# T53: current="Todo" target="In Progress" → continue
if [[ $(check_idempotent "Todo" "In Progress") == "continue" ]]; then
  pass "T53: Todo→In Progress → continue"
else
  fail "T53: Todo→In Progress should continue"
fi

# T54: current="" target="Done" → continue
if [[ $(check_idempotent "" "Done") == "continue" ]]; then
  pass "T54: (empty)→Done → continue"
else
  fail "T54: (empty)→Done should continue"
fi

# T55: current="null" target="Done" → continue
if [[ $(check_idempotent "null" "Done") == "continue" ]]; then
  pass "T55: 'null'→Done → continue"
else
  fail "T55: 'null'→Done should continue"
fi

###############################################################################
# Section 11 - setup-ai-dlc-board.sh Scale Validation (T56-T63)
# Tests the regex at setup-ai-dlc-board.sh L73
###############################################################################
echo ""
echo "=== Section 11: Scale Validation ==="

validate_scale() {
  [[ "$1" =~ ^(solo|pod|squad|enterprise)$ ]] && echo "accept" || echo "reject"
}

# T56: "solo" → accept
if [[ $(validate_scale "solo") == "accept" ]]; then
  pass "T56: scale 'solo' accepted"
else
  fail "T56: scale 'solo' should be accepted"
fi

# T57: "pod" → accept
if [[ $(validate_scale "pod") == "accept" ]]; then
  pass "T57: scale 'pod' accepted"
else
  fail "T57: scale 'pod' should be accepted"
fi

# T58: "squad" → accept
if [[ $(validate_scale "squad") == "accept" ]]; then
  pass "T58: scale 'squad' accepted"
else
  fail "T58: scale 'squad' should be accepted"
fi

# T59: "enterprise" → accept
if [[ $(validate_scale "enterprise") == "accept" ]]; then
  pass "T59: scale 'enterprise' accepted"
else
  fail "T59: scale 'enterprise' should be accepted"
fi

# T60: "team" → reject
if [[ $(validate_scale "team") == "reject" ]]; then
  pass "T60: scale 'team' rejected"
else
  fail "T60: scale 'team' should be rejected"
fi

# T61: "" → reject
if [[ $(validate_scale "") == "reject" ]]; then
  pass "T61: scale '' rejected"
else
  fail "T61: scale '' should be rejected"
fi

# T62: "sol" → reject
if [[ $(validate_scale "sol") == "reject" ]]; then
  pass "T62: scale 'sol' rejected"
else
  fail "T62: scale 'sol' should be rejected"
fi

# T63: "Solo" (uppercase) → reject
if [[ $(validate_scale "Solo") == "reject" ]]; then
  pass "T63: scale 'Solo' (uppercase) rejected"
else
  fail "T63: scale 'Solo' should be rejected"
fi

###############################################################################
# Section 12 - Scale Field Matrix (T64-T73)
# Tests field generation logic at setup-ai-dlc-board.sh L183-213
###############################################################################
echo ""
echo "=== Section 12: Scale Field Matrix ==="

# Replicate the field generation logic from setup-ai-dlc-board.sh
get_fields_for_scale() {
  local scale="$1"
  local fields=("Priority" "Size")  # Solo: always
  if [[ "$scale" =~ ^(pod|squad|enterprise)$ ]]; then
    fields+=("AI-Confidence" "Turns-Used" "Spec-Link" "Review-Priority")
  fi
  if [[ "$scale" =~ ^(squad|enterprise)$ ]]; then
    fields+=("Component" "Agent-Assigned" "MTTV-Hours" "Rework-Count" "Sprint-Goal" "Blocked-By")
  fi
  if [[ "$scale" == "enterprise" ]]; then
    fields+=("Security-Flag" "Domain-Cluster" "Compliance-Tag" "Cost-USD" "Approval-Status")
  fi
  echo "${fields[@]}"
}

count_fields() {
  local fields
  fields=$(get_fields_for_scale "$1")
  echo "$fields" | wc -w
}

contains_field() {
  local fields
  fields=$(get_fields_for_scale "$1")
  [[ " $fields " == *" $2 "* ]] && echo "yes" || echo "no"
}

# T64: Solo → 2 fields
SOLO_COUNT=$(count_fields "solo")
if [[ "$SOLO_COUNT" == "2" ]]; then
  pass "T64: Solo → $SOLO_COUNT custom fields"
else
  fail "T64: Solo expected 2 fields, got $SOLO_COUNT"
fi

# T65: Pod → 6 fields
POD_COUNT=$(count_fields "pod")
if [[ "$POD_COUNT" == "6" ]]; then
  pass "T65: Pod → $POD_COUNT custom fields"
else
  fail "T65: Pod expected 6 fields, got $POD_COUNT"
fi

# T66: Squad → 12 fields
SQUAD_COUNT=$(count_fields "squad")
if [[ "$SQUAD_COUNT" == "12" ]]; then
  pass "T66: Squad → $SQUAD_COUNT custom fields"
else
  fail "T66: Squad expected 12 fields, got $SQUAD_COUNT"
fi

# T67: Enterprise → 17 fields
ENT_COUNT=$(count_fields "enterprise")
if [[ "$ENT_COUNT" == "17" ]]; then
  pass "T67: Enterprise → $ENT_COUNT custom fields"
else
  fail "T67: Enterprise expected 17 fields, got $ENT_COUNT"
fi

# T68: Solo does NOT have AI-Confidence
if [[ $(contains_field "solo" "AI-Confidence") == "no" ]]; then
  pass "T68: Solo does not have AI-Confidence"
else
  fail "T68: Solo should not have AI-Confidence"
fi

# T69: Pod does NOT have Component
if [[ $(contains_field "pod" "Component") == "no" ]]; then
  pass "T69: Pod does not have Component"
else
  fail "T69: Pod should not have Component"
fi

# T70: Squad does NOT have Security-Flag
if [[ $(contains_field "squad" "Security-Flag") == "no" ]]; then
  pass "T70: Squad does not have Security-Flag"
else
  fail "T70: Squad should not have Security-Flag"
fi

# T71: Enterprise contains all Pod fields
ENT_HAS_POD=true
for f in AI-Confidence Turns-Used Spec-Link Review-Priority; do
  if [[ $(contains_field "enterprise" "$f") != "yes" ]]; then
    ENT_HAS_POD=false
    break
  fi
done
if $ENT_HAS_POD; then
  pass "T71: Enterprise contains all Pod fields"
else
  fail "T71: Enterprise missing Pod field: $f"
fi

# T72: Enterprise contains all Squad fields
ENT_HAS_SQUAD=true
for f in Component Agent-Assigned MTTV-Hours Rework-Count Sprint-Goal Blocked-By; do
  if [[ $(contains_field "enterprise" "$f") != "yes" ]]; then
    ENT_HAS_SQUAD=false
    break
  fi
done
if $ENT_HAS_SQUAD; then
  pass "T72: Enterprise contains all Squad fields"
else
  fail "T72: Enterprise missing Squad field: $f"
fi

# T73: Field types are correct
# Replicate type assignment from setup-ai-dlc-board.sh
get_field_type() {
  case "$1" in
    Priority|Size|Review-Priority|Component|Agent-Assigned|Security-Flag|Domain-Cluster|Compliance-Tag|Approval-Status) echo "SINGLE_SELECT" ;;
    AI-Confidence|Turns-Used|MTTV-Hours|Rework-Count|Cost-USD) echo "NUMBER" ;;
    Spec-Link|Sprint-Goal|Blocked-By) echo "TEXT" ;;
  esac
}

T73_OK=true
[[ $(get_field_type "Priority") != "SINGLE_SELECT" ]] && T73_OK=false
[[ $(get_field_type "AI-Confidence") != "NUMBER" ]] && T73_OK=false
[[ $(get_field_type "Spec-Link") != "TEXT" ]] && T73_OK=false
if $T73_OK; then
  pass "T73: Field types correct (Priority=SINGLE_SELECT, AI-Confidence=NUMBER, Spec-Link=TEXT)"
else
  fail "T73: Field types incorrect"
fi

###############################################################################
# Section 13 - Issue Number Extraction (Implementation-aligned) (T74-T78)
# Tests the actual hook regex: ([0-9]{2,}) + GH-([0-9]+)
# (differs from Section 3's (^|[/_-])([0-9]+)(-|$))
###############################################################################
echo ""
echo "=== Section 13: Issue Number Extraction (Hook Implementation) ==="

# Matches the implementation in board-status-updater.sh L73-77
extract_issue_number_impl() {
  local branch="$1"
  if [[ "$branch" =~ ([0-9]{2,}) ]]; then
    echo "${BASH_REMATCH[1]}"
  elif [[ "$branch" =~ GH-([0-9]+) ]]; then
    echo "${BASH_REMATCH[1]}"
  else
    echo ""
  fi
}

# T74: "feature/42-auth" → 42
RESULT74=$(extract_issue_number_impl "feature/42-auth")
if [[ "$RESULT74" == "42" ]]; then
  pass "T74: feature/42-auth → $RESULT74"
else
  fail "T74: feature/42-auth expected 42, got '$RESULT74'"
fi

# T75: "fix-3-typo" (1 digit) → empty ({2,} requires 2+ digits)
RESULT75=$(extract_issue_number_impl "fix-3-typo")
if [[ -z "$RESULT75" ]]; then
  pass "T75: fix-3-typo → (empty, 1-digit excluded by {2,})"
else
  fail "T75: fix-3-typo expected empty, got '$RESULT75'"
fi

# T76: "GH-5-fix-bug" → 5 (GH- prefix match)
RESULT76=$(extract_issue_number_impl "GH-5-fix-bug")
if [[ "$RESULT76" == "5" ]]; then
  pass "T76: GH-5-fix-bug → $RESULT76"
else
  fail "T76: GH-5-fix-bug expected 5, got '$RESULT76'"
fi

# T77: "main" → empty
RESULT77=$(extract_issue_number_impl "main")
if [[ -z "$RESULT77" ]]; then
  pass "T77: main → (empty)"
else
  fail "T77: main expected empty, got '$RESULT77'"
fi

# T78: "release/2026-02" → 2026 (first 2+ digit match)
RESULT78=$(extract_issue_number_impl "release/2026-02")
if [[ "$RESULT78" == "2026" ]]; then
  pass "T78: release/2026-02 → $RESULT78"
else
  fail "T78: release/2026-02 expected 2026, got '$RESULT78'"
fi

###############################################################################
# Summary
###############################################################################
echo ""
TOTAL=$((PASS+FAIL))
echo "Results: $PASS PASS / $FAIL FAIL / $TOTAL total"

if [[ $FAIL -gt 0 ]]; then
  exit 1
fi
