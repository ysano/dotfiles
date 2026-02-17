#!/bin/bash
set -euo pipefail

###############################################################################
# Integration tests for GitHub Projects V2 board management scripts.
# Uses mock-gh.sh to avoid real API calls.
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

# --- Locate scripts relative to this test file ---
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BOARD_TRANSITION="$REPO_ROOT/claude-home/skills/github-projects-v2/scripts/board-transition.sh"
SETUP_BOARD="$REPO_ROOT/claude-home/skills/github-projects-v2/scripts/setup-ai-dlc-board.sh"
BOARD_STATUS_UPDATER="$REPO_ROOT/claude-home/hooks/board-status-updater.sh"
AI_CONFIDENCE_RECORDER="$REPO_ROOT/claude-home/hooks/ai-confidence-recorder.sh"
MOCK_GH="$SCRIPT_DIR/mock-gh.sh"

# Save original PATH/dir for reset between tests
ORIG_PATH="$PATH"
ORIG_DIR="$(pwd)"

# --- Shared setup: sets TEST_TMPDIR, PATH, MOCK_GH_LOG in current shell ---
setup_test_env() {
  TEST_TMPDIR=$(mktemp -d)
  cp "$MOCK_GH" "$TEST_TMPDIR/gh"
  chmod +x "$TEST_TMPDIR/gh"
  git init -q "$TEST_TMPDIR/repo"
  cd "$TEST_TMPDIR/repo"
  git config user.email "test@example.com"
  git config user.name "Test User"
  git remote add origin "https://github.com/testuser/testrepo.git"
  cat > "$TEST_TMPDIR/repo/CLAUDE.md" <<'XMLEOF'
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
  </field>
  <field name="AI-Confidence" id="PVTF_confidence001"/>
  <field name="Turns-Used" id="PVTF_turns001"/>
</github-project>
XMLEOF
  git add -A && git commit -q -m "init" --allow-empty
  # Set up fake remote tracking so hooks can find main branch
  local default_branch
  default_branch=$(git branch --show-current)
  git update-ref "refs/remotes/origin/$default_branch" "$(git rev-parse HEAD)"
  git symbolic-ref refs/remotes/origin/HEAD "refs/remotes/origin/$default_branch"
  export MOCK_GH_LOG="$TEST_TMPDIR/gh-calls.log"
  export PATH="$TEST_TMPDIR:$ORIG_PATH"
  export RATE_LIMIT_DELAY=0
}

teardown_test_env() {
  cd "$ORIG_DIR"
  rm -rf "$TEST_TMPDIR"
  export PATH="$ORIG_PATH"
  unset MOCK_GH_LOG RATE_LIMIT_DELAY
}

###############################################################################
# Section I1 - board-transition.sh Full Flow (T-I01 to T-I08)
###############################################################################
echo "=== Section I1: board-transition.sh Full Flow ==="

# T-I01: Todo→Review transition: item-edit with correct option ID
setup_test_env
bash "$BOARD_TRANSITION" 42 "Review" > /dev/null 2>&1
if grep -q -- '--single-select-option-id 98456003' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I01: Todo→Review transition uses correct option ID 98456003"
else
  fail "T-I01: Expected item-edit with option ID 98456003. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I02: Idempotent skip: issue 99 already Review → no item-edit
setup_test_env
bash "$BOARD_TRANSITION" 99 "Review" > /dev/null 2>&1
if ! grep -q 'item-edit' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I02: Idempotent skip: already Review, no item-edit"
else
  fail "T-I02: item-edit should not be called for already-Review issue"
fi
teardown_test_env

# T-I03: Issue not in project → exit 0, no mutation
setup_test_env
EXIT_I3=0
bash "$BOARD_TRANSITION" 777 "Done" > /dev/null 2>&1 || EXIT_I3=$?
if [[ $EXIT_I3 -eq 0 ]] && ! grep -q 'item-edit' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I03: Issue not in project → exit 0, no mutation"
else
  fail "T-I03: Expected exit 0 and no item-edit for missing issue"
fi
teardown_test_env

# T-I04: Non-existent status name → exit 1
setup_test_env
EXIT_I4=0
bash "$BOARD_TRANSITION" 42 "Blocked" > /dev/null 2>&1 || EXIT_I4=$?
if [[ $EXIT_I4 -ne 0 ]]; then
  pass "T-I04: Non-existent status → exit 1"
else
  fail "T-I04: Expected exit 1 for non-existent status (exit=$EXIT_I4)"
fi
teardown_test_env

# T-I05: CLAUDE.md absent → exit 1
setup_test_env
rm -f "$TEST_TMPDIR/repo/CLAUDE.md"
EXIT_I5=0
bash "$BOARD_TRANSITION" 42 "Review" > /dev/null 2>&1 || EXIT_I5=$?
if [[ $EXIT_I5 -ne 0 ]]; then
  pass "T-I05: CLAUDE.md absent → exit 1"
else
  fail "T-I05: Expected exit 1 when CLAUDE.md missing"
fi
teardown_test_env

# T-I06: Non-numeric issue_number → exit 1
setup_test_env
EXIT_I6=0
bash "$BOARD_TRANSITION" "abc" "Review" > /dev/null 2>&1 || EXIT_I6=$?
if [[ $EXIT_I6 -ne 0 ]]; then
  pass "T-I06: Non-numeric issue_number → exit 1"
else
  fail "T-I06: Expected exit 1 for non-numeric issue_number"
fi
teardown_test_env

# T-I07: No arguments → exit 1 (usage)
setup_test_env
EXIT_I7=0
bash "$BOARD_TRANSITION" > /dev/null 2>&1 || EXIT_I7=$?
if [[ $EXIT_I7 -ne 0 ]]; then
  pass "T-I07: No arguments → exit 1"
else
  fail "T-I07: Expected exit 1 with no arguments"
fi
teardown_test_env

# T-I08: SSH remote format → correct URL construction
setup_test_env
git remote set-url origin "git@github.com:testuser/testrepo.git"
bash "$BOARD_TRANSITION" 42 "Review" > /dev/null 2>&1
if grep -q 'item-list' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I08: SSH remote format → gh commands executed"
else
  fail "T-I08: SSH remote should still trigger gh commands"
fi
teardown_test_env

###############################################################################
# Section I2 - board-status-updater.sh Hook Flow (T-I09 to T-I16)
###############################################################################
echo ""
echo "=== Section I2: board-status-updater.sh Hook Flow ==="

run_hook_updater() {
  local stdin_json="$1"
  printf '%s\n' "$stdin_json" | bash "$BOARD_STATUS_UPDATER" 2>/dev/null
  return $?
}

# T-I09: gh pr create → Review item-edit
setup_test_env
git checkout -qb "feature/42-add-auth"
STDIN_I9='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"gh pr create --title \"feat: add auth\""},"tool_output":{"stdout":"https://github.com/testuser/testrepo/pull/42\n","stderr":""}}'
run_hook_updater "$STDIN_I9" || true
if grep -q -- '--single-select-option-id 98456003' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I09: gh pr create → Review (option ID 98456003)"
else
  fail "T-I09: Expected item-edit with Review option ID. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I10: git push → In Progress item-edit
setup_test_env
git checkout -qb "feature/42-add-auth"
STDIN_I10='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push -u origin feature/42-add-auth"},"tool_output":{"stdout":"","stderr":""}}'
run_hook_updater "$STDIN_I10" || true
if grep -q -- '--single-select-option-id 98456002' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I10: git push → In Progress (option ID 98456002)"
else
  fail "T-I10: Expected item-edit with In Progress option ID. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I11: push rejected → exit 0, no gh calls
setup_test_env
git checkout -qb "feature/42-add-auth"
STDIN_I11='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push"},"tool_output":{"stdout":"! [rejected] main -> main (fetch first)","stderr":""}}'
run_hook_updater "$STDIN_I11" || true
if [[ ! -s "$MOCK_GH_LOG" ]]; then
  pass "T-I11: push rejected → no gh calls"
else
  fail "T-I11: Expected no gh calls for rejected push. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I12: git status (non-trigger) → exit 0, no gh calls
setup_test_env
STDIN_I12='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git status"},"tool_output":{"stdout":"","stderr":""}}'
run_hook_updater "$STDIN_I12" || true
if [[ ! -s "$MOCK_GH_LOG" ]]; then
  pass "T-I12: git status (non-trigger) → no gh calls"
else
  fail "T-I12: Expected no gh calls for git status"
fi
teardown_test_env

# T-I13: No issue number in branch (master) → exit 0, no item-edit
setup_test_env
STDIN_I13='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push"},"tool_output":{"stdout":"","stderr":""}}'
run_hook_updater "$STDIN_I13" || true
if ! grep -q 'item-edit' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I13: No issue in branch → no item-edit"
else
  fail "T-I13: Expected no item-edit for branch without issue number"
fi
teardown_test_env

# T-I14: CLAUDE.md absent → exit 0 (silent)
setup_test_env
git checkout -qb "feature/42-test"
rm -f "$TEST_TMPDIR/repo/CLAUDE.md"
git add -A && git commit -q -m "rm claude"
STDIN_I14='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push"},"tool_output":{"stdout":"","stderr":""}}'
EXIT_I14=0
run_hook_updater "$STDIN_I14" || EXIT_I14=$?
if [[ $EXIT_I14 -eq 0 ]]; then
  pass "T-I14: CLAUDE.md absent → exit 0 (silent)"
else
  fail "T-I14: Expected exit 0 when CLAUDE.md missing"
fi
teardown_test_env

# T-I15: jq guard logic validation
JQ_GUARD='command -v jq &>/dev/null || exit 0'
if bash -c "$JQ_GUARD"; then
  pass "T-I15: jq present → guard passes (jq available in test env)"
else
  fail "T-I15: jq guard check failed"
fi

# T-I16: Full pipeline: trigger→branch→XML→API
setup_test_env
git checkout -qb "feature/42-full-pipeline"
STDIN_I16='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"gh pr create --title \"test\""},"tool_output":{"stdout":"https://github.com/testuser/testrepo/pull/42\n","stderr":""}}'
run_hook_updater "$STDIN_I16" || true
if grep -q 'item-list' "$MOCK_GH_LOG" 2>/dev/null && \
   grep -q 'item-edit' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I16: Full pipeline: trigger→branch→XML→item-list→item-edit"
else
  fail "T-I16: Expected both item-list and item-edit in log. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

###############################################################################
# Section I3 - ai-confidence-recorder.sh Hook Flow (T-I17 to T-I24)
###############################################################################
echo ""
echo "=== Section I3: ai-confidence-recorder.sh Hook Flow ==="

run_hook_confidence() {
  local stdin_json="$1"
  printf '%s\n' "$stdin_json" | bash "$AI_CONFIDENCE_RECORDER" 2>/dev/null
  return $?
}

PR_CREATE_STDIN='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"gh pr create --title \"test\""},"tool_output":{"stdout":"https://github.com/testuser/testrepo/pull/42\n","stderr":""}}'

# T-I17: 3 commits → AI-Confidence 85, Turns-Used 3
setup_test_env
git checkout -qb "feature/42-conf"
for i in 1 2 3; do
  echo "$i" > "file$i.txt"
  git add "file$i.txt" && git commit -q -m "commit $i"
done
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 85' "$MOCK_GH_LOG" 2>/dev/null && \
   grep -q -- '--number 3' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I17: 3 commits → AI-Confidence 85, Turns-Used 3"
else
  fail "T-I17: Expected --number 85 and --number 3. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I18: 7 commits → AI-Confidence 70
setup_test_env
git checkout -qb "feature/42-conf7"
for i in $(seq 1 7); do
  echo "$i" > "file$i.txt"
  git add "file$i.txt" && git commit -q -m "commit $i"
done
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 70' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I18: 7 commits → AI-Confidence 70"
else
  fail "T-I18: Expected --number 70. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I19: 10 commits → AI-Confidence 55
setup_test_env
git checkout -qb "feature/42-conf10"
for i in $(seq 1 10); do
  echo "$i" > "file$i.txt"
  git add "file$i.txt" && git commit -q -m "commit $i"
done
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 55' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I19: 10 commits → AI-Confidence 55"
else
  fail "T-I19: Expected --number 55. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I20: 20 commits → AI-Confidence 40
setup_test_env
git checkout -qb "feature/42-conf20"
for i in $(seq 1 20); do
  echo "$i" > "file$i.txt"
  git add "file$i.txt" && git commit -q -m "commit $i"
done
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 40' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I20: 20 commits → AI-Confidence 40"
else
  fail "T-I20: Expected --number 40. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I21: 0 commits (new branch, no divergence) → Turns-Used minimum 1
setup_test_env
git checkout -qb "feature/42-zero"
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 1' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I21: 0 commits → Turns-Used minimum 1"
else
  fail "T-I21: Expected --number 1 for minimum turns. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I22: Metrics file present → use file's value
setup_test_env
git checkout -qb "feature/42-metrics"
echo "x" > f.txt && git add f.txt && git commit -q -m "c1"
mkdir -p .claude/metrics
echo '{"ai_confidence": 92}' > .claude/metrics/session-001.json
run_hook_confidence "$PR_CREATE_STDIN" || true
if grep -q -- '--number 92' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I22: Metrics file → AI-Confidence 92 (from file)"
else
  fail "T-I22: Expected --number 92 from metrics file. Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

# T-I23: Non-PR-create command → skip
setup_test_env
git checkout -qb "feature/42-nope"
STDIN_I23='{"hook_event_name":"PostToolUse","tool_name":"Bash","tool_input":{"command":"git push"},"tool_output":{"stdout":"","stderr":""}}'
run_hook_confidence "$STDIN_I23" || true
if [[ ! -s "$MOCK_GH_LOG" ]]; then
  pass "T-I23: Non-PR-create command → skip (no gh calls)"
else
  fail "T-I23: Expected no gh calls for git push"
fi
teardown_test_env

# T-I24: XML without AI-Confidence field → only Turns-Used recorded
setup_test_env
git checkout -qb "feature/42-noconf"
echo "x" > f.txt && git add f.txt && git commit -q -m "c1"
cat > "$TEST_TMPDIR/repo/CLAUDE.md" <<'XMLEOF'
## Task Management
<github-project id="PVT_test123" url="https://github.com/users/testuser/projects/4">
  <field name="Status" id="PVTSSF_status001">
    <option name="Todo" id="98456001"/>
  </field>
  <field name="Turns-Used" id="PVTF_turns001"/>
</github-project>
XMLEOF
git add -A && git commit -q -m "update claude"
run_hook_confidence "$PR_CREATE_STDIN" || true
EDIT_COUNT=$(grep -c 'item-edit' "$MOCK_GH_LOG" 2>/dev/null || echo "0")
if [[ "$EDIT_COUNT" == "1" ]] && grep -q -- "--field-id PVTF_turns001" "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I24: No AI-Confidence field → only Turns-Used recorded"
else
  fail "T-I24: Expected 1 item-edit for Turns-Used only. Count=$EDIT_COUNT, Log: $(cat "$MOCK_GH_LOG" 2>/dev/null)"
fi
teardown_test_env

###############################################################################
# Section I4 - setup-ai-dlc-board.sh Full Flow (T-I25 to T-I32)
###############################################################################
echo ""
echo "=== Section I4: setup-ai-dlc-board.sh Full Flow ==="

# T-I25: Solo → field-create 2 times
setup_test_env
bash "$SETUP_BOARD" --owner testuser --title "Test Solo" --scale solo > /dev/null 2>&1 || true
FIELD_CREATE_COUNT=$(grep -c 'field-create' "$MOCK_GH_LOG" 2>/dev/null || echo "0")
if [[ "$FIELD_CREATE_COUNT" == "2" ]]; then
  pass "T-I25: Solo → field-create $FIELD_CREATE_COUNT times"
else
  fail "T-I25: Solo expected 2 field-create, got $FIELD_CREATE_COUNT"
fi
teardown_test_env

# T-I26: Pod → field-create 6 times
setup_test_env
bash "$SETUP_BOARD" --owner testuser --title "Test Pod" --scale pod > /dev/null 2>&1 || true
FIELD_CREATE_COUNT=$(grep -c 'field-create' "$MOCK_GH_LOG" 2>/dev/null || echo "0")
if [[ "$FIELD_CREATE_COUNT" == "6" ]]; then
  pass "T-I26: Pod → field-create $FIELD_CREATE_COUNT times"
else
  fail "T-I26: Pod expected 6 field-create, got $FIELD_CREATE_COUNT"
fi
teardown_test_env

# T-I27: Squad → field-create 12 times
setup_test_env
bash "$SETUP_BOARD" --owner testuser --title "Test Squad" --scale squad > /dev/null 2>&1 || true
FIELD_CREATE_COUNT=$(grep -c 'field-create' "$MOCK_GH_LOG" 2>/dev/null || echo "0")
if [[ "$FIELD_CREATE_COUNT" == "12" ]]; then
  pass "T-I27: Squad → field-create $FIELD_CREATE_COUNT times"
else
  fail "T-I27: Squad expected 12 field-create, got $FIELD_CREATE_COUNT"
fi
teardown_test_env

# T-I28: Enterprise → field-create 17 times
setup_test_env
bash "$SETUP_BOARD" --owner testuser --title "Test Ent" --scale enterprise > /dev/null 2>&1 || true
FIELD_CREATE_COUNT=$(grep -c 'field-create' "$MOCK_GH_LOG" 2>/dev/null || echo "0")
if [[ "$FIELD_CREATE_COUNT" == "17" ]]; then
  pass "T-I28: Enterprise → field-create $FIELD_CREATE_COUNT times"
else
  fail "T-I28: Enterprise expected 17 field-create, got $FIELD_CREATE_COUNT"
fi
teardown_test_env

# T-I29: XML output contains <github-project id=
setup_test_env
OUTPUT_I29=$(bash "$SETUP_BOARD" --owner testuser --title "Test XML" --scale pod 2>/dev/null) || true
if echo "$OUTPUT_I29" | grep -q '<github-project id='; then
  pass "T-I29: XML output contains <github-project id="
else
  fail "T-I29: Expected <github-project id= in output"
fi

# T-I30: XML output contains scale="pod"
if echo "$OUTPUT_I29" | grep -q 'scale="pod"'; then
  pass "T-I30: XML output contains scale=\"pod\""
else
  fail "T-I30: Expected scale=\"pod\" in output"
fi
teardown_test_env

# T-I31: --repo → project link called
setup_test_env
bash "$SETUP_BOARD" --owner testuser --title "Test Link" --scale solo --repo testrepo > /dev/null 2>&1 || true
if grep -q 'project link' "$MOCK_GH_LOG" 2>/dev/null; then
  pass "T-I31: --repo → project link called"
else
  fail "T-I31: Expected 'project link' in log"
fi
teardown_test_env

# T-I32: --owner missing → exit 1
EXIT_I32=0
bash "$SETUP_BOARD" --title "No Owner" --scale solo > /dev/null 2>&1 || EXIT_I32=$?
if [[ $EXIT_I32 -ne 0 ]]; then
  pass "T-I32: --owner missing → exit 1"
else
  fail "T-I32: Expected exit 1 when --owner missing"
fi

###############################################################################
# Section I5 - REPO_NAME Parsing (T-I33 to T-I37)
###############################################################################
echo ""
echo "=== Section I5: REPO_NAME Parsing ==="

# board-transition.sh pattern: sed 's/.*[:/]//' | sed 's/\.git$//'
parse_repo_transition() {
  echo "$1" | sed 's/.*[:/]//' | sed 's/\.git$//'
}

# hooks pattern: sed 's/.*\///' | sed 's/\.git$//'
parse_repo_hooks() {
  echo "$1" | sed 's/.*\///' | sed 's/\.git$//'
}

# T-I33: HTTPS with .git
RESULT_I33=$(parse_repo_transition "https://github.com/user/repo.git")
if [[ "$RESULT_I33" == "repo" ]]; then
  pass "T-I33: HTTPS .git → '$RESULT_I33'"
else
  fail "T-I33: Expected 'repo', got '$RESULT_I33'"
fi

# T-I34: SSH format
RESULT_I34=$(parse_repo_transition "git@github.com:user/repo.git")
if [[ "$RESULT_I34" == "repo" ]]; then
  pass "T-I34: SSH format → '$RESULT_I34'"
else
  fail "T-I34: Expected 'repo', got '$RESULT_I34'"
fi

# T-I35: HTTPS without .git
RESULT_I35=$(parse_repo_transition "https://github.com/user/repo")
if [[ "$RESULT_I35" == "repo" ]]; then
  pass "T-I35: HTTPS no .git → '$RESULT_I35'"
else
  fail "T-I35: Expected 'repo', got '$RESULT_I35'"
fi

# T-I36: Org HTTPS with .git
RESULT_I36=$(parse_repo_transition "https://github.com/org/repo.git")
if [[ "$RESULT_I36" == "repo" ]]; then
  pass "T-I36: Org HTTPS .git → '$RESULT_I36'"
else
  fail "T-I36: Expected 'repo', got '$RESULT_I36'"
fi

# T-I37: hooks pattern equivalence
URLS=("https://github.com/user/repo.git" "git@github.com:user/repo.git" "https://github.com/user/repo")
ALL_MATCH=true
MISMATCH_URL=""
for url in "${URLS[@]}"; do
  R1=$(parse_repo_transition "$url")
  R2=$(parse_repo_hooks "$url")
  if [[ "$R1" != "$R2" ]]; then
    ALL_MATCH=false
    MISMATCH_URL="$url"
    break
  fi
done
if $ALL_MATCH; then
  pass "T-I37: Both sed patterns produce identical results"
else
  fail "T-I37: Patterns differ for URL '$MISMATCH_URL'"
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
