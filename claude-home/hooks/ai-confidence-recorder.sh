#!/bin/bash
# ai-confidence-recorder.sh - Record AI-Confidence and Turns-Used on PR creation.
# PostToolUse hook (Bash matcher):
#   - `gh pr create` detected → Write AI-Confidence and Turns-Used to board fields
#
# Metrics source:
#   - AI-Confidence: from aggregate-sprint.py session data or default estimate
#   - Turns-Used: from session transcript metadata
#
# Requires CLAUDE.md to contain <github-project> XML tags with field IDs.
# All failures are silent (exit 0) since this is a PostToolUse hook.

input=$(cat)

# Require jq and gh
command -v jq  &>/dev/null || exit 0
command -v gh  &>/dev/null || exit 0

tool_command=$(echo "$input" | jq -r '.tool_input.command // ""')
tool_output=$(echo "$input" | jq -r '.tool_output.stdout // ""')

# Only process gh pr create commands
[[ "$tool_command" == *"gh pr create"* ]] || exit 0

# --- Find CLAUDE.md with <github-project> tags ---
find_claude_md() {
  local dir
  dir=$(git rev-parse --show-toplevel 2>/dev/null) || return 1
  for candidate in "$dir/CLAUDE.md" "$dir/.claude/CLAUDE.md"; do
    if [[ -f "$candidate" ]] && grep -q '<github-project' "$candidate" 2>/dev/null; then
      echo "$candidate"
      return 0
    fi
  done
  return 1
}

CLAUDE_MD=$(find_claude_md) || exit 0

# --- Parse project config ---
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_ID" ]] && exit 0

# Get field IDs for AI-Confidence and Turns-Used
AI_CONFIDENCE_FIELD_ID=$(grep 'name="AI-Confidence"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
TURNS_USED_FIELD_ID=$(grep 'name="Turns-Used"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)

# Need at least one field to proceed
[[ -z "$AI_CONFIDENCE_FIELD_ID" && -z "$TURNS_USED_FIELD_ID" ]] && exit 0

# --- Get issue number from branch ---
branch=$(git branch --show-current 2>/dev/null)
[[ -z "$branch" ]] && exit 0

issue_number=""
if [[ "$branch" =~ ([0-9]{2,}) ]]; then
  issue_number="${BASH_REMATCH[1]}"
elif [[ "$branch" =~ GH-([0-9]+) ]]; then
  issue_number="${BASH_REMATCH[1]}"
fi
[[ -z "$issue_number" ]] && exit 0

# --- Find Item ID ---
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' "$CLAUDE_MD" | head -1)
[[ -z "$OWNER" ]] && exit 0

REPO_NAME=$(git remote get-url origin 2>/dev/null | sed 's/.*\///' | sed 's/\.git$//')
[[ -z "$REPO_NAME" ]] && exit 0

ISSUE_URL="https://github.com/${OWNER}/${REPO_NAME}/issues/${issue_number}"

PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_NUMBER" ]] && exit 0

ITEM_ID=$(gh project item-list "$PROJECT_NUMBER" --owner "$OWNER" --format json --limit 200 2>/dev/null \
  | jq -r ".items[] | select(.content.url == \"$ISSUE_URL\") | .id" 2>/dev/null | head -1)
[[ -z "$ITEM_ID" || "$ITEM_ID" == "null" ]] && exit 0

# --- Compute metrics ---

# Turns-Used: count commits on this branch (proxy for agent turns)
MAIN_BRANCH=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")
TURNS_USED=$(git rev-list --count "${MAIN_BRANCH}..HEAD" 2>/dev/null || echo "0")
# Minimum 1 turn if we got here
[[ "$TURNS_USED" -eq 0 ]] && TURNS_USED=1

# AI-Confidence: try to read from latest metrics file, fallback to estimate
AI_CONFIDENCE=""
PROJECT_DIR=$(git rev-parse --show-toplevel 2>/dev/null)
METRICS_DIR="${PROJECT_DIR}/.claude/metrics"
if [[ -d "$METRICS_DIR" ]]; then
  LATEST_METRIC=$(ls -t "$METRICS_DIR"/*.json 2>/dev/null | head -1)
  if [[ -n "$LATEST_METRIC" ]]; then
    AI_CONFIDENCE=$(jq -r '.ai_confidence // empty' "$LATEST_METRIC" 2>/dev/null)
  fi
fi

# Fallback: estimate from turns (fewer turns = higher confidence)
if [[ -z "$AI_CONFIDENCE" ]]; then
  if [[ "$TURNS_USED" -le 3 ]]; then
    AI_CONFIDENCE=85
  elif [[ "$TURNS_USED" -le 7 ]]; then
    AI_CONFIDENCE=70
  elif [[ "$TURNS_USED" -le 15 ]]; then
    AI_CONFIDENCE=55
  else
    AI_CONFIDENCE=40
  fi
fi

# --- Write metrics to board ---

# Write AI-Confidence
if [[ -n "$AI_CONFIDENCE_FIELD_ID" && -n "$AI_CONFIDENCE" ]]; then
  gh project item-edit \
    --project-id "$PROJECT_ID" \
    --id "$ITEM_ID" \
    --field-id "$AI_CONFIDENCE_FIELD_ID" \
    --number "$AI_CONFIDENCE" \
    &>/dev/null || true
  sleep 1
fi

# Write Turns-Used
if [[ -n "$TURNS_USED_FIELD_ID" && -n "$TURNS_USED" ]]; then
  gh project item-edit \
    --project-id "$PROJECT_ID" \
    --id "$ITEM_ID" \
    --field-id "$TURNS_USED_FIELD_ID" \
    --number "$TURNS_USED" \
    &>/dev/null || true
fi

exit 0
