#!/bin/bash
# board-status-updater.sh - Auto-update GitHub Projects V2 board status.
# PostToolUse hook (Bash matcher):
#   - `gh pr create` detected → Issue Status → Review
#   - First `git push` detected → Issue Status → In Progress
#
# Requires CLAUDE.md to contain <github-project> XML tags with field/option IDs.
# All failures are silent (exit 0) since this is a PostToolUse hook.

input=$(cat)

# Require jq and gh
command -v jq  &>/dev/null || exit 0
command -v gh  &>/dev/null || exit 0

tool_command=$(echo "$input" | jq -r '.tool_input.command // ""')
tool_output=$(echo "$input" | jq -r '.tool_output.stdout // ""')

# --- Detect trigger type ---
TRIGGER=""
if [[ "$tool_command" == *"gh pr create"* ]]; then
  TRIGGER="pr_create"
elif [[ "$tool_command" == *"git push"* ]]; then
  # Skip failed pushes
  if [[ "$tool_output" == *"rejected"* ]] || [[ "$tool_output" == *"error"* ]] || [[ "$tool_output" == *"fatal"* ]]; then
    exit 0
  fi
  TRIGGER="git_push"
else
  exit 0
fi

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

# --- Parse project config from XML tags ---
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_ID" ]] && exit 0

# Get Status field ID
STATUS_FIELD_ID=$(grep -A1 'name="Status"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
[[ -z "$STATUS_FIELD_ID" ]] && exit 0

# --- Determine target status ---
TARGET_STATUS=""
case "$TRIGGER" in
  pr_create)  TARGET_STATUS="Review" ;;
  git_push)   TARGET_STATUS="In Progress" ;;
esac

# Get target status option ID
TARGET_OPTION_ID=$(grep -B0 -A0 "name=\"$TARGET_STATUS\"" "$CLAUDE_MD" \
  | grep -oP '<option name="'"$TARGET_STATUS"'" id="\K[^"]+' | head -1)
[[ -z "$TARGET_OPTION_ID" ]] && exit 0

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

# For PR create, also try to extract issue from PR body output
if [[ "$TRIGGER" == "pr_create" && -z "$issue_number" ]]; then
  issue_number=$(echo "$tool_output" | grep -oP '(?:Fixes|Closes|Resolves) #\K\d+' | head -1)
  [[ -z "$issue_number" ]] && exit 0
fi

# For git_push, skip if Status is already beyond "In Progress" (e.g., Review/Done)
# We only want the first push to set In Progress

# --- Find Item ID for the issue in the project ---
OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' "$CLAUDE_MD" | head -1)
[[ -z "$OWNER" ]] && exit 0

REPO_NAME=$(git remote get-url origin 2>/dev/null | sed 's/.*\///' | sed 's/\.git$//')
[[ -z "$REPO_NAME" ]] && exit 0

ISSUE_URL="https://github.com/${OWNER}/${REPO_NAME}/issues/${issue_number}"

# Get project number from URL
PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_NUMBER" ]] && exit 0

ITEM_ID=$(gh project item-list "$PROJECT_NUMBER" --owner "$OWNER" --format json --limit 200 2>/dev/null \
  | jq -r ".items[] | select(.content.url == \"$ISSUE_URL\") | .id" 2>/dev/null | head -1)
[[ -z "$ITEM_ID" || "$ITEM_ID" == "null" ]] && exit 0

# --- Update status ---
gh project item-edit \
  --project-id "$PROJECT_ID" \
  --id "$ITEM_ID" \
  --field-id "$STATUS_FIELD_ID" \
  --single-select-option-id "$TARGET_OPTION_ID" \
  &>/dev/null || true

exit 0
