#!/usr/bin/env bash
# board-transition.sh - Generic status transition for GitHub Projects V2.
# Reads project config from CLAUDE.md <github-project> XML tags.
#
# Usage:
#   board-transition.sh <issue_number> <target_status>
#
# Examples:
#   board-transition.sh 42 "In Progress"
#   board-transition.sh 7 Done
#   board-transition.sh 15 Review
#
# Requirements:
#   - gh CLI with `project` scope: gh auth refresh -s project
#   - jq
#   - CLAUDE.md with <github-project> XML tags (see setup-ai-dlc-board.sh)
#
# Behavior:
#   - Idempotent: skips update if already at target status
#   - Rate limited: 1 second between mutations
#   - Exit 0 if issue not found in project (may not be added yet)
#   - Exit 1 on config or validation errors

set -euo pipefail

RATE_LIMIT_DELAY=1  # seconds between mutations

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- Helpers ---
info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
ok()    { echo -e "${GREEN}[OK]${NC} $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }
die()   { error "$@"; exit 1; }

# --- Usage ---
usage() {
  cat <<'EOF'
Usage: board-transition.sh <issue_number> <target_status>

Arguments:
  issue_number    GitHub issue number (e.g., 42)
  target_status   Target status name (e.g., "In Progress", "Review", "Done")

Examples:
  board-transition.sh 42 "In Progress"
  board-transition.sh 7 Done
  board-transition.sh 15 Review
EOF
  exit 1
}

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

# --- Parse Arguments ---
[[ $# -lt 2 ]] && usage

ISSUE_NUMBER="$1"
TARGET_STATUS="$2"

[[ "$ISSUE_NUMBER" =~ ^[0-9]+$ ]] || die "issue_number must be a positive integer, got: $ISSUE_NUMBER"
[[ -z "$TARGET_STATUS" ]] && die "target_status must not be empty"

# --- Validate Dependencies ---
command -v gh  &>/dev/null || die "gh CLI not found. Install: https://cli.github.com/"
command -v jq  &>/dev/null || die "jq not found. Install: https://jqlang.github.io/jq/"

# --- Find CLAUDE.md ---
CLAUDE_MD=$(find_claude_md) || die "CLAUDE.md with <github-project> tags not found. Searched from git root for CLAUDE.md and .claude/CLAUDE.md"
info "Using config: $CLAUDE_MD"

# --- Parse project config from XML tags ---
PROJECT_ID=$(grep -oP '<github-project id="\K[^"]+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_ID" ]] && die "PROJECT_ID not found in <github-project> tag in $CLAUDE_MD"

PROJECT_NUMBER=$(grep -oP 'url="https://github.com/(?:users|orgs)/[^/]+/projects/\K\d+' "$CLAUDE_MD" | head -1)
[[ -z "$PROJECT_NUMBER" ]] && die "PROJECT_NUMBER not found in <github-project> url attribute in $CLAUDE_MD"

OWNER=$(grep -oP 'url="https://github.com/(?:users|orgs)/\K[^/"]+' "$CLAUDE_MD" | head -1)
[[ -z "$OWNER" ]] && die "OWNER not found in <github-project> url attribute in $CLAUDE_MD"

STATUS_FIELD_ID=$(grep -A1 'name="Status"' "$CLAUDE_MD" | grep -oP 'id="\K[^"]+' | head -1)
[[ -z "$STATUS_FIELD_ID" ]] && die "STATUS_FIELD_ID not found. Ensure <field name=\"Status\" id=\"...\"> exists in $CLAUDE_MD"

info "Project: owner=$OWNER number=$PROJECT_NUMBER"

# --- Find target status option ID ---
TARGET_OPTION_ID=$(grep -B0 -A0 "name=\"$TARGET_STATUS\"" "$CLAUDE_MD" \
  | grep -oP '<option name="'"$TARGET_STATUS"'" id="\K[^"]+' | head -1)

if [[ -z "$TARGET_OPTION_ID" ]]; then
  # Show available options for a helpful error message
  AVAILABLE_OPTIONS=$(grep -oP '<option name="\K[^"]+' "$CLAUDE_MD" 2>/dev/null | sort -u | paste -sd ', ' -)
  die "Status '$TARGET_STATUS' not found in CLAUDE.md options. Available: $AVAILABLE_OPTIONS"
fi

info "Target status: '$TARGET_STATUS' (option_id=$TARGET_OPTION_ID)"

# --- Resolve issue URL ---
REPO_NAME=$(git remote get-url origin 2>/dev/null | sed 's/.*[:/]//' | sed 's/\.git$//')
[[ -z "$REPO_NAME" ]] && die "Could not determine repository name from git remote"

# Handle both HTTPS and SSH remote formats: owner may already be in REPO_NAME
if [[ "$REPO_NAME" == */* ]]; then
  ISSUE_URL="https://github.com/${REPO_NAME}/issues/${ISSUE_NUMBER}"
else
  ISSUE_URL="https://github.com/${OWNER}/${REPO_NAME}/issues/${ISSUE_NUMBER}"
fi

info "Issue URL: $ISSUE_URL"

# --- Find item ID in project ---
info "Looking up issue #$ISSUE_NUMBER in project..."

ITEMS_JSON=$(gh project item-list "$PROJECT_NUMBER" --owner "$OWNER" --format json --limit 500 2>&1) || \
  die "Failed to list project items: $ITEMS_JSON"

ITEM_ID=$(echo "$ITEMS_JSON" | jq -r ".items[] | select(.content.url == \"$ISSUE_URL\") | .id" 2>/dev/null | head -1)

if [[ -z "$ITEM_ID" || "$ITEM_ID" == "null" ]]; then
  ok "Issue #$ISSUE_NUMBER not found in project (may not be added yet). Skipping."
  exit 0
fi

info "Item ID: $ITEM_ID"

# --- Check current status (idempotent) ---
CURRENT_STATUS=$(echo "$ITEMS_JSON" | jq -r ".items[] | select(.id == \"$ITEM_ID\") | .status" 2>/dev/null | head -1)

if [[ "$CURRENT_STATUS" == "$TARGET_STATUS" ]]; then
  ok "Issue #$ISSUE_NUMBER is already '$TARGET_STATUS'. No update needed."
  exit 0
fi

if [[ -n "$CURRENT_STATUS" && "$CURRENT_STATUS" != "null" ]]; then
  info "Current status: '$CURRENT_STATUS' -> '$TARGET_STATUS'"
else
  info "Current status: (none) -> '$TARGET_STATUS'"
fi

# --- Update status ---
sleep "$RATE_LIMIT_DELAY"

UPDATE_OUTPUT=$(gh project item-edit \
  --project-id "$PROJECT_ID" \
  --id "$ITEM_ID" \
  --field-id "$STATUS_FIELD_ID" \
  --single-select-option-id "$TARGET_OPTION_ID" 2>&1) || \
  die "Failed to update status: $UPDATE_OUTPUT"

ok "Issue #$ISSUE_NUMBER status updated to '$TARGET_STATUS'"
