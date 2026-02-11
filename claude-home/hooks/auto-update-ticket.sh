#!/bin/bash
# Auto-update ticket hook for AI-DLC workflow.
# PostToolUse hook: after a successful git push, comments on the associated GitHub issue.
# All failures are silent (exit 0) since this is a PostToolUse hook.

# Read JSON input from stdin
input=$(cat)

# Require jq
if ! command -v jq &> /dev/null; then
    exit 0
fi

# Extract tool input
tool_command=$(echo "$input" | jq -r '.tool_input.command // ""')
tool_output=$(echo "$input" | jq -r '.tool_output.stdout // ""')

# Only process git push commands
if [[ "$tool_command" != *"git push"* ]]; then
    exit 0
fi

# Check for push success indicators in output
if [[ "$tool_output" == *"rejected"* ]] || [[ "$tool_output" == *"error"* ]] || [[ "$tool_output" == *"fatal"* ]]; then
    exit 0
fi

# Require gh CLI
if ! command -v gh &> /dev/null; then
    exit 0
fi

# Get current branch name
branch=$(git branch --show-current 2>/dev/null)
if [[ -z "$branch" ]]; then
    exit 0
fi

# Extract issue number from branch name
# Patterns: feature/123-xxx, fix/GH-123, issue-123, 123-xxx, etc.
issue_number=""
if [[ "$branch" =~ ([0-9]{2,}) ]]; then
    issue_number="${BASH_REMATCH[1]}"
elif [[ "$branch" =~ GH-([0-9]+) ]]; then
    issue_number="${BASH_REMATCH[1]}"
fi

if [[ -z "$issue_number" ]]; then
    exit 0
fi

# Get latest commit info
commit_hash=$(git rev-parse --short HEAD 2>/dev/null)
commit_message=$(git log -1 --pretty=format:"%s" 2>/dev/null)

if [[ -z "$commit_hash" ]]; then
    exit 0
fi

# Post comment to issue (silent failure)
comment_body=$(cat <<EOF
Auto-linked: \`${commit_hash}\` pushed to \`${branch}\`

> ${commit_message}
EOF
)

gh issue comment "$issue_number" --body "$comment_body" &> /dev/null || true

exit 0
