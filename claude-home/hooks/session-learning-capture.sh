#!/bin/bash
# Stop hook: Prompt session learning capture before ending.
# This hook only outputs a reminder; it does not write files automatically.

cd "${CLAUDE_PROJECT_DIR:-.}" 2>/dev/null || exit 0
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0

# Skip if last commit already captured learnings (prevent re-trigger loop)
last_msg=$(git log -1 --pretty=%B 2>/dev/null)
echo "$last_msg" | grep -qi "session.learning\|memory.*update\|CLAUDE\.md" && exit 0

# Skip if no meaningful activity
has_changes=$(git diff --stat HEAD 2>/dev/null)
has_staged=$(git diff --cached --stat 2>/dev/null)
recent_commits=$(git log --since="2 hours ago" --oneline 2>/dev/null | head -1)

[ -z "$has_changes" ] && [ -z "$has_staged" ] && [ -z "$recent_commits" ] && exit 0

echo "Session had changes. Review for learnings to persist to memory files (CLAUDE.md / CLAUDE.local.md)."
