#!/bin/bash
# Claude Code Status Display for tmux status bar (read-only)
# This script only reads status files without triggering notifications

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
STATUS_DIR="$HOME/.tmux/status"
STATUS_FILE="$STATUS_DIR/window-${WINDOW_ID}.status"

# Read current status from file
if [ -f "$STATUS_FILE" ]; then
    cat "$STATUS_FILE" 2>/dev/null
else
    # No status file exists - return empty
    echo ""
fi