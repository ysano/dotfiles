#!/bin/bash
# Window-specific Claude Status Display
# Called from tmux status format to show window status

# Get window ID from argument or current window
WINDOW_ID=${1:-$(tmux display-message -p '#I')}

# Status directory
STATUS_DIR="$HOME/.tmux/status"
STATUS_FILE="$STATUS_DIR/window-${WINDOW_ID}.status"

# Read status from file (updated by monitor script)
if [ -f "$STATUS_FILE" ]; then
    status=$(cat "$STATUS_FILE" 2>/dev/null)
    if [ -n "$status" ]; then
        # Add space before status emoji
        echo " $status"
    fi
fi