#!/bin/bash
# Simple Claude status updater
# Updates status files without notification

STATUS_DIR="$HOME/.tmux/status"
mkdir -p "$STATUS_DIR"

# Update all windows
for window_id in $(tmux list-windows -F '#I' 2>/dev/null); do
    status=$(~/.tmux/scripts/claude-status-enhanced.sh "$window_id")
    status_file="$STATUS_DIR/window-${window_id}.status"
    
    # Write status (even if empty)
    echo "$status" > "$status_file"
done