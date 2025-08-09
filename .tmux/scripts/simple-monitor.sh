#!/bin/bash
# Simple Claude Monitor

STATUS_DIR="$HOME/.tmux/status"
mkdir -p "$STATUS_DIR"

while true; do
    for window_id in 2 3 4 5; do
        # Get current status
        current_status=$(~/.tmux/scripts/claude-status-unified.sh "$window_id" 2>/dev/null)
        
        # Get previous status
        status_file="$STATUS_DIR/window-${window_id}.status"
        previous_status=$(cat "$status_file" 2>/dev/null)
        
        # If status changed, trigger notification
        if [ -n "$current_status" ] && [ "$previous_status" != "$current_status" ]; then
            echo "[$(date '+%H:%M:%S')] Window $window_id: $previous_status → $current_status"
            echo "$current_status" > "$status_file"
            ~/.tmux/scripts/claude-notify.sh "$window_id" "$previous_status" "$current_status" &
        fi
    done
    sleep 2
done