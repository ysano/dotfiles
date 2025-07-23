#!/bin/bash
# Background Claude Code Monitor for All Windows
# Monitors status changes in all tmux windows and triggers notifications

MONITOR_INTERVAL=${CLAUDE_MONITOR_INTERVAL:-3}
STATUS_DIR="$HOME/.tmux/status"
MONITOR_PID_FILE="$STATUS_DIR/.monitor.pid"

# Create status directory
mkdir -p "$STATUS_DIR"

# Check if monitor is already running
if [ -f "$MONITOR_PID_FILE" ]; then
    old_pid=$(cat "$MONITOR_PID_FILE" 2>/dev/null)
    if [ -n "$old_pid" ] && kill -0 "$old_pid" 2>/dev/null; then
        # Monitor already running
        exit 0
    fi
fi

# Save current PID
echo $$ > "$MONITOR_PID_FILE"

# Cleanup function
cleanup() {
    rm -f "$MONITOR_PID_FILE"
    exit 0
}

trap cleanup TERM INT

# Main monitoring loop
while true; do
    # Get all tmux windows
    if tmux list-windows -F '#I' 2>/dev/null | while read -r window_id; do
        # Skip if window doesn't exist
        if ! tmux list-windows -F '#I' 2>/dev/null | grep -q "^${window_id}$"; then
            continue
        fi
        
        # Get current status for this window
        current_status=$(~/.tmux/scripts/claude-status-enhanced.sh "$window_id")
        
        # Check previous status
        status_file="$STATUS_DIR/window-${window_id}.status"
        previous_status=""
        
        if [ -f "$status_file" ]; then
            previous_status=$(cat "$status_file" 2>/dev/null)
        fi
        
        # Update status file
        if [ -n "$current_status" ]; then
            echo "$current_status" > "$status_file"
        fi
        
        # Trigger notification if status changed
        if [ -n "$current_status" ] && [ "$previous_status" != "$current_status" ]; then
            ~/.tmux/scripts/claude-notify.sh "$window_id" "$previous_status" "$current_status" >/dev/null 2>&1 &
        fi
    done; then
        :  # Command succeeded
    else
        # tmux not available, exit gracefully
        cleanup
    fi
    
    sleep "$MONITOR_INTERVAL"
done