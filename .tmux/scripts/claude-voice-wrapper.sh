#!/bin/bash
# Claude Voice Wrapper - Status detection with notification
# This wrapper both displays status AND triggers notifications

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
PANE_ID=${2:-$(tmux display-message -p '#P')}

# Get current status
CURRENT_STATUS=$("$SCRIPT_DIR/claude-status-enhanced.sh" "$WINDOW_ID" "$PANE_ID")

# Check previous status and trigger notification if changed
STATUS_FILE="$HOME/.tmux/status/window-${WINDOW_ID}.status"
PREVIOUS_STATUS=""

if [ -f "$STATUS_FILE" ]; then
    PREVIOUS_STATUS=$(cat "$STATUS_FILE" 2>/dev/null)
fi

# Update status file
mkdir -p "$(dirname "$STATUS_FILE")"
echo "$CURRENT_STATUS" > "$STATUS_FILE"

# Trigger notification if status changed
if [ "$PREVIOUS_STATUS" != "$CURRENT_STATUS" ] && [ -n "$CURRENT_STATUS" ]; then
    "$SCRIPT_DIR/claude-notify.sh" "$WINDOW_ID" "$PREVIOUS_STATUS" "$CURRENT_STATUS" >/dev/null 2>&1 &
fi

# Output status for display
echo "$CURRENT_STATUS"