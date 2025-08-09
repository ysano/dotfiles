#!/bin/bash
# Claude Code Status Detection Script using Process Tree
# Most reliable detection method using process tree analysis

WINDOW_ID=${1:-$(tmux display-message -p '#I')}

# Function to detect Claude Code status from pane content
detect_status() {
    local window_id="$1"
    local status=""
    
    # Get the PID of the pane (using list-panes to avoid current window PID issue)
    local pane_pid=$(tmux list-panes -t "$window_id" -F '#{pane_pid}' 2>/dev/null | head -1)
    
    if [ -z "$pane_pid" ]; then
        echo ""
        return
    fi
    
    # Check if claude process exists in the process tree
    if ! pstree -p "$pane_pid" 2>/dev/null | grep -q 'claude'; then
        # No Claude Code running
        echo ""
        return
    fi
    
    # Claude Code is running, now determine its status
    # Capture only the last 10 lines of the pane to check current state (without escape sequences)
    local pane_content=$(tmux capture-pane -t "$window_id" -p -S -10 2>/dev/null | tail -10 || echo "")
    
    # Check for BUSY state - "esc to interrupt" should be in the recent lines
    # Also check for active status indicators like Running…, Whirring… etc
    if echo "$pane_content" | grep -qE "(esc to interrupt|Running…|Whirring…|Thinking…|Scheming…|Puzzling…|Concocting…)" 2>/dev/null; then
        status="⚡"
    # Check for WAITING state - menu selections or prompts (check only in recent lines)
    elif echo "$pane_content" | grep -qE "(Do you want to proceed\?|❯ 1|❯ 2|❯ 3|tell Claude what|Should I|Would you like|Yes, and|No, keep|Choose an option)" 2>/dev/null; then
        status="⌛"
    # Otherwise it's IDLE (Claude Code is present but not doing anything)
    else
        # Claude Code is running but idle
        status="✅"
    fi
    
    echo "$status"
}

# Main execution
detect_status "$WINDOW_ID"