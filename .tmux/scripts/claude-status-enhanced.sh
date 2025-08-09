#!/bin/bash
# Claude Code Status Detection Script v2
# Clear logic based on specific patterns

WINDOW_ID=${1:-$(tmux display-message -p '#I')}

# Function to detect Claude Code status from pane content
detect_status() {
    local window_id="$1"
    local status=""
    
    # Get all panes in the window
    local panes=$(tmux list-panes -t "$window_id" -F '#{pane_id}' 2>/dev/null)
    
    if [ -z "$panes" ]; then
        echo ""
        return
    fi
    
    # Check each pane for Claude Code activity
    for pane_id in $panes; do
        # Get current command in the pane
        local current_cmd=$(tmux display-message -t "$window_id.$pane_id" -p '#{pane_current_command}' 2>/dev/null)
        
        # Check if it's a process that could host Claude Code
        case "$current_cmd" in
            *node*|*cursor*|*code*|*claude*)
                # This could be Claude Code
                ;;
            *)
                # Not Claude Code, skip
                continue
                ;;
        esac
        
        # Capture the visible content of the pane
        local pane_content=$(tmux capture-pane -t "$window_id.$pane_id" -p 2>/dev/null || echo "")
        
        # Step 1: Check if Claude Code is actually running (look for its signature)
        # Claude Code always shows "esc to interrupt" when busy or has specific UI elements
        if ! echo "$pane_content" | grep -qE "(esc to interrupt|Do you want to proceed\?|❯ 1|tell Claude what|\? for shortcuts.*Bypassing Permissions)" 2>/dev/null; then
            # Not Claude Code or not active
            continue
        fi
        
        # Step 2: Determine the status
        # Check for BUSY state - "esc to interrupt" is the clearest indicator
        if echo "$pane_content" | grep -q "esc to interrupt" 2>/dev/null; then
            status="⚡"
            break
        # Check for WAITING state - menu selections or prompts
        elif echo "$pane_content" | grep -qE "(Do you want to proceed\?|❯ 1|❯ 2|❯ 3|tell Claude what|Should I|Would you like|Yes, and|No, keep|Choose|Select|Confirm)" 2>/dev/null; then
            status="⌛"
            break
        # Otherwise it's IDLE (Claude Code is present but not doing anything)
        else
            # Claude Code is running but idle
            status="✅"  # Idle - show checkmark
        fi
    done
    
    echo "$status"
}

# Main execution
detect_status "$WINDOW_ID"