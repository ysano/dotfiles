#!/bin/bash
# Claude Code Status Detection Script v3
# Improved detection using pane title and UI patterns

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
        # Method 1: Check pane title for Claude Code
        # Note: Use window ID directly for single-pane windows
        local pane_title=$(tmux display-message -t "$window_id" -p '#{pane_title}' 2>/dev/null)
        local pane_cmd=$(tmux display-message -t "$window_id" -p '#{pane_current_command}' 2>/dev/null)
        
        # Claude Code sets the pane title with specific patterns
        local is_claude_by_title=false
        if echo "$pane_title" | grep -qE "(Claude|claude)" 2>/dev/null; then
            is_claude_by_title=true
        fi
        
        # Method 2: Check for Claude Code UI patterns
        local pane_content=$(tmux capture-pane -t "$window_id" -p 2>/dev/null || echo "")
        
        # Look for Claude Code specific UI elements:
        # - Box drawing characters (╭──╮, ╰──╯) for the input box
        # - Status indicators (⏺, ✳, ✶, ✻)
        # - "? for shortcuts" at the bottom
        local is_claude_by_ui=false
        if echo "$pane_content" | grep -qE "╭─.*─╮" 2>/dev/null && \
           echo "$pane_content" | grep -qE "╰─.*─╯" 2>/dev/null && \
           echo "$pane_content" | grep -q "? for shortcuts" 2>/dev/null; then
            is_claude_by_ui=true
        fi
        
        # Also check for active Claude Code indicators
        if echo "$pane_content" | grep -qE "(⏺|✳|✶|✻)" 2>/dev/null && \
           echo "$pane_content" | grep -q "Running…\|Whirring…\|Shimmying…\|Puzzling…\|Scheming…\|Thinking…" 2>/dev/null; then
            is_claude_by_ui=true
        fi
        
        # If not Claude Code by either method, skip
        if [ "$is_claude_by_title" = false ] && [ "$is_claude_by_ui" = false ]; then
            continue
        fi
        
        # Step 2: Determine the status
        # Check for BUSY state - "esc to interrupt" is the clearest indicator
        if echo "$pane_content" | grep -q "esc to interrupt" 2>/dev/null; then
            status="⚡"
            break
        # Check for WAITING state - menu selections or prompts
        elif echo "$pane_content" | grep -qE "(Do you want to proceed\?|❯ 1|❯ 2|❯ 3|tell Claude what|Should I|Would you like|Yes, and|No, keep|Choose|Select|Confirm|\(esc\))" 2>/dev/null; then
            status="⌛"
            break
        # Otherwise it's IDLE (Claude Code is present but not doing anything)
        else
            # Claude Code is running but idle
            status="✅"
        fi
    done
    
    echo "$status"
}

# Main execution
detect_status "$WINDOW_ID"