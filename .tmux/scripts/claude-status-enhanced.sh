#!/bin/bash
# Claude Code Status Detection Script
# Detects Claude Code status in tmux windows

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
        
        # Check if it's likely a Claude Code related command
        case "$current_cmd" in
            *node*|*cursor*|*code*|*nvim*|*vim*|*emacs*|*python*|*ruby*|*bash*|*zsh*|*sh*)
                # Capture the visible content of the pane (last 20 lines for more recent status)
                local pane_content=$(tmux capture-pane -t "$window_id.$pane_id" -p -S -20 2>/dev/null || echo "")
                
                # Get only the last 5 lines for more accurate current state detection
                local recent_content=$(echo "$pane_content" | tail -5)
                
                # Detect Claude Code specific patterns
                # Check for Claude Code UI elements and states
                
                # Check for active busy indicators first (highest priority) - only in recent lines
                if echo "$recent_content" | grep -qE "(Running…|Whirring…|⏺)" 2>/dev/null; then
                    status="⚡"
                    break
                # Claude Code waiting for input (check in recent lines to avoid false positives)
                elif echo "$recent_content" | grep -qE "(tell Claude what|proceed with|\(esc\)|Should I|Would you like|Please enter|Type your|Choose|Select|Confirm|Press|Continue\?|Y/N|yes/no|y/n|password:|Username:)" 2>/dev/null; then
                    status="⌛"
                    break
                # Check for other busy indicators in full content
                elif echo "$pane_content" | grep -qE "(Thinking|Analyzing|Processing|Working|Generating|Creating|Building|Testing|Executing|Writing|Reading|Searching|Loading|Updating|Installing|Compiling|\[.*▪.*\]|\[.*●.*\]|░▒▓|⣾⣽⣻⢿⡿⣟⣯⣷|⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏|◐◓◑◒|▁▂▃▄▅▆▇█)" 2>/dev/null; then
                    status="⚡"
                    break
                # Claude Code completion indicators
                elif echo "$pane_content" | grep -qE "(Complete|Completed|Finished|Done|Success|Passed|✓|✅|Successfully|All tests passed|Build successful|0 errors|0 failures)" 2>/dev/null; then
                    status="✅"
                    # Don't break - check other panes for active states
                # Finally check for idle state (command prompt)
                elif echo "$recent_content" | grep -qE "(\$\s*$|%\s*$|>\s*$|#\s*$|❯\s*$|➜\s*$|→\s*$|λ\s*$|🚀\s*$)" 2>/dev/null; then
                    # Idle - has prompt but no other activity
                    status=""
                fi
                ;;
        esac
    done
    
    echo "$status"
}

# Main execution
detect_status "$WINDOW_ID"