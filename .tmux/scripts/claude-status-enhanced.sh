#!/bin/bash
# Optimized Claude Code status detection - 3-state focused
# Version 3.0 - Simplified for Busy/Waiting/Idle detection

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
PANE_ID=${2:-$(tmux display-message -p '#P')}

# Debug function
debug_log() {
    [ "$CLAUDE_STATUS_DEBUG" = "1" ] && echo "[DEBUG] $1" >&2
}

# State icons (bash 3.x compatible)
get_state_icon() {
    case "$1" in
        "Busy") echo "⚡" ;;
        "Waiting") echo "⌛" ;;
        "Idle") echo "✅" ;;
        *) echo "" ;;
    esac
}

debug_log "Starting Claude status detection for window $WINDOW_ID, pane $PANE_ID"

# Core 3-state detection function
detect_claude_status() {
    local output="$1"
    
    debug_log "Analyzing output: ${#output} chars"
    
    # Get recent output for pattern matching
    local recent_output=$(echo "$output" | tail -20)
    
    # 1. Busy: Processing with tokens and interrupt (highest priority - active processing)
    debug_log "Checking for busy patterns in output..."
    if echo "$output" | grep -qE '\([0-9]+s\s+·.*tokens.*interrupt\)'; then
        debug_log "Detected: Busy (processing pattern in full output)"
        echo "Busy"
        return
    fi
    
    if echo "$recent_output" | grep -qE '\([0-9]+s\s+·.*tokens.*interrupt\)'; then
        debug_log "Detected: Busy (processing pattern in recent output)"
        echo "Busy"
        return
    fi
    
    # Also check for other busy indicators
    if echo "$recent_output" | grep -qE '(Finagling|Thinking|Processing).*\([0-9]+s'; then
        debug_log "Detected: Busy (general processing pattern)"
        echo "Busy" 
        return
    fi
    
    # 2. Waiting: User input required (interactive prompts and plan mode)
    # Look for active prompt patterns in recent output (but exclude processing and completion contexts)
    if echo "$recent_output" | grep -qE '(Do you want to proceed\?|Would you like to|Continue\?.*❯)' && \
       ! echo "$recent_output" | grep -qE '(Finagling|tokens.*interrupt|✅.*完了|Showing detailed transcript)'; then
        debug_log "Detected: Waiting (active input prompt in recent output)"
        echo "Waiting"
        return
    fi
    
    # Specific choice prompts
    if echo "$recent_output" | grep -qE '❯ 1\. Yes.*2\. Yes.*3\. No'; then
        debug_log "Detected: Waiting (choice prompt)"
        echo "Waiting"
        return
    fi
    
    # Check for plan mode (user needs to review/approve plan)
    # Exclude settings display - only detect actual plan approval needed
    if echo "$recent_output" | grep -qE 'plan mode.*exit.*approve' && \
       ! echo "$recent_output" | grep -qE '(plan mode on|auto-accept.*on)'; then
        debug_log "Detected: Waiting (plan approval needed)"
        echo "Waiting"
        return
    fi
    
    # Check for error states that require user action
    if echo "$recent_output" | grep -qE '(Error:|Failed:|Exception:.*❯)'; then
        debug_log "Detected: Waiting (error requiring user action)"
        echo "Waiting"
        return
    fi
    
    # 3. Idle: Task completed or ready for input
    # Check for completion patterns
    if echo "$recent_output" | grep -qE '(✅.*完了|Showing detailed transcript|Ctrl\+R to toggle)'; then
        debug_log "Detected: Idle (task completed)"
        echo "Idle"
        return
    fi
    
    # Check for ready state with shortcuts available
    if echo "$recent_output" | grep -qE '\? for shortcuts' && echo "$recent_output" | grep -qE '>\s*$'; then
        debug_log "Detected: Idle (ready with shortcuts available)"
        echo "Idle"
        return
    fi
    
    # Check for ready prompt pattern  
    if echo "$ui_context" | grep -qE '>\s*$'; then
        debug_log "Detected: Idle (prompt ready in UI context)"
        echo "Idle"
        return
    fi
    
    # Default to Idle
    debug_log "Detected: Idle (default fallback)"
    echo "Idle"
}

# Claude Code session verification
is_claude_code_session() {
    local output="$1"
    
    # Check for Claude Code UI elements
    if echo "$output" | grep -qE '(╭─|╰─|\? for shortcuts|claude\.ai|claude code)'; then
        debug_log "Claude Code UI confirmed"
        return 0
    fi
    
    debug_log "No Claude Code UI found"
    return 1
}

# Step 1: Check if Claude Code is running globally
CLAUDE_PROCESS=$(pgrep -f "claude" 2>/dev/null)
if [ -z "$CLAUDE_PROCESS" ]; then
    debug_log "No Claude process found globally"
    echo ""
    exit 0
fi

# Step 2: Get pane information
if [ "$WINDOW_ID" != "" ] && [ "$PANE_ID" != "" ]; then
    PANE_PID=$(tmux display-message -t "${WINDOW_ID}.${PANE_ID}" -p '#{pane_pid}' 2>/dev/null)
else
    PANE_PID=$(tmux display-message -p '#{pane_pid}' 2>/dev/null)
fi

if [ -z "$PANE_PID" ]; then
    debug_log "Failed to get pane PID"
    echo ""
    exit 0
fi

# Step 3: Check if claude is in the process tree
PANE_PROCESSES=$(pstree -p "$PANE_PID" 2>/dev/null || echo "")
if ! echo "$PANE_PROCESSES" | grep -q "claude"; then
    debug_log "Claude not found in pane process tree"
    echo ""
    exit 0
fi

# Step 4: Capture terminal output
TERMINAL_OUTPUT=$(tmux capture-pane -p -S -30 -t "${WINDOW_ID}.${PANE_ID}" 2>/dev/null)
if [ -z "$TERMINAL_OUTPUT" ]; then
    debug_log "Failed to capture terminal output"
    echo ""
    exit 0
fi

debug_log "Captured ${#TERMINAL_OUTPUT} chars of terminal output"

# Step 5: Verify Claude Code session and detect status
if ! is_claude_code_session "$TERMINAL_OUTPUT"; then
    debug_log "Not a Claude Code session"
    echo ""
    exit 0
fi

# Step 6: Detect status using optimized 3-state logic
STATUS=$(detect_claude_status "$TERMINAL_OUTPUT")
debug_log "Detected status: $STATUS"

# Step 7: Output the appropriate icon
STATUS_ICON=$(get_state_icon "$STATUS")
if [ -n "$STATUS_ICON" ]; then
    echo "$STATUS_ICON"
else
    echo ""
fi

debug_log "Final output: $STATUS_ICON"