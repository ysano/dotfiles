#!/bin/bash
# Enhanced Claude Code status monitoring script (ccmanager-inspired, tmux-optimized)
# Debug mode: set CLAUDE_STATUS_DEBUG=1 to enable detailed logging

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
PANE_ID=${2:-$(tmux display-message -p '#P')}

# Debug function
debug_log() {
    [ "$CLAUDE_STATUS_DEBUG" = "1" ] && echo "[DEBUG] $1" >&2
}

debug_log "Starting Claude status detection for window $WINDOW_ID, pane $PANE_ID"

# Step 1: Check if Claude Code is running globally
CLAUDE_PROCESS=$(pgrep -f "claude" 2>/dev/null)

if [ -z "$CLAUDE_PROCESS" ]; then
    debug_log "No Claude process found globally"
    echo ""
    exit 0
fi

debug_log "Claude process found globally: $CLAUDE_PROCESS"

# Step 2: Check if Claude Code is running in this specific pane
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

debug_log "Pane PID: $PANE_PID"

# Check if claude is in the process tree of this pane
PANE_PROCESSES=$(pstree -p "$PANE_PID" 2>/dev/null || echo "")
if ! echo "$PANE_PROCESSES" | grep -q "claude"; then
    debug_log "Claude not found in pane process tree"
    echo ""
    exit 0
fi

debug_log "Claude found in pane process tree"

# Capture terminal output (30 lines as per ccmanager)
TERMINAL_OUTPUT=$(tmux capture-pane -p -S -30 -t "${WINDOW_ID}.${PANE_ID}" 2>/dev/null)

if [ -z "$TERMINAL_OUTPUT" ]; then
    debug_log "Failed to capture terminal output"
    echo ""
    exit 0
fi

debug_log "Captured terminal output (${#TERMINAL_OUTPUT} chars)"
if [ "$CLAUDE_STATUS_DEBUG" = "1" ]; then
    echo "[DEBUG] Terminal output:" >&2
    echo "$TERMINAL_OUTPUT" >&2
    echo "[DEBUG] ---" >&2
fi

# Step 3: Verify this is actually Claude Code by checking for characteristic patterns
# Convert to lowercase for case-insensitive matching
LOWER_OUTPUT=$(echo "$TERMINAL_OUTPUT" | tr '[:upper:]' '[:lower:]')

# Check for Claude Code characteristic patterns
CLAUDE_UI_PATTERNS="(╭─|╰─|claude\.ai|claude code|>\s*$|auto-accept edits|shift\+tab|esc to interrupt|multiplexing)"
if ! echo "$TERMINAL_OUTPUT" | grep -qE "$CLAUDE_UI_PATTERNS"; then
    debug_log "No Claude Code UI patterns found - not a Claude Code session"
    echo ""
    exit 0
fi

debug_log "Claude Code UI patterns confirmed"

STATUS=""

# Check for busy state first (processing/working)
if echo "$LOWER_OUTPUT" | grep -qE "esc to interrupt"; then
    STATUS="⚡"  # Busy state
    debug_log "Detected busy state"
# Check for explicit waiting input state (user confirmation required)
elif echo "$TERMINAL_OUTPUT" | grep -qE "(Do you want|Would you like|Continue\?|Press.*continue|awaiting.*input)"; then
    STATUS="⌛"  # Waiting for input
    debug_log "Detected waiting_input state"
# Check for specific auto-accept prompts (but not just idle prompt)
elif echo "$TERMINAL_OUTPUT" | grep -qE "(auto-accept edits.*shift\+tab|shift\+tab.*auto-accept)"; then
    STATUS="⌛"  # Ready for input
    debug_log "Detected auto-accept configuration state"
# If we have Claude Code UI but no active patterns, it's idle
else
    STATUS="✅"  # Idle state
    debug_log "Detected idle state (Claude Code confirmed but no active patterns)"
fi

debug_log "Final status: $STATUS"
echo "$STATUS"