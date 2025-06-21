#!/bin/bash
# Optimized Claude Code status monitoring script

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
PANE_ID=${2:-$(tmux display-message -p '#P')}

# Cache directory for performance optimization
CACHE_DIR="$HOME/.tmux/cache"
CACHE_FILE="$CACHE_DIR/claude-status-${WINDOW_ID}-${PANE_ID}"
PROCESS_CACHE_FILE="$CACHE_DIR/claude-process"

mkdir -p "$CACHE_DIR"

# Performance optimization: Check process less frequently
check_process_cache() {
    local now=$(date +%s)
    local cache_age=0
    
    if [ -f "$PROCESS_CACHE_FILE" ]; then
        local cache_time=$(stat -c %Y "$PROCESS_CACHE_FILE" 2>/dev/null || echo 0)
        cache_age=$((now - cache_time))
    fi
    
    # Only check process every 10 seconds instead of every 3
    if [ $cache_age -gt 10 ]; then
        local claude_process=$(pgrep -f "claude.*code|claude-code|code.*claude" 2>/dev/null)
        echo "${claude_process:-none}" > "$PROCESS_CACHE_FILE"
        echo "$claude_process"
    else
        local cached_process=$(cat "$PROCESS_CACHE_FILE" 2>/dev/null)
        [ "$cached_process" = "none" ] && echo "" || echo "$cached_process"
    fi
}

# Check if Claude Code is running using cached result
CLAUDE_PROCESS=$(check_process_cache)

# If no Claude Code process, return empty (no icon)
if [ -z "$CLAUDE_PROCESS" ]; then
    echo ""
    # Clean up cache when process is not running
    rm -f "$CACHE_FILE" "$PROCESS_CACHE_FILE" 2>/dev/null
    exit 0
fi

# Performance optimization: Only capture terminal output if process is running
# and reduce lines captured from 30 to 10 for better performance
TERMINAL_OUTPUT=$(tmux capture-pane -p -S -10 -t "${WINDOW_ID}.${PANE_ID}" 2>/dev/null)

# Cache the output hash to avoid expensive regex operations on same content
OUTPUT_HASH=$(echo "$TERMINAL_OUTPUT" | md5sum | cut -d' ' -f1)
CACHED_HASH=""
CACHED_STATUS=""

if [ -f "$CACHE_FILE" ]; then
    CACHED_HASH=$(head -n1 "$CACHE_FILE" 2>/dev/null)
    CACHED_STATUS=$(tail -n1 "$CACHE_FILE" 2>/dev/null)
fi

# Return cached result if content hasn't changed
if [ "$OUTPUT_HASH" = "$CACHED_HASH" ] && [ -n "$CACHED_STATUS" ]; then
    echo "$CACHED_STATUS"
    exit 0
fi

# Determine status with optimized regex patterns
STATUS=""
if echo "$TERMINAL_OUTPUT" | grep -qE "(esc to interrupt|interrupt.*esc)"; then
    STATUS="⚡"  # Busy state
elif echo "$TERMINAL_OUTPUT" | grep -qE "(│.*Do you want|>\s*$|Continue\?|Press.*continue|awaiting.*input)"; then
    STATUS="⌛"  # Waiting state
else
    STATUS="✅"  # Idle state
fi

# Cache the result
{
    echo "$OUTPUT_HASH"
    echo "$STATUS"
} > "$CACHE_FILE"

echo "$STATUS"