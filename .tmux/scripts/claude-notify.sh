#!/bin/bash
# Optimized Claude Code notification script with proper cleanup

WINDOW_ID=${1:-$(tmux display-message -p '#I')}
OLD_STATUS="$2"
NEW_STATUS="$3"

# Status directory with better organization
STATUS_DIR="$HOME/.tmux/status"
STATUS_FILE="$STATUS_DIR/window-${WINDOW_ID}.status"
CLEANUP_MARKER="$STATUS_DIR/.last_cleanup"

mkdir -p "$STATUS_DIR"

# Cleanup old status files periodically (once per hour)
cleanup_old_status_files() {
    local now=$(date +%s)
    local last_cleanup=0
    
    if [ -f "$CLEANUP_MARKER" ]; then
        last_cleanup=$(cat "$CLEANUP_MARKER" 2>/dev/null || echo 0)
    fi
    
    # Run cleanup every hour (3600 seconds)
    if [ $((now - last_cleanup)) -gt 3600 ]; then
        # Remove status files for non-existent tmux windows
        for status_file in "$STATUS_DIR"/window-*.status; do
            [ -f "$status_file" ] || continue
            
            local window_id=$(basename "$status_file" | sed 's/window-\([0-9]*\)\.status/\1/')
            
            # Check if tmux window still exists
            if ! tmux list-windows -F '#I' 2>/dev/null | grep -q "^${window_id}$"; then
                rm -f "$status_file"
            fi
        done
        
        # Remove status files older than 24 hours
        find "$STATUS_DIR" -name "window-*.status" -mtime +1 -delete 2>/dev/null
        
        echo "$now" > "$CLEANUP_MARKER"
    fi
}

# Run cleanup in background to avoid blocking
cleanup_old_status_files &

# Get previous status with error handling
if [ -z "$OLD_STATUS" ]; then
    OLD_STATUS=$(cat "$STATUS_FILE" 2>/dev/null || echo "")
fi

# Get current status if not provided
if [ -z "$NEW_STATUS" ]; then
    NEW_STATUS=$(~/.tmux/scripts/claude-status.sh "$WINDOW_ID")
fi

# Early exit if no change detected
if [ "$OLD_STATUS" = "$NEW_STATUS" ]; then
    exit 0
fi

# Save current status with error handling
if ! echo "$NEW_STATUS" > "$STATUS_FILE" 2>/dev/null; then
    # If we can't write status file, create directory and try again
    mkdir -p "$STATUS_DIR" && echo "$NEW_STATUS" > "$STATUS_FILE"
fi

# Rate limiting to prevent notification spam
RATE_LIMIT_FILE="$STATUS_DIR/.rate_limit_${WINDOW_ID}"
RATE_LIMIT_SECONDS=2

rate_limit_check() {
    local now=$(date +%s)
    local last_notify=0
    
    if [ -f "$RATE_LIMIT_FILE" ]; then
        last_notify=$(cat "$RATE_LIMIT_FILE" 2>/dev/null || echo 0)
    fi
    
    if [ $((now - last_notify)) -lt $RATE_LIMIT_SECONDS ]; then
        return 1  # Rate limited
    fi
    
    echo "$now" > "$RATE_LIMIT_FILE"
    return 0  # OK to proceed
}

# Enhanced notification function with better error handling
notify_status_change() {
    # Skip notification if rate limited
    if ! rate_limit_check; then
        return 0
    fi
    
    # Check if notifications are disabled
    if [ -f "$HOME/.tmux/notifications_disabled" ]; then
        return 0
    fi
    
    # Audio notification with reduced frequency and better patterns
    case "$NEW_STATUS" in
        "✅")
            # Process completed - gentle notification
            if command -v paplay >/dev/null 2>&1; then
                # Use system sound if available (Linux)
                paplay /usr/share/sounds/alsa/Front_Left.wav 2>/dev/null &
            else
                echo -e "\a"
            fi
            ;;
        "⌛")
            # Waiting for input - attention sound
            if command -v paplay >/dev/null 2>&1; then
                paplay /usr/share/sounds/alsa/Side_Left.wav 2>/dev/null &
            else
                echo -e "\a"
                sleep 0.1
                echo -e "\a"
            fi
            ;;
        "⚡")
            # Process started - brief notification
            echo -e "\a"
            ;;
    esac
}

# Only notify for meaningful status changes (non-empty states)
if [ -n "$OLD_STATUS" ] && [ -n "$NEW_STATUS" ] && [ "$OLD_STATUS" != "$NEW_STATUS" ]; then
    notify_status_change
fi