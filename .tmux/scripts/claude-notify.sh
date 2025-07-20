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

# OS Detection for notifications
detect_os_type() {
    case "$OSTYPE" in
        darwin*)
            echo "macos"
            ;;
        linux*)
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                echo "wsl"
            else
                echo "linux"
            fi
            ;;
        *)
            echo "other"
            ;;
    esac
}

# macOS notification cleanup function
cleanup_macos_notifications() {
    if command -v terminal-notifier >/dev/null 2>&1; then
        # Remove old notifications from the same group
        terminal-notifier -remove "claude-code-waiting" 2>/dev/null
        terminal-notifier -remove "claude-code-complete" 2>/dev/null
        terminal-notifier -remove "claude-code-busy" 2>/dev/null
    fi
}

# Check if Do Not Disturb is enabled on macOS
is_dnd_enabled() {
    if [ "$(uname)" = "Darwin" ]; then
        # Check Do Not Disturb status via plutil
        local dnd_status=$(plutil -extract dnd_prefs.userPref.enabled xml1 ~/Library/Preferences/com.apple.ncprefs.plist -o - 2>/dev/null | grep -o '<true/>' | head -1)
        if [ "$dnd_status" = "<true/>" ]; then
            return 0  # DND is enabled
        fi
    fi
    return 1  # DND is disabled or not macOS
}

# Smart notification function that respects user preferences
send_notification_smart() {
    local title="$1"
    local subtitle="$2"
    local message="$3"
    local sound="$4"
    local group="$5"
    
    if command -v terminal-notifier >/dev/null 2>&1; then
        # Check if DND is enabled and user wants to respect it
        if is_dnd_enabled && [ "${CLAUDE_RESPECT_DND:-true}" = "true" ]; then
            # Send notification without sound if DND is enabled
            terminal-notifier \
                -title "$title" \
                -subtitle "$subtitle" \
                -message "$message" \
                -group "$group" \
                -sender "com.apple.Terminal" 2>/dev/null &
        else
            # Send full notification with sound
            terminal-notifier \
                -title "$title" \
                -subtitle "$subtitle" \
                -message "$message" \
                -sound "$sound" \
                -group "$group" \
                -sender "com.apple.Terminal" \
                -ignoreDnD 2>/dev/null &
        fi
    fi
}

# Find PowerShell executable path for WSL
find_powershell_path() {
    local powershell_paths=(
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Windows/System32/powershell.exe"
        "powershell.exe"
    )
    
    for path in "${powershell_paths[@]}"; do
        if [[ -f "$path" ]]; then
            echo "$path"
            return 0
        fi
    done
    
    return 1
}

# Waiting notification (⌛) - Input required
notify_waiting() {
    local os_type=$(detect_os_type)
    
    case "$os_type" in
        "macos")
            # macOS: Cleanup old notifications first
            cleanup_macos_notifications
            
            # macOS: System sound + terminal-notifier
            if command -v afplay >/dev/null 2>&1; then
                afplay /System/Library/Sounds/Glass.aiff 2>/dev/null &
            fi
            # Enhanced notification with terminal-notifier
            if command -v terminal-notifier >/dev/null 2>&1; then
                send_notification_smart "Claude Code" "⌛ Waiting" "入力待ちです" "Glass" "claude-code-waiting"
            else
                # Fallback to osascript if terminal-notifier not available
                osascript -e 'display notification "入力待ちです" with title "Claude Code" subtitle "⌛ Waiting"' 2>/dev/null &
            fi
            ;;
        "wsl")
            # WSL: PowerShell beep notification sound
            local powershell_path=$(find_powershell_path)
            if [[ -n "$powershell_path" ]]; then
                "$powershell_path" -Command "
                [console]::beep(659, 100)
                Start-Sleep -Milliseconds 50
                [console]::beep(880, 150)
                " 2>/dev/null &
            else
                echo -e "\a"
                sleep 0.1
                echo -e "\a"
            fi
            ;;
        "linux")
            # Linux: System sound or fallback
            if command -v paplay >/dev/null 2>&1; then
                paplay /usr/share/sounds/alsa/Side_Left.wav 2>/dev/null &
            elif command -v notify-send >/dev/null 2>&1; then
                notify-send "Claude Code" "⌛ 入力待ちです" 2>/dev/null &
            else
                echo -e "\a"
                sleep 0.1
                echo -e "\a"
            fi
            ;;
        *)
            # Fallback: Terminal bell
            echo -e "\a"
            sleep 0.1
            echo -e "\a"
            ;;
    esac
}

# Complete notification (✅) - Task completed
notify_complete() {
    local os_type=$(detect_os_type)
    
    case "$os_type" in
        "macos")
            # macOS: Cleanup old notifications first
            cleanup_macos_notifications
            
            # macOS: Completion sound + terminal-notifier
            if command -v afplay >/dev/null 2>&1; then
                afplay /System/Library/Sounds/Hero.aiff 2>/dev/null &
            fi
            # Enhanced notification with terminal-notifier
            if command -v terminal-notifier >/dev/null 2>&1; then
                send_notification_smart "Claude Code" "✅ Complete" "処理が完了しました" "Hero" "claude-code-complete"
            else
                # Fallback to osascript if terminal-notifier not available
                osascript -e 'display notification "処理が完了しました" with title "Claude Code" subtitle "✅ Complete"' 2>/dev/null &
            fi
            ;;
        "wsl")
            # WSL: PowerShell completion sound (ascending melody)
            local powershell_path=$(find_powershell_path)
            if [[ -n "$powershell_path" ]]; then
                "$powershell_path" -Command "
                [console]::beep(523, 80)
                [console]::beep(659, 80)
                [console]::beep(783, 80)
                [console]::beep(1046, 120)
                " 2>/dev/null &
            else
                echo -e "\a"
            fi
            ;;
        "linux")
            # Linux: System sound or fallback
            if command -v paplay >/dev/null 2>&1; then
                paplay /usr/share/sounds/alsa/Front_Left.wav 2>/dev/null &
            elif command -v notify-send >/dev/null 2>&1; then
                notify-send "Claude Code" "✅ 処理完了" 2>/dev/null &
            else
                echo -e "\a"
            fi
            ;;
        *)
            # Fallback: Terminal bell
            echo -e "\a"
            ;;
    esac
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
    
    # Enhanced notification with OS-specific sounds and alerts
    case "$NEW_STATUS" in
        "✅")
            # Process completed - use enhanced completion notification
            notify_complete
            
            # Optional: Auto-trigger Claude Voice summary on completion
            if [ "${CLAUDE_VOICE_AUTO_SUMMARY:-false}" = "true" ] && [ "${CLAUDE_VOICE_ON_COMPLETE:-true}" = "true" ]; then
                # Phase 1 Fix: Execute voice in foreground with proper session management
                # Use osascript to ensure desktop audio session access
                osascript -e 'do shell script "~/.tmux/claude/bin/claude-voice brief 20 Kyoko"' &
            fi
            ;;
        "⌛")
            # Waiting for input - use enhanced input notification
            notify_waiting
            
            # Optional: Auto-trigger Claude Voice summary on input waiting
            if [ "${CLAUDE_VOICE_AUTO_SUMMARY:-false}" = "true" ] && [ "${CLAUDE_VOICE_ON_WAITING:-true}" = "true" ]; then
                # Phase 1 Fix: Execute voice in foreground with proper session management
                # Use osascript to ensure desktop audio session access
                osascript -e 'do shell script "~/.tmux/claude/bin/claude-voice brief 15 Kyoko"' &
            fi
            ;;
        "⚡")
            # Process started - enhanced notification for macOS
            local os_type=$(detect_os_type)
            if [ "$os_type" = "macos" ]; then
                cleanup_macos_notifications
                if command -v terminal-notifier >/dev/null 2>&1; then
                    send_notification_smart "Claude Code" "⚡ Busy" "処理中です" "Ping" "claude-code-busy"
                else
                    echo -e "\a"
                fi
            else
                echo -e "\a"
            fi
            
            # Optional: Auto-trigger Claude Voice summary on busy start (usually not needed)
            if [ "${CLAUDE_VOICE_AUTO_SUMMARY:-false}" = "true" ] && [ "${CLAUDE_VOICE_ON_BUSY:-false}" = "true" ]; then
                # Phase 1 Fix: Execute voice in foreground with proper session management
                # Use osascript to ensure desktop audio session access
                osascript -e 'do shell script "~/.tmux/claude/bin/claude-voice brief 10 Kyoko"' &
            fi
            ;;
    esac
}

# Only notify for meaningful status changes (non-empty states)
# Allow notifications when OLD_STATUS is empty (initial state) but NEW_STATUS is meaningful
if [ -n "$NEW_STATUS" ] && [ "$OLD_STATUS" != "$NEW_STATUS" ]; then
    notify_status_change
fi