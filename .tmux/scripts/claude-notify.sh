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
    NEW_STATUS=$(~/.tmux/scripts/claude-status-enhanced.sh "$WINDOW_ID")
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
    local sound_played=false
    
    case "$os_type" in
        "macos")
            # macOS: Cleanup old notifications first
            cleanup_macos_notifications
            
            # macOS: System sound with panning support
            if command -v ~/.tmux/claude/os/darwin.sh >/dev/null 2>&1; then
                # Use panning-enabled sound playback
                (source ~/.tmux/claude/core/base.sh && source ~/.tmux/claude/os/darwin.sh && play_sound_file_with_panning "/System/Library/Sounds/Glass.aiff" "1.0" "$WINDOW_ID") &
                sound_played=true
            elif command -v afplay >/dev/null 2>&1 && afplay -v 1.0 /System/Library/Sounds/Glass.aiff 2>/dev/null &
            then
                sound_played=true
            fi
            # Enhanced notification with terminal-notifier (no sound since afplay already played)
            if command -v terminal-notifier >/dev/null 2>&1; then
                terminal-notifier \
                    -title "Claude Code" \
                    -subtitle "⌛ Waiting" \
                    -message "入力待ちです" \
                    -group "claude-code-waiting" \
                    -sender "com.apple.Terminal" 2>/dev/null &
            else
                # Fallback to osascript if terminal-notifier not available (no sound)
                osascript -e 'display notification "入力待ちです" with title "Claude Code" subtitle "⌛ Waiting"' 2>/dev/null &
            fi
            ;;
        "wsl")
            # WSL: PowerShell waiting melody (ascending tone)
            local powershell_path=$(find_powershell_path)
            if [[ -n "$powershell_path" ]] && "$powershell_path" -Command "
                [console]::beep(659, 100)
                Start-Sleep -Milliseconds 50
                [console]::beep(880, 150)
                Start-Sleep -Milliseconds 50
                [console]::beep(1175, 100)
                " 2>/dev/null &
            then
                sound_played=true
            fi
            ;;
        "linux")
            # Linux: Waiting notification sound (chime pattern)
            if command -v paplay >/dev/null 2>&1; then
                # Try various common Linux sound files for waiting
                if [ -f "/usr/share/sounds/freedesktop/stereo/dialog-information.oga" ] && paplay /usr/share/sounds/freedesktop/stereo/dialog-information.oga 2>/dev/null &
                then
                    sound_played=true
                elif [ -f "/usr/share/sounds/alsa/Side_Left.wav" ] && paplay /usr/share/sounds/alsa/Side_Left.wav 2>/dev/null &
                then
                    sound_played=true
                elif [ -f "/usr/share/sounds/ubuntu/stereo/dialog-information.ogg" ] && paplay /usr/share/sounds/ubuntu/stereo/dialog-information.ogg 2>/dev/null &
                then
                    sound_played=true
                fi
            elif command -v notify-send >/dev/null 2>&1; then
                notify-send "Claude Code" "⌛ 入力待ちです" 2>/dev/null &
            fi
            ;;
        *)
            # Unknown OS: sound_played remains false
            ;;
    esac
    
    # Fallback beep if no sound was played
    if [ "$sound_played" = "false" ]; then
        echo -e "\a"
    fi
}

# Complete notification (✅) - Task completed
notify_complete() {
    local os_type=$(detect_os_type)
    local sound_played=false
    
    case "$os_type" in
        "macos")
            # macOS: Cleanup old notifications first
            cleanup_macos_notifications
            
            # macOS: Completion sound with panning support
            if command -v ~/.tmux/claude/os/darwin.sh >/dev/null 2>&1; then
                # Use panning-enabled sound playback
                (source ~/.tmux/claude/core/base.sh && source ~/.tmux/claude/os/darwin.sh && play_sound_file_with_panning "/System/Library/Sounds/Hero.aiff" "1.0" "$WINDOW_ID") &
                sound_played=true
            elif command -v afplay >/dev/null 2>&1 && afplay -v 1.0 /System/Library/Sounds/Hero.aiff 2>/dev/null &
            then
                sound_played=true
            fi
            # Enhanced notification with terminal-notifier (no sound since afplay already played)
            if command -v terminal-notifier >/dev/null 2>&1; then
                terminal-notifier \
                    -title "Claude Code" \
                    -subtitle "✅ Complete" \
                    -message "処理が完了しました" \
                    -group "claude-code-complete" \
                    -sender "com.apple.Terminal" 2>/dev/null &
            else
                # Fallback to osascript if terminal-notifier not available (no sound)
                osascript -e 'display notification "処理が完了しました" with title "Claude Code" subtitle "✅ Complete"' 2>/dev/null &
            fi
            ;;
        "wsl")
            # WSL: PowerShell completion sound (ascending melody)
            local powershell_path=$(find_powershell_path)
            if [[ -n "$powershell_path" ]] && "$powershell_path" -Command "
                [console]::beep(523, 80)
                [console]::beep(659, 80)
                [console]::beep(783, 80)
                [console]::beep(1046, 120)
                " 2>/dev/null &
            then
                sound_played=true
            fi
            ;;
        "linux")
            # Linux: Completion notification sound (success pattern)
            if command -v paplay >/dev/null 2>&1; then
                # Try various common Linux sound files for completion
                if [ -f "/usr/share/sounds/freedesktop/stereo/complete.oga" ] && paplay /usr/share/sounds/freedesktop/stereo/complete.oga 2>/dev/null &
                then
                    sound_played=true
                elif [ -f "/usr/share/sounds/ubuntu/stereo/message-new-instant.ogg" ] && paplay /usr/share/sounds/ubuntu/stereo/message-new-instant.ogg 2>/dev/null &
                then
                    sound_played=true
                elif [ -f "/usr/share/sounds/alsa/Front_Left.wav" ] && paplay /usr/share/sounds/alsa/Front_Left.wav 2>/dev/null &
                then
                    sound_played=true
                elif [ -f "/usr/share/sounds/ubuntu/stereo/system-ready.ogg" ] && paplay /usr/share/sounds/ubuntu/stereo/system-ready.ogg 2>/dev/null &
                then
                    sound_played=true
                fi
            elif command -v notify-send >/dev/null 2>&1; then
                notify-send "Claude Code" "✅ 処理完了" 2>/dev/null &
            fi
            ;;
        *)
            # Unknown OS: sound_played remains false
            ;;
    esac
    
    # Fallback beep if no sound was played
    if [ "$sound_played" = "false" ]; then
        echo -e "\a"
    fi
}

# Get notification mode
get_notification_mode() {
    local mode_file="$HOME/.tmux/claude/notification_mode"
    if [ -f "$mode_file" ]; then
        cat "$mode_file" 2>/dev/null || echo "sound"
    else
        echo "sound"
    fi
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
    
    # Check notification mode
    local notification_mode
    notification_mode=$(get_notification_mode)
    if [ "$notification_mode" = "beep" ]; then
        # BEEP mode: only system bell
        echo -e "\a"
        return 0
    fi
    
    # Enhanced notification with OS-specific sounds and alerts
    case "$NEW_STATUS" in
        "✅")
            # Optional: Auto-trigger Claude Voice summary on completion
            # Phase 2 Fix: Get environment variables from tmux directly
            local auto_summary=$(tmux show-environment -g CLAUDE_VOICE_AUTO_SUMMARY 2>/dev/null | cut -d= -f2 || echo "false")
            local on_complete=$(tmux show-environment -g CLAUDE_VOICE_ON_COMPLETE 2>/dev/null | cut -d= -f2 || echo "true")
            
            # Always play notification sound first
            notify_complete
            
            if [ "$auto_summary" = "true" ] && [ "$on_complete" = "true" ]; then
                # Fast voice synthesis startup - direct call for speed (window.pane format)
                ~/.tmux/claude/bin/claude-voice brief 20 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
            fi
            ;;
        "⌛")
            # Optional: Auto-trigger Claude Voice summary on input waiting
            # Phase 2 Fix: Get environment variables from tmux directly
            local auto_summary=$(tmux show-environment -g CLAUDE_VOICE_AUTO_SUMMARY 2>/dev/null | cut -d= -f2 || echo "false")
            local on_waiting=$(tmux show-environment -g CLAUDE_VOICE_ON_WAITING 2>/dev/null | cut -d= -f2 || echo "true")
            
            # Always play notification sound first
            notify_waiting
            
            if [ "$auto_summary" = "true" ] && [ "$on_waiting" = "true" ]; then
                # Fast voice synthesis startup - direct call for speed (window.pane format)
                ~/.tmux/claude/bin/claude-voice brief 15 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
            fi
            ;;
        "⚡")
            # Process started - enhanced notification for macOS
            local os_type=$(detect_os_type)
            local sound_played=false
            
            if [ "$os_type" = "macos" ]; then
                cleanup_macos_notifications
                
                # Ping.aiff for processing/busy state with panning support
                if command -v ~/.tmux/claude/os/darwin.sh >/dev/null 2>&1; then
                    # Use panning-enabled sound playback
                    (source ~/.tmux/claude/core/base.sh && source ~/.tmux/claude/os/darwin.sh && play_sound_file_with_panning "/System/Library/Sounds/Ping.aiff" "1.0" "$WINDOW_ID") &
                    sound_played=true
                elif command -v afplay >/dev/null 2>&1 && afplay -v 1.0 /System/Library/Sounds/Ping.aiff 2>/dev/null &
                then
                    sound_played=true
                fi
                
                # Enhanced notification with terminal-notifier (no sound since afplay already played)
                if command -v terminal-notifier >/dev/null 2>&1; then
                    terminal-notifier \
                        -title "Claude Code" \
                        -subtitle "⚡ Busy" \
                        -message "処理中です" \
                        -group "claude-code-busy" \
                        -sender "com.apple.Terminal" 2>/dev/null &
                fi
            elif [ "$os_type" = "wsl" ]; then
                # WSL: PowerShell busy melody (rapid alert pattern)
                local powershell_path=$(find_powershell_path)
                if [[ -n "$powershell_path" ]] && "$powershell_path" -Command "
                    [console]::beep(800, 80)
                    Start-Sleep -Milliseconds 30
                    [console]::beep(800, 80)
                    Start-Sleep -Milliseconds 30
                    [console]::beep(600, 100)
                    " 2>/dev/null &
                then
                    sound_played=true
                fi
            elif [ "$os_type" = "linux" ]; then
                # Linux: Busy notification sound (alert pattern)
                if command -v paplay >/dev/null 2>&1; then
                    # Try various common Linux sound files for busy/alert
                    if [ -f "/usr/share/sounds/freedesktop/stereo/dialog-warning.oga" ] && paplay /usr/share/sounds/freedesktop/stereo/dialog-warning.oga 2>/dev/null &
                    then
                        sound_played=true
                    elif [ -f "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg" ] && paplay /usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg 2>/dev/null &
                    then
                        sound_played=true
                    elif [ -f "/usr/share/sounds/alsa/Rear_Left.wav" ] && paplay /usr/share/sounds/alsa/Rear_Left.wav 2>/dev/null &
                    then
                        sound_played=true
                    fi
                elif command -v notify-send >/dev/null 2>&1; then
                    notify-send "Claude Code" "⚡ 処理中" 2>/dev/null &
                fi
            fi
            
            # Fallback beep if no sound was played
            if [ "$sound_played" = "false" ]; then
                echo -e "\a"
            fi
            
            # Optional: Auto-trigger Claude Voice summary on busy start (usually not needed)
            # Phase 2 Fix: Get environment variables from tmux directly
            local auto_summary=$(tmux show-environment -g CLAUDE_VOICE_AUTO_SUMMARY 2>/dev/null | cut -d= -f2 || echo "false")
            local on_busy=$(tmux show-environment -g CLAUDE_VOICE_ON_BUSY 2>/dev/null | cut -d= -f2 || echo "false")
            
            if [ "$auto_summary" = "true" ] && [ "$on_busy" = "true" ]; then
                # Fast voice synthesis startup - direct call for speed (window.pane format)
                ~/.tmux/claude/bin/claude-voice brief 10 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
            fi
            ;;
    esac
}

# Only notify for meaningful status changes (non-empty states)
# Allow notifications when OLD_STATUS is empty (initial state) but NEW_STATUS is meaningful
if [ -n "$NEW_STATUS" ] && [ "$OLD_STATUS" != "$NEW_STATUS" ]; then
    notify_status_change
fi