#!/bin/bash
# Claude Voice - macOS specific functions
# macOS固有の音声・通知機能

# macOS固有の依存関係チェック
check_macos_dependencies() {
    local missing_deps=()

    # sayコマンドの確認（macOS標準）
    if ! has_command say; then
        missing_deps+=("say (macOS built-in)")
    fi

    # osascriptの確認（macOS標準）
    if ! has_command osascript; then
        missing_deps+=("osascript (macOS built-in)")
    fi

    # オプショナルな依存関係
    if ! has_command terminal-notifier; then
        log "WARN" "terminal-notifier not found, using fallback notifications"
    fi

    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log "ERROR" "Missing dependencies: ${missing_deps[*]}"
        return 1
    fi

    log "DEBUG" "macOS dependencies check passed"
    return 0
}

# macOSサウンドデバイス検出（抽象的なデバイス概念を使用）
get_audio_devices() {
    log "DEBUG" "Getting macOS default audio output concept"

    # システムのデフォルト出力を使用（具体的なデバイス名ではなく概念）
    echo "system_default"
}

# say コマンド対応デバイス一覧
get_say_devices() {
    log "DEBUG" "Getting say-compatible audio devices"

    if has_command say; then
        local devices=$(say -a '?' 2>&1 | sed 's/^[[:space:]]*[0-9]*[[:space:]]*//')
        echo "$devices"
    else
        log "ERROR" "say command not available"
        return 1
    fi
}

# 利用可能な音声一覧
get_available_voices() {
    log "DEBUG" "Getting available voices for macOS"

    if has_command say; then
        local voices=$(say -v '?' 2>/dev/null | awk '{print $1}' | sort)
        echo "$voices"
    else
        log "ERROR" "say command not available for voice listing"
        echo "Kyoko" # デフォルト
    fi
}

# デバイス名の正規化（抽象的概念から実際の設定へ）
normalize_device_name() {
    local device_concept="$1"

    log "DEBUG" "Normalizing device concept: $device_concept"

    case "$device_concept" in
        "system_default" | "auto" | "")
            # Use system default - no explicit device specification
            # This allows macOS to route through user's configured default output
            echo ""
            ;;
        "alert_device")
            # For alert sounds, ensure we use the alert volume routing
            if has_command osascript; then
                # Get current alert device (usually same as output but respects alert volume)
                echo "" # Let macOS handle alert routing
            else
                echo ""
            fi
            ;;
        "sound_effects")
            # For sound effects, try to use the effects audio device
            echo "" # macOS handles this through the sound effects setting
            ;;
        *)
            # For specific device names, validate they exist
            if [[ -n "$device_concept" ]]; then
                local available_devices=$(get_say_devices 2>/dev/null)
                if echo "$available_devices" | grep -qi "$device_concept"; then
                    echo "$device_concept"
                else
                    log "WARN" "Device '$device_concept' not found, using system default"
                    echo ""
                fi
            else
                echo ""
            fi
            ;;
    esac
}

# 音声合成実行
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "Kyoko")}"
    local device="${3:-auto}"
    local rate="${4:-$(get_config "audio.speech_rate" "200")}"

    log "DEBUG" "Speaking text on macOS: voice=$voice, device=$device, rate=$rate"

    # 依存関係チェック
    if ! check_macos_dependencies; then
        log "ERROR" "Cannot speak: missing dependencies"
        return 1
    fi

    # テキストの前処理
    local processed_text=$(preprocess_speech_text "$text")

    # デバイスの解決
    local target_device=""
    if [[ "$device" == "auto" ]]; then
        local system_device=$(get_audio_devices)
        target_device=$(normalize_device_name "$system_device")
    else
        target_device=$(normalize_device_name "$device")
    fi

    # 音声合成の実行（システムデフォルトを使用）
    local say_args=()
    say_args+=("-v" "$voice")
    say_args+=("-r" "$rate")

    # デバイス指定がある場合のみ追加（空文字列の場合はシステムデフォルト）
    if [[ -n "$target_device" ]]; then
        say_args+=("-a" "$target_device")
        log "DEBUG" "Using specific audio device: $target_device"
    else
        log "DEBUG" "Using system default audio output"
    fi

    # 実行とエラーハンドリング - macOS音声セッション管理を強化
    local start_time=$(start_timer)

    log "DEBUG" "Executing: say ${say_args[*]} \"$processed_text\""

    # Phase 2 Fix: Ensure proper audio session access for background processes
    if [[ -n "${TMUX:-}" ]] && [[ "${CLAUDE_VOICE_BACKGROUND_MODE:-false}" == "true" ]]; then
        # When running in tmux background mode, use osascript for audio session access
        local escaped_text=$(printf '%s\n' "$processed_text" | sed 's/[[\.*^$()+?{|]/\\&/g')
        local say_command="say"
        for arg in "${say_args[@]}"; do
            say_command="$say_command $(printf '%q' "$arg")"
        done
        say_command="$say_command $(printf '%q' "$processed_text")"

        log "DEBUG" "Background mode: using osascript for audio session access"
        if osascript -e "do shell script \"$say_command\"" 2>/dev/null; then
            local duration=$(end_timer "$start_time")
            log "INFO" "Speech synthesis completed via osascript (${duration}s)"
            return 0
        fi
    fi

    # Standard execution path
    if say "${say_args[@]}" "$processed_text"; then
        local duration=$(end_timer "$start_time")
        log "INFO" "Speech synthesis completed (${duration}s)"
        return 0
    else
        # エラーの場合、最小限の引数で再試行
        log "WARN" "Speech failed, retrying with minimal arguments"
        log "DEBUG" "Fallback: say -v \"$voice\" \"$processed_text\""
        if say -v "$voice" "$processed_text"; then
            local duration=$(end_timer "$start_time")
            log "INFO" "Speech synthesis completed with fallback (${duration}s)"
            return 0
        fi

        # Final fallback: osascript execution
        log "WARN" "Standard say failed, trying osascript fallback"
        local escaped_text=$(printf '%s\n' "$processed_text" | sed 's/[[\.*^$()+?{|]/\\&/g')
        if osascript -e "say \"$escaped_text\" using \"$voice\"" 2>/dev/null; then
            local duration=$(end_timer "$start_time")
            log "INFO" "Speech synthesis completed via osascript fallback (${duration}s)"
            return 0
        fi

        log "ERROR" "Speech synthesis failed"
        return 1
    fi
}

# 音声テキストの前処理
preprocess_speech_text() {
    local text="$1"

    # 特殊文字の読み上げ対応
    local processed=$(echo "$text" |
        sed 's/⏺/○/g' |
        sed 's/✅/成功/g' |
        sed 's/❌/エラー/g' |
        sed 's/📁/フォルダ/g' |
        sed 's/🔧/設定/g' |
        sed 's/&/アンド/g' |
        sed 's/@/アット/g' |
        sed 's/#/シャープ/g')

    # URL の簡略化
    processed=$(echo "$processed" | sed 's|https\?://[^ ]*|URL|g')

    # 長すぎるテキストの短縮
    local max_length=$(get_config "audio.max_speech_length" "500")
    if [[ ${#processed} -gt $max_length ]]; then
        processed="${processed:0:$max_length}。以下省略。"
    fi

    echo "$processed"
}

# システム通知の送信
send_notification() {
    local title="$1"
    local message="$2"
    local sound="${3:-$(get_config "audio.notification_sound" "Glass")}"
    local urgency="${4:-normal}"

    log "DEBUG" "Sending macOS notification: title=$title, sound=$sound, urgency=$urgency"

    # Do Not Disturb状態の確認
    local respect_dnd=$(get_config "audio.respect_dnd" "true")
    local dnd_enabled=false

    if [[ "$respect_dnd" == "true" ]] && is_dnd_enabled; then
        dnd_enabled=true
        log "DEBUG" "Do Not Disturb is enabled"
    fi

    # terminal-notifier の使用を試行
    if has_command terminal-notifier; then
        local tn_args=()
        tn_args+=("-title" "$title")
        tn_args+=("-message" "$message")
        tn_args+=("-group" "claude-voice")
        tn_args+=("-sender" "com.apple.Terminal")

        if [[ "$dnd_enabled" != "true" ]]; then
            tn_args+=("-sound" "$sound")
            if [[ "$urgency" == "critical" ]]; then
                tn_args+=("-ignoreDnD")
            fi
        fi

        if terminal-notifier "${tn_args[@]}" 2>/dev/null; then
            log "DEBUG" "Notification sent via terminal-notifier"
            return 0
        else
            log "WARN" "terminal-notifier failed, falling back to osascript"
        fi
    fi

    # osascript フォールバック
    if has_command osascript; then
        local script="display notification \"$message\" with title \"$title\""

        if [[ "$dnd_enabled" != "true" && "$sound" != "none" ]]; then
            script+=" sound name \"$sound\""
        fi

        if osascript -e "$script" 2>/dev/null; then
            log "DEBUG" "Notification sent via osascript"
            return 0
        else
            log "ERROR" "osascript notification failed"
        fi
    fi

    # 最終フォールバック（コンソール出力）
    echo "通知: $title - $message"
    return 1
}

# Do Not Disturb状態の確認
is_dnd_enabled() {
    # macOS Monterey以降の方法
    if has_command plutil; then
        local dnd_status=$(plutil -extract dnd_prefs.userPref.enabled xml1 \
            ~/Library/Preferences/com.apple.ncprefs.plist -o - 2>/dev/null |
            grep -o '<true/>' | head -1)

        if [[ "$dnd_status" == "<true/>" ]]; then
            return 0 # DND is enabled
        fi
    fi

    # 代替方法: Focus状態の確認
    if has_command shortcuts; then
        local focus_status=$(shortcuts run "Get Current Focus" 2>/dev/null)
        if [[ -n "$focus_status" ]] && [[ "$focus_status" != "None" ]]; then
            return 0 # Focus mode is active
        fi
    fi

    return 1 # DND is disabled
}

# システムビープ音
system_beep() {
    local count="${1:-1}"
    local frequency="${2:-1000}"
    local duration="${3:-200}"

    log "DEBUG" "Playing system beep on macOS: count=$count"

    if has_command osascript; then
        for ((i = 1; i <= count; i++)); do
            osascript -e 'beep' 2>/dev/null
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep 0.2
            fi
        done
    else
        # フォールバック
        for ((i = 1; i <= count; i++)); do
            echo -e '\a'
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep 0.2
            fi
        done
    fi
}

# サウンドファイルの再生
play_sound_file() {
    local file_path="$1"
    local volume="${2:-$(get_config "audio.volume" "0.8")}"

    log "DEBUG" "Playing sound file on macOS: $file_path, volume=$volume"

    if [[ ! -f "$file_path" ]]; then
        log "ERROR" "Sound file not found: $file_path"
        return 1
    fi

    if has_command afplay; then
        local afplay_args=()
        afplay_args+=("-v" "$volume")

        if afplay "${afplay_args[@]}" "$file_path" 2>/dev/null; then
            log "DEBUG" "Sound file played successfully"
            return 0
        else
            log "ERROR" "Failed to play sound file with afplay"
            return 1
        fi
    else
        log "ERROR" "afplay not available for sound file playback"
        return 1
    fi
}

# 音量制御
set_system_volume() {
    local volume="$1"         # 0-100
    local type="${2:-output}" # output, alert, input

    log "DEBUG" "Setting macOS system volume: $volume ($type)"

    if has_command osascript; then
        case "$type" in
            "output")
                osascript -e "set volume output volume $volume" 2>/dev/null
                ;;
            "alert")
                osascript -e "set volume alert volume $volume" 2>/dev/null
                ;;
            "input")
                osascript -e "set volume input volume $volume" 2>/dev/null
                ;;
            *)
                log "ERROR" "Invalid volume type: $type"
                return 1
                ;;
        esac

        log "DEBUG" "System volume set successfully"
        return 0
    else
        log "ERROR" "osascript not available for volume control"
        return 1
    fi
}

# 現在の音量取得
get_system_volume() {
    local type="${1:-output}"

    if has_command osascript; then
        local volume_info=$(osascript -e "get volume settings" 2>/dev/null)

        case "$type" in
            "output")
                echo "$volume_info" | sed -n 's/.*output volume:\([0-9]*\).*/\1/p'
                ;;
            "alert")
                echo "$volume_info" | sed -n 's/.*alert volume:\([0-9]*\).*/\1/p'
                ;;
            "input")
                echo "$volume_info" | sed -n 's/.*input volume:\([0-9]*\).*/\1/p'
                ;;
            *)
                echo "$volume_info"
                ;;
        esac
    else
        echo "0"
    fi
}

# macOS固有の初期化
init_macos_audio() {
    log "INFO" "Initializing macOS audio subsystem"

    # 依存関係チェック
    if ! check_macos_dependencies; then
        return 1
    fi

    # デフォルト設定の確認
    local default_voice=$(get_config "audio.default_voice" "Kyoko")
    local available_voices=$(get_available_voices)

    if ! echo "$available_voices" | grep -q "^$default_voice$"; then
        log "WARN" "Default voice '$default_voice' not available, using system default"
    fi

    # オーディオデバイスの確認
    local audio_devices=$(get_audio_devices)
    log "DEBUG" "Available audio devices: $audio_devices"

    log "INFO" "macOS audio subsystem initialized successfully"
    return 0
}

# このモジュールのテスト関数
test_macos_functions() {
    echo "Testing macOS-specific functions..."

    # 依存関係チェック
    if check_macos_dependencies; then
        echo "Dependencies: OK"
    else
        echo "Dependencies: MISSING"
    fi

    # オーディオデバイスの取得
    local devices=$(get_audio_devices)
    echo "Audio devices: $devices"

    # 利用可能な音声の取得
    local voices=$(get_available_voices | head -5)
    echo "Available voices (first 5): $voices"

    # 現在の音量取得
    local volume=$(get_system_volume "output")
    echo "Current output volume: $volume"

    # DND状態の確認
    if is_dnd_enabled; then
        echo "Do Not Disturb: ENABLED"
    else
        echo "Do Not Disturb: DISABLED"
    fi

    # 短いテスト音声（オプション）
    local test_speech=$(get_config "test.enable_speech" "false")
    if [[ "$test_speech" == "true" ]]; then
        echo "Testing speech synthesis..."
        speak_text "テスト" "Kyoko" "auto" "300"
    fi

    echo "macOS functions test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    source "$SCRIPT_DIR/core/base.sh"

    claude_voice_init true
    test_macos_functions
fi
