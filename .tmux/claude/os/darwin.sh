#!/bin/bash
# Claude Voice - macOS specific functions  
# macOS固有の音声・通知機能
# WSL改善統合版 - ユニバーサル音声システム統合

# ユニバーサル音声システムの読み込み
UNIVERSAL_VOICE_SCRIPT="$HOME/.tmux/claude/core/universal_voice.sh"
if [[ -f "$UNIVERSAL_VOICE_SCRIPT" ]]; then
    source "$UNIVERSAL_VOICE_SCRIPT"
fi

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

    # macOSではsayコマンドがシステムデフォルト出力を使用するため
    # 単純にsystem_defaultを返す
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
        "speaker_fallback")
            # BlackHole や仮想デバイス使用時の実際スピーカーへのフォールバック
            # Try to find actual speakers/headphones
            local physical_devices=$(say -a '?' 2>&1 | grep -E "(Built-in|Internal|Speakers|Headphones)" | head -1 | sed 's/^[[:space:]]*[0-9]*[[:space:]]*//')
            if [[ -n "$physical_devices" ]]; then
                log "DEBUG" "Using physical audio device: $physical_devices"
                echo "$physical_devices"
            else
                log "WARN" "No physical audio device found, using system default"
                echo ""
            fi
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

# Claude Code実行中ウィンドウの検出
get_active_claude_windows() {
    local active_windows=()
    
    # ステータスファイルから実行中ウィンドウを検出
    if [[ -d "$HOME/.tmux/status" ]]; then
        for status_file in "$HOME/.tmux/status"/window-*.status; do
            [[ -f "$status_file" ]] || continue
            
            local window_id=$(basename "$status_file" | sed 's/window-\([0-9]*\)\.status/\1/')
            local status_content=$(cat "$status_file" 2>/dev/null)
            
            # Claude Codeのアイコン（⚡、⌛、✅）があるウィンドウを検出
            if [[ "$status_content" == "⚡" || "$status_content" == "⌛" || "$status_content" == "✅" ]]; then
                # tmuxウィンドウが実際に存在することを確認
                if tmux list-windows -F '#I' 2>/dev/null | grep -q "^${window_id}$"; then
                    active_windows+=("$window_id")
                fi
            fi
        done
    fi
    
    # ウィンドウIDでソート
    printf '%s\n' "${active_windows[@]}" | sort -n
}

# 均等配置パンニング位置の計算
calculate_dynamic_panning() {
    local target_window_id="$1"
    
    # ウィンドウIDが無効な場合は中央
    if [[ -z "$target_window_id" || ! "$target_window_id" =~ ^[0-9]+$ ]]; then
        echo "0.5 0.5"
        return
    fi
    
    # 実行中Claude Codeウィンドウのリストを取得
    local active_windows=()
    local active_list=$(get_active_claude_windows)
    if [[ -n "$active_list" ]]; then
        while IFS= read -r window_id; do
            [[ -n "$window_id" ]] && active_windows+=("$window_id")
        done <<< "$active_list"
    fi
    
    local window_count=${#active_windows[@]}
    
    # アクティブウィンドウが見つからない場合は中央
    if [[ $window_count -eq 0 ]]; then
        log "DEBUG" "No active Claude windows found, using center position"
        echo "0.5 0.5"
        return
    fi
    
    # 現在のウィンドウがリストに含まれているかチェック
    local window_index=-1
    local i=0
    for window in "${active_windows[@]}"; do
        if [[ "$window" == "$target_window_id" ]]; then
            window_index=$i
            break
        fi
        ((i++))
    done
    
    # 対象ウィンドウが見つからない場合は中央
    if [[ $window_index -eq -1 ]]; then
        log "DEBUG" "Target window $target_window_id not in active list, using center position"
        echo "0.5 0.5"
        return
    fi
    
    # 均等配置の位置計算
    local position
    if [[ $window_count -eq 1 ]]; then
        position=0.5  # 1つのウィンドウ: 中央
    else
        # 複数ウィンドウ: 0.0から1.0を等分割
        position=$(awk "BEGIN { printf \"%.3f\", $window_index / ($window_count - 1) }")
    fi
    
    # デシベル計算による左右チャンネルのゲイン計算
    # Equal Power Pan Law (-3dB center) を使用
    local pan_angle=$(awk "BEGIN { printf \"%.6f\", $position * 1.5707963267948966 }")  # π/2 radians
    local left_gain=$(awk "BEGIN { printf \"%.6f\", cos($pan_angle) * 1.414213562373095 }")   # √2 * cos(θ)
    local right_gain=$(awk "BEGIN { printf \"%.6f\", sin($pan_angle) * 1.414213562373095 }")  # √2 * sin(θ)
    
    # デシベル制限 (最小-60dB, 最大0dB)
    local left_db=$(awk "BEGIN { 
        if ($left_gain > 0.001) 
            printf \"%.2f\", 20 * log($left_gain) / log(10)
        else 
            printf \"-60.00\"
    }")
    local right_db=$(awk "BEGIN { 
        if ($right_gain > 0.001) 
            printf \"%.2f\", 20 * log($right_gain) / log(10)
        else 
            printf \"-60.00\"
    }")
    
    log "DEBUG" "Dynamic dB panning - Window $target_window_id (${window_index}/${window_count}): position=$position"
    log "DEBUG" "Pan angle: ${pan_angle}rad, L_gain=${left_gain}(${left_db}dB), R_gain=${right_gain}(${right_db}dB)"
    log "DEBUG" "Active windows: ${active_windows[*]}"
    
    echo "$left_gain $right_gain"
}

# 後方互換性のためのエイリアス関数
calculate_panning() {
    calculate_dynamic_panning "$1"
}

# パンニング音声再生
speak_text_with_panning() {
    local text="$1"
    local voice="$2"
    local rate="$3"
    local window_id="$4"
    
    # 動的パンニング値を計算
    local panning_values=$(calculate_dynamic_panning "$window_id")
    local left_gain=$(echo "$panning_values" | cut -d' ' -f1)
    local right_gain=$(echo "$panning_values" | cut -d' ' -f2)
    
    log "DEBUG" "Panning for window $window_id: L=$left_gain, R=$right_gain"
    
    # 一時音声ファイルを作成
    local temp_audio_file=$(mktemp -t "claude_voice_XXXXXX.aiff")
    
    # sayコマンドで音声ファイルを生成
    if say -v "$voice" -r "$rate" -o "$temp_audio_file" "$text" 2>/dev/null; then
        log "DEBUG" "Generated audio file: $temp_audio_file"
        
        # ffplayでパンニング再生
        local pan_filter="pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0"
        
        if ffplay -i "$temp_audio_file" -af "$pan_filter" -autoexit -nodisp -loglevel quiet 2>/dev/null; then
            log "DEBUG" "Panned audio playback successful"
            rm -f "$temp_audio_file"
            return 0
        else
            log "WARN" "Panned playback failed, falling back to direct say"
            rm -f "$temp_audio_file"
            return 1
        fi
    else
        log "ERROR" "Failed to generate audio file"
        rm -f "$temp_audio_file"
        return 1
    fi
}

# 音声合成実行
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "Kyoko")}"
    local device="${3:-auto}"
    local rate="${4:-$(get_config "audio.speech_rate" "200")}"
    local window_id="${5:-}"  # ウィンドウIDによるパンニング制御

    log "DEBUG" "Speaking text on macOS: voice=$voice, device=$device, rate=$rate, window_id=$window_id"
    
    # パンニング機能の有効性チェック
    local use_panning=false
    if [[ -n "$window_id" && "$window_id" != "." ]] && has_command ffplay; then
        use_panning=true
        log "DEBUG" "Panning enabled for window $window_id"
    fi

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
    
    # パンニング再生の分岐
    if [[ "$use_panning" == "true" ]]; then
        log "DEBUG" "Using panned audio playback for window $window_id"
        if speak_text_with_panning "$processed_text" "$voice" "$rate" "$window_id"; then
            local elapsed_time=$(get_elapsed_time "$start_time")
            log "INFO" "Panned speech completed successfully in ${elapsed_time}ms"
            return 0
        else
            log "WARN" "Panned speech failed, falling back to standard say"
            # フォールバックとして通常のsay実行を継続
        fi
    fi

    log "DEBUG" "Executing: say ${say_args[*]} \"$processed_text\""

    # タイムアウト制限（20秒でタイムアウト）
    local timeout_duration=20
    
    # macOS用のタイムアウト実装（brew install coreutils でインストールされる gtimeout を優先）
    local timeout_cmd=""
    if has_command gtimeout; then
        timeout_cmd="gtimeout"
    elif has_command timeout; then
        timeout_cmd="timeout"
    else
        # フォールバック: タイムアウトなしで実行（タイムアウト機能を無効化）
        timeout_cmd=""
        log "WARN" "No timeout command available, speech may run without time limit"
    fi

    # Phase 2 Fix: Ensure proper audio session access for background processes
    if [[ -n "${TMUX:-}" ]] && [[ "${CLAUDE_VOICE_BACKGROUND_MODE:-false}" == "true" ]]; then
        # When running in tmux background mode, use osascript for audio session access
        local escaped_text=$(printf '%s\n' "$processed_text" | sed 's/[[\.*^$()+?{|]/\\&/g')
        local say_command="say"
        for arg in "${say_args[@]}"; do
            say_command="$say_command $(printf '%q' "$arg")"
        done
        say_command="$say_command $(printf '%q' "$processed_text")"

        log "DEBUG" "Background mode: using osascript for audio session access (timeout: ${timeout_duration}s)"
        if [[ -n "$timeout_cmd" ]]; then
            if "$timeout_cmd" "$timeout_duration" osascript -e "do shell script \"$say_command\"" 2>/dev/null; then
                local duration=$(end_timer "$start_time")
                log "INFO" "Speech synthesis completed via osascript (${duration}s)"
                return 0
            elif [[ $? -eq 124 ]]; then
                log "WARN" "Speech synthesis timed out after ${timeout_duration}s (osascript)"
                return 1
            fi
        else
            # フォールバック: タイムアウトなしで実行
            if osascript -e "do shell script \"$say_command\"" 2>/dev/null; then
                local duration=$(end_timer "$start_time")
                log "INFO" "Speech synthesis completed via osascript (${duration}s)"
                return 0
            fi
        fi
    fi

    # Standard execution path with timeout
    if [[ -n "$timeout_cmd" ]]; then
        if "$timeout_cmd" "$timeout_duration" say "${say_args[@]}" "$processed_text"; then
            local duration=$(end_timer "$start_time")
            log "INFO" "Speech synthesis completed (${duration}s)"
            return 0
        elif [[ $? -eq 124 ]]; then
            log "WARN" "Speech synthesis timed out after ${timeout_duration}s (say)"
            return 1
        else
            # エラーの場合、最小限の引数で再試行
            log "WARN" "Speech failed, retrying with minimal arguments"
            log "DEBUG" "Fallback: say -v \"$voice\" \"$processed_text\""
            if "$timeout_cmd" "$timeout_duration" say -v "$voice" "$processed_text"; then
                local duration=$(end_timer "$start_time")
                log "INFO" "Speech synthesis completed with fallback (${duration}s)"
                return 0
            elif [[ $? -eq 124 ]]; then
                log "WARN" "Speech synthesis fallback timed out after ${timeout_duration}s"
                return 1
            fi

            # Final fallback: osascript execution
            log "WARN" "Standard say failed, trying osascript fallback"
            local escaped_text=$(printf '%s\n' "$processed_text" | sed 's/[[\.*^$()+?{|]/\\&/g')
            if "$timeout_cmd" "$timeout_duration" osascript -e "say \"$escaped_text\" using \"$voice\"" 2>/dev/null; then
                local duration=$(end_timer "$start_time")
                log "INFO" "Speech synthesis completed via osascript fallback (${duration}s)"
                return 0
            elif [[ $? -eq 124 ]]; then
                log "WARN" "Speech synthesis osascript fallback timed out after ${timeout_duration}s"
                return 1
            fi

            log "ERROR" "Speech synthesis failed"
            return 1
        fi
    else
        # タイムアウトコマンドが利用できない場合の通常実行
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

    # 長すぎるテキストの短縮（パフォーマンス改善）
    local max_length=$(get_config "audio.max_speech_length" "300")  # 500→300文字に短縮
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

# パンニング対応サウンドファイル再生
play_sound_file_with_panning() {
    local file_path="$1"
    local volume="${2:-$(get_config "audio.volume" "4.0")}"
    local window_id="${3:-}"

    log "DEBUG" "Playing sound file with panning on macOS: $file_path, volume=$volume, window_id=$window_id"

    if [[ ! -f "$file_path" ]]; then
        log "ERROR" "Sound file not found: $file_path"
        return 1
    fi

    # パンニング機能の有効性チェック
    local use_panning=false
    if [[ -n "$window_id" && "$window_id" != "." ]] && has_command ffplay; then
        use_panning=true
        log "DEBUG" "Panning enabled for sound file with window $window_id"
    fi

    # パンニング再生の実行
    if [[ "$use_panning" == "true" ]]; then
        # 動的パンニング値を計算
        local panning_values=$(calculate_dynamic_panning "$window_id")
        local left_gain=$(echo "$panning_values" | cut -d' ' -f1)
        local right_gain=$(echo "$panning_values" | cut -d' ' -f2)
        
        log "DEBUG" "Sound file panning for window $window_id: L=$left_gain, R=$right_gain"
        
        # ffplayでパンニング再生
        local pan_filter="pan=stereo|c0=${left_gain}*c0|c1=${right_gain}*c0,volume=${volume}"
        
        if ffplay -i "$file_path" -af "$pan_filter" -autoexit -nodisp -loglevel quiet 2>/dev/null; then
            log "DEBUG" "Panned sound file playback successful"
            return 0
        else
            log "WARN" "Panned sound file playback failed, falling back to afplay"
            # フォールバックとして通常のafplay実行を継続
        fi
    fi

    # 通常のafplay再生（フォールバック）
    if has_command afplay; then
        local afplay_args=()
        afplay_args+=("-v" "$volume")

        if afplay "${afplay_args[@]}" "$file_path" 2>/dev/null; then
            log "DEBUG" "Sound file played successfully with afplay fallback"
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

# 動的パンニングシステムのテスト関数
test_dynamic_panning() {
    echo "=== Dynamic Panning System Test ==="
    echo ""
    
    # 1. アクティブウィンドウの検出テスト
    echo "1. Testing active Claude windows detection..."
    local active_windows=()
    while IFS= read -r window_id; do
        [[ -n "$window_id" ]] && active_windows+=("$window_id")
    done < <(get_active_claude_windows)
    
    local window_count=${#active_windows[@]}
    echo "   Found $window_count active Claude Code windows: ${active_windows[*]}"
    
    if [[ $window_count -eq 0 ]]; then
        echo "   ⚠️  No active windows found. Create some Claude Code activity to test panning."
        return 1
    fi
    
    # 2. 各ウィンドウのパンニング計算テスト
    echo ""
    echo "2. Testing dynamic panning calculations..."
    for window_id in "${active_windows[@]}"; do
        local panning_values=$(calculate_dynamic_panning "$window_id")
        local left_gain=$(echo "$panning_values" | cut -d' ' -f1)
        local right_gain=$(echo "$panning_values" | cut -d' ' -f2)
        
        echo "   Window $window_id: L=$left_gain, R=$right_gain"
        
        # パンニング値の妥当性チェック
        local total_gain=$(awk "BEGIN { printf \"%.3f\", $left_gain + $right_gain }")
        if [[ "$total_gain" != "1.000" ]]; then
            echo "   ❌ Invalid gain total: $total_gain (should be 1.000)"
        else
            echo "   ✅ Gain values valid"
        fi
    done
    
    # 3. エッジケースのテスト
    echo ""
    echo "3. Testing edge cases..."
    
    # 存在しないウィンドウ
    local invalid_panning=$(calculate_dynamic_panning "999")
    echo "   Invalid window (999): $invalid_panning"
    
    # 空文字列
    local empty_panning=$(calculate_dynamic_panning "")
    echo "   Empty window ID: $empty_panning"
    
    # 4. 実際の音声テスト（オプション）
    echo ""
    echo "4. Audio panning test (optional)..."
    local test_audio=$(get_config "test.enable_panning_audio" "false")
    if [[ "$test_audio" == "true" && $window_count -gt 0 ]]; then
        echo "   Testing audio panning for each active window..."
        for window_id in "${active_windows[@]}"; do
            echo "   Playing test audio for window $window_id..."
            speak_text "ウィンドウ ${window_id} からのテスト音声" "Kyoko" "system_default" "200" "$window_id" &
            sleep 2
        done
        wait
    else
        echo "   ⚠️  Audio test disabled (set test.enable_panning_audio=true to enable)"
    fi
    
    echo ""
    echo "✅ Dynamic panning system test completed"
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

    # 動的パンニングシステムのテスト
    echo ""
    test_dynamic_panning

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
