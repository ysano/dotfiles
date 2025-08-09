#!/bin/bash
# Claude Voice - Audio Manager
# 音量管理と音声設定の統合管理

# 設定ファイルのパス
AUDIO_CONFIG_FILE="${CLAUDE_VOICE_HOME}/config/audio.conf"

# デフォルト音量設定
DEFAULT_SAY_VOLUME="0.5"
DEFAULT_SOUND_VOLUME="0.3"
DEFAULT_ALERT_VOLUME="0.4"
DEFAULT_SPEECH_RATE="200"
DEFAULT_VOICE="Kyoko"

# 音量設定を取得
get_audio_config() {
    local key="$1"
    local default="$2"
    
    if [[ -f "$AUDIO_CONFIG_FILE" ]]; then
        local value=$(grep "^${key}=" "$AUDIO_CONFIG_FILE" 2>/dev/null | cut -d'=' -f2 | tr -d ' ')
        if [[ -n "$value" ]]; then
            echo "$value"
        else
            echo "$default"
        fi
    else
        echo "$default"
    fi
}

# 音量設定を更新
set_audio_config() {
    local key="$1"
    local value="$2"
    
    # 設定ファイルが存在しない場合は作成
    if [[ ! -f "$AUDIO_CONFIG_FILE" ]]; then
        mkdir -p "$(dirname "$AUDIO_CONFIG_FILE")"
        touch "$AUDIO_CONFIG_FILE"
    fi
    
    # 既存の設定を更新または新規追加
    if grep -q "^${key}=" "$AUDIO_CONFIG_FILE"; then
        # macOSとLinuxの両方で動作するsed
        if [[ "$OSTYPE" == "darwin"* ]]; then
            sed -i '' "s/^${key}=.*/${key}=${value}/" "$AUDIO_CONFIG_FILE"
        else
            sed -i "s/^${key}=.*/${key}=${value}/" "$AUDIO_CONFIG_FILE"
        fi
    else
        echo "${key}=${value}" >> "$AUDIO_CONFIG_FILE"
    fi
    
    log "INFO" "Audio config updated: ${key}=${value}"
}

# プラットフォーム別の音量取得
get_platform_volume() {
    local type="$1"  # say, sound, alert
    local platform="${2:-$(detect_os)}"
    
    case "$platform" in
        "macos")
            case "$type" in
                "say")
                    get_audio_config "macos.say.volume" "$DEFAULT_SAY_VOLUME"
                    ;;
                "sound")
                    get_audio_config "sound_effects.volume" "$DEFAULT_SOUND_VOLUME"
                    ;;
                "alert")
                    get_audio_config "alert.volume" "$DEFAULT_ALERT_VOLUME"
                    ;;
                *)
                    echo "$DEFAULT_SAY_VOLUME"
                    ;;
            esac
            ;;
        "wsl")
            get_audio_config "wsl.volume" "50"
            ;;
        "linux")
            get_audio_config "linux.volume" "0.5"
            ;;
        "freebsd")
            get_audio_config "freebsd.volume" "0.5"
            ;;
        *)
            echo "0.5"
            ;;
    esac
}

# 音声合成のパラメータ取得
get_speech_params() {
    local platform="${1:-$(detect_os)}"
    
    local voice rate volume
    
    case "$platform" in
        "macos")
            voice=$(get_audio_config "default_voice" "$DEFAULT_VOICE")
            rate=$(get_audio_config "speech_rate" "$DEFAULT_SPEECH_RATE")
            volume=$(get_platform_volume "say" "$platform")
            ;;
        "wsl")
            voice=$(get_audio_config "wsl.voice" "Microsoft Haruka Desktop")
            rate=$(get_audio_config "speech_rate" "$DEFAULT_SPEECH_RATE")
            volume=$(get_audio_config "wsl.volume" "50")
            ;;
        "linux")
            voice=$(get_audio_config "linux.voice" "japanese")
            rate=$(get_audio_config "speech_rate" "$DEFAULT_SPEECH_RATE")
            volume=$(get_audio_config "linux.volume" "0.5")
            ;;
        *)
            voice="$DEFAULT_VOICE"
            rate="$DEFAULT_SPEECH_RATE"
            volume="0.5"
            ;;
    esac
    
    echo "$voice|$rate|$volume"
}

# 音量の正規化（0-1の範囲に収める）
normalize_volume() {
    local volume="$1"
    
    # 数値チェック
    if ! [[ "$volume" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
        echo "$DEFAULT_SAY_VOLUME"
        return
    fi
    
    # 0-1の範囲に正規化
    if (( $(echo "$volume < 0" | bc -l) )); then
        echo "0"
    elif (( $(echo "$volume > 1" | bc -l) )); then
        echo "1"
    else
        echo "$volume"
    fi
}

# macOS専用：システム音量の調整
adjust_macos_system_volume() {
    local target_volume="$1"  # 0-100
    local type="${2:-output}"  # output, alert, input
    
    if [[ "$OSTYPE" != "darwin"* ]]; then
        log "WARN" "System volume adjustment is only available on macOS"
        return 1
    fi
    
    # 現在の音量を取得
    local current_volume=$(get_system_volume "$type")
    
    # 音量を設定
    if set_system_volume "$target_volume" "$type"; then
        log "INFO" "System $type volume changed: $current_volume -> $target_volume"
        return 0
    else
        log "ERROR" "Failed to adjust system $type volume"
        return 1
    fi
}

# 音量プリセットの適用
apply_volume_preset() {
    local preset="$1"
    
    case "$preset" in
        "quiet")
            set_audio_config "macos.say.volume" "0.3"
            set_audio_config "sound_effects.volume" "0.2"
            set_audio_config "alert.volume" "0.3"
            log "INFO" "Applied 'quiet' volume preset"
            ;;
        "normal")
            set_audio_config "macos.say.volume" "0.5"
            set_audio_config "sound_effects.volume" "0.3"
            set_audio_config "alert.volume" "0.4"
            log "INFO" "Applied 'normal' volume preset"
            ;;
        "loud")
            set_audio_config "macos.say.volume" "0.8"
            set_audio_config "sound_effects.volume" "0.5"
            set_audio_config "alert.volume" "0.6"
            log "INFO" "Applied 'loud' volume preset"
            ;;
        *)
            log "ERROR" "Unknown volume preset: $preset"
            return 1
            ;;
    esac
}

# 音量設定のCLIインターフェース
audio_config_cli() {
    local action="$1"
    shift
    
    case "$action" in
        "get")
            local key="$1"
            if [[ -n "$key" ]]; then
                get_audio_config "$key" ""
            else
                cat "$AUDIO_CONFIG_FILE" 2>/dev/null || echo "No audio configuration found"
            fi
            ;;
        "set")
            local key="$1"
            local value="$2"
            if [[ -n "$key" && -n "$value" ]]; then
                set_audio_config "$key" "$value"
                echo "Set $key=$value"
            else
                echo "Usage: audio_config_cli set <key> <value>"
                return 1
            fi
            ;;
        "preset")
            local preset="$1"
            if [[ -n "$preset" ]]; then
                apply_volume_preset "$preset"
            else
                echo "Available presets: quiet, normal, loud"
            fi
            ;;
        "test")
            echo "Testing audio with current settings..."
            local params=$(get_speech_params)
            IFS='|' read -r voice rate volume <<< "$params"
            echo "Voice: $voice, Rate: $rate, Volume: $volume"
            
            # OS別のテスト実行
            if [[ "$OSTYPE" == "darwin"* ]]; then
                # sayコマンドで音量を直接指定できないため、afplayで代替
                local test_text="音量テスト中です"
                say -v "$voice" -r "$rate" "$test_text"
            fi
            ;;
        *)
            echo "Usage: audio_config_cli {get|set|preset|test} [args...]"
            echo "  get [key]      - Get audio configuration"
            echo "  set key value  - Set audio configuration"
            echo "  preset name    - Apply volume preset (quiet/normal/loud)"
            echo "  test           - Test current audio settings"
            return 1
            ;;
    esac
}

# エクスポート
export -f get_audio_config
export -f set_audio_config
export -f get_platform_volume
export -f get_speech_params
export -f normalize_volume
export -f adjust_macos_system_volume
export -f apply_volume_preset
export -f audio_config_cli