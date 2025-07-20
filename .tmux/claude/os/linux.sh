#!/bin/bash
# Claude Voice - Linux specific functions
# Linux固有の音声・通知機能

# Linux固有の依存関係チェック
check_linux_dependencies() {
    local missing_deps=()
    local optional_deps=()
    
    # 音声合成エンジンの確認（優先順位順）
    local tts_engines=("espeak-ng" "espeak" "festival" "spd-say" "pico2wave")
    local tts_available=false
    
    for engine in "${tts_engines[@]}"; do
        if has_command "$engine"; then
            tts_available=true
            log "DEBUG" "Found TTS engine: $engine"
            break
        fi
    done
    
    if [[ "$tts_available" != "true" ]]; then
        missing_deps+=("TTS engine (espeak-ng, espeak, festival, spd-say, or pico2wave)")
    fi
    
    # 音声システムの確認
    local audio_systems=("pulseaudio" "alsa" "pipewire")
    local audio_available=false
    
    for system in "${audio_systems[@]}"; do
        if has_command "pactl" || has_command "amixer" || has_command "pw-cat"; then
            audio_available=true
            log "DEBUG" "Found audio system: $system"
            break
        fi
    done
    
    if [[ "$audio_available" != "true" ]]; then
        optional_deps+=("audio system (pulseaudio, alsa, or pipewire)")
    fi
    
    # 通知システムの確認
    if ! has_command notify-send; then
        optional_deps+=("notify-send (libnotify)")
    fi
    
    # エラー報告
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log "ERROR" "Missing critical dependencies: ${missing_deps[*]}"
        return 1
    fi
    
    if [[ ${#optional_deps[@]} -gt 0 ]]; then
        log "WARN" "Missing optional dependencies: ${optional_deps[*]}"
    fi
    
    log "DEBUG" "Linux dependencies check passed"
    return 0
}

# Linuxオーディオシステムの検出
detect_audio_system() {
    log "DEBUG" "Detecting Linux audio system"
    
    # PulseAudio
    if has_command pactl && pgrep -x pulseaudio >/dev/null 2>&1; then
        echo "pulseaudio"
        return 0
    fi
    
    # PipeWire
    if has_command pw-cat && pgrep -x pipewire >/dev/null 2>&1; then
        echo "pipewire"
        return 0
    fi
    
    # ALSA
    if has_command amixer; then
        echo "alsa"
        return 0
    fi
    
    log "WARN" "No recognized audio system found"
    echo "none"
    return 1
}

# オーディオデバイスの取得
get_audio_devices() {
    local audio_system=$(detect_audio_system)
    
    log "DEBUG" "Getting audio devices for system: $audio_system"
    
    case "$audio_system" in
        "pulseaudio")
            pactl list short sinks 2>/dev/null | awk '{print $2}' | head -1
            ;;
        "pipewire")
            pw-cat --list-targets 2>/dev/null | grep -E "^[0-9]+" | head -1 | awk '{print $2}'
            ;;
        "alsa")
            # ALSAデフォルトデバイス
            echo "default"
            ;;
        *)
            echo "default"
            ;;
    esac
}

# 最適なTTSエンジンの選択
select_tts_engine() {
    local language="${1:-ja}"
    local quality="${2:-normal}"
    
    log "DEBUG" "Selecting TTS engine for language: $language, quality: $quality"
    
    # 優先順位に基づくエンジン選択
    local engines=()
    
    case "$language" in
        "ja"|"japanese")
            # 日本語対応エンジンの優先順位
            engines=("espeak-ng" "espeak" "festival" "spd-say")
            ;;
        "en"|"english")
            # 英語対応エンジンの優先順位
            engines=("espeak-ng" "espeak" "festival" "spd-say" "pico2wave")
            ;;
        *)
            # 汎用エンジン
            engines=("espeak-ng" "espeak" "spd-say" "festival")
            ;;
    esac
    
    # 利用可能な最初のエンジンを選択
    for engine in "${engines[@]}"; do
        if has_command "$engine"; then
            echo "$engine"
            return 0
        fi
    done
    
    log "ERROR" "No suitable TTS engine found"
    return 1
}

# 音声合成実行
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "auto")}"
    local device="${3:-auto}"
    local rate="${4:-$(get_config "audio.speech_rate" "150")}"
    
    log "DEBUG" "Speaking text on Linux: voice=$voice, device=$device, rate=$rate"
    
    # 依存関係チェック
    if ! check_linux_dependencies; then
        log "ERROR" "Cannot speak: missing dependencies"
        return 1
    fi
    
    # テキストの前処理
    local processed_text=$(preprocess_speech_text "$text")
    
    # TTSエンジンの選択
    local tts_engine=$(select_tts_engine "ja" "normal")
    if [[ -z "$tts_engine" ]]; then
        log "ERROR" "No TTS engine available"
        return 1
    fi
    
    # デバイスの解決
    local target_device=""
    if [[ "$device" == "auto" ]]; then
        target_device=$(get_audio_devices)
    else
        target_device="$device"
    fi
    
    # エンジン別の音声合成実行
    local start_time=$(start_timer)
    local success=false
    
    case "$tts_engine" in
        "espeak-ng"|"espeak")
            local espeak_args=()
            espeak_args+=("-s" "$rate")
            espeak_args+=("-v" "$voice")
            
            if [[ "$target_device" != "default" ]] && [[ -n "$target_device" ]]; then
                # 特定デバイス向けの出力は、パイプライン経由で実現
                if has_command paplay; then
                    echo "$processed_text" | $tts_engine "${espeak_args[@]}" --stdout | paplay --device="$target_device" 2>/dev/null
                else
                    echo "$processed_text" | $tts_engine "${espeak_args[@]}" 2>/dev/null
                fi
            else
                echo "$processed_text" | $tts_engine "${espeak_args[@]}" 2>/dev/null
            fi
            
            if [[ $? -eq 0 ]]; then
                success=true
            fi
            ;;
            
        "festival")
            # Festival の場合
            local festival_cmd="(SayText \"$processed_text\")"
            
            if echo "$festival_cmd" | festival --batch 2>/dev/null; then
                success=true
            fi
            ;;
            
        "spd-say")
            local spd_args=()
            spd_args+=("-r" "$rate")
            spd_args+=("-v" "$voice")
            
            if spd-say "${spd_args[@]}" "$processed_text" 2>/dev/null; then
                success=true
            fi
            ;;
            
        "pico2wave")
            # pico2wave + 再生
            local temp_wav="/tmp/claude_voice_$$.wav"
            
            if pico2wave -l en-US -w "$temp_wav" "$processed_text" 2>/dev/null; then
                if has_command paplay; then
                    paplay "$temp_wav" 2>/dev/null && success=true
                elif has_command aplay; then
                    aplay "$temp_wav" 2>/dev/null && success=true
                fi
                rm -f "$temp_wav"
            fi
            ;;
    esac
    
    local duration=$(end_timer "$start_time")
    
    if [[ "$success" == "true" ]]; then
        log "INFO" "Speech synthesis completed on Linux (${duration}s, engine: $tts_engine)"
        return 0
    else
        log "ERROR" "Speech synthesis failed with engine: $tts_engine"
        return 1
    fi
}

# 音声テキストの前処理（Linux向け）
preprocess_speech_text() {
    local text="$1"
    
    # 特殊文字の読み上げ対応
    local processed=$(echo "$text" | \
        sed 's/⏺/maru/g' | \
        sed 's/✅/success/g' | \
        sed 's/❌/error/g' | \
        sed 's/📁/folder/g' | \
        sed 's/🔧/config/g' | \
        sed 's/&/and/g' | \
        sed 's/@/at/g' | \
        sed 's/#/hash/g')
    
    # URL の簡略化
    processed=$(echo "$processed" | sed 's|https\?://[^ ]*|URL|g')
    
    # 英語と日本語の混在テキストの処理
    # 日本語を含む場合は、英語部分を読みやすく調整
    if echo "$processed" | grep -q '[ひらがなカタカナ一-龯]'; then
        # 日本語がある場合の英語単語の調整
        processed=$(echo "$processed" | sed 's/\b[A-Z]\+\b/& /g')
    fi
    
    # 長すぎるテキストの短縮
    local max_length=$(get_config "audio.max_speech_length" "300")
    if [[ ${#processed} -gt $max_length ]]; then
        processed="${processed:0:$max_length}... 以下省略"
    fi
    
    echo "$processed"
}

# システム通知の送信
send_notification() {
    local title="$1"
    local message="$2"
    local sound="${3:-default}"
    local urgency="${4:-normal}"
    local timeout="${5:-5000}"
    
    log "DEBUG" "Sending Linux notification: title=$title, urgency=$urgency"
    
    # notify-send による通知
    if has_command notify-send; then
        local notify_args=()
        notify_args+=("-t" "$timeout")
        notify_args+=("-u" "$urgency")
        
        # アイコンの設定
        case "$urgency" in
            "critical")
                notify_args+=("-i" "dialog-error")
                ;;
            "normal")
                notify_args+=("-i" "dialog-information")
                ;;
            "low")
                notify_args+=("-i" "dialog-question")
                ;;
        esac
        
        # カテゴリの設定
        notify_args+=("-c" "claude-voice")
        
        if notify-send "${notify_args[@]}" "$title" "$message" 2>/dev/null; then
            log "DEBUG" "Notification sent via notify-send"
            
            # サウンド再生（オプション）
            if [[ "$sound" != "none" ]] && [[ "$sound" != "default" ]]; then
                play_notification_sound "$sound" &
            fi
            
            return 0
        else
            log "WARN" "notify-send failed"
        fi
    fi
    
    # zenity フォールバック
    if has_command zenity; then
        local zenity_args=()
        zenity_args+=("--info")
        zenity_args+=("--title=$title")
        zenity_args+=("--text=$message")
        zenity_args+=("--timeout=5")
        
        if zenity "${zenity_args[@]}" 2>/dev/null; then
            log "DEBUG" "Notification sent via zenity"
            return 0
        else
            log "WARN" "zenity notification failed"
        fi
    fi
    
    # 最終フォールバック（コンソール出力）
    echo "通知: $title - $message"
    return 1
}

# 通知音の再生
play_notification_sound() {
    local sound_name="$1"
    
    # システム音の検索パス
    local sound_paths=(
        "/usr/share/sounds"
        "/usr/share/sounds/alsa"
        "/usr/share/sounds/freedesktop/stereo"
        "/usr/local/share/sounds"
    )
    
    # 音声ファイルの検索
    local sound_file=""
    for path in "${sound_paths[@]}"; do
        if [[ -f "$path/$sound_name.wav" ]]; then
            sound_file="$path/$sound_name.wav"
            break
        elif [[ -f "$path/$sound_name.ogg" ]]; then
            sound_file="$path/$sound_name.ogg"
            break
        fi
    done
    
    # 音声ファイルの再生
    if [[ -n "$sound_file" ]]; then
        if has_command paplay; then
            paplay "$sound_file" 2>/dev/null
        elif has_command aplay; then
            aplay "$sound_file" 2>/dev/null
        fi
    else
        # フォールバック: システムビープ
        system_beep
    fi
}

# システムビープ音
system_beep() {
    local count="${1:-1}"
    local frequency="${2:-800}"
    local duration="${3:-200}"
    
    log "DEBUG" "Playing system beep on Linux: count=$count, freq=$frequency"
    
    # ビープ音の生成方法を試行
    
    # 1. pactl経由でのビープ音
    if has_command pactl && has_command paplay; then
        for ((i=1; i<=count; i++)); do
            # 簡易的な正弦波生成
            (
                echo "freq=$frequency; sample_rate=8000; duration=0.2"
                echo "for i in {1..1600}; do"
                echo "  echo \"scale=10; s(2*3.14159*\$freq*\$i/\$sample_rate)\" | bc -l"
                echo "done | awk '{printf(\"%c\", int(\$1*127+128))}';"
            ) | bash 2>/dev/null | paplay --raw --format=u8 --rate=8000 --channels=1 2>/dev/null
            
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep 0.3
            fi
        done
        return 0
    fi
    
    # 2. speaker-test経由
    if has_command speaker-test; then
        for ((i=1; i<=count; i++)); do
            timeout 0.3 speaker-test -t sine -f $frequency -l 1 2>/dev/null || true
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep 0.2
            fi
        done
        return 0
    fi
    
    # 3. ターミナルベル（フォールバック）
    for ((i=1; i<=count; i++)); do
        echo -e '\a'
        if [[ $count -gt 1 && $i -lt $count ]]; then
            sleep 0.2
        fi
    done
}

# 音量制御
set_system_volume() {
    local volume="$1"  # 0-100
    local type="${2:-output}"  # output, input
    local audio_system=$(detect_audio_system)
    
    log "DEBUG" "Setting Linux system volume: $volume ($type, system: $audio_system)"
    
    case "$audio_system" in
        "pulseaudio")
            case "$type" in
                "output")
                    pactl set-sink-volume @DEFAULT_SINK@ "${volume}%" 2>/dev/null
                    ;;
                "input")
                    pactl set-source-volume @DEFAULT_SOURCE@ "${volume}%" 2>/dev/null
                    ;;
            esac
            ;;
        "alsa")
            case "$type" in
                "output")
                    amixer set Master "${volume}%" 2>/dev/null
                    ;;
                "input")
                    amixer set Capture "${volume}%" 2>/dev/null
                    ;;
            esac
            ;;
        "pipewire")
            # PipeWire の音量制御（wpctl使用）
            if has_command wpctl; then
                case "$type" in
                    "output")
                        wpctl set-volume @DEFAULT_AUDIO_SINK@ "${volume}%" 2>/dev/null
                        ;;
                    "input")
                        wpctl set-volume @DEFAULT_AUDIO_SOURCE@ "${volume}%" 2>/dev/null
                        ;;
                esac
            fi
            ;;
    esac
    
    log "DEBUG" "System volume set successfully"
    return 0
}

# 現在の音量取得
get_system_volume() {
    local type="${1:-output}"
    local audio_system=$(detect_audio_system)
    
    case "$audio_system" in
        "pulseaudio")
            case "$type" in
                "output")
                    pactl get-sink-volume @DEFAULT_SINK@ 2>/dev/null | head -1 | awk '{print $5}' | sed 's/%//'
                    ;;
                "input")
                    pactl get-source-volume @DEFAULT_SOURCE@ 2>/dev/null | head -1 | awk '{print $5}' | sed 's/%//'
                    ;;
            esac
            ;;
        "alsa")
            case "$type" in
                "output")
                    amixer get Master 2>/dev/null | grep -o '[0-9]\+%' | head -1 | sed 's/%//'
                    ;;
                "input")
                    amixer get Capture 2>/dev/null | grep -o '[0-9]\+%' | head -1 | sed 's/%//'
                    ;;
            esac
            ;;
        *)
            echo "50"  # デフォルト値
            ;;
    esac
}

# Linux固有の初期化
init_linux_audio() {
    log "INFO" "Initializing Linux audio subsystem"
    
    # 依存関係チェック
    if ! check_linux_dependencies; then
        return 1
    fi
    
    # オーディオシステムの検出
    local audio_system=$(detect_audio_system)
    log "DEBUG" "Detected audio system: $audio_system"
    
    # TTSエンジンの確認
    local tts_engine=$(select_tts_engine "ja" "normal")
    log "DEBUG" "Selected TTS engine: $tts_engine"
    
    # デフォルト設定の確認
    local audio_devices=$(get_audio_devices)
    log "DEBUG" "Available audio devices: $audio_devices"
    
    log "INFO" "Linux audio subsystem initialized successfully"
    return 0
}

# このモジュールのテスト関数
test_linux_functions() {
    echo "Testing Linux-specific functions..."
    
    # 依存関係チェック
    if check_linux_dependencies; then
        echo "Dependencies: OK"
    else
        echo "Dependencies: ISSUES"
    fi
    
    # オーディオシステムの検出
    local audio_system=$(detect_audio_system)
    echo "Audio system: $audio_system"
    
    # TTSエンジンの選択
    local tts_engine=$(select_tts_engine "ja")
    echo "TTS engine: $tts_engine"
    
    # オーディオデバイスの取得
    local devices=$(get_audio_devices)
    echo "Audio devices: $devices"
    
    # 現在の音量取得
    local volume=$(get_system_volume "output")
    echo "Current output volume: $volume"
    
    # 短いテスト音声（オプション）
    local test_speech=$(get_config "test.enable_speech" "false")
    if [[ "$test_speech" == "true" ]]; then
        echo "Testing speech synthesis..."
        speak_text "Linux test" "auto" "auto" "150"
    fi
    
    # テストビープ音
    echo "Testing system beep..."
    system_beep 1 600 100
    
    echo "Linux functions test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    source "$SCRIPT_DIR/core/base.sh"
    
    claude_voice_init true
    test_linux_functions
fi