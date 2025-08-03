#!/bin/bash
# Cross-Platform Voice Engine
# クロスプラットフォーム音声エンジン統合API

# プラットフォーム検出器を読み込み
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "$SCRIPT_DIR/platform_detector.sh"

# 統一音声API関数

# 音声合成（テキスト読み上げ）
speak() {
    local text="$1"
    local voice_options="${2:-}"
    
    if ! check_platform_capability "speech"; then
        return 1
    fi
    
    execute_platform_command "speech" "$text" $voice_options
}

# 効果音再生
play_sound() {
    local sound_file="$1"
    
    if ! check_platform_capability "audio"; then
        return 1
    fi
    
    execute_platform_command "audio" "$sound_file"
}

# 統合通知（音声+効果音）
notify() {
    local message="$1"
    local status_icon="${2:-ℹ}"
    local enable_speech="${3:-1}"
    local notification_mode="${4:-both}"
    
    case "$notification_mode" in
        "sound")
            play_status_sound "$status_icon"
            ;;
        "speech")
            [[ "$enable_speech" == "1" ]] && speak "$message"
            ;;
        "both")
            play_status_sound "$status_icon"
            [[ "$enable_speech" == "1" ]] && speak "$message"
            ;;
    esac
}

# Claude Codeステータス別効果音
play_status_sound() {
    local status_icon="$1"
    local platform=$(detect_platform)
    
    case "$status_icon" in
        "✅"|"完了")
            case "$platform" in
                "wsl")
                    execute_platform_command "audio" "success" "523,659,783,1046" "200"
                    ;;
                "macos")
                    # macOS用の成功音（システム音またはカスタム）
                    osascript -e "beep 1"
                    ;;
                "linux")
                    # Linux用の成功音パターン
                    if command -v speaker-test >/dev/null 2>&1; then
                        for freq in 523 659 783 1046; do
                            speaker-test -t sine -f $freq -l 1 -s 1 >/dev/null 2>&1 &
                            sleep 0.2
                            pkill speaker-test 2>/dev/null
                        done
                    fi
                    ;;
            esac
            ;;
        "⌛"|"待機")
            case "$platform" in
                "wsl")
                    execute_platform_command "audio" "waiting" "659,880,1175" "300"
                    ;;
                "macos")
                    osascript -e "beep 2"
                    ;;
                "linux")
                    if command -v speaker-test >/dev/null 2>&1; then
                        for freq in 659 880 1175; do
                            speaker-test -t sine -f $freq -l 1 -s 1 >/dev/null 2>&1 &
                            sleep 0.3
                            pkill speaker-test 2>/dev/null
                        done
                    fi
                    ;;
            esac
            ;;
        "⚡"|"忙しい")
            case "$platform" in
                "wsl")
                    execute_platform_command "audio" "busy" "800,800,600" "150"
                    ;;
                "macos")
                    osascript -e "beep 3"
                    ;;
                "linux")
                    if command -v speaker-test >/dev/null 2>&1; then
                        for freq in 800 800 600; do
                            speaker-test -t sine -f $freq -l 1 -s 1 >/dev/null 2>&1 &
                            sleep 0.15
                            pkill speaker-test 2>/dev/null
                        done
                    fi
                    ;;
            esac
            ;;
    esac
}

# プラットフォーム情報表示
show_platform_info() {
    local platform=$(detect_platform)
    local platform_path=$(get_platform_path)
    
    echo "Platform: $platform"
    echo "Platform Path: $platform_path"
    echo "Audio Capability: $(check_platform_capability "audio" && echo "Available" || echo "Not Available")"
    echo "Speech Capability: $(check_platform_capability "speech" && echo "Available" || echo "Not Available")"
}

# テスト関数
test_voice_engine() {
    echo "Testing Cross-Platform Voice Engine..."
    show_platform_info
    echo ""
    
    echo "Testing status sounds..."
    for status in "✅" "⌛" "⚡"; do
        echo "Playing status sound: $status"
        play_status_sound "$status"
        sleep 1
    done
    
    echo ""
    echo "Testing speech synthesis..."
    speak "テスト音声です。Claude Voice統合機能が正常に動作しています。"
    
    echo ""
    echo "Testing notification..."
    notify "通知テストです" "✅" 1 "both"
}

# メイン実行部
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_voice_engine
            ;;
        "speak")
            shift
            speak "$@"
            ;;
        "play")
            shift
            play_sound "$@"
            ;;
        "notify")
            shift
            notify "$@"
            ;;
        "status")
            shift
            play_status_sound "$@"
            ;;
        "info")
            show_platform_info
            ;;
        *)
            echo "Usage: $0 {test|speak <text>|play <file>|notify <msg> [icon] [speech] [mode]|status <icon>|info}"
            exit 1
            ;;
    esac
fi