#!/bin/bash
# Audio Fallback System - Platform-Independent Sound Generation
# プラットフォーム非依存音声生成システム

set -euo pipefail

readonly AUDIO_FALLBACK_VERSION="2.0.0"

# === 環境変数 ===
export AUDIO_FALLBACK_DEBUG="${AUDIO_FALLBACK_DEBUG:-false}"

# === ログ関数 ===
log_audio() {
    local level="$1"
    shift
    if [[ "${AUDIO_FALLBACK_DEBUG:-false}" == "true" ]] || [[ "$level" == "ERROR" ]]; then
        echo "[$(date '+%H:%M:%S')] [AUDIO-FALLBACK] [$level] $*" >&2
    fi
}

# === プラットフォーム検出 ===
detect_platform() {
    case "$(uname)" in
        "Darwin") echo "macos" ;;
        "Linux")
            if grep -qi microsoft /proc/version 2>/dev/null || [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
                echo "wsl"
            else
                echo "linux"
            fi
            ;;
        *) echo "unknown" ;;
    esac
}

# === 音声ファイル存在チェック ===
check_audio_file() {
    local file_path="$1"
    [[ -f "$file_path" && -r "$file_path" ]]
}

# === オーディオコマンド存在チェック ===
check_audio_command() {
    local command="$1"
    command -v "$command" >/dev/null 2>&1
}

# === プラットフォーム別音声ファイルパス ===
get_platform_audio_files() {
    local platform=$(detect_platform)
    local status_icon="$1"
    
    case "$platform" in
        "macos")
            case "$status_icon" in
                "⚡") echo "/System/Library/Sounds/Funk.aiff" ;;
                "⌛") echo "/System/Library/Sounds/Tink.aiff" ;;
                "✅") echo "/System/Library/Sounds/Glass.aiff" ;;
                *) echo "/System/Library/Sounds/Ping.aiff" ;;
            esac
            ;;
        "linux")
            case "$status_icon" in
                "⚡") echo "/usr/share/sounds/alsa/Front_Left.wav" ;;
                "⌛") echo "/usr/share/sounds/alsa/Rear_Left.wav" ;;
                "✅") echo "/usr/share/sounds/alsa/Front_Right.wav" ;;
                *) echo "/usr/share/sounds/alsa/Front_Center.wav" ;;
            esac
            ;;
        "wsl")
            case "$status_icon" in
                "⚡") echo "C:\\Windows\\Media\\Windows Exclamation.wav" ;;
                "⌛") echo "C:\\Windows\\Media\\Windows Notify System Generic.wav" ;;
                "✅") echo "C:\\Windows\\Media\\Windows Ding.wav" ;;
                *) echo "C:\\Windows\\Media\\Windows Ding.wav" ;;
            esac
            ;;
        *)
            echo ""
            ;;
    esac
}

# === フォールバック音声生成 (ビープ音) ===
generate_fallback_beep() {
    local status_icon="$1"
    local platform=$(detect_platform)
    
    log_audio "DEBUG" "Generating fallback beep for $status_icon on $platform"
    
    case "$status_icon" in
        "⚡")
            # 忙しい状態: 2回の短いビープ
            for i in {1..2}; do
                printf '\a'
                sleep 0.1
            done
            ;;
        "⌛")
            # 待機状態: 1回の長いビープ
            printf '\a'
            ;;
        "✅")
            # 完了状態: 3回の短いビープ
            for i in {1..3}; do
                printf '\a'
                sleep 0.05
            done
            ;;
        *)
            printf '\a'
            ;;
    esac
    
    log_audio "INFO" "Fallback beep generated for $status_icon"
}

# === 高度なフォールバック (speaker-test使用) ===
generate_advanced_fallback() {
    local status_icon="$1"
    
    if ! check_audio_command "speaker-test"; then
        log_audio "WARN" "speaker-test not available, using basic beep"
        generate_fallback_beep "$status_icon"
        return
    fi
    
    log_audio "DEBUG" "Using speaker-test for advanced fallback"
    
    case "$status_icon" in
        "⚡")
            # 忙しい状態: 800Hz 0.1秒 x2
            speaker-test -t sine -f 800 -l 1 -s 1 >/dev/null 2>&1 &
            sleep 0.1
            kill $! 2>/dev/null || true
            sleep 0.05
            speaker-test -t sine -f 800 -l 1 -s 1 >/dev/null 2>&1 &
            sleep 0.1
            kill $! 2>/dev/null || true
            ;;
        "⌛")
            # 待機状態: 659Hz → 880Hz 上昇音
            speaker-test -t sine -f 659 -l 1 -s 1 >/dev/null 2>&1 &
            sleep 0.15
            kill $! 2>/dev/null || true
            speaker-test -t sine -f 880 -l 1 -s 1 >/dev/null 2>&1 &
            sleep 0.15
            kill $! 2>/dev/null || true
            ;;
        "✅")
            # 完了状態: 523Hz → 659Hz → 783Hz 和音
            for freq in 523 659 783; do
                speaker-test -t sine -f $freq -l 1 -s 1 >/dev/null 2>&1 &
                sleep 0.1
                kill $! 2>/dev/null || true
                sleep 0.02
            done
            ;;
        *)
            speaker-test -t sine -f 440 -l 1 -s 1 >/dev/null 2>&1 &
            sleep 0.2
            kill $! 2>/dev/null || true
            ;;
    esac
    
    log_audio "INFO" "Advanced fallback generated for $status_icon"
}

# === プラットフォーム音声再生 ===
play_platform_audio() {
    local status_icon="$1"
    local platform=$(detect_platform)
    local audio_file=$(get_platform_audio_files "$status_icon")
    
    log_audio "DEBUG" "Attempting platform audio: $audio_file"
    
    case "$platform" in
        "macos")
            if check_audio_command "afplay" && check_audio_file "$audio_file"; then
                afplay "$audio_file" 2>/dev/null &
                log_audio "INFO" "macOS audio played: $audio_file"
                return 0
            fi
            ;;
        "linux")
            if check_audio_command "paplay" && check_audio_file "$audio_file"; then
                paplay "$audio_file" 2>/dev/null &
                log_audio "INFO" "Linux audio played: $audio_file"
                return 0
            elif check_audio_command "aplay" && check_audio_file "$audio_file"; then
                aplay "$audio_file" 2>/dev/null &
                log_audio "INFO" "Linux ALSA audio played: $audio_file"
                return 0
            fi
            ;;
        "wsl")
            # WSL環境ではPowerShell経由でWindows音声再生
            if command -v powershell.exe >/dev/null 2>&1; then
                local ps_command="try { \$player = New-Object Media.SoundPlayer '$audio_file'; \$player.PlaySync(); } catch { exit 1 }"
                if powershell.exe -Command "$ps_command" 2>/dev/null; then
                    log_audio "INFO" "WSL Windows audio played: $audio_file"
                    return 0
                fi
            fi
            ;;
    esac
    
    log_audio "WARN" "Platform audio failed, falling back"
    return 1
}

# === システム通知機能 ===
send_system_notification() {
    local status_icon="$1"
    local message="${2:-Status notification}"
    local platform=$(detect_platform)
    
    log_audio "DEBUG" "Sending system notification: $status_icon on $platform"
    
    case "$platform" in
        "macos")
            if check_audio_command "osascript"; then
                osascript -e "display notification \"$message\" with title \"TMux Status\" sound name \"Glass\"" 2>/dev/null
                log_audio "INFO" "macOS notification sent"
                return 0
            fi
            ;;
        "linux")
            if check_audio_command "notify-send"; then
                notify-send "TMux Status" "$message" -i dialog-information 2>/dev/null
                log_audio "INFO" "Linux notification sent"
                return 0
            elif check_audio_command "zenity"; then
                zenity --notification --text="TMux Status: $message" 2>/dev/null &
                log_audio "INFO" "Linux zenity notification sent"
                return 0
            fi
            ;;
        "wsl")
            if command -v powershell.exe >/dev/null 2>&1; then
                local ps_command="Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.MessageBox]::Show('$message', 'TMux Status', [System.Windows.Forms.MessageBoxButtons]::OK, [System.Windows.Forms.MessageBoxIcon]::Information)"
                powershell.exe -Command "$ps_command" 2>/dev/null &
                log_audio "INFO" "WSL Windows notification sent"
                return 0
            fi
            ;;
    esac
    
    log_audio "WARN" "System notification failed"
    return 1
}

# === 統合音声再生機能 ===
play_status_audio() {
    local status_icon="$1"
    local mode="${2:-auto}"  # auto, audio, notification, beep, advanced, silent
    local message="${3:-Status update}"
    
    log_audio "DEBUG" "Playing status audio: $status_icon (mode: $mode)"
    
    # SSH/リモート環境チェック
    if [[ -n "${SSH_CONNECTION:-}" ]] && [[ "$mode" == "auto" ]]; then
        log_audio "INFO" "SSH environment detected, using notification mode"
        if send_system_notification "$status_icon" "$message"; then
            return 0
        else
            generate_fallback_beep "$status_icon"
            return 0
        fi
    fi
    
    # モード別処理
    case "$mode" in
        "audio")
            # 音声デバイス再生のみ
            if play_platform_audio "$status_icon"; then
                return 0
            else
                log_audio "WARN" "Audio playback failed"
                return 1
            fi
            ;;
        "notification")
            # システム通知のみ
            if send_system_notification "$status_icon" "$message"; then
                return 0
            else
                log_audio "WARN" "System notification failed"
                return 1
            fi
            ;;
        "beep")
            generate_fallback_beep "$status_icon"
            ;;
        "advanced")
            generate_advanced_fallback "$status_icon"
            ;;
        "silent")
            log_audio "INFO" "Silent mode, no audio output"
            ;;
        "auto")
            # 自動選択戦略: 音声デバイス → システム通知 → 高度フォールバック → ビープ
            if play_platform_audio "$status_icon"; then
                return 0
            elif send_system_notification "$status_icon" "$message"; then
                return 0
            elif command -v speaker-test >/dev/null 2>&1; then
                generate_advanced_fallback "$status_icon"
            else
                generate_fallback_beep "$status_icon"
            fi
            ;;
        *)
            log_audio "ERROR" "Unknown mode: $mode"
            generate_fallback_beep "$status_icon"
            ;;
    esac
}

# === 診断機能 ===
diagnose_audio() {
    echo "=== Audio Fallback System Diagnostics ==="
    echo "Version: $AUDIO_FALLBACK_VERSION"
    echo
    
    local platform=$(detect_platform)
    echo "Platform: $platform"
    echo
    
    echo "Audio Commands Availability:"
    for cmd in afplay paplay aplay speaker-test powershell.exe; do
        if check_audio_command "$cmd"; then
            echo "  ✅ $cmd: Available"
        else
            echo "  ❌ $cmd: Not available"
        fi
    done
    
    echo
    echo "System Notification Commands:"
    case "$platform" in
        "macos")
            if check_audio_command "osascript"; then
                echo "  ✅ osascript: Available (macOS notifications)"
            else
                echo "  ❌ osascript: Not available"
            fi
            ;;
        "linux")
            if check_audio_command "notify-send"; then
                echo "  ✅ notify-send: Available (libnotify)"
            else
                echo "  ❌ notify-send: Not available"
            fi
            if check_audio_command "zenity"; then
                echo "  ✅ zenity: Available (GTK notifications)"
            else
                echo "  ❌ zenity: Not available"
            fi
            ;;
        "wsl")
            if command -v powershell.exe >/dev/null 2>&1; then
                echo "  ✅ powershell.exe: Available (Windows notifications)"
            else
                echo "  ❌ powershell.exe: Not available"
            fi
            ;;
    esac
    
    echo
    echo "Platform Audio Files:"
    for status in "⚡" "⌛" "✅"; do
        local audio_file=$(get_platform_audio_files "$status")
        if [[ -n "$audio_file" ]] && check_audio_file "$audio_file"; then
            echo "  ✅ $status: $audio_file"
        elif [[ -n "$audio_file" ]]; then
            echo "  ❌ $status: $audio_file (not found)"
        else
            echo "  ❌ $status: No audio file defined"
        fi
    done
    
    echo
    echo "Environment Detection:"
    if [[ -n "${SSH_CONNECTION:-}" ]]; then
        echo "  🌐 SSH connection detected"
    else
        echo "  🖥️  Local environment"
    fi
    
    echo
    echo "=== End Diagnostics ==="
}

# === テスト機能 ===
test_audio_fallback() {
    echo "=== Audio Fallback System Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: プラットフォーム検出
    echo "Test 1: Platform detection"
    ((tests_total++))
    local platform=$(detect_platform)
    if [[ -n "$platform" && "$platform" != "unknown" ]]; then
        echo "✅ PASS - Platform: $platform"
        ((tests_passed++))
    else
        echo "❌ FAIL - Platform detection"
    fi
    
    # Test 2-4: 各ステータス音テスト (ビープモード)
    for status in "⚡" "⌛" "✅"; do
        echo "Test $((tests_total + 1)): Status audio $status (beep mode)"
        ((tests_total++))
        if play_status_audio "$status" "beep" "Test notification" >/dev/null 2>&1; then
            echo "✅ PASS - Status audio: $status"
            ((tests_passed++))
        else
            echo "❌ FAIL - Status audio: $status"
        fi
        sleep 0.5
    done
    
    # Test 5: システム通知テスト
    echo "Test $((tests_total + 1)): System notification"
    ((tests_total++))
    if play_status_audio "✅" "notification" "Test system notification" >/dev/null 2>&1; then
        echo "✅ PASS - System notification"
        ((tests_passed++))
    else
        echo "❌ FAIL - System notification"
    fi
    
    # Test 6: 自動モードテスト
    echo "Test $((tests_total + 1)): Auto mode"
    ((tests_total++))
    if play_status_audio "✅" "auto" "Auto mode test" >/dev/null 2>&1; then
        echo "✅ PASS - Auto mode"
        ((tests_passed++))
    else
        echo "❌ FAIL - Auto mode"
    fi
    
    echo
    echo "Test Results: $tests_passed/$tests_total passed"
    
    if [[ $tests_passed -eq $tests_total ]]; then
        echo "🎉 All tests passed!"
        return 0
    else
        echo "⚠️ Some tests failed"
        return 1
    fi
}

# === メイン処理 ===
main() {
    case "${1:-help}" in
        "play")
            shift
            play_status_audio "$@"
            ;;
        "audio")
            shift
            play_status_audio "$1" "audio" "${2:-Audio test}"
            ;;
        "notification")
            shift
            play_status_audio "$1" "notification" "${2:-Notification test}"
            ;;
        "test-platform")
            shift
            play_platform_audio "$@"
            ;;
        "beep")
            shift
            generate_fallback_beep "$@"
            ;;
        "advanced")
            shift
            generate_advanced_fallback "$@"
            ;;
        "system-notify")
            shift
            send_system_notification "$@"
            ;;
        "diagnose")
            diagnose_audio
            ;;
        "test")
            test_audio_fallback
            ;;
        "help"|"-h"|"--help")
            cat << EOF
Audio Fallback System - Usage Guide

COMMANDS:
  play <status> [mode] [message]      - ステータス音再生 (自動フォールバック)
  audio <status> [message]            - 音声デバイス再生のみ
  notification <status> [message]     - システム通知のみ
  test-platform <status>              - プラットフォーム音声のみテスト
  beep <status>                       - ビープ音フォールバック
  advanced <status>                   - 高度フォールバック (speaker-test)
  system-notify <status> [message]    - システム通知送信
  diagnose                            - システム診断
  test                                - テスト実行
  help                                - このヘルプ

STATUS ICONS:
  ⚡ - Busy/Alert
  ⌛ - Waiting/Processing
  ✅ - Complete/Success

MODES:
  auto         - 自動選択 (音声→通知→フォールバック)
  audio        - 音声デバイス再生のみ
  notification - システム通知のみ
  beep         - 基本ビープ音
  advanced     - 高度な音声生成 (speaker-test)
  silent       - 無音

EXAMPLES:
  $0 play "✅"                                    # 自動モード
  $0 audio "⚡" "Processing started"              # 音声デバイスのみ
  $0 notification "✅" "Task completed"           # システム通知のみ
  $0 play "⌛" "auto" "Waiting for input"        # 自動選択
  $0 diagnose                                     # システム診断
  $0 test                                         # 全機能テスト

EOF
            ;;
        *)
            echo "Unknown command: $1" >&2
            main "help"
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi