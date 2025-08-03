#!/bin/bash
# WSL Voice Engine v2.0 - Modular Architecture
# Claude Code WSL音声統合システム（リファクタリング版）

set -euo pipefail

# === スクリプト設定 ===
readonly SCRIPT_VERSION="2.0.0"
readonly SCRIPT_NAME="WSL Voice Engine v2"
readonly CORE_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly MODULE_DIR="$CORE_DIR/modules"

# === 環境変数設定 ===
export CLAUDE_VOICE_DEBUG="${CLAUDE_VOICE_DEBUG:-false}"

# === 基本ライブラリ読み込み ===
source "$CORE_DIR/base.sh"

# === モジュール読み込み ===
source "$MODULE_DIR/wsl_environment.sh"
source "$MODULE_DIR/powershell_interface.sh"
source "$MODULE_DIR/status_sound_engine.sh"
source "$MODULE_DIR/voice_detection.sh"

# === グローバル変数 ===
declare -g WSL_ENVIRONMENT=""
declare -g POWERSHELL_PATH=""
declare -g SELECTED_VOICE=""

# === 初期化 ===
initialize_voice_engine() {
    log "INFO" "$SCRIPT_NAME initialized (version: $SCRIPT_VERSION)"
    
    # WSL環境検出
    WSL_ENVIRONMENT=$(detect_wsl_environment)
    if [[ "$WSL_ENVIRONMENT" == "none" ]]; then
        log "ERROR" "Not running in WSL environment"
        return 1
    fi
    log "DEBUG" "WSL environment: $WSL_ENVIRONMENT"
    
    # PowerShell確認
    if ! POWERSHELL_PATH=$(find_powershell); then
        log "ERROR" "PowerShell not available"
        return 1
    fi
    log "DEBUG" "PowerShell path: $POWERSHELL_PATH"
    
    # Windows音声システム確認
    if ! check_windows_speech; then
        log "WARN" "Windows Speech API may not be available"
    fi
    
    # 最適音声選択
    if SELECTED_VOICE=$(auto_select_voice japanese); then
        log "DEBUG" "Selected voice: $SELECTED_VOICE"
    else
        log "WARN" "Voice auto-selection failed"
    fi
    
    return 0
}

# === 音声合成（高レベルインターフェース） ===
speak() {
    local text="$1"
    local voice="${2:-$SELECTED_VOICE}"
    local rate="${3:-0}"
    local volume="${4:-100}"
    
    if [[ -z "$text" ]]; then
        log "ERROR" "No text specified for speech synthesis"
        return 1
    fi
    
    log "DEBUG" "Speaking: $text (voice: $voice)"
    
    if execute_powershell_speech "$text" "$voice" "$rate" "$volume"; then
        log "INFO" "Speech synthesis completed"
        return 0
    else
        log "ERROR" "Speech synthesis failed"
        return 1
    fi
}

# === ステータス音声再生（高レベルインターフェース） ===
play_sound() {
    local status_icon="$1"
    local method="${2:-wav}"
    
    if [[ -z "$status_icon" ]]; then
        log "ERROR" "No status icon specified"
        return 1
    fi
    
    if play_status_sound_with_fallback "$status_icon"; then
        log "INFO" "Status sound played: $status_icon"
        return 0
    else
        log "ERROR" "Status sound failed: $status_icon"
        return 1
    fi
}

# === 複合通知（音声+効果音） ===
notify() {
    local text="$1"
    local status_icon="${2:-✅}"
    local window_id="${3:-1}"
    local mode="${4:-both}"  # sound, speech, both
    
    case "$mode" in
        "sound")
            play_sound "$status_icon"
            ;;
        "speech")
            speak "$text"
            ;;
        "both")
            play_sound "$status_icon" &
            speak "$text"
            wait
            ;;
        *)
            log "ERROR" "Unknown notification mode: $mode"
            return 1
            ;;
    esac
}

# === 診断機能 ===
diagnose() {
    echo "=== $SCRIPT_NAME Diagnostics ==="
    echo "Version: $SCRIPT_VERSION"
    echo "WSL Environment: $WSL_ENVIRONMENT"
    echo "PowerShell Path: $POWERSHELL_PATH"
    echo "Selected Voice: $SELECTED_VOICE"
    echo
    
    echo "=== Module Status ==="
    echo "WSL Environment Module: OK"
    echo "PowerShell Interface Module: OK"
    echo "Status Sound Engine Module: OK"
    echo "Voice Detection Module: OK"
    echo
    
    echo "=== Available Voices ==="
    detect_available_voices
    echo
    
    echo "=== Available Status Sounds ==="
    list_available_statuses
    echo
    
    echo "=== System Tests ==="
    if check_windows_speech; then
        echo "Windows Speech API: ✓ Available"
    else
        echo "Windows Speech API: ✗ Not Available"
    fi
    
    return 0
}

# === テスト機能 ===
test_engine() {
    echo "=== $SCRIPT_NAME Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: 基本音声合成
    echo "Test 1: Basic speech synthesis"
    ((tests_total++))
    if speak "テスト音声です"; then
        echo "✓ PASS"
        ((tests_passed++))
    else
        echo "✗ FAIL"
    fi
    
    # Test 2: ステータス音響効果
    echo "Test 2: Status sound effects"
    for status in "⚡" "⌛" "✅"; do
        echo "  Testing status: $status"
        ((tests_total++))
        if play_sound "$status"; then
            echo "  ✓ PASS"
            ((tests_passed++))
        else
            echo "  ✗ FAIL"
        fi
        sleep 1
    done
    
    # Test 3: 複合通知
    echo "Test 3: Composite notification"
    ((tests_total++))
    if notify "テスト完了しました" "✅" 1 "both"; then
        echo "✓ PASS"
        ((tests_passed++))
    else
        echo "✗ FAIL"
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

# === ヘルプ表示 ===
show_help() {
    cat << EOF
$SCRIPT_NAME - Usage Guide

COMMANDS:
  speak <text> [voice] [rate] [volume]   - 音声合成
  sound <status> [method]                - ステータス音再生
  notify <text> [status] [window] [mode] - 複合通知
  diagnose                               - システム診断
  test                                   - テスト実行
  help                                   - このヘルプ

EXAMPLES:
  $0 speak "こんにちは"
  $0 sound "✅"
  $0 notify "完了しました" "✅" 1 "both"
  $0 diagnose
  $0 test

STATUS ICONS:
  ⚡ - Busy/Alert
  ⌛ - Waiting/Processing
  ✅ - Complete/Success

NOTIFICATION MODES:
  sound  - 効果音のみ
  speech - 音声合成のみ
  both   - 効果音+音声合成

EOF
}

# === メイン処理 ===
main() {
    # 初期化
    if ! initialize_voice_engine; then
        log "FATAL" "Voice engine initialization failed"
        exit 1
    fi
    
    # コマンド処理
    case "${1:-help}" in
        "speak")
            shift
            speak "$@"
            ;;
        "sound")
            shift
            play_sound "$@"
            ;;
        "notify")
            shift
            notify "$@"
            ;;
        "diagnose")
            diagnose
            ;;
        "test")
            test_engine
            ;;
        "help"|"-h"|"--help")
            show_help
            ;;
        *)
            log "ERROR" "Unknown command: $1"
            show_help
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi