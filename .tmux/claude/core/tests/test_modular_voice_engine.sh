#!/bin/bash
# Modular Voice Engine Unit Tests
# モジュラー音声エンジンのユニットテストスイート

set -euo pipefail

# === テスト設定 ===
readonly TEST_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly CORE_DIR="$(dirname "$TEST_DIR")"
readonly MODULE_DIR="$CORE_DIR/modules"

# === 基本ライブラリ読み込み ===
source "$CORE_DIR/base.sh"

# === テストカウンタ ===
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# === カラー出力 ===
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# === ログ関数 ===
test_log() {
    local level="$1"
    shift
    case "$level" in
        "INFO") echo -e "${BLUE}[TEST-INFO]${NC} $*" ;;
        "PASS") echo -e "${GREEN}[TEST-PASS]${NC} $*" ;;
        "FAIL") echo -e "${RED}[TEST-FAIL]${NC} $*" ;;
        "WARN") echo -e "${YELLOW}[TEST-WARN]${NC} $*" ;;
    esac
}

# === テスト実行関数 ===
run_test() {
    local test_name="$1"
    local test_function="$2"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    test_log "INFO" "Running: $test_name"
    
    if $test_function; then
        test_log "PASS" "✓ $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        test_log "FAIL" "✗ $test_name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# === WSL環境モジュールテスト ===
test_wsl_environment_module() {
    source "$MODULE_DIR/wsl_environment.sh"
    
    # WSL環境検出テスト
    local wsl_result
    wsl_result=$(detect_wsl_environment)
    [[ "$wsl_result" != "none" ]]
}

test_powershell_detection() {
    source "$MODULE_DIR/wsl_environment.sh"
    
    # PowerShell検出テスト
    local ps_path
    ps_path=$(find_powershell)
    [[ -x "$ps_path" ]]
}

test_windows_speech_check() {
    source "$MODULE_DIR/wsl_environment.sh"
    
    # Windows音声API確認
    check_windows_speech >/dev/null 2>&1
}

# === PowerShellインターフェースモジュールテスト ===
test_powershell_interface_module() {
    source "$MODULE_DIR/powershell_interface.sh"
    return 0  # モジュール読み込み成功
}

test_powershell_beep() {
    source "$MODULE_DIR/powershell_interface.sh"
    
    # 簡単なBeepテスト（短時間）
    execute_powershell_beep 800 50 >/dev/null 2>&1
}

test_powershell_wav() {
    source "$MODULE_DIR/powershell_interface.sh"
    
    # システム音ファイルテスト
    execute_powershell_wav "C:\\Windows\\Media\\Windows Ding.wav" "false" >/dev/null 2>&1
}

# === ステータス音響エンジンモジュールテスト ===
test_status_sound_engine_module() {
    source "$MODULE_DIR/status_sound_engine.sh"
    return 0  # モジュール読み込み成功
}

test_status_sound_config() {
    source "$MODULE_DIR/status_sound_engine.sh"
    
    # 設定存在確認
    [[ -n "${STATUS_SOUND_CONFIGS["⚡"]:-}" ]] && \
    [[ -n "${STATUS_SOUND_CONFIGS["⌛"]:-}" ]] && \
    [[ -n "${STATUS_SOUND_CONFIGS["✅"]:-}" ]]
}

test_status_sound_playback() {
    source "$MODULE_DIR/status_sound_engine.sh"
    
    # WAV方式テスト（実際の音は出さない）
    play_status_sound "✅" "wav" >/dev/null 2>&1
}

# === 音声検出モジュールテスト ===
test_voice_detection_module() {
    source "$MODULE_DIR/voice_detection.sh"
    return 0  # モジュール読み込み成功
}

test_voice_detection() {
    source "$MODULE_DIR/voice_detection.sh"
    
    # 音声検出テスト
    local voices
    voices=$(detect_available_voices)
    [[ -n "$voices" ]]
}

test_voice_selection() {
    source "$MODULE_DIR/voice_detection.sh"
    
    # 音声選択テスト
    local selected_voice
    selected_voice=$(auto_select_voice japanese)
    [[ -n "$selected_voice" ]]
}

# === 統合モジュールテスト ===
test_v2_engine_initialization() {
    "$CORE_DIR/wsl_voice_engine_v2.sh" diagnose >/dev/null 2>&1
}

test_v2_engine_help() {
    "$CORE_DIR/wsl_voice_engine_v2.sh" help >/dev/null 2>&1
}

# === パフォーマンステスト ===
test_module_load_performance() {
    local start_time end_time duration
    start_time=$(date +%s%N)
    
    source "$MODULE_DIR/wsl_environment.sh"
    source "$MODULE_DIR/powershell_interface.sh"
    source "$MODULE_DIR/status_sound_engine.sh"
    source "$MODULE_DIR/voice_detection.sh"
    
    end_time=$(date +%s%N)
    duration=$(( (end_time - start_time) / 1000000 ))  # ミリ秒
    
    test_log "INFO" "Module load time: ${duration}ms"
    [[ $duration -lt 1000 ]]  # 1秒未満
}

# === メインテストスイート ===
run_all_tests() {
    echo "================================================================"
    echo "Modular Voice Engine Unit Test Suite"
    echo "================================================================"
    echo
    
    # WSL環境モジュールテスト
    echo "=== WSL Environment Module Tests ==="
    run_test "WSL Environment Detection" test_wsl_environment_module
    run_test "PowerShell Detection" test_powershell_detection
    run_test "Windows Speech Check" test_windows_speech_check
    echo
    
    # PowerShellインターフェースモジュールテスト
    echo "=== PowerShell Interface Module Tests ==="
    run_test "PowerShell Interface Module Load" test_powershell_interface_module
    run_test "PowerShell Beep Execution" test_powershell_beep
    run_test "PowerShell WAV Execution" test_powershell_wav
    echo
    
    # ステータス音響エンジンモジュールテスト
    echo "=== Status Sound Engine Module Tests ==="
    run_test "Status Sound Engine Module Load" test_status_sound_engine_module
    run_test "Status Sound Configuration" test_status_sound_config
    run_test "Status Sound Playback" test_status_sound_playback
    echo
    
    # 音声検出モジュールテスト
    echo "=== Voice Detection Module Tests ==="
    run_test "Voice Detection Module Load" test_voice_detection_module
    run_test "Voice Detection" test_voice_detection
    run_test "Voice Selection" test_voice_selection
    echo
    
    # 統合テスト
    echo "=== Integration Tests ==="
    run_test "V2 Engine Initialization" test_v2_engine_initialization
    run_test "V2 Engine Help" test_v2_engine_help
    echo
    
    # パフォーマンステスト
    echo "=== Performance Tests ==="
    run_test "Module Load Performance" test_module_load_performance
    echo
    
    # 結果サマリー
    echo "================================================================"
    echo "Test Results Summary"
    echo "================================================================"
    echo "Total Tests: $TESTS_RUN"
    echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Failed: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo
        test_log "PASS" "🎉 All tests passed!"
        echo
        echo "Modular Voice Engine is ready for production use."
        return 0
    else
        echo
        test_log "FAIL" "⚠️ Some tests failed"
        echo "Please check the failed tests and fix issues before deployment."
        return 1
    fi
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    run_all_tests "$@"
fi