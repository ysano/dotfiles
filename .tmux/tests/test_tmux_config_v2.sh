#!/bin/bash
# tmux Configuration v2.0 Test Suite
# 新しいモダン設定の包括的テスト

set -euo pipefail

# === テスト設定 ===
readonly TEST_SESSION_NAME="tmux_v2_test_$$"
readonly TEST_CONFIG_DIR="$(dirname "${BASH_SOURCE[0]}")/.."
readonly V2_CONFIG_FILE="$TEST_CONFIG_DIR/../.tmux_v2.conf"

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

# === 前提条件テスト ===
test_v2_config_exists() {
    [[ -f "$V2_CONFIG_FILE" ]]
}

test_variables_config_exists() {
    [[ -f "$TEST_CONFIG_DIR/config/variables.conf" ]]
}

test_smart_status_script_exists() {
    [[ -x "$TEST_CONFIG_DIR/scripts/claude-status-smart.sh" ]]
}

# === tmux設定テスト ===
test_tmux_config_syntax() {
    # 設定ファイルの構文チェック
    local temp_session="syntax_test_$$"
    
    # tmux設定の読み込みテスト
    if tmux -f "$V2_CONFIG_FILE" new-session -d -s "$temp_session" 2>/dev/null; then
        tmux -f "$V2_CONFIG_FILE" kill-session -t "$temp_session" 2>/dev/null || true
        return 0
    else
        return 1
    fi
}

test_variable_expansion() {
    # 変数展開のテスト
    local temp_session="var_test_$$"
    
    if tmux -f "$V2_CONFIG_FILE" new-session -d -s "$temp_session" 2>/dev/null; then
        # PRIMARY_COLORが正しく設定されているかチェック
        local status_style
        status_style=$(tmux -f "$V2_CONFIG_FILE" -S "$temp_session" show-options -g status-style 2>/dev/null | grep -o "colour[0-9]*" | head -1)
        
        tmux -f "$V2_CONFIG_FILE" kill-session -t "$temp_session" 2>/dev/null || true
        
        [[ -n "$status_style" ]] && [[ "$status_style" =~ ^colour[0-9]+$ ]]
    else
        return 1
    fi
}

test_conditional_loading() {
    # 条件付き読み込みのテスト
    local temp_session="cond_test_$$"
    
    if tmux -f "$V2_CONFIG_FILE" new-session -d -s "$temp_session" 2>/dev/null; then
        # ステータスバーが正しく設定されているかチェック
        local status_left
        status_left=$(tmux -f "$V2_CONFIG_FILE" -S "$temp_session" show-options -g status-left 2>/dev/null)
        
        tmux -f "$V2_CONFIG_FILE" kill-session -t "$temp_session" 2>/dev/null || true
        
        [[ -n "$status_left" ]]
    else
        return 1
    fi
}

# === スクリプト統合テスト ===
test_smart_status_execution() {
    # スマートステータススクリプトの実行テスト
    if command -v "$TEST_CONFIG_DIR/scripts/claude-status-smart.sh" >/dev/null 2>&1; then
        # スクリプトが実行可能で、エラーなく終了するかテスト
        timeout 5s "$TEST_CONFIG_DIR/scripts/claude-status-smart.sh" 1 0 >/dev/null 2>&1
    else
        return 1
    fi
}

test_voice_engine_detection() {
    # Claude Voice エンジン検出のテスト
    if [[ -x "$TEST_CONFIG_DIR/claude/core/wsl_voice_engine_v2.sh" ]]; then
        # v2.0エンジンが利用可能
        "$TEST_CONFIG_DIR/claude/core/wsl_voice_engine_v2.sh" diagnose >/dev/null 2>&1
    elif [[ -x "$TEST_CONFIG_DIR/claude/core/wsl_voice_engine.sh" ]]; then
        # v1.0エンジンでフォールバック
        "$TEST_CONFIG_DIR/claude/core/wsl_voice_engine.sh" diagnose >/dev/null 2>&1
    else
        # エンジンなしでも正常動作
        return 0
    fi
}

# === パフォーマンステスト ===
test_config_load_performance() {
    # 設定読み込み時間の測定
    local start_time end_time duration
    start_time=$(date +%s%N)
    
    local temp_session="perf_test_$$"
    if tmux -f "$V2_CONFIG_FILE" new-session -d -s "$temp_session" 2>/dev/null; then
        tmux -f "$V2_CONFIG_FILE" kill-session -t "$temp_session" 2>/dev/null || true
        
        end_time=$(date +%s%N)
        duration=$(( (end_time - start_time) / 1000000 ))  # ミリ秒
        
        test_log "INFO" "Config load time: ${duration}ms"
        [[ $duration -lt 1000 ]]  # 1秒未満
    else
        return 1
    fi
}

test_status_update_efficiency() {
    # ステータス更新効率のテスト
    local temp_session="status_test_$$"
    
    if tmux -f "$V2_CONFIG_FILE" new-session -d -s "$temp_session" 2>/dev/null; then
        # ステータス間隔の確認
        local status_interval
        status_interval=$(tmux -f "$V2_CONFIG_FILE" -S "$temp_session" show-options -g status-interval 2>/dev/null | awk '{print $2}')
        
        tmux -f "$V2_CONFIG_DIR" kill-session -t "$temp_session" 2>/dev/null || true
        
        [[ -n "$status_interval" ]] && [[ "$status_interval" -ge 2 ]] && [[ "$status_interval" -le 10 ]]
    else
        return 1
    fi
}

# === OS固有テスト ===
test_os_detection() {
    # OS検出とOS固有設定のテスト
    local current_os
    current_os=$(uname)
    
    case "$current_os" in
        "Linux")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                [[ -f "$TEST_CONFIG_DIR/os/wsl.conf" ]]
            else
                [[ -f "$TEST_CONFIG_DIR/os/linux.conf" ]]
            fi
            ;;
        "Darwin")
            [[ -f "$TEST_CONFIG_DIR/os/darwin.conf" ]]
            ;;
        "FreeBSD")
            [[ -f "$TEST_CONFIG_DIR/os/freebsd.conf" ]]
            ;;
        *)
            # 未知のOSでも基本動作
            return 0
            ;;
    esac
}

# === クリーンアップ ===
cleanup() {
    # テスト用セッションのクリーンアップ
    tmux list-sessions 2>/dev/null | grep "_test_" | cut -d: -f1 | xargs -r -I {} tmux kill-session -t {} 2>/dev/null || true
}

# === メインテストスイート ===
run_all_tests() {
    echo "================================================================"
    echo "tmux Configuration v2.0 Test Suite"
    echo "================================================================"
    echo
    
    # 前提条件テスト
    echo "=== Prerequisites Tests ==="
    run_test "v2.0 Config File Exists" test_v2_config_exists
    run_test "Variables Config Exists" test_variables_config_exists
    run_test "Smart Status Script Exists" test_smart_status_script_exists
    echo
    
    # tmux設定テスト
    echo "=== tmux Configuration Tests ==="
    run_test "Config Syntax Validation" test_tmux_config_syntax
    run_test "Variable Expansion" test_variable_expansion
    run_test "Conditional Loading" test_conditional_loading
    echo
    
    # スクリプト統合テスト
    echo "=== Script Integration Tests ==="
    run_test "Smart Status Execution" test_smart_status_execution
    run_test "Voice Engine Detection" test_voice_engine_detection
    echo
    
    # パフォーマンステスト
    echo "=== Performance Tests ==="
    run_test "Config Load Performance" test_config_load_performance
    run_test "Status Update Efficiency" test_status_update_efficiency
    echo
    
    # OS固有テスト
    echo "=== OS-Specific Tests ==="
    run_test "OS Detection and Config" test_os_detection
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
        echo "tmux Configuration v2.0 is ready for production use."
        return 0
    else
        echo
        test_log "FAIL" "⚠️ Some tests failed"
        echo "Please check the failed tests before deploying v2.0 configuration."
        return 1
    fi
}

# === シグナルハンドラ ===
trap cleanup EXIT

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    run_all_tests "$@"
fi