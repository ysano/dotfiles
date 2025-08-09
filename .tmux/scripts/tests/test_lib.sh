#!/bin/bash
# TMux Scripts Library Test Suite
# Version: 1.0.0
#
# 共通ライブラリのテストスイート

set -euo pipefail

# テストディレクトリ
readonly TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPTS_DIR="$(dirname "$TEST_DIR")"
readonly LIB_DIR="$SCRIPTS_DIR/lib"

# テスト用一時ディレクトリ
readonly TEST_TEMP_DIR="/tmp/tmux_scripts_test_$$"
mkdir -p "$TEST_TEMP_DIR"

# テスト結果カウンター (bash 3.x互換)
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# カラーコード
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# === テストユーティリティ ===

test_start() {
    local test_name="$1"
    echo -e "${BLUE}[TEST]${NC} $test_name"
    ((TESTS_TOTAL++))
}

test_pass() {
    local message="${1:-OK}"
    echo -e "  ${GREEN}✓${NC} $message"
    ((TESTS_PASSED++))
}

test_fail() {
    local message="$1"
    echo -e "  ${RED}✗${NC} $message"
    ((TESTS_FAILED++))
}

test_skip() {
    local message="$1"
    echo -e "  ${YELLOW}⊘${NC} $message (SKIPPED)"
    ((TESTS_SKIPPED++))
}

assert() {
    local condition="$1"
    local message="${2:-Assertion}"
    
    if eval "$condition"; then
        test_pass "$message"
        return 0
    else
        test_fail "$message: $condition"
        return 1
    fi
}

# === core.sh テスト ===

test_core_library() {
    test_start "core.sh - ライブラリロード"
    
    if source "$LIB_DIR/core.sh" 2>/dev/null; then
        test_pass "core.sh loaded successfully"
    else
        test_fail "Failed to load core.sh"
        return 1
    fi
    
    # インポートガードテスト
    test_start "core.sh - インポートガード"
    local first_load="$_TMUX_CORE_LOADED"
    source "$LIB_DIR/core.sh"  # 2回目のロード
    assert "[[ '$_TMUX_CORE_LOADED' == '$first_load' ]]" "Import guard working"
    
    # ログ関数テスト
    test_start "core.sh - ログ関数"
    
    export TMUX_SCRIPTS_LOG_FILE="$TEST_TEMP_DIR/test.log"
    export TMUX_SCRIPTS_LOG_LEVEL="DEBUG"
    
    log_debug "Debug message"
    log_info "Info message"
    log_warn "Warning message"
    log_error "Error message"
    
    assert "[[ -f '$TMUX_SCRIPTS_LOG_FILE' ]]" "Log file created"
    assert "grep -q 'Debug message' '$TMUX_SCRIPTS_LOG_FILE'" "Debug logged"
    assert "grep -q 'Info message' '$TMUX_SCRIPTS_LOG_FILE'" "Info logged"
    assert "grep -q 'Warning message' '$TMUX_SCRIPTS_LOG_FILE'" "Warning logged"
    assert "grep -q 'Error message' '$TMUX_SCRIPTS_LOG_FILE'" "Error logged"
    
    # コマンド存在チェックテスト
    test_start "core.sh - コマンド存在チェック"
    assert "command_exists bash" "bash exists"
    assert "! command_exists nonexistent_command_xyz" "nonexistent command not found"
    
    # クリーンアップテスト
    test_start "core.sh - クリーンアップ機能"
    
    local test_file="$TEST_TEMP_DIR/cleanup_test.txt"
    touch "$test_file"
    register_cleanup "$test_file"
    assert "[[ -f '$test_file' ]]" "Test file exists before cleanup"
    # Note: 実際のクリーンアップはスクリプト終了時に実行される
}

# === platform.sh テスト ===

test_platform_library() {
    test_start "platform.sh - ライブラリロード"
    
    if source "$LIB_DIR/platform.sh" 2>/dev/null; then
        test_pass "platform.sh loaded successfully"
    else
        test_fail "Failed to load platform.sh"
        return 1
    fi
    
    # プラットフォーム検出テスト
    test_start "platform.sh - プラットフォーム検出"
    
    local platform=$(detect_platform)
    assert "[[ -n '$platform' ]]" "Platform detected: $platform"
    
    case "$platform" in
        macos|linux|wsl|windows|freebsd|openbsd|netbsd)
            test_pass "Valid platform: $platform"
            ;;
        *)
            test_fail "Unknown platform: $platform"
            ;;
    esac
    
    # プラットフォーム判定ヘルパーテスト
    test_start "platform.sh - プラットフォーム判定ヘルパー"
    
    case "$platform" in
        macos)
            assert "is_macos" "is_macos returns true on macOS"
            assert "! is_linux" "is_linux returns false on macOS"
            ;;
        linux)
            assert "is_linux" "is_linux returns true on Linux"
            assert "! is_macos" "is_macos returns false on Linux"
            ;;
        wsl)
            assert "is_wsl" "is_wsl returns true on WSL"
            ;;
    esac
    
    # システム情報取得テスト
    test_start "platform.sh - システム情報取得"
    
    local os_info=$(get_os_info)
    assert "[[ -n '$os_info' ]]" "OS info retrieved: $os_info"
    
    local cpu_info=$(get_cpu_info)
    assert "[[ -n '$cpu_info' ]]" "CPU info retrieved"
    
    local memory_info=$(get_memory_info)
    assert "[[ -n '$memory_info' ]]" "Memory info retrieved"
}

# === tmux_ops.sh テスト ===

test_tmux_ops_library() {
    test_start "tmux_ops.sh - ライブラリロード"
    
    if source "$LIB_DIR/tmux_ops.sh" 2>/dev/null; then
        test_pass "tmux_ops.sh loaded successfully"
    else
        test_fail "Failed to load tmux_ops.sh"
        return 1
    fi
    
    # tmux環境チェック
    test_start "tmux_ops.sh - tmux環境チェック"
    
    if tmux_available; then
        test_pass "tmux is available"
        
        local version=$(get_tmux_version)
        assert "[[ -n '$version' ]]" "tmux version: $version"
    else
        test_skip "tmux is not available"
    fi
    
    # tmux内外の判定
    test_start "tmux_ops.sh - tmux内外判定"
    
    if in_tmux; then
        test_pass "Running inside tmux"
        
        local session=$(get_current_session)
        assert "[[ -n '$session' ]]" "Current session: $session"
        
        local window=$(get_current_window)
        assert "[[ -n '$window' ]]" "Current window: $window"
    else
        test_pass "Running outside tmux"
    fi
}

# === notification.sh テスト ===

test_notification_library() {
    test_start "notification.sh - ライブラリロード"
    
    if source "$LIB_DIR/notification.sh" 2>/dev/null; then
        test_pass "notification.sh loaded successfully"
    else
        test_fail "Failed to load notification.sh"
        return 1
    fi
    
    # レート制限テスト
    test_start "notification.sh - レート制限"
    
    # 最初のリクエストは通る
    assert "check_rate_limit 'test_key' 2" "First request passes"
    
    # 2秒以内の2回目は制限される
    assert "! check_rate_limit 'test_key' 2" "Second request blocked"
    
    # リセット後は通る
    reset_rate_limit 'test_key'
    assert "check_rate_limit 'test_key' 2" "Request after reset passes"
    
    # DND機能テスト
    test_start "notification.sh - DND機能"
    
    assert "! is_dnd_enabled" "DND initially disabled"
    
    enable_dnd 1  # 1秒間有効化
    assert "is_dnd_enabled" "DND enabled"
    
    sleep 2  # 期限切れまで待つ
    assert "! is_dnd_enabled" "DND expired"
    
    # 通知モードテスト
    test_start "notification.sh - 通知モード"
    
    set_notification_mode "verbose"
    assert "[[ '$(get_notification_mode)' == 'verbose' ]]" "Mode set to verbose"
    
    set_notification_mode "normal"
    assert "[[ '$(get_notification_mode)' == 'normal' ]]" "Mode set to normal"
}

# === validation.sh テスト ===

test_validation_library() {
    test_start "validation.sh - ライブラリロード"
    
    if source "$LIB_DIR/validation.sh" 2>/dev/null; then
        test_pass "validation.sh loaded successfully"
    else
        test_fail "Failed to load validation.sh"
        return 1
    fi
    
    # 検証結果管理テスト
    test_start "validation.sh - 検証結果管理"
    
    clear_validation_results
    
    add_validation_result "$VALIDATION_PASS" "Test" "Pass test"
    assert "[[ $_VALIDATION_PASS_COUNT -eq 1 ]]" "Pass count incremented"
    
    add_validation_result "$VALIDATION_FAIL" "Test" "Fail test"
    assert "[[ $_VALIDATION_FAIL_COUNT -eq 1 ]]" "Fail count incremented"
    
    add_validation_result "$VALIDATION_WARN" "Test" "Warn test"
    assert "[[ $_VALIDATION_WARN_COUNT -eq 1 ]]" "Warn count incremented"
    
    # ファイル検証テスト
    test_start "validation.sh - ファイル検証"
    
    local test_file="$TEST_TEMP_DIR/validation_test.txt"
    echo "test content" > "$test_file"
    
    clear_validation_results
    validate_file_exists "$test_file" "TestFile"
    assert "[[ $_VALIDATION_PASS_COUNT -eq 1 ]]" "File exists validation passed"
    
    validate_file_exists "/nonexistent/file" "TestFile"
    assert "[[ $_VALIDATION_FAIL_COUNT -eq 1 ]]" "Nonexistent file validation failed"
    
    # アサーションテスト
    test_start "validation.sh - アサーション"
    
    clear_validation_results
    assert_equals "hello" "hello" "String equality"
    assert "[[ $_VALIDATION_PASS_COUNT -eq 1 ]]" "assert_equals passed"
    
    assert_contains "hello world" "world" "String contains"
    assert "[[ $_VALIDATION_PASS_COUNT -eq 2 ]]" "assert_contains passed"
    
    assert_true "[[ 1 -eq 1 ]]" "Condition true"
    assert "[[ $_VALIDATION_PASS_COUNT -eq 3 ]]" "assert_true passed"
}

# === 統合テスト ===

test_integration() {
    test_start "統合 - 複数ライブラリの連携"
    
    # すべてのライブラリをロード
    source "$LIB_DIR/core.sh"
    source "$LIB_DIR/platform.sh"
    source "$LIB_DIR/tmux_ops.sh"
    source "$LIB_DIR/notification.sh"
    source "$LIB_DIR/validation.sh"
    
    # ログ設定
    setup_logging "$TEST_TEMP_DIR/integration.log" "DEBUG"
    
    # プラットフォーム検出とログ
    local platform=$(detect_platform)
    log_info "Platform: $platform"
    
    # 検証実行
    clear_validation_results
    validate_command_exists "bash" "Shell"
    
    # レポート生成
    local report_file="$TEST_TEMP_DIR/validation_report.txt"
    if generate_validation_report "$report_file"; then
        assert "[[ -f '$report_file' ]]" "Validation report generated"
    fi
    
    test_pass "Integration test completed"
}

# === メイン処理 ===

main() {
    echo "======================================"
    echo "TMux Scripts Library Test Suite"
    echo "======================================"
    echo ""
    
    # 各ライブラリのテスト実行
    test_core_library
    echo ""
    
    test_platform_library
    echo ""
    
    test_tmux_ops_library
    echo ""
    
    test_notification_library
    echo ""
    
    test_validation_library
    echo ""
    
    test_integration
    echo ""
    
    # 結果サマリー
    echo "======================================"
    echo "Test Results Summary"
    echo "======================================"
    echo -e "Total:   $TESTS_TOTAL"
    echo -e "Passed:  ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Failed:  ${RED}$TESTS_FAILED${NC}"
    echo -e "Skipped: ${YELLOW}$TESTS_SKIPPED${NC}"
    echo ""
    
    # クリーンアップ
    rm -rf "$TEST_TEMP_DIR"
    
    # 終了コード
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    else
        echo -e "${RED}Some tests failed.${NC}"
        exit 1
    fi
}

# テスト実行
main "$@"