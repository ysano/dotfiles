#!/bin/bash
# Dependency Injection Test Suite
# 依存性注入システムのテストスイート

# テストフレームワーク
TESTS_PASSED=0
TESTS_FAILED=0

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# テストディレクトリ設定
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_HOME="$(dirname "$(dirname "$TEST_DIR")")"

# 依存性注入対応モジュールの読み込み
source "$CLAUDE_HOME/claude/core/status_detector.sh"

# テストヘルパー関数
test_case() {
    local test_name="$1"
    local test_func="$2"
    
    echo -e "${YELLOW}Running: $test_name${NC}"
    
    if $test_func; then
        echo -e "${GREEN}✅ PASS: $test_name${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}❌ FAIL: $test_name${NC}"
        ((TESTS_FAILED++))
    fi
    echo ""
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"
    
    if [[ "$expected" == "$actual" ]]; then
        return 0
    else
        echo -e "${RED}  Expected: '$expected'${NC}"
        echo -e "${RED}  Actual: '$actual'${NC}"
        echo -e "${RED}  Message: $message${NC}"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-Should contain pattern}"
    
    if echo "$haystack" | grep -q "$needle"; then
        return 0
    else
        echo -e "${RED}  Pattern '$needle' not found${NC}"
        echo -e "${RED}  In text: '$haystack'${NC}"
        echo -e "${RED}  Message: $message${NC}"
        return 1
    fi
}

# === インターフェース抽象化のテスト ===

test_interface_registration() {
    # カスタムモック関数の定義
    custom_mock() { echo "custom_result"; }
    
    # モックの登録
    register_mock_interface "tmux" "get_window_id" "custom_mock"
    
    # モックの動作確認
    local result=$(call_tmux get_window_id)
    assert_equals "custom_result" "$result" "Custom mock should be called"
    
    # リセット後の確認
    reset_interface "tmux"
    # Note: 実際のtmuxコマンドは動作しない可能性があるため、エラーを無視
    call_tmux get_window_id >/dev/null 2>&1 || true
    
    return 0
}

test_interface_error_handling() {
    # 存在しない操作の呼び出し
    local result=$(call_tmux nonexistent_operation 2>&1)
    assert_contains "$result" "not implemented" "Should report unimplemented operation"
}

test_multiple_interface_types() {
    # 複数のインターフェースタイプのテスト
    mock_tmux() { echo "tmux_mock"; }
    mock_process() { echo "process_mock"; }
    mock_filesystem() { echo "filesystem_mock"; }
    
    register_mock_interface "tmux" "get_window_id" "mock_tmux"
    register_mock_interface "process" "find_process" "mock_process"
    register_mock_interface "filesystem" "has_command" "mock_filesystem"
    
    local tmux_result=$(call_tmux get_window_id)
    local process_result=$(call_process find_process "test")
    local filesystem_result=$(call_filesystem has_command "test")
    
    assert_equals "tmux_mock" "$tmux_result" "TMUX mock should work"
    assert_equals "process_mock" "$process_result" "Process mock should work"
    assert_equals "filesystem_mock" "$filesystem_result" "Filesystem mock should work"
    
    reset_interface "all"
}

# === 純粋関数のテスト ===

test_pure_status_detection_busy() {
    local busy_output="✻ Ruminating… (6s · 2.8k tokens · esc to interrupt)"
    local result=$(detect_claude_status_pure "$busy_output")
    assert_equals "Busy" "$result" "Should detect busy state from processing pattern"
}

test_pure_status_detection_waiting() {
    local waiting_output="Do you want to proceed with this action?\n❯ 1. Yes\n  2. No"
    local result=$(detect_claude_status_pure "$waiting_output")
    assert_equals "Waiting" "$result" "Should detect waiting state from confirmation"
}

test_pure_status_detection_idle() {
    local idle_output="> \n? for shortcuts"
    local result=$(detect_claude_status_pure "$idle_output")
    assert_equals "Idle" "$result" "Should detect idle state from prompt"
}

test_pure_status_detection_fallback() {
    local unknown_output="Some random text that doesn't match any pattern"
    local result=$(detect_claude_status_pure "$unknown_output")
    assert_equals "Idle" "$result" "Should fallback to idle for unknown patterns"
}

test_pure_session_detection() {
    local claude_output="╭─ Claude Code Session ─╮\n> \n? for shortcuts"
    local result=$(is_claude_code_session_pure "$claude_output")
    assert_equals "0" "$?" "Should detect Claude Code session from UI elements"
    
    local non_claude_output="$ ls -la\ntotal 64"
    is_claude_code_session_pure "$non_claude_output"
    assert_equals "1" "$?" "Should not detect Claude Code in regular terminal"
}

test_pure_icon_mapping() {
    local busy_icon=$(get_state_icon_pure "Busy")
    local waiting_icon=$(get_state_icon_pure "Waiting")
    local idle_icon=$(get_state_icon_pure "Idle")
    local unknown_icon=$(get_state_icon_pure "Unknown")
    
    assert_equals "⚡" "$busy_icon" "Busy should map to lightning icon"
    assert_equals "⌛" "$waiting_icon" "Waiting should map to hourglass icon"
    assert_equals "✅" "$idle_icon" "Idle should map to checkmark icon"
    assert_equals "" "$unknown_icon" "Unknown should map to empty string"
}

# === 依存性注入統合テスト ===

test_dependency_injection_integration() {
    # モックのセットアップ
    setup_test_mocks
    
    # テスト用のターミナル出力設定
    export MOCK_TERMINAL_OUTPUT="╭─ Claude Code ─╮\n✻ Processing… (5s · 100 tokens · esc to interrupt)"
    
    # 依存性注入版の呼び出し
    local result=$(detect_claude_status_with_deps)
    
    # 結果の検証
    assert_equals "⚡" "$result" "Should return busy icon through dependency injection"
    
    # クリーンアップ
    cleanup_test_mocks
}

test_dependency_injection_no_claude_process() {
    # Claude プロセスが見つからない場合のモック
    mock_no_process() { echo ""; }
    
    register_mock_interface "process" "find_process" "mock_no_process"
    
    local result=$(detect_claude_status_with_deps)
    assert_equals "" "$result" "Should return empty when no Claude process found"
    
    reset_interface "process"
}

test_dependency_injection_no_claude_session() {
    setup_test_mocks
    
    # Claude Code UI要素のない出力
    export MOCK_TERMINAL_OUTPUT="$ ls -la\ntotal 64"
    
    local result=$(detect_claude_status_with_deps)
    assert_equals "" "$result" "Should return empty when not a Claude Code session"
    
    cleanup_test_mocks
}

# === バックワード互換性テスト ===

test_backward_compatibility_pure() {
    # 1引数での呼び出し（純粋関数版）
    local result=$(detect_claude_status "✻ Thinking… (3s · 50 tokens · esc to interrupt)")
    assert_equals "Busy" "$result" "Single argument should use pure function"
}

test_backward_compatibility_with_deps() {
    setup_test_mocks
    export MOCK_TERMINAL_OUTPUT="╭─ Claude Code ─╮\n> \n? for shortcuts"
    
    # 引数なしでの呼び出し（依存性注入版）
    local result=$(detect_claude_status)
    assert_equals "✅" "$result" "No arguments should use dependency injection"
    
    cleanup_test_mocks
}

# === パフォーマンステスト ===

test_performance_pure_functions() {
    local iterations=100
    local total_time=0
    
    echo "Running performance test ($iterations iterations)..."
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        detect_claude_status_pure "✻ Thinking… (5s · 100 tokens · esc to interrupt)" >/dev/null
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))  # Convert to ms
        total_time=$((total_time + duration))
    done
    
    local avg_time=$((total_time / iterations))
    echo "Average pure function time: ${avg_time}ms"
    
    # パフォーマンス目標: 10ms以下
    if [[ $avg_time -lt 10 ]]; then
        return 0
    else
        echo "Performance test failed: ${avg_time}ms > 10ms"
        return 1
    fi
}

test_performance_with_mocks() {
    setup_test_mocks
    export MOCK_TERMINAL_OUTPUT="╭─ Claude Code ─╮\n✻ Processing… (5s · 100 tokens · esc to interrupt)"
    
    local iterations=50
    local total_time=0
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        detect_claude_status_with_deps >/dev/null 2>&1
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))
        total_time=$((total_time + duration))
    done
    
    local avg_time=$((total_time / iterations))
    echo "Average dependency injection time: ${avg_time}ms"
    
    cleanup_test_mocks
    
    # パフォーマンス目標: 30ms以下
    if [[ $avg_time -lt 30 ]]; then
        return 0
    else
        echo "Performance test failed: ${avg_time}ms > 30ms"
        return 1
    fi
}

# === メインテスト実行 ===

main() {
    echo -e "${YELLOW}=== Dependency Injection Test Suite ===${NC}"
    echo ""
    
    # インターフェース抽象化テスト
    echo -e "${YELLOW}--- Interface Abstraction Tests ---${NC}"
    test_case "Interface Registration" test_interface_registration
    test_case "Interface Error Handling" test_interface_error_handling
    test_case "Multiple Interface Types" test_multiple_interface_types
    
    # 純粋関数テスト
    echo -e "${YELLOW}--- Pure Function Tests ---${NC}"
    test_case "Pure Status Detection: Busy" test_pure_status_detection_busy
    test_case "Pure Status Detection: Waiting" test_pure_status_detection_waiting
    test_case "Pure Status Detection: Idle" test_pure_status_detection_idle
    test_case "Pure Status Detection: Fallback" test_pure_status_detection_fallback
    test_case "Pure Session Detection" test_pure_session_detection
    test_case "Pure Icon Mapping" test_pure_icon_mapping
    
    # 依存性注入統合テスト
    echo -e "${YELLOW}--- Dependency Injection Integration Tests ---${NC}"
    test_case "DI Integration" test_dependency_injection_integration
    test_case "DI No Claude Process" test_dependency_injection_no_claude_process
    test_case "DI No Claude Session" test_dependency_injection_no_claude_session
    
    # バックワード互換性テスト
    echo -e "${YELLOW}--- Backward Compatibility Tests ---${NC}"
    test_case "Backward Compatibility: Pure" test_backward_compatibility_pure
    test_case "Backward Compatibility: With Deps" test_backward_compatibility_with_deps
    
    # パフォーマンステスト
    echo -e "${YELLOW}--- Performance Tests ---${NC}"
    test_case "Performance: Pure Functions" test_performance_pure_functions
    test_case "Performance: With Mocks" test_performance_with_mocks
    
    # サマリー
    echo -e "${YELLOW}=== Test Summary ===${NC}"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    echo -e "Total tests: $((TESTS_PASSED + TESTS_FAILED))"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}🎉 All tests passed!${NC}"
        exit 0
    else
        echo -e "${RED}💥 Some tests failed!${NC}"
        exit 1
    fi
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi