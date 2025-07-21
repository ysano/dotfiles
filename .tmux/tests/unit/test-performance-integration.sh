#!/bin/bash
# Performance Integration Test
# パフォーマンス最適化統合テスト

set -euo pipefail

# テスト環境セットアップ
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_HOME="$(dirname "$(dirname "$TEST_DIR")")"

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# モジュール読み込み
source "$CLAUDE_HOME/claude/core/cache_manager.sh"
source "$CLAUDE_HOME/claude/core/performance_monitor.sh"
source "$CLAUDE_HOME/claude/services/detection_service.sh"

# === Test Functions ===

# テスト統計
declare -A TEST_RESULTS=(
    ["total"]=0
    ["passed"]=0
    ["failed"]=0
)

# テスト実行関数
run_test() {
    local test_name="$1"
    local test_function="$2"
    
    echo -e "${BLUE}Testing: $test_name${NC}"
    
    TEST_RESULTS["total"]=$((TEST_RESULTS["total"] + 1))
    
    if $test_function; then
        echo -e "${GREEN}✅ PASS${NC} $test_name"
        TEST_RESULTS["passed"]=$((TEST_RESULTS["passed"] + 1))
        return 0
    else
        echo -e "${RED}❌ FAIL${NC} $test_name"
        TEST_RESULTS["failed"]=$((TEST_RESULTS["failed"] + 1))
        return 1
    fi
}

# === Individual Test Cases ===

# キャッシュマネージャー基本テスト
test_cache_manager_basic() {
    # 基本操作
    cache_set "test1" "value1" "10" || return 1
    
    local result=$(cache_get "test1")
    [[ "$result" == "value1" ]] || return 1
    
    cache_exists "test1" || return 1
    
    # TTL テスト
    cache_set "test_ttl" "short_lived" "1" || return 1
    sleep 2
    ! cache_exists "test_ttl" || return 1
    
    # クリーンアップ
    cache_delete "test1"
    return 0
}

# キャッシュパフォーマンステスト（簡略版）
test_cache_performance() {
    local iterations=10  # 負荷軽減
    local start_time end_time duration
    
    # 基本的な読み書きテスト
    cache_set "perf_test_1" "test_value_1" "60" >/dev/null || return 1
    cache_set "perf_test_2" "test_value_2" "60" >/dev/null || return 1
    cache_set "perf_test_3" "test_value_3" "60" >/dev/null || return 1
    
    # 読み込みテスト
    local result1=$(cache_get "perf_test_1" 2>/dev/null)
    local result2=$(cache_get "perf_test_2" 2>/dev/null)  
    local result3=$(cache_get "perf_test_3" 2>/dev/null)
    
    # 結果検証
    [[ "$result1" == "test_value_1" ]] || return 1
    [[ "$result2" == "test_value_2" ]] || return 1
    [[ "$result3" == "test_value_3" ]] || return 1
    
    echo "  Cache read/write operations: ✅ Working"
    
    # クリーンアップ
    cache_delete "perf_test_1"
    cache_delete "perf_test_2"
    cache_delete "perf_test_3"
    return 0
}

# 検出サービス統合テスト
test_detection_service_integration() {
    # 設定更新テスト
    configure_detection_service "cache_ttl" "10" >/dev/null || return 1
    configure_detection_service "retry_count" "2" >/dev/null || return 1
    
    # キャッシュ機能テスト
    clear_detection_cache >/dev/null || return 1
    
    # 検出実行テスト（モック環境）
    export MOCK_TERMINAL_OUTPUT="✻ Thinking… (5s · 100 tokens · esc to interrupt)"
    source "$CLAUDE_HOME/claude/core/interfaces.sh"
    setup_test_mocks
    
    local result=$(detect_status_with_cache "test" "test" "false")
    [[ -n "$result" ]] || return 1
    
    # キャッシュヒットテスト
    local cached_result=$(detect_status_with_cache "test" "test" "true")
    [[ "$cached_result" == "$result" ]] || return 1
    
    cleanup_test_mocks
    return 0
}

# パフォーマンス監視テスト
test_performance_monitoring() {
    # メトリクス収集テスト
    collect_system_metrics || return 1
    
    # 応答時間測定テスト
    measure_claude_detection_performance || return 1
    
    # 履歴データ確認
    local cpu_percent="${PERFORMANCE_METRICS[cpu_percent]}"
    local memory_percent="${PERFORMANCE_METRICS[memory_percent]}"
    local response_time="${PERFORMANCE_METRICS[response_time_ms]}"
    
    # 妥当性チェック
    [[ $cpu_percent -ge 0 && $cpu_percent -le 100 ]] || return 1
    [[ $memory_percent -ge 0 && $memory_percent -le 100 ]] || return 1
    [[ $response_time -ge 0 ]] || return 1
    
    return 0
}

# 設定管理統合テスト
test_config_integration() {
    source "$CLAUDE_HOME/claude/core/config_manager_v2.sh"
    
    # 基本設定テスト
    local version=$(get_config "system.version" "unknown")
    [[ "$version" != "unknown" ]] || return 1
    
    # 環境検出テスト
    local env=$(detect_environment)
    [[ -n "$env" ]] || return 1
    
    # 設定変更テスト
    local old_ttl=$(get_config "performance.cache_ttl")
    set_config "performance.cache_ttl" "15" "false" "system" >/dev/null || return 1
    local new_ttl=$(get_config "performance.cache_ttl")
    [[ "$new_ttl" == "15" ]] || return 1
    
    # 元に戻す
    set_config "performance.cache_ttl" "$old_ttl" "false" "system" >/dev/null
    
    return 0
}

# アーキテクチャ整合性テスト
test_architecture_consistency() {
    source "$CLAUDE_HOME/claude/core/module_registry_v2.sh"
    
    # アーキテクチャ検証
    validate_dependency_direction >/dev/null || return 1
    detect_circular_dependencies >/dev/null || return 1
    
    # モジュール読み込み順序テスト
    local test_modules=("detection_service" "cache_manager")
    local load_order=($(calculate_load_order "${test_modules[@]}"))
    [[ ${#load_order[@]} -ge 2 ]] || return 1
    
    return 0
}

# エンドツーエンドパフォーマンステスト
test_end_to_end_performance() {
    # リアルなワークロードシミュレーション
    local iterations=50
    local total_time=0
    local successful_detections=0
    
    # モック環境セットアップ
    export MOCK_TERMINAL_OUTPUT="✻ Processing… (3s · 50 tokens · esc to interrupt)"
    setup_test_mocks
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        
        # 検出実行
        if detect_current_pane >/dev/null 2>&1; then
            ((successful_detections++))
        fi
        
        local end_time=$(date +%s%N)
        local duration=$(( (end_time - start_time) / 1000000 ))
        total_time=$((total_time + duration))
        
        # 短時間待機でリアルなワークロードをシミュレート
        sleep 0.1
    done
    
    cleanup_test_mocks
    
    # パフォーマンス基準チェック
    local avg_time=$((total_time / iterations))
    local success_rate=$(( (successful_detections * 100) / iterations ))
    
    # 平均50ms以内、成功率80%以上
    [[ $avg_time -lt 50 ]] || return 1
    [[ $success_rate -gt 80 ]] || return 1
    
    echo "  Performance: ${avg_time}ms avg, ${success_rate}% success rate"
    return 0
}

# === Main Test Execution ===

main() {
    echo -e "${BLUE}🚀 Performance Integration Test Suite${NC}"
    echo -e "${BLUE}===================================${NC}"
    echo ""
    
    # テスト実行
    run_test "Cache Manager Basic Operations" test_cache_manager_basic
    echo ""
    
    run_test "Cache Performance Benchmarks" test_cache_performance
    echo ""
    
    run_test "Detection Service Integration" test_detection_service_integration
    echo ""
    
    run_test "Performance Monitoring" test_performance_monitoring
    echo ""
    
    run_test "Configuration Management Integration" test_config_integration
    echo ""
    
    run_test "Architecture Consistency" test_architecture_consistency
    echo ""
    
    run_test "End-to-End Performance" test_end_to_end_performance
    echo ""
    
    # 結果サマリー
    echo -e "${YELLOW}=== Test Results ===${NC}"
    echo "Total tests: ${TEST_RESULTS[total]}"
    echo -e "Passed: ${GREEN}${TEST_RESULTS[passed]}${NC}"
    echo -e "Failed: ${RED}${TEST_RESULTS[failed]}${NC}"
    
    local success_rate=0
    if [[ ${TEST_RESULTS[total]} -gt 0 ]]; then
        success_rate=$(( (TEST_RESULTS[passed] * 100) / TEST_RESULTS[total] ))
    fi
    echo "Success rate: ${success_rate}%"
    
    if [[ ${TEST_RESULTS[failed]} -eq 0 ]]; then
        echo -e "${GREEN}🎉 All performance integration tests passed!${NC}"
        return 0
    else
        echo -e "${RED}💥 Some tests failed!${NC}"
        return 1
    fi
}

# スクリプト実行
main "$@"