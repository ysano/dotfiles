#!/bin/bash
# Enhanced Test Runner with Dependency Injection Support
# 依存性注入対応の統合テストランナー

set -euo pipefail

# テスト設定
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_HOME="$(dirname "$TEST_DIR")"
LOG_FILE="$CLAUDE_HOME/claude/logs/test-runner.log"

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# テスト統計
declare -A TEST_STATS=(
    ["total"]=0
    ["passed"]=0
    ["failed"]=0
    ["skipped"]=0
)

declare -A TEST_SUITES=(
    ["unit"]=0
    ["integration"]=0
    ["performance"]=0
)

# ログ関数
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# テストスイート実行関数
run_test_suite() {
    local test_file="$1"
    local suite_name="$2"
    local description="$3"
    
    echo -e "${BLUE}📋 Running: $suite_name${NC}"
    echo -e "   $description"
    echo ""
    
    local start_time=$(date +%s)
    local temp_log="/tmp/test_output_$$.log"
    
    if bash "$test_file" > "$temp_log" 2>&1; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        # 成功した場合の統計抽出（カラーコード除去）
        local passed=$(grep "Tests passed:" "$temp_log" | sed 's/\[[0-9;]*m//g' | grep -o "[0-9]\+" | head -1 || echo "0")
        local failed=$(grep "Tests failed:" "$temp_log" | sed 's/\[[0-9;]*m//g' | grep -o "[0-9]\+" | head -1 || echo "0")
        local total=$(grep "Total tests:" "$temp_log" | sed 's/\[[0-9;]*m//g' | grep -o "[0-9]\+" | head -1 || echo "0")
        
        # ✅❤️マーカーでもカウント
        if [[ $total -eq 0 ]]; then
            local pass_count=$(grep -c "✅ PASS" "$temp_log" || echo "0")
            local fail_count=$(grep -c "❌ FAIL" "$temp_log" || echo "0")
            passed=${pass_count:-$passed}
            failed=${fail_count:-$failed}
            total=$((passed + failed))
        fi
        
        TEST_STATS["total"]=$((TEST_STATS["total"] + total))
        TEST_STATS["passed"]=$((TEST_STATS["passed"] + passed))
        TEST_STATS["failed"]=$((TEST_STATS["failed"] + failed))
        
        echo -e "${GREEN}✅ PASSED${NC} $suite_name (${duration}s, $passed/$total tests)"
        
        # 詳細ログの保存
        cat "$temp_log" >> "$LOG_FILE"
        log "INFO" "$suite_name: PASSED ($passed/$total tests, ${duration}s)"
        
        return 0
    else
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        # 失敗した場合のエラー詳細表示
        echo -e "${RED}❌ FAILED${NC} $suite_name (${duration}s)"
        echo -e "${YELLOW}--- Error Details ---${NC}"
        tail -20 "$temp_log"
        echo -e "${YELLOW}--- End Error Details ---${NC}"
        
        # 統計更新（失敗として）
        TEST_STATS["total"]=$((TEST_STATS["total"] + 1))
        TEST_STATS["failed"]=$((TEST_STATS["failed"] + 1))
        
        log "ERROR" "$suite_name: FAILED (${duration}s)"
        
        rm -f "$temp_log"
        return 1
    fi
    
    rm -f "$temp_log"
}

# 個別テスト実行（オプション）
run_specific_test() {
    local test_name="$1"
    
    case "$test_name" in
        "3state"|"3-state")
            run_test_suite "$TEST_DIR/test-3state-detection.sh" "3-State Detection" "Basic 3-state Claude detection logic"
            ;;
        "enhanced"|"enhanced-detection")
            run_test_suite "$TEST_DIR/unit/test-enhanced-detection.sh" "Enhanced Detection" "Enhanced detection with fixture integration"
            ;;
        "dependency-injection"|"di")
            run_test_suite "$TEST_DIR/unit/test-dependency-injection.sh" "Dependency Injection" "Testable architecture with dependency injection"
            ;;
        "wsl")
            run_test_suite "$TEST_DIR/wsl-integration-test.sh" "WSL Integration" "Windows Subsystem for Linux integration"
            ;;
        *)
            echo -e "${RED}Unknown test: $test_name${NC}"
            echo "Available tests: 3state, enhanced, dependency-injection, wsl"
            return 1
            ;;
    esac
}

# 全体テストスイート実行
run_full_test_suite() {
    echo -e "${CYAN}🧪 Claude Voice Test Suite${NC}"
    echo -e "${CYAN}===========================${NC}"
    echo ""
    
    # 環境情報の表示
    echo -e "${YELLOW}Environment Information:${NC}"
    echo "  OS: $(uname -s)"
    echo "  Kernel: $(uname -r)"
    echo "  Shell: $SHELL"
    echo "  tmux: $(tmux -V 2>/dev/null || echo 'Not available')"
    echo "  PowerShell: $(command -v powershell.exe >/dev/null && echo 'Available' || echo 'Not available')"
    echo ""
    
    local overall_success=true
    
    # Unit Tests
    echo -e "${YELLOW}=== Unit Tests ===${NC}"
    
    if ! run_test_suite "$TEST_DIR/test-3state-detection.sh" "3-State Detection" "Basic 3-state Claude detection logic"; then
        overall_success=false
    fi
    echo ""
    
    if ! run_test_suite "$TEST_DIR/unit/test-enhanced-detection.sh" "Enhanced Detection" "Enhanced detection with fixture integration"; then
        overall_success=false
    fi
    echo ""
    
    if ! run_test_suite "$TEST_DIR/unit/test-dependency-injection.sh" "Dependency Injection" "Testable architecture with dependency injection"; then
        overall_success=false
    fi
    echo ""
    
    # Integration Tests
    echo -e "${YELLOW}=== Integration Tests ===${NC}"
    
    if ! run_test_suite "$TEST_DIR/wsl-integration-test.sh" "WSL Integration" "Windows Subsystem for Linux integration"; then
        # WSL テストは環境依存なので、失敗してもoverall_successには影響しない
        echo -e "${YELLOW}⚠️  WSL integration test failed (environment dependent)${NC}"
        TEST_STATS["skipped"]=$((TEST_STATS["skipped"] + 1))
    fi
    echo ""
    
    # 結果サマリー
    echo -e "${YELLOW}=== Test Summary ===${NC}"
    echo -e "Total tests: ${TEST_STATS["total"]}"
    echo -e "Passed: ${GREEN}${TEST_STATS["passed"]}${NC}"
    echo -e "Failed: ${RED}${TEST_STATS["failed"]}${NC}"
    echo -e "Skipped: ${YELLOW}${TEST_STATS["skipped"]}${NC}"
    
    local success_rate=0
    if [[ ${TEST_STATS["total"]} -gt 0 ]]; then
        success_rate=$(( (TEST_STATS["passed"] * 100) / TEST_STATS["total"] ))
    fi
    echo -e "Success rate: ${success_rate}%"
    
    # ログファイルの情報
    echo ""
    echo -e "Detailed logs: ${LOG_FILE}"
    
    if [[ "$overall_success" == "true" && ${TEST_STATS["failed"]} -eq 0 ]]; then
        echo -e "${GREEN}🎉 All critical tests passed!${NC}"
        log "INFO" "Test suite completed successfully"
        return 0
    else
        echo -e "${RED}💥 Some tests failed!${NC}"
        log "ERROR" "Test suite completed with failures"
        return 1
    fi
}

# パフォーマンスベンチマーク
run_performance_benchmark() {
    echo -e "${CYAN}🚀 Performance Benchmark${NC}"
    echo -e "${CYAN}========================${NC}"
    echo ""
    
    # 純粋関数のベンチマーク
    echo "Testing pure function performance..."
    local iterations=1000
    local total_time=0
    
    # 依存性注入モジュールの読み込み
    source "$CLAUDE_HOME/claude/core/status_detector.sh"
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        detect_claude_status_pure "✻ Thinking… (5s · 100 tokens · esc to interrupt)" >/dev/null
        local end_time=$(date +%s%N)
        local duration=$(( (end_time - start_time) / 1000000 ))
        total_time=$((total_time + duration))
    done
    
    local avg_time=$((total_time / iterations))
    echo "Pure function average: ${avg_time}ms (${iterations} iterations)"
    
    # 依存性注入版のベンチマーク
    echo "Testing dependency injection performance..."
    setup_test_mocks
    export MOCK_TERMINAL_OUTPUT="╭─ Claude Code ─╮\n✻ Processing… (5s · 100 tokens · esc to interrupt)"
    
    total_time=0
    iterations=100
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        detect_claude_status_with_deps >/dev/null 2>&1
        local end_time=$(date +%s%N)
        local duration=$(( (end_time - start_time) / 1000000 ))
        total_time=$((total_time + duration))
    done
    
    avg_time=$((total_time / iterations))
    echo "Dependency injection average: ${avg_time}ms (${iterations} iterations)"
    
    cleanup_test_mocks
    
    echo -e "${GREEN}Benchmark completed${NC}"
}

# メイン関数
main() {
    # ログディレクトリの作成
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # 引数処理
    case "${1:-all}" in
        "all"|"full")
            run_full_test_suite
            ;;
        "benchmark"|"perf")
            run_performance_benchmark
            ;;
        "help"|"-h"|"--help")
            echo "Usage: $0 [OPTION]"
            echo ""
            echo "Options:"
            echo "  all, full              Run full test suite (default)"
            echo "  benchmark, perf        Run performance benchmark"
            echo "  3state                 Run 3-state detection test"
            echo "  enhanced               Run enhanced detection test"
            echo "  dependency-injection   Run dependency injection test"
            echo "  wsl                    Run WSL integration test"
            echo "  help                   Show this help"
            ;;
        *)
            run_specific_test "$1"
            ;;
    esac
}

# スクリプト実行
main "$@"