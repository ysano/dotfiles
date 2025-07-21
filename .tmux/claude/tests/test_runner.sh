#!/bin/bash
# Test Runner - Claude Voice テストスイート実行ツール
# リファクタリングされた5つのモジュールの包括的テスト

set -euo pipefail

# テスト環境設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
TEST_DIR="$(dirname "${BASH_SOURCE[0]}")"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
TEST_OUTPUT_DIR="$CLAUDE_VOICE_HOME/tests/output"
TEST_REPORT_FILE="$TEST_OUTPUT_DIR/test_report_$(date +%Y%m%d_%H%M%S).txt"

# テスト結果カウンタ
total_tests=0
passed_tests=0
failed_tests=0
skipped_tests=0

# カラー出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ユーティリティ関数
log_test() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    case "$level" in
        "PASS")
            echo -e "${GREEN}✅ PASS${NC}: $message" | tee -a "$TEST_REPORT_FILE"
            ;;
        "FAIL")
            echo -e "${RED}❌ FAIL${NC}: $message" | tee -a "$TEST_REPORT_FILE"
            ;;
        "SKIP")
            echo -e "${YELLOW}⏸️  SKIP${NC}: $message" | tee -a "$TEST_REPORT_FILE"
            ;;
        "INFO")
            echo -e "${BLUE}ℹ️  INFO${NC}: $message" | tee -a "$TEST_REPORT_FILE"
            ;;
        *)
            echo "[$timestamp] $message" | tee -a "$TEST_REPORT_FILE"
            ;;
    esac
}

# テスト初期化
init_test_environment() {
    echo "=== Claude Voice Test Suite ===" | tee "$TEST_REPORT_FILE"
    echo "開始時刻: $(date)" | tee -a "$TEST_REPORT_FILE"
    echo "" | tee -a "$TEST_REPORT_FILE"

    # テスト出力ディレクトリ作成
    mkdir -p "$TEST_OUTPUT_DIR"

    # テスト用環境変数設定
    export CLAUDE_VOICE_TEST_MODE=true
    export CLAUDE_VOICE_LOG_LEVEL=DEBUG

    log_test "INFO" "テスト環境初期化完了"
    log_test "INFO" "CLAUDE_VOICE_HOME: $CLAUDE_VOICE_HOME"
    log_test "INFO" "テストレポート: $TEST_REPORT_FILE"
}

# 単体テスト実行
run_unit_tests() {
    log_test "INFO" "=== 単体テスト開始 ==="

    # user_interface.sh テスト
    if run_single_test "test_user_interface.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))

    # stats_monitor.sh テスト
    if run_single_test "test_stats_monitor.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))

    # config_manager.sh テスト
    if run_single_test "test_config_manager.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))

    # health_diagnostics.sh テスト
    if run_single_test "test_health_diagnostics.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))

    # execution_engine.sh テスト
    if run_single_test "test_execution_engine.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))
}

# 統合テスト実行
run_integration_tests() {
    log_test "INFO" "=== 統合テスト開始 ==="

    # モジュール間連携テスト
    if run_single_test "test_module_integration.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))

    # エンドツーエンドテスト
    if run_single_test "test_end_to_end.sh"; then
        ((passed_tests++))
    else
        ((failed_tests++))
    fi
    ((total_tests++))
}

# 高速構文チェック
run_syntax_check() {
    log_test "INFO" "=== 構文チェック開始 ==="

    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )

    local syntax_errors=0

    for module in "${modules[@]}"; do
        local module_path="$CORE_DIR/$module"
        if [[ -f "$module_path" ]]; then
            if bash -n "$module_path" 2>/dev/null; then
                log_test "PASS" "$module - 構文OK"
                ((passed_tests++))
            else
                log_test "FAIL" "$module - 構文エラー"
                ((failed_tests++))
                ((syntax_errors++))
            fi
            ((total_tests++))
        else
            log_test "SKIP" "$module - ファイルなし"
            ((skipped_tests++))
        fi
    done

    if [[ $syntax_errors -eq 0 ]]; then
        log_test "PASS" "全モジュール構文チェック成功"
    else
        log_test "FAIL" "${syntax_errors}個のモジュールに構文エラー"
    fi
}

# 個別テスト実行
run_single_test() {
    local test_file="$1"
    local test_path="$TEST_DIR/$test_file"

    if [[ ! -f "$test_path" ]]; then
        log_test "SKIP" "$test_file (ファイルが存在しません)"
        ((skipped_tests++))
        return 1
    fi

    log_test "INFO" "$test_file 実行中..."

    # テスト実行（タイムアウト付き）
    if timeout 120 bash "$test_path" >>"$TEST_REPORT_FILE" 2>&1; then
        log_test "PASS" "$test_file"
        return 0
    else
        log_test "FAIL" "$test_file"
        return 1
    fi
}

# カバレッジ分析（簡易版）
analyze_coverage() {
    log_test "INFO" "=== カバレッジ分析 ==="

    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )

    for module in "${modules[@]}"; do
        local module_path="$CORE_DIR/$module"
        if [[ -f "$module_path" ]]; then
            local total_functions=$(grep -c "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*(" "$module_path" || echo 0)
            log_test "INFO" "$module: $total_functions 関数"
        fi
    done
}

# パフォーマンステスト
run_performance_tests() {
    log_test "INFO" "=== パフォーマンステスト ==="

    # 各モジュールの読み込み時間測定
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )

    for module in "${modules[@]}"; do
        local module_path="$CORE_DIR/$module"
        if [[ -f "$module_path" ]]; then
            local start_time=$(date +%s%3N)
            if source "$module_path" 2>/dev/null; then
                local end_time=$(date +%s%3N)
                local duration=$((end_time - start_time))
                log_test "INFO" "$module 読み込み時間: ${duration}ms"
            else
                log_test "FAIL" "$module 読み込みエラー"
            fi
        fi
    done
}

# 最終レポート生成
generate_final_report() {
    echo "" | tee -a "$TEST_REPORT_FILE"
    echo "=== テスト結果サマリー ===" | tee -a "$TEST_REPORT_FILE"
    echo "実行時刻: $(date)" | tee -a "$TEST_REPORT_FILE"
    echo "総テスト数: $total_tests" | tee -a "$TEST_REPORT_FILE"
    echo "成功: $passed_tests" | tee -a "$TEST_REPORT_FILE"
    echo "失敗: $failed_tests" | tee -a "$TEST_REPORT_FILE"
    echo "スキップ: $skipped_tests" | tee -a "$TEST_REPORT_FILE"

    local success_rate=0
    if [[ $total_tests -gt 0 ]]; then
        success_rate=$((passed_tests * 100 / total_tests))
    fi
    echo "成功率: ${success_rate}%" | tee -a "$TEST_REPORT_FILE"

    if [[ $failed_tests -eq 0 ]]; then
        log_test "PASS" "🎉 全テストが成功しました！"
        return 0
    else
        log_test "FAIL" "❌ ${failed_tests}個のテストが失敗しました"
        return 1
    fi
}

# メイン実行
main() {
    local test_type="${1:-all}"

    init_test_environment

    local quick_mode="${2:-false}"
    if [[ "$quick_mode" == "--quick" ]]; then
        export CLAUDE_VOICE_QUICK_TEST=true
        log_test "INFO" "高速テストモード有効"
    fi

    case "$test_type" in
        "unit")
            run_unit_tests
            ;;
        "integration")
            run_integration_tests
            ;;
        "performance")
            run_performance_tests
            ;;
        "coverage")
            analyze_coverage
            ;;
        "quick")
            export CLAUDE_VOICE_QUICK_TEST=true
            log_test "INFO" "=== 高速テスト開始 ==="
            # 構文チェックのみ実行
            run_syntax_check
            ;;
        "all")
            run_unit_tests
            run_integration_tests
            run_performance_tests
            analyze_coverage
            ;;
        *)
            echo "使用法: $0 [unit|integration|performance|coverage|quick|all] [--quick]"
            echo ""
            echo "  unit        - 単体テスト実行"
            echo "  integration - 統合テスト実行"
            echo "  performance - パフォーマンステスト実行"
            echo "  coverage    - カバレッジ分析"
            echo "  quick       - 高速構文チェック"
            echo "  all         - 全テスト実行（デフォルト）"
            exit 1
            ;;
    esac

    generate_final_report
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
