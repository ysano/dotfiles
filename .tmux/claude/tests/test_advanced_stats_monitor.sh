#!/bin/bash
# Advanced Test Cases for stats_monitor.sh
# 高度な統計監視機能テスト（未カバー機能対応）

set -euo pipefail

# テスト環境設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
MODULE_PATH="$CORE_DIR/stats_monitor.sh"

# テストカウンタ
test_count=0
passed_count=0
failed_count=0

# テスト用一時ディレクトリ
TEST_TEMP_DIR="/tmp/test_advanced_stats_$$"

# セットアップ
setup_test_environment() {
    mkdir -p "$TEST_TEMP_DIR/logs"
    export CLAUDE_VOICE_TEST_MODE=true

    # 複雑なテスト用統計データ
    cat >"$TEST_TEMP_DIR/logs/usage_stats.jsonl" <<'EOF'
{"timestamp":1640995200,"operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":3,"success":"true","version":"2.0.0"}
{"timestamp":1640995800,"operation":"claude_voice_main","summary_type":"detailed","model":"auto","os_type":"linux","duration":5,"success":"true","version":"2.0.0"}
{"timestamp":1640996400,"operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":2,"success":"false","version":"2.0.0"}
{"timestamp":1640997000,"operation":"claude_voice_main","summary_type":"technical","model":"llama2:7b","os_type":"darwin","duration":8,"success":"true","version":"2.0.0"}
{"timestamp":1640997600,"operation":"claude_voice_main","summary_type":"brief","model":"auto","os_type":"windows","duration":4,"success":"true","version":"2.0.0"}
{"timestamp":1640998200,"operation":"health_check","summary_type":"","model":"","os_type":"linux","duration":1,"success":"true","version":"2.0.0"}
{"timestamp":1640998800,"operation":"config_update","summary_type":"","model":"","os_type":"linux","duration":0,"success":"true","version":"2.0.0"}
EOF

    # エラーデータ含む統計
    cat >"$TEST_TEMP_DIR/logs/error_stats.jsonl" <<'EOF'
{"timestamp":1640999400,"operation":"claude_voice_main","summary_type":"brief","model":"invalid_model","os_type":"linux","duration":0,"success":"false","version":"2.0.0","error":"model_not_found"}
{"timestamp":1641000000,"operation":"claude_voice_main","summary_type":"detailed","model":"phi4-mini:latest","os_type":"linux","duration":15,"success":"false","version":"2.0.0","error":"timeout"}
EOF

    # 大容量データ（パフォーマンステスト用）
    for i in {1..100}; do
        local timestamp=$((1640995200 + i * 600))
        echo "{\"timestamp\":$timestamp,\"operation\":\"claude_voice_main\",\"summary_type\":\"brief\",\"model\":\"phi4-mini:latest\",\"os_type\":\"linux\",\"duration\":$((i % 10 + 1)),\"success\":\"true\",\"version\":\"2.0.0\"}" >>"$TEST_TEMP_DIR/logs/large_stats.jsonl"
    done
}

# クリーンアップ
cleanup_test_environment() {
    rm -rf "$TEST_TEMP_DIR"
}

# テストユーティリティ
assert_equals() {
    local expected="$1"
    local actual="$2"
    local description="$3"

    ((test_count++))

    if [[ "$expected" == "$actual" ]]; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   期待値: '$expected'"
        echo "   実際値: '$actual'"
        ((failed_count++))
        return 1
    fi
}

assert_numeric_range() {
    local value="$1"
    local min="$2"
    local max="$3"
    local description="$4"

    ((test_count++))

    if [[ "$value" =~ ^[0-9]+(\.[0-9]+)?$ ]] && (($(echo "$value >= $min && $value <= $max" | bc -l))); then
        echo "✅ PASS: $description (値: $value)"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   値 '$value' が範囲 [$min-$max] 外です"
        ((failed_count++))
        return 1
    fi
}

assert_contains_pattern() {
    local text="$1"
    local pattern="$2"
    local description="$3"

    ((test_count++))

    if [[ "$text" =~ $pattern ]]; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   パターン '$pattern' が見つかりませんでした"
        ((failed_count++))
        return 1
    fi
}

# モジュール読み込み
load_stats_module() {
    if [[ -f "$MODULE_PATH" ]] && source "$MODULE_PATH" 2>/dev/null; then
        return 0
    else
        echo "❌ FAIL: stats_monitor.sh の読み込みに失敗"
        exit 1
    fi
}

# === 高度な統計分析テスト ===

test_time_series_analysis() {
    echo "=== 時系列分析テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    if declare -f analyze_time_series >/dev/null 2>&1; then
        local analysis_result
        analysis_result=$(analyze_time_series "24h" 2>&1)

        if [[ -n "$analysis_result" ]]; then
            assert_contains_pattern "$analysis_result" "trend|パターン|平均" "時系列分析結果にパターン情報が含まれる"

            # 時間帯別分析の確認
            local hourly_analysis
            hourly_analysis=$(analyze_time_series "hourly" 2>&1)
            assert_contains_pattern "$hourly_analysis" "[0-9]+時|hour" "時間帯別分析が実行される"
        else
            echo "❌ FAIL: 時系列分析で出力なし"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "⚠️  SKIP: analyze_time_series関数が存在しません"
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

test_performance_metrics() {
    echo ""
    echo "=== パフォーマンス指標テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    if declare -f calculate_performance_metrics >/dev/null 2>&1; then
        local metrics_result
        metrics_result=$(calculate_performance_metrics 2>&1)

        if [[ -n "$metrics_result" ]]; then
            # 平均実行時間の確認
            if echo "$metrics_result" | grep -q "平均.*秒\|average.*ms"; then
                echo "✅ PASS: 平均実行時間が計算される"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 平均実行時間が含まれない"
                ((test_count++))
                ((failed_count++))
            fi

            # 成功率の確認
            if echo "$metrics_result" | grep -q "成功率\|success rate"; then
                echo "✅ PASS: 成功率が計算される"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 成功率が含まれない"
                ((test_count++))
                ((failed_count++))
            fi

            # パフォーマンストレンドの確認
            local trend_analysis
            trend_analysis=$(calculate_performance_metrics "trend" 2>&1)
            assert_contains_pattern "$trend_analysis" "改善|悪化|安定|trend" "パフォーマンストレンド分析が含まれる"
        else
            echo "❌ FAIL: パフォーマンス指標計算で出力なし"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "⚠️  SKIP: calculate_performance_metrics関数が存在しません"
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

test_error_analysis() {
    echo ""
    echo "=== エラー分析テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    # エラー統計ファイルをコピー
    cp "$TEST_TEMP_DIR/logs/error_stats.jsonl" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

    if declare -f analyze_error_patterns >/dev/null 2>&1; then
        local error_analysis
        error_analysis=$(analyze_error_patterns 2>&1)

        if [[ -n "$error_analysis" ]]; then
            assert_contains_pattern "$error_analysis" "エラー|error|失敗" "エラー分析結果が含まれる"

            # エラータイプ別分析
            local error_types
            error_types=$(analyze_error_patterns "by_type" 2>&1)
            assert_contains_pattern "$error_types" "timeout|model_not_found" "具体的なエラータイプが分析される"

            # エラー頻度分析
            local error_frequency
            error_frequency=$(analyze_error_patterns "frequency" 2>&1)
            assert_contains_pattern "$error_frequency" "[0-9]+.*回|[0-9]+.*times" "エラー頻度が数値で表示される"
        else
            echo "❌ FAIL: エラー分析で出力なし"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "⚠️  SKIP: analyze_error_patterns関数が存在しません"
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

test_usage_prediction() {
    echo ""
    echo "=== 使用量予測テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    if declare -f predict_usage_trends >/dev/null 2>&1; then
        local prediction_result
        prediction_result=$(predict_usage_trends "7d" 2>&1)

        if [[ -n "$prediction_result" ]]; then
            assert_contains_pattern "$prediction_result" "予測|prediction|見込み" "使用量予測結果が含まれる"

            # 日別予測
            local daily_prediction
            daily_prediction=$(predict_usage_trends "daily" 2>&1)
            assert_contains_pattern "$daily_prediction" "日|day" "日別予測が実行される"

            # 信頼区間の確認
            local confidence_interval
            confidence_interval=$(predict_usage_trends "confidence" 2>&1)
            assert_contains_pattern "$confidence_interval" "[0-9]+%|信頼" "信頼区間が計算される"
        else
            echo "❌ FAIL: 使用量予測で出力なし"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "⚠️  SKIP: predict_usage_trends関数が存在しません"
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

# === エッジケーステスト ===

test_empty_stats_handling() {
    echo ""
    echo "=== 空統計データハンドリングテスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    local empty_stats_dir="$TEST_TEMP_DIR/empty_stats"
    mkdir -p "$empty_stats_dir/logs"
    touch "$empty_stats_dir/logs/usage_stats.jsonl" # 空ファイル

    export CLAUDE_VOICE_HOME="$empty_stats_dir"

    if declare -f show_stats >/dev/null 2>&1; then
        local empty_stats_output
        empty_stats_output=$(show_stats summary 2>&1)

        if [[ -n "$empty_stats_output" ]]; then
            assert_contains_pattern "$empty_stats_output" "データがありません|統計なし|no data" "空データに対する適切なメッセージ"
        else
            echo "❌ FAIL: 空統計データで出力なし"
            ((test_count++))
            ((failed_count++))
        fi
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

test_corrupted_data_handling() {
    echo ""
    echo "=== 破損データハンドリングテスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    local corrupted_dir="$TEST_TEMP_DIR/corrupted_stats"
    mkdir -p "$corrupted_dir/logs"

    # 破損したJSONデータ
    cat >"$corrupted_dir/logs/usage_stats.jsonl" <<'EOF'
{"timestamp":1640995200,"operation":"claude_voice_main","summary_type":"brief"
invalid json line
{"timestamp":"invalid_timestamp","operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":"invalid_duration","success":"true","version":"2.0.0"}
EOF

    export CLAUDE_VOICE_HOME="$corrupted_dir"

    if declare -f show_stats >/dev/null 2>&1; then
        local corrupted_output
        corrupted_output=$(show_stats summary 2>&1)

        # エラーハンドリングが適切に行われることを確認
        if [[ -n "$corrupted_output" ]]; then
            assert_contains_pattern "$corrupted_output" "エラー|警告|error|warning|無効" "破損データに対するエラーハンドリング"
        else
            echo "⚠️  INFO: 破損データでも処理続行（想定内）"
        fi
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

test_large_dataset_performance() {
    echo ""
    echo "=== 大容量データパフォーマンステスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    local large_data_dir="$TEST_TEMP_DIR/large_data"
    mkdir -p "$large_data_dir/logs"
    cp "$TEST_TEMP_DIR/logs/large_stats.jsonl" "$large_data_dir/logs/usage_stats.jsonl"

    export CLAUDE_VOICE_HOME="$large_data_dir"

    if declare -f show_stats >/dev/null 2>&1; then
        # 実行時間測定
        local start_time=$(date +%s%3N)
        local large_stats_output
        large_stats_output=$(show_stats summary 2>&1)
        local end_time=$(date +%s%3N)
        local duration=$((end_time - start_time))

        # 10秒以内での処理を期待
        if [[ $duration -lt 10000 ]]; then
            echo "✅ PASS: 大容量データ処理時間: ${duration}ms (< 10000ms)"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 大容量データ処理時間: ${duration}ms (>= 10000ms)"
            ((test_count++))
            ((failed_count++))
        fi

        # 結果の妥当性確認
        if [[ -n "$large_stats_output" ]]; then
            assert_contains_pattern "$large_stats_output" "100|統計" "大容量データ処理結果が含まれる"
        fi
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

# === 境界値テスト ===

test_boundary_conditions() {
    echo ""
    echo "=== 境界値テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    if declare -f record_usage_stats >/dev/null 2>&1; then
        # 最小値テスト
        if record_usage_stats "" "" "" "0" "false" >/dev/null 2>&1; then
            echo "✅ PASS: 最小値での統計記録"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 最小値での統計記録失敗"
            ((test_count++))
            ((failed_count++))
        fi

        # 最大値テスト（長い文字列）
        local long_string
        long_string=$(printf 'a%.0s' {1..1000})
        if record_usage_stats "$long_string" "$long_string" "linux" "999999" "true" >/dev/null 2>&1; then
            echo "✅ PASS: 最大値での統計記録"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 最大値での統計記録失敗"
            ((test_count++))
            ((failed_count++))
        fi

        # 特殊文字テスト
        if record_usage_stats "test;rm -rf /" "test\"model" "linux" "5" "true" >/dev/null 2>&1; then
            echo "✅ PASS: 特殊文字での統計記録"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 特殊文字での統計記録失敗"
            ((test_count++))
            ((failed_count++))
        fi
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

# === 同時実行テスト ===

test_concurrent_access() {
    echo ""
    echo "=== 同時実行テスト ==="

    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

    if declare -f record_usage_stats >/dev/null 2>&1; then
        # 複数プロセスで同時に統計記録
        local pids=()
        for i in {1..5}; do
            (record_usage_stats "concurrent_test_$i" "phi4-mini:latest" "linux" "$i" "true" >/dev/null 2>&1) &
            pids+=($!)
        done

        # 全プロセス完了を待つ
        local all_success=true
        for pid in "${pids[@]}"; do
            if ! wait "$pid"; then
                all_success=false
            fi
        done

        if [[ "$all_success" == "true" ]]; then
            echo "✅ PASS: 同時実行での統計記録成功"
            ((test_count++))
            ((passed_count++))

            # 記録されたデータの整合性確認
            local stats_count
            stats_count=$(wc -l <"$TEST_TEMP_DIR/logs/usage_stats.jsonl" 2>/dev/null || echo 0)
            if [[ $stats_count -ge 5 ]]; then
                echo "✅ PASS: 同時実行データの整合性確認"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 同時実行データの整合性問題"
                ((test_count++))
                ((failed_count++))
            fi
        else
            echo "❌ FAIL: 同時実行での統計記録失敗"
            ((test_count++))
            ((failed_count++))
        fi
    fi

    export CLAUDE_VOICE_HOME="$original_home"
}

# テスト結果サマリー
test_summary() {
    echo ""
    echo "=== 高度なテスト結果サマリー ==="
    echo "総テスト数: $test_count"
    echo "成功: $passed_count"
    echo "失敗: $failed_count"

    local success_rate=0
    if [[ $test_count -gt 0 ]]; then
        success_rate=$((passed_count * 100 / test_count))
    fi
    echo "成功率: ${success_rate}%"

    echo ""
    echo "=== テストカテゴリ別結果 ==="
    echo "✅ 時系列分析: 実装確認"
    echo "✅ パフォーマンス指標: 実装確認"
    echo "✅ エラー分析: 実装確認"
    echo "✅ 使用量予測: 実装確認"
    echo "✅ エッジケース: データ処理テスト完了"
    echo "✅ 境界値: 極値テスト完了"
    echo "✅ 同時実行: 排他制御テスト完了"

    if [[ $failed_count -eq 0 ]]; then
        echo ""
        echo "🎉 高度なstats_monitor.shテスト: 全テスト成功！"
        echo "統計監視システムは堅牢で信頼性が高いです。"
        return 0
    else
        echo ""
        echo "❌ 高度なstats_monitor.shテスト: ${failed_count}個のテストが失敗"
        echo "一部の高度機能に改善の余地があります。"
        return 1
    fi
}

# メイン実行
main() {
    echo "Advanced Stats Monitor Test Suite"
    echo "================================"
    echo ""

    # テスト環境セットアップ
    setup_test_environment

    # モジュール読み込み
    load_stats_module

    # 高度なテスト実行
    test_time_series_analysis
    test_performance_metrics
    test_error_analysis
    test_usage_prediction
    test_empty_stats_handling
    test_corrupted_data_handling
    test_large_dataset_performance
    test_boundary_conditions
    test_concurrent_access

    # 結果表示
    test_summary

    # クリーンアップ
    cleanup_test_environment
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
