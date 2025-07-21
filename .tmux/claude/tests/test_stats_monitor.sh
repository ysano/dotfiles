#!/bin/bash
# Unit Test for stats_monitor.sh
# 統計監視機能のテスト

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
TEST_TEMP_DIR="/tmp/test_stats_monitor_$$"
TEST_STATS_FILE="$TEST_TEMP_DIR/usage_stats.jsonl"

# セットアップ
setup_test_environment() {
    mkdir -p "$TEST_TEMP_DIR"
    export CLAUDE_VOICE_TEST_MODE=true

    # テスト用統計データの作成
    cat >"$TEST_STATS_FILE" <<'EOF'
{"timestamp":1640995200,"operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":3,"success":"true","version":"2.0.0"}
{"timestamp":1640995800,"operation":"claude_voice_main","summary_type":"detailed","model":"auto","os_type":"linux","duration":5,"success":"true","version":"2.0.0"}
{"timestamp":1640996400,"operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":2,"success":"false","version":"2.0.0"}
EOF
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

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local description="$3"

    ((test_count++))

    if [[ "$haystack" == *"$needle"* ]]; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   文字列 '$needle' が見つかりませんでした"
        ((failed_count++))
        return 1
    fi
}

assert_function_exists() {
    local function_name="$1"
    local description="$2"

    ((test_count++))

    if declare -f "$function_name" >/dev/null 2>&1; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   関数 '$function_name' が定義されていません"
        ((failed_count++))
        return 1
    fi
}

assert_numeric() {
    local value="$1"
    local description="$2"

    ((test_count++))

    if [[ "$value" =~ ^[0-9]+$ ]]; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   '$value' は数値ではありません"
        ((failed_count++))
        return 1
    fi
}

# モジュール読み込みテスト
test_module_loading() {
    echo "=== モジュール読み込みテスト ==="

    if [[ ! -f "$MODULE_PATH" ]]; then
        echo "❌ FAIL: モジュールファイルが存在しません: $MODULE_PATH"
        ((test_count++))
        ((failed_count++))
        return 1
    fi

    # 構文チェック
    if bash -n "$MODULE_PATH" 2>/dev/null; then
        echo "✅ PASS: 構文チェック"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: 構文エラーがあります"
        ((test_count++))
        ((failed_count++))
        return 1
    fi

    # モジュール読み込み
    if source "$MODULE_PATH" 2>/dev/null; then
        echo "✅ PASS: モジュール読み込み"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: モジュール読み込みエラー"
        ((test_count++))
        ((failed_count++))
        return 1
    fi
}

# 関数存在チェックテスト
test_function_existence() {
    echo ""
    echo "=== 関数存在チェックテスト ==="

    local required_functions=(
        "record_usage_stats"
        "show_stats"
        "export_stats"
        "analyze_usage_patterns"
        "calculate_stats_summary"
        "format_stats_output"
    )

    for func in "${required_functions[@]}"; do
        assert_function_exists "$func" "必須関数: $func"
    done
}

# 統計記録機能テスト
test_record_usage_stats() {
    echo ""
    echo "=== 統計記録機能テスト ==="

    if declare -f record_usage_stats >/dev/null 2>&1; then
        # テスト用ファイルの準備
        local test_stats_file="$TEST_TEMP_DIR/test_record.jsonl"

        # 元の環境変数を保存
        local original_home="$CLAUDE_VOICE_HOME"

        # テスト環境設定
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"

        # 統計記録テスト
        if record_usage_stats "brief" "phi4-mini:latest" "linux" "3" "true" >/dev/null 2>&1; then
            echo "✅ PASS: 統計記録実行"
            ((test_count++))
            ((passed_count++))

            # 記録されたファイルの確認
            local stats_file="$TEST_TEMP_DIR/logs/usage_stats.jsonl"
            if [[ -f "$stats_file" ]]; then
                echo "✅ PASS: 統計ファイル作成"
                ((test_count++))
                ((passed_count++))

                # JSON形式の確認
                local last_entry
                last_entry=$(tail -1 "$stats_file")
                if echo "$last_entry" | grep -q '"summary_type":"brief"'; then
                    echo "✅ PASS: 統計データ形式"
                    ((test_count++))
                    ((passed_count++))
                else
                    echo "❌ FAIL: 統計データ形式が不正"
                    ((test_count++))
                    ((failed_count++))
                fi
            else
                echo "❌ FAIL: 統計ファイルが作成されませんでした"
                ((test_count++))
                ((failed_count++))
            fi
        else
            echo "❌ FAIL: 統計記録実行エラー"
            ((test_count++))
            ((failed_count++))
        fi

        # 環境変数復元
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: record_usage_stats関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 統計表示機能テスト
test_show_stats() {
    echo ""
    echo "=== 統計表示機能テスト ==="

    if declare -f show_stats >/dev/null 2>&1; then
        # 元の環境変数を保存
        local original_home="$CLAUDE_VOICE_HOME"

        # テスト環境設定
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"
        cp "$TEST_STATS_FILE" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

        # 統計表示テスト
        local stats_output
        stats_output=$(show_stats summary 2>&1)

        if [[ -n "$stats_output" ]]; then
            assert_contains "$stats_output" "統計" "統計表示に統計情報が含まれる"
            assert_contains "$stats_output" "成功" "統計表示に成功率が含まれる"
            assert_contains "$stats_output" "使用回数" "統計表示に使用回数が含まれる"
        else
            echo "❌ FAIL: 統計表示で出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        # 環境変数復元
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: show_stats関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 統計エクスポート機能テスト
test_export_stats() {
    echo ""
    echo "=== 統計エクスポート機能テスト ==="

    if declare -f export_stats >/dev/null 2>&1; then
        # 元の環境変数を保存
        local original_home="$CLAUDE_VOICE_HOME"

        # テスト環境設定
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"
        cp "$TEST_STATS_FILE" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

        # エクスポートテスト
        local export_file="$TEST_TEMP_DIR/exported_stats.csv"

        if export_stats csv "$export_file" >/dev/null 2>&1; then
            echo "✅ PASS: 統計エクスポート実行"
            ((test_count++))
            ((passed_count++))

            # エクスポートファイルの確認
            if [[ -f "$export_file" ]]; then
                echo "✅ PASS: エクスポートファイル作成"
                ((test_count++))
                ((passed_count++))

                # CSV形式の確認
                local first_line
                first_line=$(head -1 "$export_file")
                if [[ "$first_line" == *"timestamp"* ]]; then
                    echo "✅ PASS: CSVヘッダー形式"
                    ((test_count++))
                    ((passed_count++))
                else
                    echo "❌ FAIL: CSVヘッダー形式が不正"
                    ((test_count++))
                    ((failed_count++))
                fi
            else
                echo "❌ FAIL: エクスポートファイルが作成されませんでした"
                ((test_count++))
                ((failed_count++))
            fi
        else
            echo "❌ FAIL: 統計エクスポート実行エラー"
            ((test_count++))
            ((failed_count++))
        fi

        # 環境変数復元
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: export_stats関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 使用パターン分析テスト
test_analyze_usage_patterns() {
    echo ""
    echo "=== 使用パターン分析テスト ==="

    if declare -f analyze_usage_patterns >/dev/null 2>&1; then
        # 元の環境変数を保存
        local original_home="$CLAUDE_VOICE_HOME"

        # テスト環境設定
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"
        cp "$TEST_STATS_FILE" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

        # パターン分析テスト
        local analysis_output
        analysis_output=$(analyze_usage_patterns 2>&1)

        if [[ -n "$analysis_output" ]]; then
            assert_contains "$analysis_output" "パターン" "パターン分析結果が含まれる"
        else
            echo "⚠️  WARN: パターン分析で出力がありません"
        fi

        # 環境変数復元
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: analyze_usage_patterns関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 統計計算機能テスト
test_calculate_stats_summary() {
    echo ""
    echo "=== 統計計算機能テスト ==="

    if declare -f calculate_stats_summary >/dev/null 2>&1; then
        # 元の環境変数を保存
        local original_home="$CLAUDE_VOICE_HOME"

        # テスト環境設定
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"
        cp "$TEST_STATS_FILE" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

        # 統計計算テスト
        local summary_output
        summary_output=$(calculate_stats_summary 2>&1)

        if [[ -n "$summary_output" ]]; then
            # 数値を含むかチェック
            if echo "$summary_output" | grep -q '[0-9]'; then
                echo "✅ PASS: 統計計算結果に数値が含まれる"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 統計計算結果に数値が含まれない"
                ((test_count++))
                ((failed_count++))
            fi
        else
            echo "⚠️  WARN: 統計計算で出力がありません"
        fi

        # 環境変数復元
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: calculate_stats_summary関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# エラーハンドリングテスト
test_error_handling() {
    echo ""
    echo "=== エラーハンドリングテスト ==="

    # 存在しないファイルでの統計表示テスト
    if declare -f show_stats >/dev/null 2>&1; then
        local original_home="$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="/tmp/nonexistent_claude_voice_$$"

        local error_output
        error_output=$(show_stats summary 2>&1 || true)

        # エラーメッセージまたは適切な処理が行われることを確認
        if [[ -n "$error_output" ]]; then
            echo "✅ PASS: 存在しないファイルでの適切なエラーハンドリング"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 存在しないファイルでのエラーハンドリングなし"
            ((test_count++))
            ((failed_count++))
        fi

        export CLAUDE_VOICE_HOME="$original_home"
    fi
}

# パフォーマンステスト
test_performance() {
    echo ""
    echo "=== パフォーマンステスト ==="

    if declare -f show_stats >/dev/null 2>&1; then
        local original_home="$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
        mkdir -p "$TEST_TEMP_DIR/logs"
        cp "$TEST_STATS_FILE" "$TEST_TEMP_DIR/logs/usage_stats.jsonl"

        # 実行時間測定
        local start_time=$(date +%s%3N)
        show_stats summary >/dev/null 2>&1 || true
        local end_time=$(date +%s%3N)
        local duration=$((end_time - start_time))

        # 5秒以内で実行されることを期待
        if [[ $duration -lt 5000 ]]; then
            echo "✅ PASS: show_stats実行時間: ${duration}ms (< 5000ms)"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: show_stats実行時間: ${duration}ms (>= 5000ms)"
            ((test_count++))
            ((failed_count++))
        fi

        export CLAUDE_VOICE_HOME="$original_home"
    fi
}

# テスト結果サマリー
test_summary() {
    echo ""
    echo "=== テスト結果サマリー ==="
    echo "総テスト数: $test_count"
    echo "成功: $passed_count"
    echo "失敗: $failed_count"

    local success_rate=0
    if [[ $test_count -gt 0 ]]; then
        success_rate=$((passed_count * 100 / test_count))
    fi
    echo "成功率: ${success_rate}%"

    if [[ $failed_count -eq 0 ]]; then
        echo "🎉 stats_monitor.sh: 全テスト成功！"
        return 0
    else
        echo "❌ stats_monitor.sh: ${failed_count}個のテストが失敗"
        return 1
    fi
}

# メイン実行
main() {
    echo "stats_monitor.sh Unit Test"
    echo "========================="

    # テスト環境セットアップ
    setup_test_environment

    # モジュール読み込み
    test_module_loading

    if [[ $failed_count -eq 0 ]]; then
        # 機能テスト実行
        test_function_existence
        test_record_usage_stats
        test_show_stats
        test_export_stats
        test_analyze_usage_patterns
        test_calculate_stats_summary
        test_error_handling
        test_performance
    else
        echo "モジュール読み込みに失敗したため、以降のテストをスキップします"
    fi

    # 結果表示
    test_summary

    # クリーンアップ
    cleanup_test_environment
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
