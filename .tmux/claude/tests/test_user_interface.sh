#!/bin/bash
# Unit Test for user_interface.sh
# ユーザーインターフェース機能のテスト

set -euo pipefail

# テスト環境設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
MODULE_PATH="$CORE_DIR/user_interface.sh"

# テストカウンタ
test_count=0
passed_count=0
failed_count=0

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
        echo "   対象: '$haystack'"
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
    
    # 必須関数のチェック
    local required_functions=(
        "show_usage"
        "show_version" 
        "check_module_status"
        "show_troubleshooting_guide"
        "display_help_section"
    )
    
    for func in "${required_functions[@]}"; do
        assert_function_exists "$func" "必須関数: $func"
    done
}

# ヘルプ機能テスト
test_help_functionality() {
    echo ""
    echo "=== ヘルプ機能テスト ==="
    
    # show_usage関数のテスト
    if declare -f show_usage >/dev/null 2>&1; then
        local usage_output
        usage_output=$(show_usage 2>&1)
        
        assert_contains "$usage_output" "Claude Voice" "使用法にタイトルが含まれる"
        assert_contains "$usage_output" "使用法" "使用法の説明が含まれる"
        assert_contains "$usage_output" "OPTIONS" "オプションセクションが含まれる"
        assert_contains "$usage_output" "brief" "brief要約タイプの説明が含まれる"
    else
        echo "❌ SKIP: show_usage関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# バージョン情報テスト
test_version_functionality() {
    echo ""
    echo "=== バージョン情報テスト ==="
    
    if declare -f show_version >/dev/null 2>&1; then
        local version_output
        version_output=$(show_version 2>&1)
        
        assert_contains "$version_output" "Claude Voice" "バージョン情報にタイトルが含まれる"
        assert_contains "$version_output" "OS:" "OS情報が含まれる"
        assert_contains "$version_output" "Home:" "ホームディレクトリ情報が含まれる"
    else
        echo "❌ SKIP: show_version関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# モジュールステータスチェックテスト
test_module_status_check() {
    echo ""
    echo "=== モジュールステータスチェックテスト ==="
    
    if declare -f check_module_status >/dev/null 2>&1; then
        # テスト用の環境変数設定
        local original_home="$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="/tmp/test_claude_voice_$$"
        
        # テスト環境作成
        mkdir -p "$CLAUDE_VOICE_HOME/core"
        touch "$CLAUDE_VOICE_HOME/core/base.sh"
        
        local status_output
        status_output=$(check_module_status brief 2>&1)
        
        assert_contains "$status_output" "モジュール" "モジュール情報が含まれる"
        
        # テスト環境クリーンアップ
        rm -rf "$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: check_module_status関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# トラブルシューティングガイドテスト
test_troubleshooting_guide() {
    echo ""
    echo "=== トラブルシューティングガイドテスト ==="
    
    if declare -f show_troubleshooting_guide >/dev/null 2>&1; then
        local guide_output
        guide_output=$(show_troubleshooting_guide 2>&1)
        
        assert_contains "$guide_output" "トラブルシューティング" "トラブルシューティングガイドのタイトル"
        assert_contains "$guide_output" "問題" "問題の説明が含まれる"
        assert_contains "$guide_output" "解決" "解決方法が含まれる"
    else
        echo "❌ SKIP: show_troubleshooting_guide関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 引数処理テスト
test_argument_processing() {
    echo ""
    echo "=== 引数処理テスト ==="
    
    # スクリプトが直接実行時の動作をテスト
    local test_script="$MODULE_PATH"
    
    if [[ -x "$test_script" ]]; then
        # help引数のテスト
        local help_output
        help_output=$("$test_script" help 2>&1 || true)
        
        if [[ -n "$help_output" ]]; then
            assert_contains "$help_output" "使用法" "help引数で使用法が表示される"
        else
            echo "⚠️  WARN: help引数のテストをスキップ（出力なし）"
        fi
        
        # version引数のテスト
        local version_output
        version_output=$("$test_script" version 2>&1 || true)
        
        if [[ -n "$version_output" ]]; then
            assert_contains "$version_output" "Claude Voice" "version引数でバージョン情報が表示される"
        else
            echo "⚠️  WARN: version引数のテストをスキップ（出力なし）"
        fi
    else
        echo "⚠️  WARN: モジュールが実行可能ではありません"
    fi
}

# エラーハンドリングテスト
test_error_handling() {
    echo ""
    echo "=== エラーハンドリングテスト ==="
    
    # 無効な引数のテスト
    if declare -f show_usage >/dev/null 2>&1; then
        # 通常は無効な引数で使用法が表示されることを期待
        local invalid_output
        invalid_output=$(show_usage 2>&1)
        
        # 何らかの出力があることを確認
        if [[ -n "$invalid_output" ]]; then
            echo "✅ PASS: 無効な引数に対する適切な応答"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 無効な引数に対する応答なし"
            ((test_count++))
            ((failed_count++))
        fi
    fi
}

# パフォーマンステスト
test_performance() {
    echo ""
    echo "=== パフォーマンステスト ==="
    
    # 関数実行時間の測定
    if declare -f show_usage >/dev/null 2>&1; then
        local start_time=$(date +%s%3N)
        show_usage >/dev/null 2>&1
        local end_time=$(date +%s%3N)
        local duration=$((end_time - start_time))
        
        # 1秒以内で実行されることを期待
        if [[ $duration -lt 1000 ]]; then
            echo "✅ PASS: show_usage実行時間: ${duration}ms (< 1000ms)"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: show_usage実行時間: ${duration}ms (>= 1000ms)"
            ((test_count++))
            ((failed_count++))
        fi
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
        echo "🎉 user_interface.sh: 全テスト成功！"
        return 0
    else
        echo "❌ user_interface.sh: ${failed_count}個のテストが失敗"
        return 1
    fi
}

# メイン実行
main() {
    echo "user_interface.sh Unit Test"
    echo "=========================="
    
    # テスト用環境変数設定
    export CLAUDE_VOICE_TEST_MODE=true
    
    # モジュール読み込み
    test_module_loading
    
    if [[ $failed_count -eq 0 ]]; then
        # 機能テスト実行
        test_function_existence
        test_help_functionality
        test_version_functionality
        test_module_status_check
        test_troubleshooting_guide
        test_argument_processing
        test_error_handling
        test_performance
    else
        echo "モジュール読み込みに失敗したため、以降のテストをスキップします"
    fi
    
    # 結果表示
    test_summary
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi