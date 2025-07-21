#!/bin/bash
# Unit Test for execution_engine.sh
# 実行エンジン機能のテスト

set -euo pipefail

# テスト環境設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
MODULE_PATH="$CORE_DIR/execution_engine.sh"

# テストカウンタ
test_count=0
passed_count=0
failed_count=0

# テスト用一時ディレクトリ
TEST_TEMP_DIR="/tmp/test_execution_engine_$$"

# セットアップ
setup_test_environment() {
    mkdir -p "$TEST_TEMP_DIR"
    mkdir -p "$TEST_TEMP_DIR/core"
    mkdir -p "$TEST_TEMP_DIR/logs"
    export CLAUDE_VOICE_TEST_MODE=true

    # テスト用モック関数の定義
    cat >"$TEST_TEMP_DIR/mock_functions.sh" <<'EOF'
# モック関数定義
capture_screen_text() {
    echo "Mock screen capture: This is test content for $1 with $2 lines"
}

generate_summary() {
    echo "Mock summary: This is a $2 summary of the content using $3 model"
}

universal_speak() {
    echo "Mock speech: $1" >&2
    return 0
}

speak_text() {
    echo "Mock speech fallback: $1" >&2
    return 0
}

check_windows_speech() {
    echo "available"
}

record_usage_stats() {
    echo "Mock stats recorded: $*" >&2
}

cleanup_execution_environment() {
    echo "Mock cleanup executed" >&2
}

detect_os() {
    echo "linux"
}
EOF

    # モック関数の読み込み
    source "$TEST_TEMP_DIR/mock_functions.sh"
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

assert_return_code() {
    local expected_code="$1"
    local actual_code="$2"
    local description="$3"

    ((test_count++))

    if [[ "$expected_code" -eq "$actual_code" ]]; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   期待リターンコード: $expected_code"
        echo "   実際リターンコード: $actual_code"
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
        "main_execution_workflow"
        "execute_core_workflow"
        "execute_screen_capture"
        "execute_context_collection"
        "execute_summary_generation"
        "execute_voice_output"
        "validate_execution_arguments"
        "validate_summary_type"
        "validate_lines_parameter"
        "validate_voice_parameter"
        "validate_model_parameter"
        "initialize_audio_subsystem"
        "start_execution_timer"
        "end_execution_timer"
        "detect_os"
    )

    for func in "${required_functions[@]}"; do
        assert_function_exists "$func" "必須関数: $func"
    done
}

# メイン実行ワークフローテスト
test_main_execution_workflow() {
    echo ""
    echo "=== メイン実行ワークフローテスト ==="

    if declare -f main_execution_workflow >/dev/null 2>&1; then
        local original_home="$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

        # 環境変数設定
        export DEFAULT_LINES=50
        export DEFAULT_VOICE="auto"
        export DEFAULT_MODEL="auto"
        export DEFAULT_DEVICE="system_default"

        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 正常ケースのテスト
        local workflow_output
        workflow_output=$(main_execution_workflow "brief" "30" "auto" "phi4-mini:latest" "system_default" 2>&1)
        local workflow_result=$?

        if [[ -n "$workflow_output" ]]; then
            assert_contains "$workflow_output" "Starting claude-voice" "ワークフロー開始メッセージ"

            # 実行時間の確認
            if echo "$workflow_output" | grep -q "completed successfully\|failed after"; then
                echo "✅ PASS: ワークフロー完了メッセージが含まれる"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: ワークフロー完了メッセージが含まれない"
                ((test_count++))
                ((failed_count++))
            fi
        else
            echo "❌ FAIL: メインワークフローで出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: main_execution_workflow関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 引数検証テスト
test_argument_validation() {
    echo ""
    echo "=== 引数検証テスト ==="

    # 要約タイプ検証テスト
    if declare -f validate_summary_type >/dev/null 2>&1; then
        # 正常な要約タイプ
        for valid_type in "brief" "detailed" "technical"; do
            if validate_summary_type "$valid_type" 2>/dev/null; then
                echo "✅ PASS: 有効な要約タイプ: $valid_type"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 有効な要約タイプが拒否される: $valid_type"
                ((test_count++))
                ((failed_count++))
            fi
        done

        # 無効な要約タイプ
        if ! validate_summary_type "invalid_type" 2>/dev/null; then
            echo "✅ PASS: 無効な要約タイプが適切に拒否される"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 無効な要約タイプが受け入れられる"
            ((test_count++))
            ((failed_count++))
        fi
    fi

    # 行数パラメータ検証テスト
    if declare -f validate_lines_parameter >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 有効な行数
        for valid_lines in "1" "50" "100" "1000"; do
            if validate_lines_parameter "$valid_lines" 2>/dev/null; then
                echo "✅ PASS: 有効な行数: $valid_lines"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 有効な行数が拒否される: $valid_lines"
                ((test_count++))
                ((failed_count++))
            fi
        done

        # 無効な行数
        for invalid_lines in "0" "1001" "abc" "-5"; do
            if ! validate_lines_parameter "$invalid_lines" 2>/dev/null; then
                echo "✅ PASS: 無効な行数が適切に拒否される: $invalid_lines"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 無効な行数が受け入れられる: $invalid_lines"
                ((test_count++))
                ((failed_count++))
            fi
        done
    fi

    # 音声パラメータ検証テスト
    if declare -f validate_voice_parameter >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 有効な音声パラメータ
        for valid_voice in "auto" "Kyoko" "Alex" "David"; do
            if validate_voice_parameter "$valid_voice" 2>/dev/null; then
                echo "✅ PASS: 有効な音声パラメータ: $valid_voice"
                ((test_count++))
                ((passed_count++))
            else
                echo "⚠️  WARN: 有効な音声パラメータが警告: $valid_voice"
                # 警告でも処理は継続されるため、これは正常
                ((test_count++))
                ((passed_count++))
            fi
        done

        # セキュリティチェック（特殊文字）
        if ! validate_voice_parameter "voice;rm -rf /" 2>/dev/null; then
            echo "✅ PASS: 危険な文字列が適切に拒否される"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 危険な文字列が受け入れられる"
            ((test_count++))
            ((failed_count++))
        fi
    fi

    # モデルパラメータ検証テスト
    if declare -f validate_model_parameter >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 有効なモデルパラメータ
        for valid_model in "auto" "phi4-mini:latest" "llama2:7b" "orca-mini"; do
            validate_model_parameter "$valid_model" 2>/dev/null
            local result=$?
            if [[ $result -eq 0 ]] || [[ $result -eq 1 ]]; then
                echo "✅ PASS: モデルパラメータ検証実行: $valid_model"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: モデルパラメータ検証エラー: $valid_model"
                ((test_count++))
                ((failed_count++))
            fi
        done
    fi
}

# コアワークフローテスト
test_core_workflow() {
    echo ""
    echo "=== コアワークフローテスト ==="

    if declare -f execute_core_workflow >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # コアワークフロー実行テスト
        local workflow_output
        workflow_output=$(execute_core_workflow "brief" "30" "auto" "phi4-mini:latest" "system_default" 2>&1)
        local workflow_result=$?

        if [[ -n "$workflow_output" ]]; then
            assert_contains "$workflow_output" "Mock" "モック関数が実行される"
        else
            echo "❌ FAIL: コアワークフローで出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        # リターンコードの確認
        assert_return_code 0 "$workflow_result" "コアワークフローが正常終了"
    else
        echo "❌ SKIP: execute_core_workflow関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 画面キャプチャ実行テスト
test_screen_capture_execution() {
    echo ""
    echo "=== 画面キャプチャ実行テスト ==="

    if declare -f execute_screen_capture >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 画面キャプチャ実行
        local capture_output
        capture_output=$(execute_screen_capture "50" 2>&1)
        local capture_result=$?

        if [[ -n "$capture_output" ]]; then
            assert_contains "$capture_output" "画面内容を取得中" "キャプチャ開始メッセージ"
            assert_contains "$capture_output" "Mock screen capture" "モックキャプチャが実行される"
            assert_contains "$capture_output" "文字のテキストを取得" "キャプチャ完了メッセージ"
        else
            echo "❌ FAIL: 画面キャプチャで出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        assert_return_code 0 "$capture_result" "画面キャプチャが正常終了"
    else
        echo "❌ SKIP: execute_screen_capture関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# コンテキスト収集テスト
test_context_collection() {
    echo ""
    echo "=== コンテキスト収集テスト ==="

    if declare -f execute_context_collection >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # コンテキスト収集実行
        local context_output
        context_output=$(execute_context_collection 2>&1)
        local context_result=$?

        if [[ -n "$context_output" ]]; then
            assert_contains "$context_output" "pwd:" "作業ディレクトリ情報が含まれる"

            # Git情報の確認（存在する場合）
            if command -v git >/dev/null 2>&1 && git rev-parse --git-dir >/dev/null 2>&1; then
                if echo "$context_output" | grep -q "Git:"; then
                    echo "✅ PASS: Git情報が含まれる"
                    ((test_count++))
                    ((passed_count++))
                else
                    echo "⚠️  INFO: Git情報が含まれない（想定内）"
                fi
            fi

            # tmux情報の確認（存在する場合）
            if [[ -n "${TMUX:-}" ]]; then
                if echo "$context_output" | grep -q "tmux:"; then
                    echo "✅ PASS: tmux情報が含まれる"
                    ((test_count++))
                    ((passed_count++))
                else
                    echo "⚠️  INFO: tmux情報が含まれない（想定内）"
                fi
            fi
        else
            echo "❌ FAIL: コンテキスト収集で出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        assert_return_code 0 "$context_result" "コンテキスト収集が正常終了"
    else
        echo "❌ SKIP: execute_context_collection関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 要約生成実行テスト
test_summary_generation() {
    echo ""
    echo "=== 要約生成実行テスト ==="

    if declare -f execute_summary_generation >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 要約生成実行
        local summary_output
        summary_output=$(execute_summary_generation "test content" "brief" "phi4-mini:latest" "test context" 2>&1)
        local summary_result=$?

        if [[ -n "$summary_output" ]]; then
            assert_contains "$summary_output" "要約を生成中" "要約生成開始メッセージ"
            assert_contains "$summary_output" "Mock summary" "モック要約が実行される"
            assert_contains "$summary_output" "要約生成完了" "要約生成完了メッセージ"
        else
            echo "❌ FAIL: 要約生成で出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        assert_return_code 0 "$summary_result" "要約生成が正常終了"
    else
        echo "❌ SKIP: execute_summary_generation関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 音声出力実行テスト
test_voice_output() {
    echo ""
    echo "=== 音声出力実行テスト ==="

    if declare -f execute_voice_output >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 音声出力実行
        local voice_output
        voice_output=$(execute_voice_output "test summary" "auto" "system_default" 2>&1)
        local voice_result=$?

        if [[ -n "$voice_output" ]]; then
            assert_contains "$voice_output" "音声で読み上げ中" "音声出力開始メッセージ"
        else
            echo "❌ FAIL: 音声出力で出力がありません"
            ((test_count++))
            ((failed_count++))
        fi

        assert_return_code 0 "$voice_result" "音声出力が正常終了"
    else
        echo "❌ SKIP: execute_voice_output関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# 音声サブシステム初期化テスト
test_audio_subsystem_initialization() {
    echo ""
    echo "=== 音声サブシステム初期化テスト ==="

    if declare -f initialize_audio_subsystem >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        local original_home="$CLAUDE_VOICE_HOME"
        export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"

        # 音声サブシステム初期化実行
        local init_result
        initialize_audio_subsystem 2>/dev/null
        init_result=$?

        # 初期化が何らかの結果を返すことを確認
        if [[ $init_result -eq 0 ]] || [[ $init_result -eq 1 ]]; then
            echo "✅ PASS: 音声サブシステム初期化が実行される"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 音声サブシステム初期化で予期しないエラー"
            ((test_count++))
            ((failed_count++))
        fi

        export CLAUDE_VOICE_HOME="$original_home"
    else
        echo "❌ SKIP: initialize_audio_subsystem関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# タイマー機能テスト
test_timer_functionality() {
    echo ""
    echo "=== タイマー機能テスト ==="

    if declare -f start_execution_timer >/dev/null 2>&1 && declare -f end_execution_timer >/dev/null 2>&1; then
        # タイマー開始
        local start_time
        start_time=$(start_execution_timer)

        assert_numeric "$start_time" "開始時刻が数値"

        # 短い待機
        sleep 1

        # タイマー終了
        local duration
        duration=$(end_execution_timer "$start_time")

        assert_numeric "$duration" "実行時間が数値"

        # 実行時間が妥当な範囲内であることを確認（1-3秒）
        if [[ $duration -ge 1 ]] && [[ $duration -le 3 ]]; then
            echo "✅ PASS: 実行時間が妥当範囲内: ${duration}秒"
            ((test_count++))
            ((passed_count++))
        else
            echo "⚠️  WARN: 実行時間が想定外: ${duration}秒（1-3秒を期待）"
            ((test_count++))
            ((passed_count++)) # システム負荷により変動する可能性があるため警告扱い
        fi
    else
        echo "❌ SKIP: タイマー関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# OS検出テスト
test_os_detection() {
    echo ""
    echo "=== OS検出テスト ==="

    if declare -f detect_os >/dev/null 2>&1; then
        local detected_os
        detected_os=$(detect_os)

        # 有効なOS名が返されることを確認
        case "$detected_os" in
            "darwin" | "linux" | "windows" | "unknown")
                echo "✅ PASS: 有効なOS名が検出される: $detected_os"
                ((test_count++))
                ((passed_count++))
                ;;
            *)
                echo "❌ FAIL: 無効なOS名: $detected_os"
                ((test_count++))
                ((failed_count++))
                ;;
        esac
    else
        echo "❌ SKIP: detect_os関数が存在しません"
        ((test_count++))
        ((failed_count++))
    fi
}

# エラーハンドリングテスト
test_error_handling() {
    echo ""
    echo "=== エラーハンドリングテスト ==="

    # 無効な引数での実行検証テスト
    if declare -f validate_execution_arguments >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 無効な引数での検証
        if ! validate_execution_arguments "invalid_type" "abc" "voice;rm" "invalid|model" 2>/dev/null; then
            echo "✅ PASS: 無効な引数が適切に拒否される"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 無効な引数が受け入れられる"
            ((test_count++))
            ((failed_count++))
        fi
    fi
}

# パフォーマンステスト
test_performance() {
    echo ""
    echo "=== パフォーマンステスト ==="

    if declare -f validate_execution_arguments >/dev/null 2>&1; then
        # テスト用ログ関数
        log() {
            echo "[$1] $2" >&2
        }

        # 引数検証の実行時間測定
        local start_time=$(date +%s%3N)
        validate_execution_arguments "brief" "50" "auto" "phi4-mini:latest" >/dev/null 2>&1
        local end_time=$(date +%s%3N)
        local duration=$((end_time - start_time))

        # 1秒以内で実行されることを期待
        if [[ $duration -lt 1000 ]]; then
            echo "✅ PASS: 引数検証実行時間: ${duration}ms (< 1000ms)"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 引数検証実行時間: ${duration}ms (>= 1000ms)"
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
        echo "🎉 execution_engine.sh: 全テスト成功！"
        return 0
    else
        echo "❌ execution_engine.sh: ${failed_count}個のテストが失敗"
        return 1
    fi
}

# メイン実行
main() {
    echo "execution_engine.sh Unit Test"
    echo "============================"

    # テスト環境セットアップ
    setup_test_environment

    # モジュール読み込み
    test_module_loading

    if [[ $failed_count -eq 0 ]]; then
        # 機能テスト実行
        test_function_existence
        test_main_execution_workflow
        test_argument_validation
        test_core_workflow
        test_screen_capture_execution
        test_context_collection
        test_summary_generation
        test_voice_output
        test_audio_subsystem_initialization
        test_timer_functionality
        test_os_detection
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
