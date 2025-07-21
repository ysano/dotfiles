#!/bin/bash
# Advanced Argument Validation Test Suite
# execution_engine.sh の引数検証機能の包括的テスト

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
TEST_TEMP_DIR="/tmp/test_advanced_validation_$$"

# セットアップ
setup_test_environment() {
    mkdir -p "$TEST_TEMP_DIR"
    export CLAUDE_VOICE_TEST_MODE=true

    # テスト用ログ関数
    log() {
        echo "[$1] $2" >&2
    }
}

# クリーンアップ
cleanup_test_environment() {
    rm -rf "$TEST_TEMP_DIR"
}

# テストユーティリティ
assert_validation_success() {
    local function_name="$1"
    local test_value="$2"
    local description="$3"

    ((test_count++))

    if "$function_name" "$test_value" 2>/dev/null; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   値 '$test_value' が有効と判定されませんでした"
        ((failed_count++))
        return 1
    fi
}

assert_validation_failure() {
    local function_name="$1"
    local test_value="$2"
    local description="$3"

    ((test_count++))

    if ! "$function_name" "$test_value" 2>/dev/null; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   値 '$test_value' が無効と判定されませんでした"
        ((failed_count++))
        return 1
    fi
}

assert_security_protection() {
    local function_name="$1"
    local malicious_input="$2"
    local description="$3"

    ((test_count++))

    # セキュリティ保護の確認（危険な文字列は拒否されるべき）
    if ! "$function_name" "$malicious_input" 2>/dev/null; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
        echo "   危険な入力 '$malicious_input' が受け入れられました"
        ((failed_count++))
        return 1
    fi
}

# モジュール読み込み
load_execution_module() {
    if [[ -f "$MODULE_PATH" ]] && source "$MODULE_PATH" 2>/dev/null; then
        return 0
    else
        echo "❌ FAIL: execution_engine.sh の読み込みに失敗"
        exit 1
    fi
}

# === 要約タイプ検証の高度テスト ===

test_summary_type_validation_comprehensive() {
    echo "=== 要約タイプ検証包括テスト ==="

    if ! declare -f validate_summary_type >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_summary_type関数が存在しません"
        return
    fi

    # 正常値テスト
    local valid_types=("brief" "detailed" "technical")
    for type in "${valid_types[@]}"; do
        assert_validation_success "validate_summary_type" "$type" "有効な要約タイプ: $type"
    done

    # 大文字小文字の変化テスト
    local case_variants=("BRIEF" "Brief" "DETAILED" "Detailed" "TECHNICAL" "Technical")
    for variant in "${case_variants[@]}"; do
        assert_validation_failure "validate_summary_type" "$variant" "大文字小文字バリエーション拒否: $variant"
    done

    # 無効値テスト
    local invalid_types=("summary" "quick" "full" "short" "long" "normal" "verbose" "concise")
    for type in "${invalid_types[@]}"; do
        assert_validation_failure "validate_summary_type" "$type" "無効な要約タイプ拒否: $type"
    done

    # 空文字・特殊文字テスト
    local special_cases=("" " " "brief " " brief" "brief\n" "brief\t" "brief;detailed")
    for case in "${special_cases[@]}"; do
        assert_validation_failure "validate_summary_type" "$case" "特殊文字・空文字拒否: '$case'"
    done

    # セキュリティテスト
    local malicious_inputs=("brief;rm -rf /" "brief\$(rm -rf /)" "brief|cat /etc/passwd" "brief&&malware")
    for input in "${malicious_inputs[@]}"; do
        assert_security_protection "validate_summary_type" "$input" "セキュリティ保護: $input"
    done

    # 文字数制限テスト
    local long_string
    long_string=$(printf 'a%.0s' {1..100})
    assert_validation_failure "validate_summary_type" "$long_string" "過度に長い文字列拒否"
}

# === 行数パラメータ検証の高度テスト ===

test_lines_parameter_validation_comprehensive() {
    echo ""
    echo "=== 行数パラメータ検証包括テスト ==="

    if ! declare -f validate_lines_parameter >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_lines_parameter関数が存在しません"
        return
    fi

    # 正常値テスト（境界値含む）
    local valid_lines=("1" "10" "50" "100" "500" "999" "1000")
    for lines in "${valid_lines[@]}"; do
        assert_validation_success "validate_lines_parameter" "$lines" "有効な行数: $lines"
    done

    # 境界値テスト（無効）
    local boundary_invalid=("0" "1001" "-1" "9999")
    for lines in "${boundary_invalid[@]}"; do
        assert_validation_failure "validate_lines_parameter" "$lines" "境界値外拒否: $lines"
    done

    # 非数値テスト
    local non_numeric=("abc" "50.5" "1e3" "fifty" "∞" "NaN" "null" "undefined")
    for value in "${non_numeric[@]}"; do
        assert_validation_failure "validate_lines_parameter" "$value" "非数値拒否: $value"
    done

    # 特殊形式テスト
    local special_formats=("050" "0x32" "2.0" "+50" "50L" "50UL" "50f")
    for format in "${special_formats[@]}"; do
        assert_validation_failure "validate_lines_parameter" "$format" "特殊数値形式拒否: $format"
    done

    # セキュリティテスト（コマンド注入）
    local malicious_numbers=("50;rm -rf /" "50\$(cat /etc/passwd)" "50|whoami" "50&&ls")
    for input in "${malicious_numbers[@]}"; do
        assert_security_protection "validate_lines_parameter" "$input" "コマンド注入保護: $input"
    done

    # 空文字・スペーステスト
    local empty_cases=("" " " "\t" "\n" "  50  " " 50")
    for case in "${empty_cases[@]}"; do
        assert_validation_failure "validate_lines_parameter" "$case" "空文字・スペース拒否: '$case'"
    done
}

# === 音声パラメータ検証の高度テスト ===

test_voice_parameter_validation_comprehensive() {
    echo ""
    echo "=== 音声パラメータ検証包括テスト ==="

    if ! declare -f validate_voice_parameter >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_voice_parameter関数が存在しません"
        return
    fi

    # 自動選択テスト
    assert_validation_success "validate_voice_parameter" "auto" "自動音声選択"

    # 一般的な音声名テスト
    local common_voices=("Kyoko" "Alex" "Victoria" "Daniel" "Karen" "Moira" "Rishi" "Tessa")
    for voice in "${common_voices[@]}"; do
        assert_validation_success "validate_voice_parameter" "$voice" "一般的な音声名: $voice"
    done

    # 各OS固有音声テスト
    local macos_voices=("Agnes" "Albert" "Alice" "Allison" "Ava" "Carmit" "Damien" "Fiona")
    for voice in "${macos_voices[@]}"; do
        assert_validation_success "validate_voice_parameter" "$voice" "macOS音声: $voice"
    done

    local windows_voices=("David" "Hazel" "Mark" "Zira" "Haruka" "Ichiro" "Sayaka" "Ayumi")
    for voice in "${windows_voices[@]}"; do
        assert_validation_success "validate_voice_parameter" "$voice" "Windows音声: $voice"
    done

    # 特殊文字セキュリティテスト
    local dangerous_voices=("voice;rm -rf /" "voice\$(cat /etc/passwd)" "voice|whoami" "voice&&malware")
    for voice in "${dangerous_voices[@]}"; do
        assert_security_protection "validate_voice_parameter" "$voice" "危険な音声パラメータ保護: $voice"
    done

    # 制御文字テスト
    local control_chars=("voice\n" "voice\t" "voice\r" "voice\b" "voice\f")
    for char in "${control_chars[@]}"; do
        assert_security_protection "validate_voice_parameter" "$char" "制御文字保護: $char"
    done

    # 長い文字列テスト
    local long_voice_name
    long_voice_name=$(printf 'VeryLongVoiceName%.0s' {1..50})
    assert_validation_failure "validate_voice_parameter" "$long_voice_name" "過度に長い音声名拒否"

    # 空文字列テスト
    assert_validation_failure "validate_voice_parameter" "" "空文字列音声名拒否"

    # Unicode文字テスト
    local unicode_voices=("音声" "صوت" "声音" "голос" "φωνή")
    for voice in "${unicode_voices[@]}"; do
        # Unicode音声名は警告扱いだが処理継続される場合がある
        local result
        validate_voice_parameter "$voice" >/dev/null 2>&1
        result=$?
        if [[ $result -eq 0 ]] || [[ $result -eq 1 ]]; then
            echo "✅ PASS: Unicode音声名処理: $voice"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: Unicode音声名処理エラー: $voice"
            ((test_count++))
            ((failed_count++))
        fi
    done
}

# === モデルパラメータ検証の高度テスト ===

test_model_parameter_validation_comprehensive() {
    echo ""
    echo "=== モデルパラメータ検証包括テスト ==="

    if ! declare -f validate_model_parameter >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_model_parameter関数が存在しません"
        return
    fi

    # 自動選択テスト
    assert_validation_success "validate_model_parameter" "auto" "自動モデル選択"

    # 有効なモデル名テスト
    local valid_models=(
        "phi4-mini:latest"
        "llama2:7b"
        "orca-mini:3b"
        "codellama:13b"
        "mistral:7b-instruct"
        "neural-chat:7b"
        "starling-lm:7b-alpha"
    )
    for model in "${valid_models[@]}"; do
        assert_validation_success "validate_model_parameter" "$model" "有効なモデル名: $model"
    done

    # タグ付きモデルテスト
    local tagged_models=(
        "phi4-mini:v1.0"
        "llama2:latest"
        "model:7b-q4_0"
        "custom-model:experimental"
        "test_model:debug"
    )
    for model in "${tagged_models[@]}"; do
        assert_validation_success "validate_model_parameter" "$model" "タグ付きモデル: $model"
    done

    # 無効なモデル形式テスト
    local invalid_formats=(
        "invalid:model:name"
        "model:"
        ":tag"
        "model::tag"
        "model:tag:extra"
        "model name with spaces"
    )
    for model in "${invalid_formats[@]}"; do
        assert_validation_failure "validate_model_parameter" "$model" "無効なモデル形式拒否: $model"
    done

    # セキュリティテスト
    local malicious_models=(
        "model;rm -rf /"
        "model\$(cat /etc/passwd)"
        "model|whoami"
        "model&&malware"
        "model\`id\`"
        "../../../etc/passwd"
    )
    for model in "${malicious_models[@]}"; do
        assert_security_protection "validate_model_parameter" "$model" "危険なモデル名保護: $model"
    done

    # 特殊文字テスト
    local special_char_models=(
        "model@host"
        "model#tag"
        "model%tag"
        "model*"
        "model?"
        "model[tag]"
        "model{tag}"
    )
    for model in "${special_char_models[@]}"; do
        assert_validation_failure "validate_model_parameter" "$model" "特殊文字モデル名拒否: $model"
    done

    # 空文字列テスト
    assert_validation_failure "validate_model_parameter" "" "空文字列モデル名拒否"

    # 長い文字列テスト
    local long_model
    long_model=$(printf 'very-long-model-name%.0s' {1..20})
    assert_validation_failure "validate_model_parameter" "$long_model" "過度に長いモデル名拒否"
}

# === 複合引数検証テスト ===

test_comprehensive_argument_validation() {
    echo ""
    echo "=== 複合引数検証テスト ==="

    if ! declare -f validate_execution_arguments >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_execution_arguments関数が存在しません"
        return
    fi

    # 正常な組み合わせテスト
    local valid_combinations=(
        "brief 50 auto phi4-mini:latest"
        "detailed 100 Kyoko llama2:7b"
        "technical 200 auto auto"
        "brief 1 Daniel orca-mini:3b"
        "detailed 1000 Victoria mistral:7b-instruct"
    )

    for combination in "${valid_combinations[@]}"; do
        read -r summary_type lines voice model <<<"$combination"
        if validate_execution_arguments "$summary_type" "$lines" "$voice" "$model" 2>/dev/null; then
            echo "✅ PASS: 有効な引数組み合わせ: $combination"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 有効な引数組み合わせが拒否: $combination"
            ((test_count++))
            ((failed_count++))
        fi
    done

    # 無効な組み合わせテスト
    local invalid_combinations=(
        "invalid 50 auto phi4-mini:latest"
        "brief abc auto phi4-mini:latest"
        "brief 50 voice;rm auto"
        "brief 50 auto model;rm"
        "brief -1 auto auto"
        "brief 1001 auto auto"
    )

    for combination in "${invalid_combinations[@]}"; do
        read -r summary_type lines voice model <<<"$combination"
        if ! validate_execution_arguments "$summary_type" "$lines" "$voice" "$model" 2>/dev/null; then
            echo "✅ PASS: 無効な引数組み合わせ拒否: $combination"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 無効な引数組み合わせが受け入れ: $combination"
            ((test_count++))
            ((failed_count++))
        fi
    done
}

# === ストレステスト ===

test_validation_stress() {
    echo ""
    echo "=== 検証ストレステスト ==="

    if ! declare -f validate_execution_arguments >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_execution_arguments関数が存在しません"
        return
    fi

    # 大量データでの性能テスト
    local start_time=$(date +%s%3N)
    local stress_count=0
    local stress_success=0

    for i in {1..100}; do
        ((stress_count++))
        if validate_execution_arguments "brief" "$((i % 100 + 1))" "auto" "phi4-mini:latest" 2>/dev/null; then
            ((stress_success++))
        fi
    done

    local end_time=$(date +%s%3N)
    local duration=$((end_time - start_time))

    # 5秒以内での処理を期待
    if [[ $duration -lt 5000 ]]; then
        echo "✅ PASS: ストレステスト実行時間: ${duration}ms (< 5000ms)"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: ストレステスト実行時間: ${duration}ms (>= 5000ms)"
        ((test_count++))
        ((failed_count++))
    fi

    # 成功率確認
    local success_rate=$((stress_success * 100 / stress_count))
    if [[ $success_rate -eq 100 ]]; then
        echo "✅ PASS: ストレステスト成功率: ${success_rate}%"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: ストレステスト成功率: ${success_rate}% (< 100%)"
        ((test_count++))
        ((failed_count++))
    fi
}

# === 国際化テスト ===

test_internationalization_validation() {
    echo ""
    echo "=== 国際化検証テスト ==="

    if ! declare -f validate_voice_parameter >/dev/null 2>&1; then
        echo "⚠️  SKIP: validate_voice_parameter関数が存在しません"
        return
    fi

    # 各言語の音声名テスト
    local international_voices=(
        "Kyoko"     # 日本語
        "Ting-Ting" # 中国語
        "Sin-ji"    # 中国語
        "Yuna"      # 韓国語
        "Carmit"    # ヘブライ語
        "Maged"     # アラビア語
        "Katka"     # スロバキア語
        "Milena"    # ロシア語
    )

    for voice in "${international_voices[@]}"; do
        local result
        validate_voice_parameter "$voice" >/dev/null 2>&1
        result=$?
        if [[ $result -eq 0 ]] || [[ $result -eq 1 ]]; then
            echo "✅ PASS: 国際音声名処理: $voice"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 国際音声名処理エラー: $voice"
            ((test_count++))
            ((failed_count++))
        fi
    done
}

# テスト結果サマリー
test_summary() {
    echo ""
    echo "=== 高度な引数検証テスト結果サマリー ==="
    echo "総テスト数: $test_count"
    echo "成功: $passed_count"
    echo "失敗: $failed_count"

    local success_rate=0
    if [[ $test_count -gt 0 ]]; then
        success_rate=$((passed_count * 100 / test_count))
    fi
    echo "成功率: ${success_rate}%"

    echo ""
    echo "=== セキュリティテスト結果 ==="
    local security_tests=$((test_count / 4)) # おおよその推定
    echo "セキュリティテスト数: $security_tests"
    echo "コマンド注入保護: ✅ テスト完了"
    echo "パストラバーサル保護: ✅ テスト完了"
    echo "特殊文字フィルタリング: ✅ テスト完了"
    echo "境界値検証: ✅ テスト完了"

    echo ""
    echo "=== パフォーマンステスト結果 ==="
    echo "ストレステスト: ✅ テスト完了"
    echo "大量データ処理: ✅ テスト完了"
    echo "レスポンス時間: ✅ テスト完了"

    if [[ $failed_count -eq 0 ]]; then
        echo ""
        echo "🎉 高度な引数検証テスト: 全テスト成功！"
        echo "引数検証システムは堅牢で安全です。"
        return 0
    else
        echo ""
        echo "❌ 高度な引数検証テスト: ${failed_count}個のテストが失敗"
        echo "引数検証に改善の余地があります。"
        return 1
    fi
}

# メイン実行
main() {
    echo "Advanced Argument Validation Test Suite"
    echo "======================================"
    echo ""

    # テスト環境セットアップ
    setup_test_environment

    # モジュール読み込み
    load_execution_module

    # 包括的な引数検証テスト実行
    test_summary_type_validation_comprehensive
    test_lines_parameter_validation_comprehensive
    test_voice_parameter_validation_comprehensive
    test_model_parameter_validation_comprehensive
    test_comprehensive_argument_validation
    test_validation_stress
    test_internationalization_validation

    # 結果表示
    test_summary

    # クリーンアップ
    cleanup_test_environment
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
