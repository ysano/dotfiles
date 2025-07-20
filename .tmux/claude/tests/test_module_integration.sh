#!/bin/bash
# Integration Test for Module Integration
# モジュール間連携テスト

set -euo pipefail

# テスト環境設定
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
TEST_DIR="$(dirname "${BASH_SOURCE[0]}")"

# テストカウンタ
test_count=0
passed_count=0
failed_count=0

# テスト用一時ディレクトリ
TEST_TEMP_DIR="/tmp/test_module_integration_$$"

# セットアップ
setup_test_environment() {
    mkdir -p "$TEST_TEMP_DIR"
    mkdir -p "$TEST_TEMP_DIR/core"
    mkdir -p "$TEST_TEMP_DIR/config"
    mkdir -p "$TEST_TEMP_DIR/logs"
    mkdir -p "$TEST_TEMP_DIR/bin"
    export CLAUDE_VOICE_TEST_MODE=true
    
    # テスト用設定ファイル
    cat > "$TEST_TEMP_DIR/config/claude-voice.conf" << 'EOF'
[llm]
default_model=phi4-mini:latest
timeout=30

[audio]
default_voice=auto
volume=80

[capture]
default_lines=50
EOF

    cat > "$TEST_TEMP_DIR/config/claude-voice.yaml" << 'EOF'
integration:
  enabled: true
llm:
  provider: ollama
voice:
  manual:
    mode: powershell
EOF

    # テスト用ログファイル
    touch "$TEST_TEMP_DIR/logs/claude-voice.log"
    touch "$TEST_TEMP_DIR/logs/usage_stats.jsonl"
    
    # テスト用統計データ
    cat > "$TEST_TEMP_DIR/logs/usage_stats.jsonl" << 'EOF'
{"timestamp":1640995200,"operation":"claude_voice_main","summary_type":"brief","model":"phi4-mini:latest","os_type":"linux","duration":3,"success":"true","version":"2.0.0"}
{"timestamp":1640995800,"operation":"claude_voice_main","summary_type":"detailed","model":"auto","os_type":"linux","duration":5,"success":"true","version":"2.0.0"}
EOF

    # テスト用バイナリ
    cat > "$TEST_TEMP_DIR/bin/claude-voice" << 'EOF'
#!/bin/bash
echo "Claude Voice Test Binary"
EOF
    chmod +x "$TEST_TEMP_DIR/bin/claude-voice"
    
    # テスト用コアファイル
    touch "$TEST_TEMP_DIR/core/base.sh"
    touch "$TEST_TEMP_DIR/core/universal_voice.sh"
    touch "$TEST_TEMP_DIR/core/voice_engine_registry.sh"
}

# クリーンアップ
cleanup_test_environment() {
    rm -rf "$TEST_TEMP_DIR"
}

# テストユーティリティ
assert_success() {
    local description="$1"
    local command="$2"
    
    ((test_count++))
    
    if eval "$command" >/dev/null 2>&1; then
        echo "✅ PASS: $description"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $description"
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

assert_module_loadable() {
    local module_name="$1"
    local module_path="$CORE_DIR/$module_name"
    
    ((test_count++))
    
    if [[ -f "$module_path" ]] && bash -n "$module_path" 2>/dev/null && source "$module_path" 2>/dev/null; then
        echo "✅ PASS: $module_name モジュールが読み込み可能"
        ((passed_count++))
        return 0
    else
        echo "❌ FAIL: $module_name モジュールの読み込みエラー"
        ((failed_count++))
        return 1
    fi
}

# モジュール存在チェック
test_module_existence() {
    echo "=== モジュール存在チェック ==="
    
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )
    
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]]; then
            echo "✅ PASS: $module が存在"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: $module が存在しません"
            ((test_count++))
            ((failed_count++))
        fi
    done
}

# モジュール読み込みテスト
test_module_loading() {
    echo ""
    echo "=== モジュール読み込みテスト ==="
    
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )
    
    for module in "${modules[@]}"; do
        assert_module_loadable "$module"
    done
}

# モジュール間関数呼び出しテスト
test_inter_module_function_calls() {
    echo ""
    echo "=== モジュール間関数呼び出しテスト ==="
    
    # 全モジュール読み込み
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )
    
    local loaded_modules=0
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]] && source "$CORE_DIR/$module" 2>/dev/null; then
            ((loaded_modules++))
        fi
    done
    
    if [[ $loaded_modules -eq ${#modules[@]} ]]; then
        echo "✅ PASS: 全モジュールが読み込まれました"
        ((test_count++))
        ((passed_count++))
        
        # 設定管理と統計の連携テスト
        if declare -f get_config_value >/dev/null 2>&1 && declare -f record_usage_stats >/dev/null 2>&1; then
            echo "✅ PASS: 設定管理と統計機能の関数が利用可能"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 設定管理または統計機能の関数が利用不可"
            ((test_count++))
            ((failed_count++))
        fi
        
        # ヘルスチェックと設定の連携テスト
        if declare -f run_health_check >/dev/null 2>&1 && declare -f validate_legacy_config >/dev/null 2>&1; then
            echo "✅ PASS: ヘルスチェックと設定検証の関数が利用可能"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: ヘルスチェックまたは設定検証の関数が利用不可"
            ((test_count++))
            ((failed_count++))
        fi
        
        # 実行エンジンと他モジュールの連携テスト
        if declare -f main_execution_workflow >/dev/null 2>&1 && declare -f detect_os >/dev/null 2>&1; then
            echo "✅ PASS: 実行エンジンとOS検出の関数が利用可能"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 実行エンジンまたはOS検出の関数が利用不可"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "❌ FAIL: 一部のモジュールが読み込めませんでした ($loaded_modules/${#modules[@]})"
        ((test_count++))
        ((failed_count++))
    fi
}

# 設定管理統合テスト
test_config_management_integration() {
    echo ""
    echo "=== 設定管理統合テスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # config_manager.sh読み込み
    if [[ -f "$CORE_DIR/config_manager.sh" ]] && source "$CORE_DIR/config_manager.sh" 2>/dev/null; then
        echo "✅ PASS: config_manager.sh読み込み成功"
        ((test_count++))
        ((passed_count++))
        
        # 設定表示テスト
        if declare -f manage_config >/dev/null 2>&1; then
            local config_output
            config_output=$(manage_config show legacy 2>&1)
            if [[ -n "$config_output" ]]; then
                assert_contains "$config_output" "claude-voice.conf" "設定ファイル表示に設定ファイル名が含まれる"
            else
                echo "❌ FAIL: 設定表示で出力なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
        
        # 設定値取得テスト
        if declare -f get_config_value >/dev/null 2>&1; then
            local config_value
            config_value=$(get_config_value "default_model" "fallback" 2>/dev/null)
            if [[ -n "$config_value" ]]; then
                echo "✅ PASS: 設定値取得が動作"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 設定値取得で結果なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
    else
        echo "❌ FAIL: config_manager.sh読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
}

# 統計管理統合テスト
test_stats_management_integration() {
    echo ""
    echo "=== 統計管理統合テスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # stats_monitor.sh読み込み
    if [[ -f "$CORE_DIR/stats_monitor.sh" ]] && source "$CORE_DIR/stats_monitor.sh" 2>/dev/null; then
        echo "✅ PASS: stats_monitor.sh読み込み成功"
        ((test_count++))
        ((passed_count++))
        
        # 統計表示テスト
        if declare -f show_stats >/dev/null 2>&1; then
            local stats_output
            stats_output=$(show_stats summary 2>&1)
            if [[ -n "$stats_output" ]]; then
                assert_contains "$stats_output" "統計" "統計表示に統計情報が含まれる"
            else
                echo "❌ FAIL: 統計表示で出力なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
        
        # 統計記録テスト
        if declare -f record_usage_stats >/dev/null 2>&1; then
            if record_usage_stats "brief" "test_model" "linux" "2" "true" >/dev/null 2>&1; then
                echo "✅ PASS: 統計記録が動作"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 統計記録で失敗"
                ((test_count++))
                ((failed_count++))
            fi
        fi
    else
        echo "❌ FAIL: stats_monitor.sh読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
}

# ヘルスチェック統合テスト
test_health_check_integration() {
    echo ""
    echo "=== ヘルスチェック統合テスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # health_diagnostics.sh読み込み
    if [[ -f "$CORE_DIR/health_diagnostics.sh" ]] && source "$CORE_DIR/health_diagnostics.sh" 2>/dev/null; then
        echo "✅ PASS: health_diagnostics.sh読み込み成功"
        ((test_count++))
        ((passed_count++))
        
        # ヘルスチェック実行テスト
        if declare -f run_health_check >/dev/null 2>&1; then
            local health_output
            health_output=$(run_health_check 2>&1)
            if [[ -n "$health_output" ]]; then
                assert_contains "$health_output" "Health Check" "ヘルスチェック出力にタイトルが含まれる"
                assert_contains "$health_output" "Health Score" "ヘルスチェック出力にスコアが含まれる"
            else
                echo "❌ FAIL: ヘルスチェックで出力なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
        
        # OS検出テスト
        if declare -f detect_os >/dev/null 2>&1; then
            local detected_os
            detected_os=$(detect_os 2>/dev/null)
            if [[ -n "$detected_os" ]]; then
                echo "✅ PASS: OS検出が動作: $detected_os"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: OS検出で結果なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
    else
        echo "❌ FAIL: health_diagnostics.sh読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
}

# 実行エンジン統合テスト
test_execution_engine_integration() {
    echo ""
    echo "=== 実行エンジン統合テスト ==="
    
    # execution_engine.sh読み込み
    if [[ -f "$CORE_DIR/execution_engine.sh" ]] && source "$CORE_DIR/execution_engine.sh" 2>/dev/null; then
        echo "✅ PASS: execution_engine.sh読み込み成功"
        ((test_count++))
        ((passed_count++))
        
        # 引数検証テスト
        if declare -f validate_execution_arguments >/dev/null 2>&1; then
            # 簡易ログ関数
            log() {
                echo "[$1] $2" >&2
            }
            
            if validate_execution_arguments "brief" "50" "auto" "phi4-mini:latest" 2>/dev/null; then
                echo "✅ PASS: 引数検証が動作"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: 引数検証で失敗"
                ((test_count++))
                ((failed_count++))
            fi
        fi
        
        # タイマー機能テスト
        if declare -f start_execution_timer >/dev/null 2>&1 && declare -f end_execution_timer >/dev/null 2>&1; then
            local start_time
            start_time=$(start_execution_timer)
            local duration
            duration=$(end_execution_timer "$start_time")
            if [[ "$duration" =~ ^[0-9]+$ ]]; then
                echo "✅ PASS: タイマー機能が動作"
                ((test_count++))
                ((passed_count++))
            else
                echo "❌ FAIL: タイマー機能で無効な結果"
                ((test_count++))
                ((failed_count++))
            fi
        fi
    else
        echo "❌ FAIL: execution_engine.sh読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
}

# ユーザーインターフェース統合テスト
test_user_interface_integration() {
    echo ""
    echo "=== ユーザーインターフェース統合テスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # user_interface.sh読み込み
    if [[ -f "$CORE_DIR/user_interface.sh" ]] && source "$CORE_DIR/user_interface.sh" 2>/dev/null; then
        echo "✅ PASS: user_interface.sh読み込み成功"
        ((test_count++))
        ((passed_count++))
        
        # 使用法表示テスト
        if declare -f show_usage >/dev/null 2>&1; then
            local usage_output
            usage_output=$(show_usage 2>&1)
            if [[ -n "$usage_output" ]]; then
                assert_contains "$usage_output" "Claude Voice" "使用法表示にタイトルが含まれる"
                assert_contains "$usage_output" "使用法" "使用法表示に使用法セクションが含まれる"
            else
                echo "❌ FAIL: 使用法表示で出力なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
        
        # バージョン表示テスト
        if declare -f show_version >/dev/null 2>&1; then
            local version_output
            version_output=$(show_version 2>&1)
            if [[ -n "$version_output" ]]; then
                assert_contains "$version_output" "Claude Voice" "バージョン表示にタイトルが含まれる"
                assert_contains "$version_output" "OS:" "バージョン表示にOS情報が含まれる"
            else
                echo "❌ FAIL: バージョン表示で出力なし"
                ((test_count++))
                ((failed_count++))
            fi
        fi
    else
        echo "❌ FAIL: user_interface.sh読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
}

# エンドツーエンド連携テスト
test_end_to_end_integration() {
    echo ""
    echo "=== エンドツーエンド連携テスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # 全モジュール読み込みと連携テスト
    local modules=(
        "config_manager.sh"
        "stats_monitor.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
        "user_interface.sh"
    )
    
    local all_loaded=true
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]] && source "$CORE_DIR/$module" 2>/dev/null; then
            continue
        else
            all_loaded=false
            break
        fi
    done
    
    if [[ "$all_loaded" == "true" ]]; then
        echo "✅ PASS: 全モジュールが同時読み込み可能"
        ((test_count++))
        ((passed_count++))
        
        # 統合ワークフローテスト
        # 1. 設定値取得
        local config_value=""
        if declare -f get_config_value >/dev/null 2>&1; then
            config_value=$(get_config_value "default_model" "auto" 2>/dev/null)
        fi
        
        # 2. 統計記録
        local stats_success=false
        if declare -f record_usage_stats >/dev/null 2>&1; then
            if record_usage_stats "brief" "$config_value" "linux" "1" "true" >/dev/null 2>&1; then
                stats_success=true
            fi
        fi
        
        # 3. ヘルスチェック実行
        local health_success=false
        if declare -f run_health_check >/dev/null 2>&1; then
            if run_health_check >/dev/null 2>&1; then
                health_success=true
            fi
        fi
        
        # 統合結果評価
        local integration_score=0
        [[ -n "$config_value" ]] && ((integration_score++))
        [[ "$stats_success" == "true" ]] && ((integration_score++))
        [[ "$health_success" == "true" ]] && ((integration_score++))
        
        if [[ $integration_score -ge 2 ]]; then
            echo "✅ PASS: 統合ワークフローが動作 ($integration_score/3)"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 統合ワークフローで不具合 ($integration_score/3)"
            ((test_count++))
            ((failed_count++))
        fi
    else
        echo "❌ FAIL: 全モジュール同時読み込み失敗"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
}

# 依存関係チェック
test_dependency_resolution() {
    echo ""
    echo "=== 依存関係チェック ==="
    
    # 循環参照がないことの確認
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )
    
    local dependency_error=false
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]]; then
            # 構文エラーがないかチェック
            if ! bash -n "$CORE_DIR/$module" 2>/dev/null; then
                echo "❌ FAIL: $module に構文エラー"
                dependency_error=true
            fi
        fi
    done
    
    if [[ "$dependency_error" == "false" ]]; then
        echo "✅ PASS: 全モジュールの構文が正常"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: 一部モジュールに構文エラー"
        ((test_count++))
        ((failed_count++))
    fi
    
    # 関数名の重複チェック
    local temp_functions="/tmp/all_functions_$$"
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]]; then
            grep -n "^[[:space:]]*[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*(" "$CORE_DIR/$module" | \
            sed 's/^[0-9]*://; s/[[:space:]]*{.*//; s/()[[:space:]]*$//' >> "$temp_functions" 2>/dev/null || true
        fi
    done
    
    if [[ -f "$temp_functions" ]]; then
        local duplicate_functions
        duplicate_functions=$(sort "$temp_functions" | uniq -d)
        if [[ -z "$duplicate_functions" ]]; then
            echo "✅ PASS: 関数名の重複なし"
            ((test_count++))
            ((passed_count++))
        else
            echo "❌ FAIL: 重複関数名が存在"
            echo "   重複: $duplicate_functions"
            ((test_count++))
            ((failed_count++))
        fi
        rm -f "$temp_functions"
    fi
}

# パフォーマンステスト
test_integration_performance() {
    echo ""
    echo "=== 統合パフォーマンステスト ==="
    
    local original_home="$CLAUDE_VOICE_HOME"
    export CLAUDE_VOICE_HOME="$TEST_TEMP_DIR"
    
    # 全モジュール読み込み時間測定
    local start_time=$(date +%s%3N)
    
    local modules=(
        "user_interface.sh"
        "stats_monitor.sh"
        "config_manager.sh"
        "health_diagnostics.sh"
        "execution_engine.sh"
    )
    
    for module in "${modules[@]}"; do
        if [[ -f "$CORE_DIR/$module" ]]; then
            source "$CORE_DIR/$module" 2>/dev/null || true
        fi
    done
    
    local end_time=$(date +%s%3N)
    local load_duration=$((end_time - start_time))
    
    # 5秒以内での読み込みを期待
    if [[ $load_duration -lt 5000 ]]; then
        echo "✅ PASS: 全モジュール読み込み時間: ${load_duration}ms (< 5000ms)"
        ((test_count++))
        ((passed_count++))
    else
        echo "❌ FAIL: 全モジュール読み込み時間: ${load_duration}ms (>= 5000ms)"
        ((test_count++))
        ((failed_count++))
    fi
    
    export CLAUDE_VOICE_HOME="$original_home"
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
        echo "🎉 モジュール統合テスト: 全テスト成功！"
        return 0
    else
        echo "❌ モジュール統合テスト: ${failed_count}個のテストが失敗"
        return 1
    fi
}

# メイン実行
main() {
    echo "Module Integration Test"
    echo "====================="
    
    # テスト環境セットアップ
    setup_test_environment
    
    # 統合テスト実行
    test_module_existence
    test_module_loading
    test_inter_module_function_calls
    test_config_management_integration
    test_stats_management_integration
    test_health_check_integration
    test_execution_engine_integration
    test_user_interface_integration
    test_end_to_end_integration
    test_dependency_resolution
    test_integration_performance
    
    # 結果表示
    test_summary
    
    # クリーンアップ
    cleanup_test_environment
}

# スクリプト直接実行の場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi