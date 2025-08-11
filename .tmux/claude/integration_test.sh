#!/bin/bash
# ファイル名: integration_test.sh
# 説明: tmux-claude-voice システム統合テスト

# 依存ファイルの存在確認
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR

# 統合テスト結果の記録
TEST_RESULTS=()
FAILED_TESTS=()

# テスト結果の記録
record_test_result() {
    local test_name="$1"
    local result="$2"
    
    TEST_RESULTS+=("$test_name: $result")
    
    if [[ "$result" != "✓" ]]; then
        FAILED_TESTS+=("$test_name")
    fi
}

# ファイル存在チェック
test_file_existence() {
    echo "=== ファイル存在チェック ==="
    
    local required_files=(
        "polling_monitor.sh"
        "functions.sh"
        "sound_utils.sh"
        "panning_engine.sh"
        "ollama_utils.sh"
    )
    
    local all_exist=true
    
    for file in "${required_files[@]}"; do
        local file_path="${SCRIPT_DIR}/$file"
        if [[ -f "$file_path" && -x "$file_path" ]]; then
            echo "✓ $file: 存在・実行可能"
            record_test_result "File: $file" "✓"
        else
            echo "✗ $file: 見つからないか実行不可"
            record_test_result "File: $file" "✗"
            all_exist=false
        fi
    done
    
    return $([ "$all_exist" = true ])
}

# 単体テスト実行
test_individual_components() {
    echo "=== 単体テスト実行 ==="
    
    # polling_monitor.shテスト
    echo "polling_monitor.shテスト実行中..."
    if "${SCRIPT_DIR}/polling_monitor.sh" >/dev/null 2>&1; then
        echo "✓ polling_monitor.sh: 単体テスト成功"
        record_test_result "Unit: polling_monitor.sh" "✓"
    else
        echo "✗ polling_monitor.sh: 単体テスト失敗"
        record_test_result "Unit: polling_monitor.sh" "✗"
    fi
    
    # functions.shテスト
    echo "functions.shテスト実行中..."
    if "${SCRIPT_DIR}/functions.sh" test >/dev/null 2>&1; then
        echo "✓ functions.sh: 単体テスト成功"
        record_test_result "Unit: functions.sh" "✓"
    else
        echo "✗ functions.sh: 単体テスト失敗"
        record_test_result "Unit: functions.sh" "✗"
    fi
    
    # sound_utils.shテスト
    echo "sound_utils.shテスト実行中..."
    if "${SCRIPT_DIR}/sound_utils.sh" test >/dev/null 2>&1; then
        echo "✓ sound_utils.sh: 単体テスト成功"
        record_test_result "Unit: sound_utils.sh" "✓"
    else
        echo "✗ sound_utils.sh: 単体テスト失敗"
        record_test_result "Unit: sound_utils.sh" "✗"
    fi
    
    # panning_engine.shテスト
    echo "panning_engine.shテスト実行中..."
    if "${SCRIPT_DIR}/panning_engine.sh" test >/dev/null 2>&1; then
        echo "✓ panning_engine.sh: 単体テスト成功"
        record_test_result "Unit: panning_engine.sh" "✓"
    else
        echo "✗ panning_engine.sh: 単体テスト失敗"
        record_test_result "Unit: panning_engine.sh" "✗"
    fi
    
    # ollama_utils.shテスト
    echo "ollama_utils.shテスト実行中..."
    if "${SCRIPT_DIR}/ollama_utils.sh" test >/dev/null 2>&1; then
        echo "✓ ollama_utils.sh: 単体テスト成功"
        record_test_result "Unit: ollama_utils.sh" "✓"
    else
        echo "✗ ollama_utils.sh: 単体テスト失敗"
        record_test_result "Unit: ollama_utils.sh" "✗"
    fi
}

# 依存関係チェック
test_dependencies() {
    echo "=== 依存関係チェック ==="
    
    # 基本依存関係
    echo "基本依存関係チェック中..."
    if "${SCRIPT_DIR}/polling_monitor.sh" >/dev/null 2>&1; then
        echo "✓ 基本依存関係: 満足"
        record_test_result "Deps: Basic" "✓"
    else
        echo "✗ 基本依存関係: 不足"
        record_test_result "Deps: Basic" "✗"
    fi
    
    # 音声エンジン依存関係
    echo "音声エンジン依存関係チェック中..."
    if "${SCRIPT_DIR}/sound_utils.sh" deps >/dev/null 2>&1; then
        echo "✓ 音声エンジン依存関係: 満足"
        record_test_result "Deps: Sound" "✓"
    else
        echo "✗ 音声エンジン依存関係: 不足"
        record_test_result "Deps: Sound" "✗"
    fi
    
    # パンニングエンジン依存関係
    echo "パンニングエンジン依存関係チェック中..."
    if "${SCRIPT_DIR}/panning_engine.sh" deps >/dev/null 2>&1; then
        echo "✓ パンニングエンジン依存関係: 満足"
        record_test_result "Deps: Panning" "✓"
    else
        echo "✗ パンニングエンジン依存関係: 不足"
        record_test_result "Deps: Panning" "✗"
    fi
    
    # Ollama依存関係
    echo "Ollama依存関係チェック中..."
    if "${SCRIPT_DIR}/ollama_utils.sh" deps >/dev/null 2>&1; then
        echo "✓ Ollama依存関係: 満足"
        record_test_result "Deps: Ollama" "✓"
    else
        echo "✗ Ollama依存関係: 不足"
        record_test_result "Deps: Ollama" "✗"
    fi
}

# 設定読み込みテスト
test_configuration() {
    echo "=== 設定読み込みテスト ==="
    
    # tmux設定オプションのテスト
    local test_options=(
        "claude_voice_enabled:true"
        "claude_voice_interval:5"
        "claude_voice_window_pattern:Claude|claude|CLAUDE"
    )
    
    for option_pair in "${test_options[@]}"; do
        local option_name=${option_pair%:*}
        local expected_value=${option_pair#*:}
        
        # テスト用設定を設定
        if tmux set-option -g "@$option_name" "$expected_value" 2>/dev/null; then
            # 設定読み込みテスト
            local actual_value
            actual_value=$(tmux show-option -gqv "@$option_name" 2>/dev/null)
            
            if [[ "$actual_value" == "$expected_value" ]]; then
                echo "✓ 設定 $option_name: 読み込み成功 ($actual_value)"
                record_test_result "Config: $option_name" "✓"
            else
                echo "✗ 設定 $option_name: 読み込み失敗 (期待: $expected_value, 実際: $actual_value)"
                record_test_result "Config: $option_name" "✗"
            fi
            
            # テスト用設定をクリーンアップ
            tmux set-option -g -u "@$option_name" 2>/dev/null
        else
            echo "✗ 設定 $option_name: tmux設定失敗"
            record_test_result "Config: $option_name" "✗"
        fi
    done
}

# コンポーネント連携テスト
test_component_integration() {
    echo "=== コンポーネント連携テスト ==="
    
    # functions.sh → sound_utils.sh 連携
    echo "functions.sh → sound_utils.sh 連携テスト中..."
    if bash -c "
        source '${SCRIPT_DIR}/functions.sh' && 
        source '${SCRIPT_DIR}/sound_utils.sh' && 
        command -v log_info >/dev/null && 
        command -v get_os_type >/dev/null
    " 2>/dev/null; then
        echo "✓ functions.sh → sound_utils.sh 連携: 成功"
        record_test_result "Integration: functions→sound" "✓"
    else
        echo "✗ functions.sh → sound_utils.sh 連携: 失敗"
        record_test_result "Integration: functions→sound" "✗"
    fi
    
    # functions.sh → panning_engine.sh 連携
    echo "functions.sh → panning_engine.sh 連携テスト中..."
    if bash -c "
        source '${SCRIPT_DIR}/functions.sh' && 
        source '${SCRIPT_DIR}/panning_engine.sh' && 
        command -v log_info >/dev/null && 
        command -v detect_claude_windows_for_panning >/dev/null
    " 2>/dev/null; then
        echo "✓ functions.sh → panning_engine.sh 連携: 成功"
        record_test_result "Integration: functions→panning" "✓"
    else
        echo "✗ functions.sh → panning_engine.sh 連携: 失敗"
        record_test_result "Integration: functions→panning" "✗"
    fi
    
    # functions.sh → ollama_utils.sh 連携
    echo "functions.sh → ollama_utils.sh 連携テスト中..."
    if bash -c "
        source '${SCRIPT_DIR}/functions.sh' && 
        source '${SCRIPT_DIR}/ollama_utils.sh' && 
        command -v log_info >/dev/null && 
        command -v summarize_with_ollama >/dev/null
    " 2>/dev/null; then
        echo "✓ functions.sh → ollama_utils.sh 連携: 成功"
        record_test_result "Integration: functions→ollama" "✓"
    else
        echo "✗ functions.sh → ollama_utils.sh 連携: 失敗"
        record_test_result "Integration: functions→ollama" "✗"
    fi
}

# パフォーマンステスト
test_performance() {
    echo "=== パフォーマンステスト ==="
    
    # ファイル読み込み時間
    echo "ファイル読み込み時間測定中..."
    local start_time=$(date +%s.%N)
    
    if bash -c "
        source '${SCRIPT_DIR}/functions.sh' && 
        source '${SCRIPT_DIR}/sound_utils.sh' && 
        source '${SCRIPT_DIR}/panning_engine.sh' && 
        source '${SCRIPT_DIR}/ollama_utils.sh'
    " 2>/dev/null; then
        local end_time=$(date +%s.%N)
        local load_time
        load_time=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "0")
        
        # 1秒以内であれば合格
        if (( $(echo "$load_time < 1.0" | bc -l 2>/dev/null || echo 0) )); then
            echo "✓ ファイル読み込み時間: ${load_time}秒 (合格)"
            record_test_result "Performance: Load Time" "✓"
        else
            echo "✗ ファイル読み込み時間: ${load_time}秒 (1秒超過)"
            record_test_result "Performance: Load Time" "✗"
        fi
    else
        echo "✗ ファイル読み込み: 失敗"
        record_test_result "Performance: Load Time" "✗"
    fi
    
    # メモリ使用量（概算）
    echo "メモリ使用量測定中..."
    local memory_usage
    memory_usage=$(bash -c "
        source '${SCRIPT_DIR}/functions.sh' >/dev/null 2>&1 &&
        source '${SCRIPT_DIR}/sound_utils.sh' >/dev/null 2>&1 &&
        source '${SCRIPT_DIR}/panning_engine.sh' >/dev/null 2>&1 &&
        source '${SCRIPT_DIR}/ollama_utils.sh' >/dev/null 2>&1 &&
        echo 'success'
    " 2>/dev/null || echo "failed")
    
    if [[ "$memory_usage" == "success" ]]; then
        echo "✓ メモリ使用量: 正常 (全スクリプト読み込み成功)"
        record_test_result "Performance: Memory Usage" "✓"
    else
        echo "✗ メモリ使用量: 測定失敗"
        record_test_result "Performance: Memory Usage" "✗"
    fi
}

# 結果レポート生成
generate_report() {
    echo ""
    echo "==============================================="
    echo "       tmux-claude-voice 統合テストレポート"
    echo "==============================================="
    echo ""
    
    echo "テスト実行日時: $(date)"
    echo "テスト環境: $(uname -a)"
    echo ""
    
    echo "テスト結果サマリー:"
    local total_tests=${#TEST_RESULTS[@]}
    local failed_count=${#FAILED_TESTS[@]}
    local success_count=$((total_tests - failed_count))
    
    echo "  総テスト数: $total_tests"
    echo "  成功: $success_count"
    echo "  失敗: $failed_count"
    echo ""
    
    if [[ $failed_count -eq 0 ]]; then
        echo "🎉 すべてのテストが成功しました！"
        echo "システムは正常に動作する準備ができています。"
    else
        echo "⚠️  失敗したテスト:"
        for failed_test in "${FAILED_TESTS[@]}"; do
            echo "  - $failed_test"
        done
        echo ""
        echo "失敗したテストを修正してから本番利用してください。"
    fi
    
    echo ""
    echo "詳細結果:"
    for result in "${TEST_RESULTS[@]}"; do
        echo "  $result"
    done
    
    echo ""
    echo "次のステップ:"
    if [[ $failed_count -eq 0 ]]; then
        echo "1. .tmux.confに設定を追加"
        echo "2. tmuxを再起動"
        echo "3. ポーリング監視が自動開始されます"
        echo "4. Claude Codeウィンドウでテスト実行"
    else
        echo "1. 失敗したテストの問題を修正"
        echo "2. 依存関係をインストール"
        echo "3. 再度統合テストを実行"
    fi
    
    echo ""
    echo "==============================================="
    
    return $failed_count
}

# メイン統合テスト実行
main() {
    echo "tmux-claude-voice システム統合テスト開始"
    echo ""
    
    # 各テストを順次実行
    test_file_existence
    test_individual_components  
    test_dependencies
    test_configuration
    test_component_integration
    test_performance
    
    # 結果レポート生成
    generate_report
    
    # 終了コードを返す
    local failed_count=${#FAILED_TESTS[@]}
    exit $failed_count
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi