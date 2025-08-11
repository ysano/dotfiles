# Tmux Claude Voice: 実装ガイド

## 実装テンプレート

### 基本関数テンプレート

```bash
#!/bin/bash
# ファイル名: function_name.sh
# 説明: 機能の説明

# 依存関係チェック
check_dependencies() {
    local deps=("tmux" "grep" "bc")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" >/dev/null 2>&1; then
            log_error "依存関係が見つかりません: $dep"
            return 1
        fi
    done
    return 0
}

# ログ機能
log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2
}

# メイン関数
main_function() {
    local input="$1"

    # 入力バリデーション
    if [[ -z "$input" ]]; then
        log_error "入力が空です"
        return 1
    fi

    # 依存関係チェック
    if ! check_dependencies; then
        return 1
    fi

    # メイン処理
    local result
    if result=$(process_input "$input"); then
        echo "$result"
        return 0
    else
        log_error "処理に失敗しました"
        return 1
    fi
}

# テスト関数
test_function() {
    echo "=== 単体テスト開始 ==="

    # 正常系テスト
    local test_input="test_data"
    local expected="expected_result"
    local actual=$(main_function "$test_input")

    if [[ "$actual" == "$expected" ]]; then
        echo "✓ 正常系テスト: 成功"
    else
        echo "✗ 正常系テスト: 失敗 (期待値: $expected, 実際: $actual)"
        return 1
    fi

    # 異常系テスト
    if ! main_function "" >/dev/null 2>&1; then
        echo "✓ 異常系テスト: 成功"
    else
        echo "✗ 異常系テスト: 失敗"
        return 1
    fi

    echo "=== 単体テスト完了 ==="
    return 0
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    if [[ "$1" == "test" ]]; then
        test_function
    else
        main_function "$1"
    fi
fi
```

### エラーハンドリング例

```bash
# 関数実行時のエラーハンドリング
safe_execute() {
    local func_name="$1"
    shift

    log_info "関数実行開始: $func_name"

    if "$func_name" "$@"; then
        log_info "関数実行成功: $func_name"
        return 0
    else
        log_error "関数実行失敗: $func_name (終了コード: $?)"
        return 1
    fi
}

# 設定値のバリデーション
validate_config() {
    local config_name="$1"
    local config_value="$2"
    local expected_type="$3"

    case "$expected_type" in
        "number")
            if ! [[ "$config_value" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
                log_error "設定値が数値ではありません: $config_name = $config_value"
                return 1
            fi
            ;;
        "boolean")
            if ! [[ "$config_value" =~ ^(true|false)$ ]]; then
                log_error "設定値が真偽値ではありません: $config_name = $config_value"
                return 1
            fi
            ;;
        "string")
            if [[ -z "$config_value" ]]; then
                log_error "設定値が空です: $config_name"
                return 1
            fi
            ;;
    esac

    return 0
}
```

### 統合テスト例

```bash
#!/bin/bash
# ファイル名: integration_test.sh

# 統合テスト実行
run_integration_tests() {
    echo "=== 統合テスト開始 ==="

    # 1. 基本監視システムテスト
    if test_monitoring_system; then
        echo "✓ 監視システム: 成功"
    else
        echo "✗ 監視システム: 失敗"
        return 1
    fi

    # 2. 音声エンジンテスト
    if test_sound_engine; then
        echo "✓ 音声エンジン: 成功"
    else
        echo "✗ 音声エンジン: 失敗"
        return 1
    fi

    # 3. デシベルパンニングテスト
    if test_panning_engine; then
        echo "✓ パンニングエンジン: 成功"
    else
        echo "✗ パンニングエンジン: 失敗"
        return 1
    fi

    echo "=== 統合テスト完了 ==="
    return 0
}

# 実際のtmux環境でのテスト
test_real_tmux_environment() {
    echo "=== エンドツーエンドテスト開始 ==="

    # tmuxセッション作成
    local session_name="test_claude_voice_$(date +%s)"
    tmux new-session -d -s "$session_name"

    # テストウィンドウ作成
    tmux new-window -t "$session_name" -n "Claude"

    # ポーリング監視テスト
    if test_polling_monitor "$session_name"; then
        echo "✓ ポーリング監視: 成功"
    else
        echo "✗ ポーリング監視: 失敗"
        tmux kill-session -t "$session_name"
        return 1
    fi

    # クリーンアップ
    tmux kill-session -t "$session_name"

    echo "=== エンドツーエンドテスト完了 ==="
    return 0
}

# ポーリング監視テスト
test_polling_monitor() {
    local session_name="$1"
    
    # ポーリング監視を実行
    if ~/.tmux/claude/polling_monitor.sh >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}
```

## ファイル構造

上記設計に基づき、以下の構造でスクリプトを実装してください。

1.  **`polling_monitor.sh`**:
    - 設定読み込み、ポーリング監視、監視対象ウィンドウの特定と処理。
    - `tmux status-right`から5秒間隔で呼び出される1回実行型スクリプト。
2.  **`functions.sh`**:
    - `analyze_pane_content()`: ペインコンテンツからステータスを判定するロジック。
    - `handle_status_change()`: 状態遷移に応じた処理の振り分け。
    - `summarize_with_ollama()`: Ollama APIと連携し要約を生成する機能。
    - `update_claude_status_icon()`: ウィンドウアイコンを更新する機能。
3.  **`sound_utils.sh`**:
    - `get_os_type()`, `speak()`, `play_notification_sound()`, `get_system_sound_path()`など、プラットフォーム依存の音声処理をすべてここにまとめる。
4.  **`panning_engine.sh`**:
    - `detect_claude_windows_for_panning()`: Claude Codeウィンドウの検出。
    - `count_claude_windows()`: Claude Codeウィンドウ数のカウント。
    - `calculate_equal_spacing()`: 均等配置位置の計算。
    - `calculate_pan_position()`: 動的音像位置の計算。
    - `apply_panning()`: デシベルパンニングの適用。
    - `create_window_identified_sound()`: ウィンドウ識別用音声の生成。
5.  **`ollama_utils.sh`**:
    - `OLLAMA_MODEL_PRIORITY`: ハードコードされたモデル優先順位リスト。
    - `get_ollama_connection()`: Ollamaサーバーの接続情報取得。
    - `get_available_ollama_models()`: 利用可能なモデルリスト取得。
    - `select_optimal_ollama_model()`: 優先順位に基づく最適なモデル選択。
    - `summarize_with_ollama()`: Ollama APIを使用した要約生成。
6.  **`integration_test.sh`**:
    - 全ファイルの統合テスト。
    - 単体テスト実行。
    - エンドツーエンドテスト。
    - ポーリング監視テスト。

## 最優先事項

- **軽量性**: ポーリング方式のため、1回実行で確実に終了し、リソース消費を最小限に抑えること。
- **堅牢性**: スクリプトがエラーで停止しないよう、コマンドの存在チェックやエラーハンドリングを適切に行うこと。
- **パフォーマンス**: `tmux`全体のパフォーマンスに影響を与えないよう、効率的な処理を心がけること。
- **音声品質**: Equal Power Pan Law対応により、デシベルパンニング処理による音質劣化を最小限に抑えること。
- **ウィンドウ識別精度**: 複数ウィンドウ間での音像位置の明確な区別を確保すること。
- **プラットフォーム最適化**: macOS（Kyoko/Otoya Enhanced）とWSL（Haruka/Ayumi/Ichiro）の高品質音声合成を活用すること。
- **保守性**: 機能ごとにファイルを分割し、変数のスコープを適切に管理すること。

## デバッグガイド

### よくある問題と解決方法

1. **tmuxコマンドが実行できない**

   ```bash
   # 解決方法: tmuxセッション内で実行されているか確認
   if [[ -z "$TMUX" ]]; then
       echo "tmuxセッション内で実行してください"
       exit 1
   fi
   ```

2. **ポーリング監視が動作しない**

   ```bash
   # 解決方法: status-right設定の確認
   tmux show-option -g status-right
   
   # 解決方法: 手動でポーリング監視をテスト
   ~/.tmux/claude/polling_monitor.sh
   ```

3. **音声が再生されない**

   ```bash
   # 解決方法: 音声デバイスの確認
   # macOS
   system_profiler SPAudioDataType

   # WSL
   powershell.exe -Command "Get-WmiObject -Class Win32_SoundDevice"
   ```

4. **Ollamaに接続できない**

   ```bash
   # 解決方法: 接続テスト
   curl -s --max-time 5 "http://localhost:11434/api/tags" || echo "Ollamaサーバーに接続できません"
   ```

5. **ffplayが見つからない**

   ```bash
   # 解決方法: インストール確認
   # macOS
   brew install ffmpeg

   # Ubuntu/Debian
   sudo apt-get install ffmpeg
   ```

### ログレベルの設定

```bash
# デバッグモードの有効化
export TMUX_CLAUDE_VOICE_DEBUG=1

# 詳細ログの出力
if [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]]; then
    log_debug() {
        echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2
    }
else
    log_debug() { :; }
fi
```

### パフォーマンス監視

```bash
# 実行時間の測定
measure_execution_time() {
    local start_time=$(date +%s.%N)
    "$@"
    local end_time=$(date +%s.%N)
    local execution_time=$(echo "$end_time - $start_time" | bc)
    log_debug "実行時間: ${execution_time}秒"
}
```

### ポーリング監視のデバッグ

```bash
# ポーリング監視の手動テスト
test_polling_monitor() {
    echo "=== ポーリング監視テスト ==="
    
    # 設定確認
    echo "システム有効化状態: $(tmux show-option -gqv @claude_voice_enabled)"
    echo "ウィンドウパターン: $(tmux show-option -gqv @claude_voice_window_pattern)"
    
    # ポーリング監視実行
    if ~/.tmux/claude/polling_monitor.sh; then
        echo "✓ ポーリング監視: 成功"
    else
        echo "✗ ポーリング監視: 失敗"
        return 1
    fi
}
```

## 最終確認事項

実装完了後、以下の項目を確認してください：

- [ ] すべての依存関係がインストールされている
- [ ] 各ファイルが独立してテスト可能
- [ ] エラーハンドリングが適切に実装されている
- [ ] ログ出力が適切に設定されている
- [ ] 設定値のバリデーションが実装されている
- [ ] 単体テストがすべて成功している
- [ ] 統合テストが成功している
- [ ] ポーリング監視が正常に動作している
- [ ] 実際のtmux環境で動作確認が完了している
- [ ] ドキュメントが最新の状態になっている

このチェックリストに従って実装することで、堅牢で保守性の高いtmux-claude-voiceシステムを構築できます。
