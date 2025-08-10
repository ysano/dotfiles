# Tmux Claude Voice: Ollama連携と要約機能

## 4.3. Ollama連携と要約指針

ローカルLLMを最大限に活用し、的確な要約を生成します。

### ホスト検出

- **macOS**: `localhost` を使用します。
- **WSL**: `/etc/resolv.conf`からWindowsホストのIPアドレスを動的に取得し、接続します。

### モデル選択

- ハードコードされた優先順位リスト（`gemma3:1b` → `gemma2:2b` → `phi4-mini:latest` → `orca-mini:latest`）から、利用可能なモデルを順次選択します。
- **ローカルOllama**: `ollama list`コマンドで利用可能なモデルを確認。
- **外部Ollama**: APIエンドポイント（`/api/tags`）で利用可能なモデルを確認。
- **フォールバック**: モデル確認が失敗した場合、優先順位リストの最初のモデル（`gemma3:1b`）を使用。

### 要約プロンプトの指針

- **目標**: 約30文字で、端的・簡潔・完結した状況報告を生成する。
- **コンテキストの重視**:
  - **最後の数行**が最も重要。`tail -n 20`などで最新のテキストを切り出す。
  - Claude Codeが出力する見出し（`⏺`）以降に注目する。
- **キーワードの優先**:
  - **完了メッセージ**: `Successfully`, `Done`, `Created`
  - **エラーメッセージ**: `Error`, `Failed`, `Exception`
  - **ユーザーへの問いかけ**: `Proceed?`, `Choose an option`, `(y/n)`
- **プロンプト例**:

  ```prompt
  以下のテキストはAIアシスタントの出力です。現在の「問い合わせ内容」について、状況を30文字以内で具体的に要約してください。特に最後の行を重視してください。

  <画面キャプチャの末尾20行>
  ```

## 実装関数

```bash
# Ollamaサーバーの接続情報を取得する関数
get_ollama_connection() {
    local host=$(tmux show-option -gv @claude_voice_ollama_host 2>/dev/null || echo "localhost")
    local port=$(tmux show-option -gv @claude_voice_ollama_port 2>/dev/null || echo "11434")
    echo "http://$host:$port"
}

# 利用可能なOllamaモデルを取得する関数
get_available_ollama_models() {
    local connection=$(get_ollama_connection)
    local timeout=$(tmux show-option -gv @claude_voice_ollama_timeout 2>/dev/null || echo "10")

    # まずローカルのollama listコマンドを試行
    if command -v ollama >/dev/null 2>&1; then
        local local_models=$(ollama list --format json 2>/dev/null | jq -r '.models[].name' 2>/dev/null)
        if [[ -n "$local_models" ]]; then
            echo "$local_models"
            return 0
        fi
    fi

    # 外部APIを使用してモデルリストを取得
    local api_response=$(curl -s --max-time "$timeout" "${connection}/api/tags" 2>/dev/null)
    if [[ -n "$api_response" ]]; then
        local api_models=$(echo "$api_response" | jq -r '.models[].name' 2>/dev/null)
        if [[ -n "$api_models" ]]; then
            echo "$api_models"
            return 0
        fi
    fi

    # フォールバック: ハードコードされた優先順位リストの最初のモデルを使用
    echo "${OLLAMA_MODEL_PRIORITY[0]}"
    return 1
}

# モデル優先順位リスト（ハードコード）
OLLAMA_MODEL_PRIORITY=("gemma3:1b" "gemma2:2b" "phi4-mini:latest" "orca-mini:latest")

# 最適なOllamaモデルを選択する関数
select_optimal_ollama_model() {
    local available_models=$(get_available_ollama_models)

    # ハードコードされた優先順位リストを使用
    for model in "${OLLAMA_MODEL_PRIORITY[@]}"; do
        if echo "$available_models" | grep -q "^${model}$"; then
            echo "$model"
            return 0
        fi
    done

    # 利用可能なモデルが見つからない場合、最初の利用可能なモデルを使用
    local first_available=$(echo "$available_models" | head -n1)
    if [[ -n "$first_available" ]]; then
        echo "$first_available"
        return 0
    fi

    # 最後のフォールバック: 優先順位リストの最初のモデル
    echo "${OLLAMA_MODEL_PRIORITY[0]}"
    return 1
}

# 画面テキストを要約する関数
summarize_with_ollama() {
    local pane_content="$1"
    local connection=$(get_ollama_connection)
    local timeout=$(tmux show-option -gv @claude_voice_ollama_timeout 2>/dev/null || echo "10")
    local model=$(select_optimal_ollama_model)

    # 最後の20行を抽出（最も重要な情報）
    local recent_content=$(echo "$pane_content" | tail -n 20)

    # プロンプトの構築
    local prompt="以下のテキストはAIアシスタントの出力です。現在の「問い合わせ内容」について、状況を30文字以内で具体的に要約してください。特に最後の行を重視してください。

$recent_content"

    # JSONリクエストの構築
    local json_request=$(cat <<EOF
{
  "model": "$model",
  "prompt": $(echo "$prompt" | jq -R .),
  "stream": false,
  "options": {
    "temperature": 0.3,
    "top_p": 0.9,
    "num_predict": 50
  }
}
EOF
)

    # API呼び出し
    local response=$(curl -s --max-time "$timeout" \
        -H "Content-Type: application/json" \
        -d "$json_request" \
        "${connection}/api/generate" 2>/dev/null)

    if [[ -n "$response" ]]; then
        local summary=$(echo "$response" | jq -r '.response' 2>/dev/null)
        if [[ -n "$summary" && "$summary" != "null" ]]; then
            echo "$summary"
            return 0
        fi
    fi

    # フォールバック: 簡単なキーワード抽出
    echo "$recent_content" | grep -oE "(Successfully|Done|Created|Error|Failed|Exception|Proceed\?|Choose an option)" | head -n1 || echo "処理完了"
    return 1
}

# WSL環境でのWindowsホストIP取得
get_windows_host_ip() {
    if [[ "$(uname)" == "Linux" ]]; then
        # WSL環境でのWindowsホストIP取得
        local windows_ip=$(grep -oP 'nameserver \K[0-9.]+' /etc/resolv.conf 2>/dev/null | head -n1)
        if [[ -n "$windows_ip" ]]; then
            echo "$windows_ip"
            return 0
        fi
    fi
    echo "localhost"
    return 1
}

# プラットフォーム固有のOllamaホスト設定
get_ollama_host() {
    local os_type=$(get_os_type)
    
    if [[ "$os_type" == "Darwin" ]]; then
        echo "localhost"
    else
        # WSL環境ではWindowsホストのIPを使用
        get_windows_host_ip
    fi
}
```

## 設定パラメータ

`.tmux.conf`で以下のパラメータをカスタマイズ可能：

```bash
# Ollama設定
set -g @claude_voice_ollama_host "localhost"  # Ollamaサーバーのホスト
set -g @claude_voice_ollama_port "11434"      # Ollamaサーバーのポート
set -g @claude_voice_ollama_timeout "10"      # API呼び出しのタイムアウト（秒）

# モデル優先順位リスト（ハードコード）
# 設定ファイルでの変更は不要、スクリプト内で直接管理
# OLLAMA_MODEL_PRIORITY=("gemma3:1b" "gemma2:2b" "phi4-mini:latest" "orca-mini:latest")

# 要約設定
set -g @claude_voice_summary_enabled "true"   # 要約機能の有効化
set -g @claude_voice_summary_length "30"      # 要約文字数制限
set -g @claude_voice_summary_lines "20"       # 分析対象行数
set -g @claude_voice_summary_temperature "0.3" # 要約生成の温度パラメータ
```

## 実装チェックリスト

### Phase 4: Ollama連携

- [ ] `ollama_utils.sh`の作成
- [ ] モデル優先順位リストの実装
- [ ] 外部API連携機能の実装
- [ ] 要約生成機能の実装
- [ ] フォールバック機能の実装
- [ ] WSL環境でのWindowsホスト検出
- [ ] エラーハンドリングの実装
- [ ] タイムアウト処理の実装
- [ ] 設定パラメータの実装
- [ ] 単体テストの作成

### テスト項目

1. **接続テスト**
   - ローカルOllamaへの接続
   - 外部Ollamaへの接続
   - WSL環境でのWindowsホスト接続

2. **モデル検出テスト**
   - 利用可能モデルの取得
   - 優先順位に基づくモデル選択
   - フォールバック機能の動作確認

3. **要約生成テスト**
   - 正常なテキストの要約
   - エラー状態の要約
   - 質問状態の要約
   - 文字数制限の確認

4. **パフォーマンステスト**
   - API呼び出しの応答時間
   - タイムアウト処理の動作確認
   - エラー時の復旧処理

## トラブルシューティング

### よくある問題

1. **Ollamaサーバーに接続できない**
   ```bash
   # 解決方法: 接続テスト
   curl -s --max-time 5 "http://localhost:11434/api/tags" || echo "Ollamaサーバーに接続できません"
   ```

2. **WSL環境でWindowsホストに接続できない**
   ```bash
   # 解決方法: WindowsホストIPの確認
   grep -oP 'nameserver \K[0-9.]+' /etc/resolv.conf
   ```

3. **要約が生成されない**
   ```bash
   # 解決方法: モデルリストの確認
   ollama list
   curl -s "http://localhost:11434/api/tags" | jq '.models[].name'
   ```

4. **要約の品質が低い**
   ```bash
   # 解決方法: プロンプトの調整
   # 温度パラメータを下げる（0.1-0.3）
   # 分析対象行数を増やす（20-30行）
   ```
