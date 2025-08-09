#!/bin/bash
# Claude Voice - LLM Manager Module
# Ollama統合とLLM通信管理

# Ollamaサーバー設定
OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"
OLLAMA_MODEL="${OLLAMA_MODEL:-gemma2:2b}"  # 軽量モデルをデフォルトに

# Ollamaの状態確認
check_ollama_health() {
    curl -s "${OLLAMA_HOST}/api/tags" >/dev/null 2>&1
    return $?
}

# LLMにプロンプトを送信
query_llm() {
    local prompt="$1"
    local max_tokens="${2:-100}"
    local temperature="${3:-0.7}"
    
    if ! check_ollama_health; then
        log "ERROR" "Ollama server is not responding at $OLLAMA_HOST"
        return 1
    fi
    
    local response=$(curl -s -X POST "${OLLAMA_HOST}/api/generate" \
        -H "Content-Type: application/json" \
        -d "{
            \"model\": \"$OLLAMA_MODEL\",
            \"prompt\": \"$prompt\",
            \"stream\": false,
            \"options\": {
                \"num_predict\": $max_tokens,
                \"temperature\": $temperature
            }
        }" 2>/dev/null)
    
    if [[ -n "$response" ]]; then
        echo "$response" | jq -r '.response // empty' 2>/dev/null
    else
        return 1
    fi
}

# スクリーンの内容を要約
summarize_screen_content() {
    local content="$1"
    local max_length="${2:-50}"
    local context="${3:-}"
    
    # コンテンツが短い場合はそのまま返す
    if [[ ${#content} -lt 100 ]]; then
        echo "$content"
        return
    fi
    
    local prompt="以下のターミナル出力を${max_length}文字以内で簡潔に日本語で要約してください。"
    
    # コンテキストに応じてプロンプトを調整
    case "$context" in
        "complete")
            prompt="以下の処理結果を${max_length}文字以内で簡潔に日本語で要約してください。何が完了したか明確に述べてください："
            ;;
        "waiting")
            prompt="以下の内容から、ユーザーに何を確認または入力を求めているか${max_length}文字以内で簡潔に日本語で説明してください："
            ;;
        "error")
            prompt="以下のエラー内容を${max_length}文字以内で簡潔に日本語で説明してください："
            ;;
    esac
    
    prompt="${prompt}

\`\`\`
${content}
\`\`\`

要約："
    
    local summary=$(query_llm "$prompt" 100 0.5)
    
    if [[ -n "$summary" ]]; then
        # 改行を除去して1行にする
        echo "$summary" | tr '\n' ' ' | sed 's/  */ /g'
    else
        echo "要約の生成に失敗しました"
    fi
}

# エクスポート
export -f check_ollama_health
export -f query_llm
export -f summarize_screen_content