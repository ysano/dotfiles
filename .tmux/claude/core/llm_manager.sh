#!/bin/bash
# Claude Voice - LLM Manager Module
# Ollama統合とLLM通信管理

# Ollamaサーバー設定
OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

# モデル優先順位リスト（ハードコード）
OLLAMA_MODEL_PRIORITY=("gemma3:1b" "gemma2:2b" "phi4-mini:latest" "orca-mini:latest")

# Ollamaの状態確認
check_ollama_health() {
    curl -s "${OLLAMA_HOST}/api/tags" >/dev/null 2>&1
    return $?
}

# 利用可能なモデルを取得
get_available_models() {
    curl -s "${OLLAMA_HOST}/api/tags" 2>/dev/null | jq -r '.models[].name' 2>/dev/null || echo ""
}

# モデルの存在確認
check_model_exists() {
    local model="$1"
    local available_models=$(get_available_models)
    
    if [[ -z "$available_models" ]]; then
        return 1
    fi
    
    echo "$available_models" | grep -q "^${model}$"
    return $?
}

# 優先順位に基づいてモデルを選択
get_best_available_model() {
    # 優先順位リストから最初に見つかったモデルを返す
    for model in "${OLLAMA_MODEL_PRIORITY[@]}"; do
        if check_model_exists "$model"; then
            echo "$model"
            return 0
        fi
    done
    
    # どのモデルも見つからない場合
    log "ERROR" "No available models found from priority list"
    return 1
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
    
    # 利用可能な最適なモデルを取得
    local model=$(get_best_available_model)
    if [[ -z "$model" ]]; then
        log "ERROR" "No model available"
        return 1
    fi
    
    # プロンプトをJSONエスケープ
    local escaped_prompt=$(echo "$prompt" | jq -Rs '.')
    
    local response=$(curl -s -X POST "${OLLAMA_HOST}/api/generate" \
        -H "Content-Type: application/json" \
        -d "{
            \"model\": \"$model\",
            \"prompt\": $escaped_prompt,
            \"stream\": false,
            \"options\": {
                \"num_predict\": $max_tokens,
                \"temperature\": $temperature
            }
        }" 2>/dev/null)
    
    if [[ -n "$response" ]]; then
        local result=$(echo "$response" | jq -r '.response // empty' 2>/dev/null)
        if [[ -n "$result" ]] && [[ "$result" != "empty" ]]; then
            echo "$result"
        else
            log "DEBUG" "Failed to extract response from: ${response:0:100}..."
            return 1
        fi
    else
        log "DEBUG" "No response from Ollama"
        return 1
    fi
}

# スクリーンの内容を要約
summarize_screen_content() {
    local content="$1"
    local max_length="${2:-30}"
    local context="${3:-}"
    
    # コンテンツが短い場合はそのまま返す
    if [[ ${#content} -lt 100 ]]; then
        echo "$content"
        return
    fi
    
    local prompt="以下のターミナル出力を日本語で要約してください。

要約の指針：
- ${max_length}文字程度を目標とする
- 文章として自然で完結していること
- 最後の数行が最も重要（最新の状態を示す）
- 見出し（##、===、---等）に注目する
- 完了メッセージやエラーメッセージを優先

"
    
    # コンテキストに応じてプロンプトを調整
    case "$context" in
        "complete")
            prompt="${prompt}最後に表示されている完了内容や結果を簡潔に述べてください："
            ;;
        "waiting")
            prompt="${prompt}最後に表示されている質問や選択肢を簡潔に述べてください："
            ;;
        "error")
            prompt="${prompt}最後に表示されているエラーメッセージを簡潔に述べてください："
            ;;
        *)
            prompt="${prompt}最後の重要な情報を簡潔に要約してください："
            ;;
    esac
    
    # 内容の最後の5行を抽出して強調
    local last_lines=$(echo "$content" | tail -5)
    
    prompt="${prompt}

=== ターミナル出力 ===
${content}

=== 最後の5行（最重要） ===
${last_lines}

簡潔な要約（${max_length}文字程度）："
    
    local summary=$(query_llm "$prompt" 100 0.5)
    
    if [[ -n "$summary" ]]; then
        # 改行を除去して1行にする
        summary=$(echo "$summary" | tr '\n' ' ' | sed 's/  */ /g')
        
        # 文字数のログ出力（デバッグ用）
        if [[ ${#summary} -gt $max_length ]]; then
            log "DEBUG" "Summary is ${#summary} chars (target: ${max_length} chars)"
        fi
        
        echo "$summary"
    else
        log "DEBUG" "Summary generation failed, prompt was: ${prompt:0:100}..."
        echo "要約の生成に失敗しました"
    fi
}

# エクスポート
export -f check_ollama_health
export -f get_available_models
export -f check_model_exists
export -f get_best_available_model
export -f query_llm
export -f summarize_screen_content