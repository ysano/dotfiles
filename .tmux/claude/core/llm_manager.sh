#!/bin/bash
# Claude Voice Core - LLM Manager Module
# Ollama及び他のLLMプロバイダーとの統合（OS非依存）

# LLMプロバイダーの定数
readonly PROVIDER_OLLAMA="ollama"
readonly PROVIDER_OPENAI="openai"
readonly PROVIDER_CLAUDE="claude"
readonly PROVIDER_SIMPLE="simple"

# OS固有のデフォルト設定
case "$(uname)" in
    "Darwin")
        DEFAULT_OLLAMA_API="http://localhost:11434"
        ;;
    "Linux")
        if grep -qi microsoft /proc/version 2>/dev/null || [[ -n "$WSL_DISTRO_NAME" ]]; then
            # WSL環境
            DEFAULT_OLLAMA_API="http://172.29.80.1:11434"
        else
            # 純粋なLinux環境
            DEFAULT_OLLAMA_API="http://localhost:11434"
        fi
        ;;
    *)
        DEFAULT_OLLAMA_API="http://localhost:11434"
        ;;
esac
DEFAULT_TIMEOUT=30
DEFAULT_MAX_RETRIES=3

# Ollamaの健康状態チェック
check_ollama_health() {
    local api_url="${1:-$(get_config "llm.ollama.api_url" "$DEFAULT_OLLAMA_API")}"
    local timeout="${2:-$(get_config "llm.timeout" "5")}"

    log "DEBUG" "Checking Ollama health: $api_url"

    if has_command curl; then
        if curl -s --connect-timeout "$timeout" "$api_url/api/tags" >/dev/null 2>&1; then
            log "DEBUG" "Ollama is healthy"
            return 0
        else
            log "DEBUG" "Ollama is not responding"
            return 1
        fi
    else
        log "WARN" "curl not available, cannot check Ollama health"
        return 1
    fi
}

# 利用可能なOllamaモデルの取得（フィルター付き）
get_available_ollama_models() {
    local api_url="${1:-$(get_config "llm.ollama.api_url" "$DEFAULT_OLLAMA_API")}"

    if check_ollama_health "$api_url"; then
        local models_json=$(curl -s "$api_url/api/tags" 2>/dev/null)

        if validate_json "$models_json"; then
            local all_models=""
            if has_command jq; then
                all_models=$(echo "$models_json" | jq -r '.models[].name' 2>/dev/null)
            else
                # jqが利用できない場合の簡易パース
                all_models=$(echo "$models_json" | grep -o '"name":"[^"]*"' | cut -d'"' -f4)
            fi

            # coder, code, embedを含むモデルを除外
            echo "$all_models" | grep -v -E "(coder|embed|codellama)" | grep -v "^$"
        else
            log "ERROR" "Invalid JSON response from Ollama models API"
            return 1
        fi
    else
        log "WARN" "Ollama not available for model listing"
        return 1
    fi
}

# モデルサイズの推定（パラメータ数ベース）
estimate_model_size() {
    local model="$1"

    # モデル名から軽量性を推定（小さいほど軽量）
    case "$model" in
        *"2b"* | *"mini"* | *"tiny"* | *"1b"*) echo "2" ;;
        *"3b"* | *"small"*) echo "3" ;;
        *"7b"* | *"medium"*) echo "7" ;;
        *"13b"* | *"large"*) echo "13" ;;
        *"70b"* | *"xl"*) echo "70" ;;
        *) echo "10" ;; # 不明な場合は中程度と仮定
    esac
}

# 推奨軽量モデルのインストール促進
suggest_lightweight_models() {
    log "INFO" "Recommending lightweight models for installation:"
    echo "推奨軽量モデル（インストールコマンド）:"
    echo "  ollama pull gemma2:2b          # 最高速度・高品質（推奨）"
    echo "  ollama pull phi4-mini:latest   # 軽量・高速"
    echo "  ollama pull orca-mini:latest   # 軽量・バランス型"
    echo ""
    echo "gemma2:2bは15.4秒の最高速度を実現し、tmux環境に最適です。"
}

# 最軽量モデルの選択
select_lightest_available() {
    local available_models="$1"
    local lightest_model=""
    local lightest_size=999

    while IFS= read -r model; do
        [[ -z "$model" ]] && continue
        local size=$(estimate_model_size "$model")
        if [[ $size -lt $lightest_size ]]; then
            lightest_size=$size
            lightest_model="$model"
        fi
    done <<<"$available_models"

    echo "$lightest_model"
}

# 最適なモデルの選択
select_best_model() {
    local preferred_model="${1:-auto}"
    local task_type="${2:-general}"

    log "DEBUG" "Selecting best model: preferred=$preferred_model, task=$task_type"

    # 明示的にモデルが指定されている場合
    if [[ "$preferred_model" != "auto" ]]; then
        echo "$preferred_model"
        return 0
    fi

    # 利用可能なモデルを取得
    local available_models=$(get_available_ollama_models)

    if [[ -z "$available_models" ]]; then
        log "WARN" "No Ollama models available, using simple fallback"
        echo "simple"
        return 0
    fi

    # 軽量・高速モデルを優先（gemma2:2bを最優先に追加）
    local priority_models=()
    case "$task_type" in
        "coding" | "technical")
            priority_models=("gemma2:2b" "phi4-mini:latest" "orca-mini:latest" "qwen2.5-coder:3b")
            ;;
        "summary" | "general")
            priority_models=("gemma2:2b" "phi4-mini:latest" "orca-mini:latest")
            ;;
        *)
            priority_models=("gemma2:2b" "phi4-mini:latest" "orca-mini:latest")
            ;;
    esac

    # 優先順位に従ってモデルを選択
    for model in "${priority_models[@]}"; do
        if echo "$available_models" | grep -q "^$model$"; then
            log "DEBUG" "Selected model: $model"
            echo "$model"
            return 0
        fi
    done

    # 推奨モデルが見つからない場合、インストールを促進
    log "WARN" "Recommended lightweight models not found"
    suggest_lightweight_models

    # 既存モデルから最軽量のものを選択
    local lightest_model=$(select_lightest_available "$available_models")
    if [[ -n "$lightest_model" ]]; then
        log "INFO" "Using lightest available model: $lightest_model"
        echo "$lightest_model"
        return 0
    fi

    # フォールバック
    log "WARN" "No suitable models found, using simple fallback"
    echo "simple"
    return 0
}

# プロンプト生成（モデル別最適化）
generate_prompt() {
    local input_text="$1"
    local summary_type="$2"
    local model="$3"
    local context="$4"

    log "DEBUG" "Generating prompt: type=$summary_type, model=$model"

    # 入力テキストの前処理
    local max_chars=$(get_config "llm.max_input_chars" "2000")
    if [[ ${#input_text} -gt $max_chars ]]; then
        input_text="${input_text:0:$max_chars}..."
        log "DEBUG" "Input text truncated to $max_chars characters"
    fi

    # コンテキスト情報の追加
    local context_info=""
    if [[ -n "$context" ]]; then
        context_info="コンテキスト: $context\n\n"
    fi

    # 超高速化プロンプト（文字数制限: 10-15文字）
    local base_prompt=""
    case "$summary_type" in
        "brief")
            if [[ "$model" == "phi4-mini"* ]]; then
                base_prompt="10-15文字で状況：\n${context_info}${input_text}\n\n状況："
            elif [[ "$model" == "orca-mini"* ]]; then
                base_prompt="10-15文字で要約：\n${context_info}${input_text}\n\n要約："
            elif [[ "$model" == "qwen2.5-coder"* ]]; then
                base_prompt="10-15文字で開発状況：\n${context_info}${input_text}\n\n状況："
            else
                base_prompt="10-15文字で要約：\n${context_info}${input_text}\n\n要約："
            fi
            ;;
        "detailed")
            base_prompt="詳細分析を50-70文字で要約：\n${context_info}${input_text}\n\n分析："
            ;;
        "technical")
            base_prompt="技術的状況を30-50文字で分析：\n${context_info}${input_text}\n\n分析："
            ;;
        *)
            base_prompt="状況を20-30文字で要約：\n${context_info}${input_text}\n\n要約："
            ;;
    esac

    echo "$base_prompt"
}

# Ollama APIリクエストの実行（再試行機能付き）
execute_ollama_request() {
    local model="$1"
    local prompt="$2"
    local max_retries="${3:-$(get_config "llm.max_retries" "$DEFAULT_MAX_RETRIES")}"
    local api_url="${4:-$(get_config "llm.ollama.api_url" "$DEFAULT_OLLAMA_API")}"

    log "DEBUG" "Executing Ollama request: model=$model, retries=$max_retries"

    local base_delay=2

    for attempt in $(seq 1 $max_retries); do
        log "DEBUG" "Attempt $attempt/$max_retries"

        local start_time=$(start_timer)

        # JSON payloadの構築（jqを使用して安全に生成）
        local payload=""
        if has_command jq; then
            payload=$(jq -n \
                --arg model "$model" \
                --arg prompt "$prompt" \
                '{
                    model: $model,
                    prompt: $prompt,
                    stream: false,
                    options: {
                        temperature: 0.3,
                        top_p: 0.9,
                        num_predict: 200
                    }
                }')
        else
            # フォールバック: 手動でJSONを構築
            local escaped_prompt=$(escape_json_string "$prompt")
            payload=$(
                cat <<EOF
{
    "model": "$model",
    "prompt": "$escaped_prompt",
    "stream": false,
    "options": {
        "temperature": 0.3,
        "top_p": 0.9,
        "num_predict": 200
    }
}
EOF
            )
        fi

        # APIリクエストの実行（デバッグ情報追加）
        local response=""
        local curl_exit_code=0

        log "DEBUG" "Executing curl request to $api_url/api/generate"
        log "DEBUG" "Payload preview: ${payload:0:200}..."

        response=$(curl -s -X POST "$api_url/api/generate" \
            -H "Content-Type: application/json" \
            -d "$payload" \
            --max-time $(get_config "llm.timeout" "$DEFAULT_TIMEOUT") \
            --connect-timeout 10 \
            2>/dev/null)
        curl_exit_code=$?

        log "DEBUG" "curl exit code: $curl_exit_code"
        log "DEBUG" "Response preview: ${response:0:200}..."

        local duration=$(end_timer "$start_time")

        # レスポンスの検証
        if [[ $curl_exit_code -eq 0 ]] && [[ -n "$response" ]] && validate_json "$response"; then
            local summary_text=""
            if has_command jq; then
                summary_text=$(echo "$response" | jq -r '.response // empty' 2>/dev/null)
            else
                # jqが利用できない場合の簡易パース
                summary_text=$(echo "$response" | grep -o '"response":"[^"]*"' | sed 's/"response":"//' | sed 's/"$//')
            fi

            if [[ -n "$summary_text" && "$summary_text" != "null" ]]; then
                log "INFO" "Ollama request succeeded on attempt $attempt (${duration}s)"
                echo "$summary_text"
                return 0
            else
                log "WARN" "Empty response from Ollama on attempt $attempt"
            fi
        else
            log "WARN" "Ollama request failed on attempt $attempt (exit: $curl_exit_code, duration: ${duration}s)"
        fi

        # 最後の試行でない場合は待機
        if [[ $attempt -lt $max_retries ]]; then
            local delay=$((base_delay * attempt))
            log "DEBUG" "Waiting ${delay}s before retry..."
            sleep $delay
        fi
    done

    log "ERROR" "All $max_retries Ollama attempts failed"
    return 1
}

# 強化された簡易要約（フォールバック）
generate_simple_summary() {
    local input_text="$1"
    local summary_type="$2"
    local context="$3"

    log "DEBUG" "Generating simple summary (fallback)"

    # キーワードベースの分析
    local error_count=$(echo "$input_text" | grep -ci "error\|エラー\|失敗\|failed\|exception\|abort")
    local success_count=$(echo "$input_text" | grep -ci "success\|成功\|完了\|done\|completed\|finished")
    local warning_count=$(echo "$input_text" | grep -ci "warning\|警告\|warn")
    local command_count=$(echo "$input_text" | grep -c "⏺ Bash\|⏺ Update\|⏺ Write\|$\|#")

    # 技術的キーワードの検出
    local tech_keywords=$(echo "$input_text" | grep -ci "npm\|yarn\|docker\|git\|python\|node\|build\|test\|deploy")

    # 超コンパクトな基本要約の構築
    local summary=""

    # コンテキスト情報の追加
    if [[ -n "$context" ]]; then
        summary+="($context) "
    fi

    # 超コンパクト状態分析
    if [[ $error_count -gt 0 ]]; then
        summary+="エラー${error_count}件"
    elif [[ $success_count -gt 0 ]]; then
        summary+="成功${success_count}件"
    elif [[ $command_count -gt 0 ]]; then
        summary+="コマンド${command_count}個実行"
    elif [[ $tech_keywords -gt 0 ]]; then
        summary+="開発作業中"
    else
        summary+="作業中"
    fi

    log "INFO" "Simple summary generated: ${summary:0:100}..."
    echo "$summary"
}

# LLMプロバイダーの統合インターフェース
generate_llm_summary() {
    local input_text="$1"
    local summary_type="${2:-brief}"
    local preferred_model="${3:-auto}"
    local context="${4:-}"

    log "INFO" "Starting LLM summary generation"

    # 入力検証
    if [[ -z "$input_text" ]]; then
        log "ERROR" "No input text provided for LLM summary"
        echo "要約する内容がありません。"
        return 1
    fi

    # モデル選択
    local selected_model=$(select_best_model "$preferred_model" "$summary_type")
    log "DEBUG" "Selected model: $selected_model"

    # プロバイダー別処理
    if [[ "$selected_model" == "simple" ]]; then
        # 簡易要約の直接実行
        generate_simple_summary "$input_text" "$summary_type" "$context"
        return 0
    else
        # Ollama経由での処理を試行
        local prompt=$(generate_prompt "$input_text" "$summary_type" "$selected_model" "$context")

        if execute_ollama_request "$selected_model" "$prompt"; then
            return 0
        else
            # Ollamaが失敗した場合は簡易要約にフォールバック
            log "WARN" "Ollama failed, falling back to simple summary"
            generate_simple_summary "$input_text" "$summary_type" "$context"
            return 0
        fi
    fi
}

# このモジュールのテスト関数
test_llm_manager() {
    echo "Testing LLM manager module..."

    # Ollamaの健康状態チェック
    if check_ollama_health; then
        echo "Ollama health: OK"

        # 利用可能なモデルの取得
        local models=$(get_available_ollama_models)
        echo "Available models: $models"

        # モデル選択のテスト
        local selected=$(select_best_model "auto" "summary")
        echo "Selected model: $selected"
    else
        echo "Ollama health: NOT AVAILABLE"
    fi

    # 簡易要約のテスト
    local test_text="⏺ Bash(ls -la) ✅ 5 files found ❌ Error: permission denied"
    local summary=$(generate_simple_summary "$test_text" "brief")
    echo "Simple summary test: $summary"

    echo "LLM manager test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    source "$SCRIPT_DIR/base.sh"

    claude_voice_init true
    test_llm_manager
fi
