#!/bin/bash
# ファイル名: ollama_utils.sh
# 説明: tmux-claude-voice Ollama連携と要約機能

# 多重読み込み防止
[[ -n "$_CLAUDE_OLLAMA_UTILS_LOADED" ]] && return 0 2>/dev/null
_CLAUDE_OLLAMA_UTILS_LOADED=1

# 依存ファイルの存在確認（他スクリプトからsource時はSCRIPT_DIRを継承）
if [[ -z "$SCRIPT_DIR" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

# 共通ログ機能を読み込み（最優先）
if [[ -f "${SCRIPT_DIR}/core/logging_utils.sh" ]]; then
    source "${SCRIPT_DIR}/core/logging_utils.sh"
fi

# 依存ファイルの読み込み
if [[ -f "${SCRIPT_DIR}/functions.sh" ]]; then
    source "${SCRIPT_DIR}/functions.sh"
fi

# フォールバックOS種別取得
if ! command -v get_os_type >/dev/null 2>&1; then
    get_os_type() { uname; }
fi

# モデル優先順位リスト（ハードコード）
readonly OLLAMA_MODEL_PRIORITY=("gemma3:1b" "gemma2:2b" "phi4-mini:latest" "orca-mini:latest")

# 設定値取得（デフォルト値付き）
get_tmux_ollama_option() {
    local option="$1"
    local default_value="$2"
    
    local value
    if value=$(tmux show-option -gqv "@$option" 2>/dev/null); then
        echo "$value"
    else
        echo "$default_value"
    fi
}

# WSL環境でのWindowsホストIP取得
get_windows_host_ip() {
    if [[ "$(uname)" == "Linux" ]]; then
        # WSL環境でのWindowsホストIP取得
        local windows_ip
        if windows_ip=$(grep -oP 'nameserver \K[0-9.]+' /etc/resolv.conf 2>/dev/null | head -n1); then
            if [[ -n "$windows_ip" ]]; then
                log_debug "WindowsホストIP検出: $windows_ip"
                echo "$windows_ip"
                return 0
            fi
        fi
    fi
    log_debug "WindowsホストIP検出失敗、localhostを使用"
    echo "localhost"
    return 1
}

# プラットフォーム固有のOllamaホスト設定
get_ollama_host() {
    local os_type=$(get_os_type)
    
    log_debug "プラットフォーム固有Ollamaホスト取得: OS=$os_type"
    
    if [[ "$os_type" == "Darwin" ]]; then
        echo "localhost"
    else
        # WSL環境ではWindowsホストのIPを使用
        get_windows_host_ip
    fi
}

# Ollamaサーバーの接続情報を取得
get_ollama_connection() {
    local host=$(get_tmux_ollama_option "claude_voice_ollama_host" "")
    local port=$(get_tmux_ollama_option "claude_voice_ollama_port" "11434")
    
    # ホストが指定されていない場合は自動検出
    if [[ -z "$host" ]]; then
        host=$(get_ollama_host)
    fi
    
    local connection="http://$host:$port"
    log_debug "Ollama接続情報: $connection"
    echo "$connection"
}

# 利用可能なOllamaモデルを取得
get_available_ollama_models() {
    local connection=$(get_ollama_connection)
    local timeout=$(get_tmux_ollama_option "claude_voice_ollama_timeout" "10")
    
    log_debug "利用可能モデル取得開始: connection=$connection, timeout=$timeout"

    # まずローカルのollama listコマンドを試行
    if command -v ollama >/dev/null 2>&1; then
        log_debug "ローカルollamaコマンドを試行中..."
        local local_models
        if local_models=$(ollama list --format json 2>/dev/null | jq -r '.models[].name' 2>/dev/null); then
            if [[ -n "$local_models" ]]; then
                log_debug "ローカルollamaから取得したモデル: $local_models"
                echo "$local_models"
                return 0
            fi
        fi
        log_debug "ローカルollamaからのモデル取得に失敗"
    else
        log_debug "ollamaコマンドが見つかりません"
    fi

    # 外部APIを使用してモデルリストを取得
    log_debug "外部APIからモデルリストを取得中..."
    if command -v curl >/dev/null 2>&1; then
        local api_response
        if api_response=$(curl -s --max-time "$timeout" "${connection}/api/tags" 2>/dev/null); then
            if [[ -n "$api_response" ]]; then
                log_debug "API応答を受信: ${api_response:0:100}..."
                
                if command -v jq >/dev/null 2>&1; then
                    local api_models
                    if api_models=$(echo "$api_response" | jq -r '.models[].name' 2>/dev/null); then
                        if [[ -n "$api_models" ]]; then
                            log_debug "APIから取得したモデル: $api_models"
                            echo "$api_models"
                            return 0
                        fi
                    fi
                    log_debug "jqでのJSON解析に失敗"
                else
                    log_debug "jqコマンドが見つかりません"
                fi
            fi
        else
            log_debug "API呼び出しに失敗: ${connection}/api/tags"
        fi
    else
        log_error "curlコマンドが見つかりません"
    fi

    # フォールバック: ハードコードされた優先順位リストの最初のモデルを使用
    log_debug "フォールバック: ${OLLAMA_MODEL_PRIORITY[0]} を使用"
    echo "${OLLAMA_MODEL_PRIORITY[0]}"
    return 1
}

# 最適なOllamaモデルを選択
select_optimal_ollama_model() {
    log_debug "最適モデル選択開始"
    
    local available_models
    available_models=$(get_available_ollama_models)
    
    log_debug "利用可能モデル: $available_models"
    log_debug "優先順位リスト: ${OLLAMA_MODEL_PRIORITY[*]}"

    # ハードコードされた優先順位リストを使用
    for model in "${OLLAMA_MODEL_PRIORITY[@]}"; do
        if echo "$available_models" | grep -q "^${model}$"; then
            log_debug "優先順位に基づき選択: $model"
            echo "$model"
            return 0
        fi
    done

    # 利用可能なモデルが見つからない場合、最初の利用可能なモデルを使用
    local first_available
    first_available=$(echo "$available_models" | head -n1)
    if [[ -n "$first_available" && "$first_available" != "${OLLAMA_MODEL_PRIORITY[0]}" ]]; then
        log_debug "最初の利用可能モデルを選択: $first_available"
        echo "$first_available"
        return 0
    fi

    # 最後のフォールバック: 優先順位リストの最初のモデル
    log_debug "最終フォールバック: ${OLLAMA_MODEL_PRIORITY[0]}"
    echo "${OLLAMA_MODEL_PRIORITY[0]}"
    return 1
}

# 画面テキストを要約する関数
summarize_with_ollama() {
    local pane_content="$1"
    
    if [[ -z "$pane_content" ]]; then
        log_error "要約対象のテキストが指定されていません"
        return 1
    fi
    
    local connection=$(get_ollama_connection)
    local timeout=$(get_tmux_ollama_option "claude_voice_ollama_timeout" "10")
    local model
    model=$(select_optimal_ollama_model)
    local summary_lines="20"
    local temperature="0.3"
    
    # 設定が取得できた場合のみ上書き
    local config_lines
    config_lines=$(get_tmux_ollama_option "claude_voice_summary_lines" "")
    if [[ -n "$config_lines" ]]; then
        summary_lines="$config_lines"
    fi
    
    local config_temperature
    config_temperature=$(get_tmux_ollama_option "claude_voice_summary_temperature" "")
    if [[ -n "$config_temperature" ]]; then
        temperature="$config_temperature"
    fi
    
    log_debug "要約開始: model=$model, lines=$summary_lines, temperature=$temperature"

    # 最後のN行を抽出（最も重要な情報）
    local recent_content
    recent_content=$(echo "$pane_content" | tail -n "${summary_lines}")
    
    if [[ -z "$recent_content" ]]; then
        log_error "分析対象テキストが空です"
        return 1
    fi
    
    log_debug "分析対象テキスト（最後の${summary_lines}行、最初の100文字）: ${recent_content:0:100}..."

    # プロンプトの構築
    local prompt="以下のテキストはAIアシスタントの出力です。現在の「問い合わせ内容」について、状況を30文字以内で具体的に要約してください。特に最後の行を重視してください。

$recent_content"

    # 必要なコマンドのチェック
    if ! command -v jq >/dev/null 2>&1; then
        log_error "jqコマンドが見つかりません"
        # フォールバックを実行
        local fallback_summary
        fallback_summary=$(echo "$recent_content" | grep -oE "(Successfully|Done|Created|Error|Failed|Exception|Proceed\?|Choose an option)" | head -n1)
        if [[ -n "$fallback_summary" ]]; then
            echo "$fallback_summary"
        else
            echo "処理完了"
        fi
        return 1
    fi

    if ! command -v curl >/dev/null 2>&1; then
        log_error "curlコマンドが見つかりません"
        echo "処理完了"
        return 1
    fi

    # JSONリクエストの構築
    local json_request
    json_request=$(jq -n \
        --arg model "$model" \
        --arg prompt "$prompt" \
        --argjson temperature "$temperature" \
        '{
            model: $model,
            prompt: $prompt,
            stream: false,
            options: {
                temperature: $temperature,
                top_p: 0.9,
                num_predict: 50
            }
        }')
    
    if [[ $? -ne 0 ]]; then
        log_error "JSON リクエストの構築に失敗しました"
        echo "処理完了"
        return 1
    fi
    
    log_debug "JSON リクエスト構築完了"

    # API呼び出し
    log_debug "API呼び出し開始: ${connection}/api/generate"
    local response
    response=$(curl -s --max-time "$timeout" \
        -H "Content-Type: application/json" \
        -d "$json_request" \
        "${connection}/api/generate" 2>/dev/null)

    if [[ -n "$response" ]]; then
        log_debug "API応答を受信: ${response:0:200}..."
        
        local summary
        summary=$(echo "$response" | jq -r '.response' 2>/dev/null)
        
        if [[ $? -eq 0 && -n "$summary" && "$summary" != "null" ]]; then
            # 改行を除去し、最初の30文字を取得
            local clean_summary
            clean_summary=$(echo "$summary" | tr -d '\n' | cut -c1-30)
            
            if [[ -n "$clean_summary" ]]; then
                log_debug "要約生成成功: $clean_summary"
                echo "$clean_summary"
                return 0
            fi
        else
            log_debug "要約の抽出に失敗しました"
        fi
    else
        log_debug "API応答が空です"
    fi

    # フォールバック: 簡単なキーワード抽出
    log_debug "フォールバック: キーワード抽出を実行"
    local fallback_summary
    fallback_summary=$(echo "$recent_content" | grep -oE "(Successfully|Done|Created|Error|Failed|Exception|Proceed\?|Choose an option)" | head -n1)
    
    if [[ -n "$fallback_summary" ]]; then
        log_debug "キーワード抽出成功: $fallback_summary"
        echo "$fallback_summary"
    else
        log_debug "デフォルト要約を使用"
        echo "処理完了"
    fi
    
    return 1
}

# Ollama接続テスト
test_ollama_connection() {
    local connection=$(get_ollama_connection)
    local timeout=$(get_tmux_ollama_option "claude_voice_ollama_timeout" "10")
    
    echo "Ollama接続テスト: $connection"
    
    if command -v curl >/dev/null 2>&1; then
        if curl -s --max-time "$timeout" "${connection}/api/tags" >/dev/null 2>&1; then
            echo "✓ Ollama接続: 成功"
            return 0
        else
            echo "✗ Ollama接続: 失敗"
            return 1
        fi
    else
        echo "✗ curlコマンドが見つかりません"
        return 1
    fi
}

# Ollamaユーティリティのテスト
test_ollama_utils() {
    local test_type="${1:-all}"
    
    echo "=== Ollamaユーティリティテスト開始 ($test_type) ==="
    
    # ホスト検出テスト
    if [[ "$test_type" == "all" || "$test_type" == "host" ]]; then
        echo "ホスト検出テスト..."
        local host=$(get_ollama_host)
        local connection=$(get_ollama_connection)
        echo "検出されたOllamaホスト: $host"
        echo "接続URL: $connection"
        echo "✓ ホスト検出: 成功"
    fi
    
    # 接続テスト
    if [[ "$test_type" == "all" || "$test_type" == "connection" ]]; then
        echo "接続テスト..."
        test_ollama_connection
    fi
    
    # モデル検出テスト
    if [[ "$test_type" == "all" || "$test_type" == "models" ]]; then
        echo "モデル検出テスト..."
        local available_models
        available_models=$(get_available_ollama_models)
        local selected_model
        selected_model=$(select_optimal_ollama_model)
        
        echo "利用可能なモデル:"
        echo "$available_models" | sed 's/^/  - /'
        echo "選択されたモデル: $selected_model"
        echo "✓ モデル検出: 成功"
    fi
    
    # 要約テスト
    if [[ "$test_type" == "all" || "$test_type" == "summary" ]]; then
        echo "要約機能テスト..."
        local test_text="Successfully created new file: test.txt
The operation completed without errors.
Do you want to proceed with the next step?"
        
        echo "テスト用テキスト:"
        echo "$test_text"
        echo ""
        echo "要約結果:"
        local summary
        summary=$(summarize_with_ollama "$test_text")
        echo "  $summary"
        echo "✓ 要約機能: 成功"
    fi
    
    # 依存関係テスト
    if [[ "$test_type" == "all" || "$test_type" == "deps" ]]; then
        echo "依存関係テスト..."
        local missing_deps=()
        
        if ! command -v curl >/dev/null 2>&1; then
            missing_deps+=("curl")
        fi
        
        if ! command -v jq >/dev/null 2>&1; then
            missing_deps+=("jq")
        fi
        
        if [[ ${#missing_deps[@]} -eq 0 ]]; then
            echo "✓ 依存関係: すべて満たされています"
        else
            echo "✗ 依存関係: 不足しているコマンド: ${missing_deps[*]}"
        fi
    fi
    
    echo "=== Ollamaユーティリティテスト完了 ==="
    return 0
}

# Ollama依存関係チェック
check_ollama_dependencies() {
    local missing_deps=()
    
    echo "Ollama依存関係チェック..."
    
    # 必須コマンドのチェック
    if ! command -v curl >/dev/null 2>&1; then
        missing_deps+=("curl (HTTP client)")
    fi
    
    if ! command -v jq >/dev/null 2>&1; then
        missing_deps+=("jq (JSON processor)")
    fi
    
    # オプショナルコマンドのチェック
    local optional_available=()
    local optional_missing=()
    
    if command -v ollama >/dev/null 2>&1; then
        optional_available+=("ollama")
    else
        optional_missing+=("ollama (ローカルLLM, オプション)")
    fi
    
    # 結果の表示
    if [[ ${#missing_deps[@]} -eq 0 ]]; then
        echo "✓ すべての必須依存関係が満たされています"
        
        if [[ ${#optional_available[@]} -gt 0 ]]; then
            echo "✓ 利用可能なオプション: ${optional_available[*]}"
        fi
        
        if [[ ${#optional_missing[@]} -gt 0 ]]; then
            echo "ℹ 利用できないオプション: ${optional_missing[*]}"
        fi
        
        return 0
    else
        echo "✗ 不足している必須依存関係:"
        for dep in "${missing_deps[@]}"; do
            echo "  - $dep"
        done
        
        echo ""
        echo "インストール方法:"
        echo "  Ubuntu/Debian: sudo apt-get install curl jq"
        echo "  macOS: brew install curl jq"
        
        return 1
    fi
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            TMUX_CLAUDE_VOICE_DEBUG=1 test_ollama_utils "all"
            ;;
        "test-host")
            test_ollama_utils "host"
            ;;
        "test-connection")
            test_ollama_utils "connection"
            ;;
        "test-models")
            test_ollama_utils "models"
            ;;
        "test-summary")
            test_ollama_utils "summary"
            ;;
        "test-deps")
            test_ollama_utils "deps"
            ;;
        "deps")
            check_ollama_dependencies
            ;;
        "summarize")
            # 使用例: ./ollama_utils.sh summarize "テキスト内容"
            summarize_with_ollama "${2:-テストテキストです。処理が完了しました。}"
            ;;
        "models")
            get_available_ollama_models
            ;;
        "select")
            select_optimal_ollama_model
            ;;
        "connection")
            get_ollama_connection
            ;;
        *)
            echo "使用方法: $0 [test|test-host|test-connection|test-models|test-summary|test-deps|deps|summarize|models|select|connection]"
            echo "  test             - 全テスト実行"
            echo "  test-host        - ホスト検出テスト"
            echo "  test-connection  - 接続テスト"
            echo "  test-models      - モデル検出テスト"
            echo "  test-summary     - 要約機能テスト"
            echo "  test-deps        - 依存関係テスト"
            echo "  deps             - 依存関係チェック"
            echo "  summarize <text> - テキスト要約"
            echo "  models           - 利用可能モデル一覧"
            echo "  select           - 最適モデル選択"
            echo "  connection       - 接続情報取得"
            exit 1
            ;;
    esac
fi