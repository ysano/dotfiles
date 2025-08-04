#!/bin/bash
# Ollama Cross-Platform Integration - Phase 2 Unified
# クロスプラットフォームOllama統合システム

set -euo pipefail

readonly SCRIPT_VERSION="2.0.0"
readonly SCRIPT_NAME="Ollama Cross-Platform Integration"

# === 環境変数 ===
export OLLAMA_CROSS_DEBUG="${OLLAMA_CROSS_DEBUG:-false}"

# === ログ関数 ===
log() {
    local level="$1"
    shift
    if [[ "${OLLAMA_CROSS_DEBUG:-false}" == "true" ]] || [[ "$level" == "ERROR" ]]; then
        echo "[$(date '+%H:%M:%S')] [OLLAMA-CROSS] [$level] $*" >&2
    fi
}

# === プラットフォーム検出 ===
detect_platform() {
    case "$(uname)" in
        "Darwin")
            echo "macos"
            ;;
        "Linux")
            if grep -qi microsoft /proc/version 2>/dev/null || [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
                echo "wsl"
            else
                echo "linux"
            fi
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# === プラットフォーム別Ollama URL解決 ===
get_ollama_url() {
    local platform=$(detect_platform)
    
    case "$platform" in
        "macos"|"linux")
            echo "http://localhost:11434"
            ;;
        "wsl")
            # WSL環境での動的IP解決
            local windows_host_ip
            if windows_host_ip=$(grep nameserver /etc/resolv.conf | awk '{print $2}' | head -1 2>/dev/null); then
                if [[ -n "$windows_host_ip" && "$windows_host_ip" != "127.0.0.1" ]]; then
                    echo "http://${windows_host_ip}:11434"
                    log "DEBUG" "WSL dynamic IP resolved: $windows_host_ip"
                    return 0
                fi
            fi
            
            # フォールバック
            echo "http://localhost:11434"
            log "WARN" "WSL dynamic IP resolution failed, using localhost"
            ;;
        *)
            echo "http://localhost:11434"
            ;;
    esac
}

# === Ollama健康状態チェック ===
check_ollama_health() {
    local ollama_url="${1:-$(get_ollama_url)}"
    local timeout="${2:-5}"
    
    log "DEBUG" "Checking Ollama health at: $ollama_url"
    
    if command -v curl >/dev/null 2>&1; then
        if curl -s --connect-timeout "$timeout" "$ollama_url/api/tags" >/dev/null 2>&1; then
            log "INFO" "Ollama health check: OK"
            return 0
        else
            log "WARN" "Ollama health check: Failed"
            return 1
        fi
    else
        log "ERROR" "curl not available for health check"
        return 1
    fi
}

# === 利用可能モデル取得 ===
get_available_models() {
    local ollama_url="${1:-$(get_ollama_url)}"
    
    if ! check_ollama_health "$ollama_url"; then
        log "ERROR" "Ollama not available for model query"
        return 1
    fi
    
    local models_json
    if models_json=$(curl -s "$ollama_url/api/tags" 2>/dev/null); then
        echo "$models_json" | grep -o '"name":"[^"]*"' | cut -d'"' -f4 | head -10
    else
        log "ERROR" "Failed to retrieve models"
        return 1
    fi
}

# === 推奨モデルの選択 ===
select_best_model() {
    local available_models
    available_models=$(get_available_models) || {
        log "ERROR" "Cannot retrieve available models"
        return 1
    }
    
    # 推奨モデルの優先順位
    local preferred_models=("gemma3:1b-it-qat" "gemma3:1b" "gemma2:2b" "phi4-mini" "llama3.2:3b" "tinyllama")
    
    for preferred in "${preferred_models[@]}"; do
        if echo "$available_models" | grep -q "^${preferred}"; then
            echo "$preferred"
            log "INFO" "Selected model: $preferred"
            return 0
        fi
    done
    
    # フォールバック: 最初の利用可能モデル
    local first_model
    first_model=$(echo "$available_models" | head -1)
    if [[ -n "$first_model" ]]; then
        echo "$first_model"
        log "INFO" "Using first available model: $first_model"
        return 0
    fi
    
    log "ERROR" "No suitable models available"
    return 1
}

# === Ollama API実行 ===
execute_ollama_request() {
    local model="$1"
    local prompt="$2"
    local ollama_url="${3:-$(get_ollama_url)}"
    local timeout="${4:-30}"
    
    log "DEBUG" "Executing Ollama request: model=$model, url=$ollama_url"
    
    if ! check_ollama_health "$ollama_url" 3; then
        log "ERROR" "Ollama not available for request execution"
        return 1
    fi
    
    local request_json
    request_json=$(cat <<EOF
{
    "model": "$model",
    "prompt": "$prompt",
    "stream": false,
    "options": {
        "temperature": 0.7,
        "num_predict": 100
    }
}
EOF
)
    
    local response
    if response=$(curl -s --connect-timeout "$timeout" \
                      -H "Content-Type: application/json" \
                      -d "$request_json" \
                      "$ollama_url/api/generate" 2>/dev/null); then
        
        local ollama_response
        ollama_response=$(echo "$response" | grep -o '"response":"[^"]*"' | cut -d'"' -f4 | head -1)
        
        if [[ -n "$ollama_response" ]]; then
            echo "$ollama_response"
            log "INFO" "Ollama request completed successfully"
            return 0
        else
            log "ERROR" "Empty response from Ollama"
            return 1
        fi
    else
        log "ERROR" "Ollama request failed"
        return 1
    fi
}

# === 診断機能 ===
diagnose() {
    echo "=== $SCRIPT_NAME Diagnostics ==="
    echo "Version: $SCRIPT_VERSION"
    echo
    
    local platform=$(detect_platform)
    echo "Platform: $platform"
    
    local ollama_url=$(get_ollama_url)
    echo "Ollama URL: $ollama_url"
    
    echo
    echo "Health Check:"
    if check_ollama_health "$ollama_url"; then
        echo "  ✅ Ollama connection: OK"
        
        echo "  Available Models:"
        get_available_models | while read -r model; do
            echo "    - $model"
        done
        
        local best_model
        if best_model=$(select_best_model); then
            echo "  Recommended Model: $best_model"
        fi
    else
        echo "  ❌ Ollama connection: Failed"
        echo "  Fix: Check if Ollama is running (ollama serve)"
    fi
    
    echo
    echo "=== End Diagnostics ==="
}

# === テスト機能 ===
test_ollama() {
    echo "=== $SCRIPT_NAME Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: プラットフォーム検出
    echo "Test 1: Platform detection"
    ((tests_total++))
    local platform=$(detect_platform)
    if [[ -n "$platform" && "$platform" != "unknown" ]]; then
        echo "✅ PASS - Platform: $platform"
        ((tests_passed++))
    else
        echo "❌ FAIL - Platform detection"
    fi
    
    # Test 2: URL解決
    echo "Test 2: Ollama URL resolution"
    ((tests_total++))
    local ollama_url=$(get_ollama_url)
    if [[ "$ollama_url" =~ ^http://.*:11434$ ]]; then
        echo "✅ PASS - URL: $ollama_url"
        ((tests_passed++))
    else
        echo "❌ FAIL - Invalid URL: $ollama_url"
    fi
    
    # Test 3: 健康状態チェック
    echo "Test 3: Health check"
    ((tests_total++))
    if check_ollama_health "$ollama_url"; then
        echo "✅ PASS - Health check"
        ((tests_passed++))
    else
        echo "❌ FAIL - Health check"
    fi
    
    # Test 4: 基本リクエスト（健康状態がOKの場合のみ）
    if [[ $tests_passed -eq $tests_total ]]; then
        echo "Test 4: Basic request"
        ((tests_total++))
        local model
        if model=$(select_best_model); then
            if execute_ollama_request "$model" "Test prompt" "$ollama_url" 10 >/dev/null; then
                echo "✅ PASS - Basic request"
                ((tests_passed++))
            else
                echo "❌ FAIL - Basic request"
            fi
        else
            echo "❌ FAIL - No model available"
        fi
    fi
    
    echo
    echo "Test Results: $tests_passed/$tests_total passed"
    
    if [[ $tests_passed -eq $tests_total ]]; then
        echo "🎉 All tests passed!"
        return 0
    else
        echo "⚠️ Some tests failed"
        return 1
    fi
}

# === メイン処理 ===
main() {
    case "${1:-help}" in
        "url")
            get_ollama_url
            ;;
        "health")
            check_ollama_health
            ;;
        "models")
            get_available_models
            ;;
        "best-model")
            select_best_model
            ;;
        "request")
            shift
            execute_ollama_request "$@"
            ;;
        "diagnose")
            diagnose
            ;;
        "test")
            test_ollama
            ;;
        "help"|"-h"|"--help")
            cat << EOF
$SCRIPT_NAME - Usage Guide

COMMANDS:
  url                                     - プラットフォーム別Ollama URL表示
  health [url]                           - 健康状態チェック
  models [url]                           - 利用可能モデル一覧
  best-model                             - 推奨モデル選択
  request <model> <prompt> [url] [timeout] - Ollama API実行
  diagnose                               - システム診断
  test                                   - テスト実行
  help                                   - このヘルプ

EXAMPLES:
  $0 url
  $0 health
  $0 models
  $0 request "gemma2:2b" "Hello world"
  $0 diagnose
  $0 test

EOF
            ;;
        *)
            echo "Unknown command: $1" >&2
            main "help"
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi