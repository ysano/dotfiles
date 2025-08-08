#!/bin/bash
# Centralized Configuration Manager - Unified config handling
# 統一設定管理 - 設定ファイルの統一管理

set -euo pipefail

readonly CONFIG_MANAGER_VERSION="1.0.0"

# 設定ファイルパス
readonly TMUX_MINIMAL_CONFIG="${TMUX_MINIMAL_CONFIG:-$HOME/.tmux/tmux-minimal.yaml}"
readonly AUDIO_MODE_CONFIG="${AUDIO_MODE_CONFIG:-$HOME/.tmux/claude/config/audio_mode.conf}"

# 設定キャッシュ
declare -A CONFIG_CACHE=()
readonly CONFIG_CACHE_TTL=30

# ログ用（logging.shが読み込まれていない場合の対策）
if ! declare -F log_config >/dev/null 2>&1; then
    log_config() {
        echo "[$(date '+%H:%M:%S')] [CONFIG] [$1] ${*:2}" >&2
    }
fi

# === YAML設定読み込み ===
read_yaml_config() {
    local config_file="${1:-$TMUX_MINIMAL_CONFIG}"
    local key_path="$2"
    
    if [[ ! -f "$config_file" ]]; then
        log_config "ERROR" "Config file not found: $config_file"
        return 1
    fi
    
    # yqが利用可能な場合
    if command -v yq >/dev/null 2>&1; then
        yq eval ".$key_path" "$config_file" 2>/dev/null || echo ""
        return 0
    fi
    
    # フォールバック: 簡易YAML解析
    _parse_yaml_simple "$config_file" "$key_path"
}

# === 簡易YAML解析 ===
_parse_yaml_simple() {
    local config_file="$1"
    local key_path="$2"
    
    # ドット記法をアンダースコア区切りに変換（例: system.audio_mode -> system_audio_mode）
    local search_key=$(echo "$key_path" | tr '.' '_')
    
    # パターンマッチングによる値抽出
    local value
    if value=$(grep -E "^\s*${search_key}\s*:" "$config_file" 2>/dev/null | head -1); then
        # 値部分の抽出（クォート除去）
        echo "$value" | sed -E 's/^[^:]*:\s*"?([^"]*)"?\s*$/\1/'
    else
        # 階層構造の対応（基本的なもののみ）
        IFS='.' read -ra key_parts <<< "$key_path"
        if [[ ${#key_parts[@]} -eq 2 ]]; then
            local section="${key_parts[0]}"
            local key="${key_parts[1]}"
            
            # セクション内での検索
            if awk "/^${section}:/{flag=1; next} /^[^[:space:]]/{flag=0} flag && /${key}:/{print; exit}" "$config_file" | grep -q .; then
                awk "/^${section}:/{flag=1; next} /^[^[:space:]]/{flag=0} flag && /${key}:/{gsub(/^[^:]*:\s*\"?|\"?\s*$/, \"\"); print; exit}" "$config_file"
            fi
        fi
    fi
}

# === 設定値取得（キャッシュ対応） ===
get_config() {
    local key_path="$1"
    local default_value="${2:-}"
    local cache_key="config_${key_path}"
    local current_time=$(date +%s)
    
    # キャッシュチェック
    if [[ -n "${CONFIG_CACHE[$cache_key]:-}" ]]; then
        local cached_entry="${CONFIG_CACHE[$cache_key]}"
        local cached_time=$(echo "$cached_entry" | cut -d'|' -f1)
        local cached_value=$(echo "$cached_entry" | cut -d'|' -f2-)
        
        if [[ $((current_time - cached_time)) -lt $CONFIG_CACHE_TTL ]]; then
            echo "$cached_value"
            return 0
        fi
    fi
    
    # 設定値読み込み
    local value
    value=$(read_yaml_config "$TMUX_MINIMAL_CONFIG" "$key_path")
    
    # デフォルト値の適用
    if [[ -z "$value" ]]; then
        value="$default_value"
    fi
    
    # キャッシュ保存
    CONFIG_CACHE["$cache_key"]="${current_time}|${value}"
    
    echo "$value"
}

# === 音声モード設定専用 ===
get_audio_mode() {
    if [[ -f "$AUDIO_MODE_CONFIG" ]]; then
        grep '^CURRENT_MODE=' "$AUDIO_MODE_CONFIG" 2>/dev/null | cut -d'=' -f2 || echo "auto"
    else
        get_config "system.audio_mode" "auto"
    fi
}

set_audio_mode() {
    local mode="$1"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # ディレクトリ作成
    mkdir -p "$(dirname "$AUDIO_MODE_CONFIG")" 2>/dev/null || true
    
    # 設定ファイル更新
    cat > "$AUDIO_MODE_CONFIG" <<EOF
# Audio Mode Configuration
# Current mode: auto, audio, notification, beep, silent
CURRENT_MODE=$mode
LAST_UPDATED=$timestamp
EOF
    
    # キャッシュクリア
    _clear_config_cache "audio_mode"
    
    log_config "INFO" "Audio mode set to: $mode"
}

# === 設定検証 ===
validate_config() {
    local config_file="${1:-$TMUX_MINIMAL_CONFIG}"
    local errors=0
    
    log_config "INFO" "Validating config file: $config_file"
    
    # ファイル存在チェック
    if [[ ! -f "$config_file" ]]; then
        log_config "ERROR" "Config file does not exist: $config_file"
        return 1
    fi
    
    # YAML構文チェック（yq利用可能時）
    if command -v yq >/dev/null 2>&1; then
        if ! yq eval '.' "$config_file" >/dev/null 2>&1; then
            log_config "ERROR" "Invalid YAML syntax in: $config_file"
            ((errors++))
        fi
    fi
    
    # 必須キーの存在チェック
    local required_keys=(
        "claude_voice.enabled"
        "ollama.models.primary"
        "system.audio_mode"
    )
    
    for key in "${required_keys[@]}"; do
        local value=$(read_yaml_config "$config_file" "$key")
        if [[ -z "$value" ]]; then
            log_config "WARN" "Missing or empty required key: $key"
        fi
    done
    
    # 音声モード設定の値チェック
    local audio_mode=$(get_config "system.audio_mode")
    case "$audio_mode" in
        "auto"|"audio"|"notification"|"beep"|"silent")
            log_config "DEBUG" "Valid audio mode: $audio_mode"
            ;;
        *)
            log_config "WARN" "Invalid audio mode: $audio_mode"
            ;;
    esac
    
    if [[ $errors -eq 0 ]]; then
        log_config "INFO" "Configuration validation passed"
        return 0
    else
        log_config "ERROR" "Configuration validation failed with $errors errors"
        return 1
    fi
}

# === キャッシュ管理 ===
_clear_config_cache() {
    local pattern="${1:-}"
    
    if [[ -n "$pattern" ]]; then
        # 特定パターンのキャッシュクリア
        for key in "${!CONFIG_CACHE[@]}"; do
            if [[ "$key" == *"$pattern"* ]]; then
                unset CONFIG_CACHE["$key"]
            fi
        done
    else
        # 全キャッシュクリア
        CONFIG_CACHE=()
    fi
    
    log_config "DEBUG" "Config cache cleared: ${pattern:-all}"
}

clear_config_cache() {
    _clear_config_cache "$@"
}

# === 設定情報表示 ===
show_config_status() {
    echo "=== Configuration Manager Status ==="
    echo "Version: $CONFIG_MANAGER_VERSION"
    echo "Main Config: $TMUX_MINIMAL_CONFIG"
    echo "Audio Config: $AUDIO_MODE_CONFIG"
    echo "Cache Entries: ${#CONFIG_CACHE[@]}"
    echo ""
    
    # 主要設定値の表示
    echo "Current Settings:"
    echo "  Audio Mode: $(get_audio_mode)"
    echo "  Voice Provider: $(get_config "claude_voice.provider" "unknown")"
    echo "  Primary Model: $(get_config "ollama.models.primary" "unknown")"
    echo "  Log Level: $(get_config "system.log_level" "INFO")"
    
    # ファイル状態
    echo ""
    echo "File Status:"
    if [[ -f "$TMUX_MINIMAL_CONFIG" ]]; then
        echo "  ✅ Main config exists ($(stat -c%s "$TMUX_MINIMAL_CONFIG" 2>/dev/null || echo 'unknown') bytes)"
    else
        echo "  ❌ Main config missing"
    fi
    
    if [[ -f "$AUDIO_MODE_CONFIG" ]]; then
        echo "  ✅ Audio config exists"
    else
        echo "  ❌ Audio config missing"
    fi
}

# === 設定のリセット ===
reset_config() {
    local config_type="${1:-audio}"
    
    case "$config_type" in
        "audio")
            set_audio_mode "auto"
            log_config "INFO" "Audio configuration reset to defaults"
            ;;
        "cache")
            clear_config_cache
            log_config "INFO" "Configuration cache cleared"
            ;;
        *)
            log_config "ERROR" "Unknown config type: $config_type"
            return 1
            ;;
    esac
}

# === メイン実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-status}" in
        "status")
            show_config_status
            ;;
        "get")
            if [[ -n "${2:-}" ]]; then
                get_config "$2" "${3:-}"
            else
                echo "Usage: $0 get <key_path> [default_value]"
                exit 1
            fi
            ;;
        "validate")
            validate_config "${2:-}"
            ;;
        "clear-cache")
            clear_config_cache "${2:-}"
            echo "Config cache cleared"
            ;;
        "reset")
            reset_config "${2:-audio}"
            ;;
        "audio-mode")
            case "${2:-get}" in
                "get")
                    get_audio_mode
                    ;;
                "set")
                    if [[ -n "${3:-}" ]]; then
                        set_audio_mode "$3"
                    else
                        echo "Usage: $0 audio-mode set <mode>"
                        exit 1
                    fi
                    ;;
                *)
                    echo "Usage: $0 audio-mode {get|set}"
                    exit 1
                    ;;
            esac
            ;;
        "test")
            echo "Testing configuration manager..."
            echo "Platform capabilities: $(get_config "platform.wsl.clipboard" "unknown")"
            echo "Ollama URL: $(get_config "ollama.url" "unknown")"
            echo "Audio mode: $(get_audio_mode)"
            echo "Test completed"
            ;;
        *)
            echo "Usage: $0 {status|get|validate|clear-cache|reset|audio-mode|test}"
            exit 1
            ;;
    esac
fi