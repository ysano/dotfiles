#!/bin/bash
# Configuration Manager V2 - Centralized Configuration System
# 中央集権化された設定管理システム

# === Global Configuration Storage ===

# システム設定
declare -A SYSTEM_CONFIG=(
    # Core System Settings
    ["system.version"]="3.0.0"
    ["system.debug_mode"]="false"
    ["system.log_level"]="INFO"
    ["system.log_file"]="~/.tmux/claude/logs/claude-voice.log"
    ["system.cache_dir"]="~/.tmux/claude/.cache"
    ["system.config_file"]="~/.tmux/claude/config/claude-voice.conf"
    
    # Performance Settings
    ["performance.cache_ttl"]="5"
    ["performance.command_timeout"]="5"
    ["performance.retry_attempts"]="3"
    ["performance.max_concurrent_ops"]="10"
    
    # Detection Settings
    ["detection.polling_interval"]="1"
    ["detection.ui_context_lines"]="30"
    ["detection.fallback_timeout"]="2"
    ["detection.cache_enabled"]="true"
    
    # Audio Settings
    ["audio.default_voice"]="auto"
    ["audio.speech_rate"]="200"
    ["audio.volume"]="0.8"
    ["audio.notification_sound"]="Glass"
    ["audio.max_speech_length"]="500"
    ["audio.respect_dnd"]="true"
    
    # UI Settings
    ["ui.status_position"]="right"
    ["ui.status_format"]="auto"
    ["ui.refresh_interval"]="1"
    ["ui.color_scheme"]="auto"
    
    # Integration Settings
    ["integration.tmux_enabled"]="true"
    ["integration.os_notifications"]="true"
    ["integration.clipboard_sync"]="false"
    ["integration.auto_start"]="false"
)

# 環境別オーバーライド設定
declare -A ENVIRONMENT_OVERRIDES=()

# 設定変更履歴
declare -A CONFIG_HISTORY=()

# === Configuration Management Functions ===

# 設定値の取得（階層対応）
get_config() {
    local key="$1"
    local default_value="${2:-}"
    local use_environment="${3:-true}"
    
    # 環境変数オーバーライドのチェック
    if [[ "$use_environment" == "true" ]]; then
        local env_key="CLAUDE_$(echo "$key" | tr '.' '_' | tr '[:lower:]' '[:upper:]')"
        if [[ -n "${!env_key:-}" ]]; then
            echo "${!env_key}"
            return 0
        fi
    fi
    
    # 環境別オーバーライドのチェック
    local current_env=$(detect_environment)
    local env_override_key="${current_env}.${key}"
    if [[ -n "${ENVIRONMENT_OVERRIDES[$env_override_key]:-}" ]]; then
        echo "${ENVIRONMENT_OVERRIDES[$env_override_key]}"
        return 0
    fi
    
    # メイン設定から取得
    if [[ -n "${SYSTEM_CONFIG[$key]:-}" ]]; then
        echo "${SYSTEM_CONFIG[$key]}"
        return 0
    fi
    
    # デフォルト値を返す
    echo "$default_value"
}

# 設定値の設定（履歴記録付き）
set_config() {
    local key="$1"
    local value="$2"
    local persist="${3:-false}"
    local scope="${4:-system}"  # system, environment, session
    
    # 現在の値を履歴に保存
    local old_value="${SYSTEM_CONFIG[$key]:-}"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    CONFIG_HISTORY["${key}:${timestamp}"]="$old_value"
    
    # スコープ別の設定
    case "$scope" in
        "system")
            SYSTEM_CONFIG["$key"]="$value"
            ;;
        "environment")
            local current_env=$(detect_environment)
            local env_key="${current_env}.${key}"
            ENVIRONMENT_OVERRIDES["$env_key"]="$value"
            ;;
        "session")
            # セッション固有設定（現在のプロセスのみ）
            export "CLAUDE_SESSION_$(echo "$key" | tr '.' '_' | tr '[:lower:]' '[:upper:]')"="$value"
            ;;
    esac
    
    # 永続化が要求された場合
    if [[ "$persist" == "true" ]]; then
        persist_config "$key" "$value" "$scope"
    fi
    
    echo "Set $scope config: $key = $value"
}

# 設定の永続化
persist_config() {
    local key="$1"
    local value="$2"
    local scope="${3:-system}"
    
    local config_file=$(get_config "system.config_file")
    config_file=$(eval echo "$config_file")  # ~を展開
    
    # 設定ファイルのディレクトリを作成
    mkdir -p "$(dirname "$config_file")"
    
    # セクション別に書き込み
    local section=""
    case "$scope" in
        "system")
            section="[system]"
            ;;
        "environment")
            local current_env=$(detect_environment)
            section="[environment:$current_env]"
            ;;
    esac
    
    # 設定ファイルの更新（簡易版）
    {
        echo "# Claude Voice Configuration"
        echo "# Generated: $(date)"
        echo ""
        echo "$section"
        echo "$key=$value"
    } >> "$config_file"
    
    echo "Persisted to $config_file"
}

# 設定ファイルからの読み込み
load_config_file() {
    local config_file="${1:-$(get_config "system.config_file")}"
    config_file=$(eval echo "$config_file")
    
    if [[ ! -f "$config_file" ]]; then
        echo "Config file not found: $config_file"
        return 1
    fi
    
    local current_section=""
    local line_count=0
    
    while IFS= read -r line; do
        ((line_count++))
        
        # コメントと空行をスキップ
        if [[ "$line" =~ ^[[:space:]]*# ]] || [[ -z "${line// }" ]]; then
            continue
        fi
        
        # セクション検出
        if [[ "$line" =~ ^\[(.+)\]$ ]]; then
            current_section="${BASH_REMATCH[1]}"
            continue
        fi
        
        # キー=値のペア処理
        if [[ "$line" =~ ^([^=]+)=(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// }"
            local value="${BASH_REMATCH[2]}"
            
            # セクション別の設定適用
            case "$current_section" in
                "system")
                    SYSTEM_CONFIG["$key"]="$value"
                    ;;
                "environment:"*)
                    local env_name="${current_section#environment:}"
                    ENVIRONMENT_OVERRIDES["${env_name}.${key}"]="$value"
                    ;;
            esac
        fi
    done < "$config_file"
    
    echo "Loaded $line_count lines from $config_file"
}

# === Environment Detection ===

# 実行環境の検出
detect_environment() {
    if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
        echo "wsl"
    elif [[ "$OSTYPE" =~ ^darwin ]]; then
        echo "macos"
    elif [[ "$OSTYPE" =~ ^linux ]]; then
        echo "linux"
    elif [[ "$OSTYPE" =~ ^msys ]] || [[ "$OSTYPE" =~ ^cygwin ]]; then
        echo "windows"
    else
        echo "unknown"
    fi
}

# === Configuration Validation ===

# 設定値の妥当性チェック
validate_config() {
    local errors=()
    
    # 必須設定のチェック
    local required_keys=(
        "system.version"
        "system.log_level"
        "performance.cache_ttl"
        "detection.polling_interval"
    )
    
    for key in "${required_keys[@]}"; do
        local value=$(get_config "$key")
        if [[ -z "$value" ]]; then
            errors+=("Missing required config: $key")
        fi
    done
    
    # 数値設定のチェック
    local numeric_keys=(
        "performance.cache_ttl"
        "performance.command_timeout"
        "performance.retry_attempts"
        "detection.polling_interval"
        "audio.speech_rate"
        "ui.refresh_interval"
    )
    
    for key in "${numeric_keys[@]}"; do
        local value=$(get_config "$key")
        if [[ -n "$value" ]] && ! [[ "$value" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
            errors+=("Invalid numeric value for $key: $value")
        fi
    done
    
    # ログレベルのチェック
    local log_level=$(get_config "system.log_level")
    if [[ -n "$log_level" ]] && ! [[ "$log_level" =~ ^(DEBUG|INFO|WARN|ERROR)$ ]]; then
        errors+=("Invalid log level: $log_level")
    fi
    
    # 結果の返却
    if [[ ${#errors[@]} -gt 0 ]]; then
        echo "Configuration validation failed:"
        printf '  %s\n' "${errors[@]}"
        return 1
    else
        echo "Configuration validation passed"
        return 0
    fi
}

# === Configuration Profiles ===

# プロファイル管理
declare -A CONFIG_PROFILES=(
    ["development"]="performance.cache_ttl=1;system.debug_mode=true;system.log_level=DEBUG"
    ["production"]="performance.cache_ttl=5;system.debug_mode=false;system.log_level=INFO"
    ["testing"]="performance.cache_ttl=0;system.debug_mode=true;system.log_level=DEBUG;detection.cache_enabled=false"
    ["minimal"]="audio.default_voice=none;ui.status_format=minimal;integration.os_notifications=false"
)

# プロファイルの適用
apply_profile() {
    local profile_name="$1"
    local persist="${2:-false}"
    
    if [[ -z "${CONFIG_PROFILES[$profile_name]:-}" ]]; then
        echo "Unknown profile: $profile_name"
        echo "Available profiles: ${!CONFIG_PROFILES[*]}"
        return 1
    fi
    
    local profile_config="${CONFIG_PROFILES[$profile_name]}"
    local applied_count=0
    
    IFS=';' read -ra config_pairs <<< "$profile_config"
    for pair in "${config_pairs[@]}"; do
        if [[ "$pair" =~ ^([^=]+)=(.*)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            local value="${BASH_REMATCH[2]}"
            set_config "$key" "$value" "$persist" "system"
            ((applied_count++))
        fi
    done
    
    echo "Applied profile '$profile_name': $applied_count settings"
}

# === Reporting and Management ===

# 全設定の表示
show_all_config() {
    local filter_pattern="${1:-.*}"
    local show_defaults="${2:-false}"
    
    echo "=== Claude Voice Configuration ==="
    echo "Environment: $(detect_environment)"
    echo ""
    
    # システム設定
    echo "System Configuration:"
    for key in $(printf '%s\n' "${!SYSTEM_CONFIG[@]}" | sort | grep -E "$filter_pattern"); do
        local value="${SYSTEM_CONFIG[$key]}"
        echo "  $key = $value"
    done
    echo ""
    
    # 環境別オーバーライド
    if [[ ${#ENVIRONMENT_OVERRIDES[@]} -gt 0 ]]; then
        echo "Environment Overrides:"
        for key in $(printf '%s\n' "${!ENVIRONMENT_OVERRIDES[@]}" | sort | grep -E "$filter_pattern"); do
            local value="${ENVIRONMENT_OVERRIDES[$key]}"
            echo "  $key = $value"
        done
        echo ""
    fi
    
    # 環境変数オーバーライド
    echo "Environment Variable Overrides:"
    local env_found=false
    for var in $(env | grep '^CLAUDE_' | sort); do
        if [[ "$var" =~ ^CLAUDE_([^=]+)=(.*)$ ]]; then
            local env_key="${BASH_REMATCH[1]}"
            local env_value="${BASH_REMATCH[2]}"
            local config_key=$(echo "$env_key" | tr '_' '.' | tr '[:upper:]' '[:lower:]')
            
            if [[ "$config_key" =~ $filter_pattern ]]; then
                echo "  $config_key = $env_value (via $var)"
                env_found=true
            fi
        fi
    done
    
    if [[ "$env_found" == "false" ]]; then
        echo "  (none)"
    fi
}

# 設定差分の表示
show_config_diff() {
    local compare_profile="${1:-production}"
    
    echo "=== Configuration Diff (current vs $compare_profile) ==="
    
    if [[ -n "${CONFIG_PROFILES[$compare_profile]:-}" ]]; then
        local profile_config="${CONFIG_PROFILES[$compare_profile]}"
        
        IFS=';' read -ra config_pairs <<< "$profile_config"
        for pair in "${config_pairs[@]}"; do
            if [[ "$pair" =~ ^([^=]+)=(.*)$ ]]; then
                local key="${BASH_REMATCH[1]}"
                local profile_value="${BASH_REMATCH[2]}"
                local current_value=$(get_config "$key")
                
                if [[ "$current_value" != "$profile_value" ]]; then
                    echo "  $key: $current_value → $profile_value"
                fi
            fi
        done
    else
        echo "Unknown profile: $compare_profile"
    fi
}

# === Test Functions ===

# 設定管理システムのテスト
test_config_manager() {
    echo "=== Configuration Manager Test ==="
    echo ""
    
    # 基本的な設定取得テスト
    echo "1. Basic Configuration Test:"
    local version=$(get_config "system.version")
    local debug_mode=$(get_config "system.debug_mode")
    echo "  Version: $version"
    echo "  Debug Mode: $debug_mode"
    echo ""
    
    # 環境検出テスト
    echo "2. Environment Detection:"
    local env=$(detect_environment)
    echo "  Current Environment: $env"
    echo ""
    
    # 設定変更テスト
    echo "3. Configuration Update Test:"
    local old_cache_ttl=$(get_config "performance.cache_ttl")
    set_config "performance.cache_ttl" "10" "false" "system"
    local new_cache_ttl=$(get_config "performance.cache_ttl")
    echo "  Cache TTL: $old_cache_ttl → $new_cache_ttl"
    
    # 元に戻す
    set_config "performance.cache_ttl" "$old_cache_ttl" "false" "system"
    echo ""
    
    # 検証テスト
    echo "4. Validation Test:"
    validate_config
    echo ""
    
    # プロファイルテスト
    echo "5. Profile Test:"
    echo "  Available profiles: ${!CONFIG_PROFILES[*]}"
}

# === Main Execution ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_config_manager
            ;;
        "show")
            show_all_config "${2:-.*}"
            ;;
        "get")
            get_config "$2" "${3:-}"
            ;;
        "set")
            set_config "$2" "$3" "${4:-false}" "${5:-system}"
            ;;
        "load")
            load_config_file "$2"
            ;;
        "validate")
            validate_config
            ;;
        "profile")
            apply_profile "$2" "${3:-false}"
            ;;
        "diff")
            show_config_diff "$2"
            ;;
        *)
            echo "Usage: $0 [test|show|get|set|load|validate|profile|diff]"
            ;;
    esac
fi