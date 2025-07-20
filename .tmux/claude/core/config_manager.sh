#!/bin/bash
# Claude Voice Configuration Manager
# YAML設定ファイルの解析と管理

# === 設定ファイルパス ===
CLAUDE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
YAML_CONFIG="$CLAUDE_HOME/config/claude-voice.yaml"
LEGACY_CONFIG="$CLAUDE_HOME/config/integration.conf"
CONFIG_CACHE="$CLAUDE_HOME/.config_cache"

# === YAML パーサー（yq使用 or フォールバック） ===
parse_yaml_value() {
    local yaml_path="$1"
    local default_value="$2"
    local config_file="${3:-$YAML_CONFIG}"
    
    # yq が利用可能な場合
    if command -v yq >/dev/null 2>&1; then
        local value=$(yq eval ".$yaml_path" "$config_file" 2>/dev/null)
        if [[ "$value" != "null" && -n "$value" ]]; then
            echo "$value"
        else
            echo "$default_value"
        fi
    else
        # フォールバック: シンプルなYAML解析
        parse_yaml_simple "$yaml_path" "$default_value" "$config_file"
    fi
}

# === シンプルYAMLパーサー（yq非依存） ===
parse_yaml_simple() {
    local yaml_path="$1"
    local default_value="$2" 
    local config_file="$3"
    
    if [[ ! -f "$config_file" ]]; then
        echo "$default_value"
        return
    fi
    
    # yaml_pathを階層に分割（例: "voice.manual.mode" -> ["voice", "manual", "mode"]）
    IFS='.' read -ra path_parts <<< "$yaml_path"
    
    local current_section=""
    local indent_level=0
    local target_indent=""
    local found_section=false
    
    while IFS= read -r line; do
        # コメントと空行をスキップ
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        [[ -z "${line// }" ]] && continue
        
        # インデントレベルの計算
        local line_indent=$(echo "$line" | sed 's/[^ ].*//')
        local line_indent_count=${#line_indent}
        
        # キー:値の抽出
        if [[ "$line" =~ ^[[:space:]]*([^:]+):[[:space:]]*(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// /}"
            local value="${BASH_REMATCH[2]}"
            
            # 値の前後の空白とクォートを除去
            value=$(echo "$value" | sed 's/^[[:space:]]*//; s/[[:space:]]*$//; s/^"//; s/"$//')
            
            # パス検索ロジック
            if [[ ${#path_parts[@]} -eq 1 && "$key" == "${path_parts[0]}" ]]; then
                echo "$value"
                return
            elif [[ ${#path_parts[@]} -gt 1 ]]; then
                # 階層構造の処理（簡易版）
                case "$yaml_path" in
                    "integration.enabled"|"integration.mode"|"integration.strict_mode")
                        if [[ "$key" == "integration" ]]; then
                            found_section=true
                            target_indent=$((line_indent_count + 2))
                        elif [[ $found_section == true && $line_indent_count -eq $target_indent ]]; then
                            local subkey="${path_parts[1]}"
                            if [[ "$key" == "$subkey" ]]; then
                                echo "$value"
                                return
                            fi
                        elif [[ $line_indent_count -le $line_indent_count ]] && [[ $found_section == true ]]; then
                            found_section=false
                        fi
                        ;;
                    "voice.manual."*|"voice.auto_complete."*|"voice.auto_waiting."*)
                        # 音声設定の処理
                        if [[ "$key" == "voice" ]]; then
                            found_section=true
                            target_indent=$((line_indent_count + 2))
                        elif [[ $found_section == true && $line_indent_count -eq $target_indent && "$key" == "${path_parts[1]}" ]]; then
                            target_indent=$((target_indent + 2))
                        elif [[ $found_section == true && $line_indent_count -eq $target_indent && "$key" == "${path_parts[2]}" ]]; then
                            echo "$value"
                            return
                        fi
                        ;;
                esac
            fi
        fi
    done < "$config_file"
    
    echo "$default_value"
}

# === 設定値取得の統一インターフェース ===
get_config() {
    local key="$1"
    local default_value="$2"
    
    # キャッシュチェック（パフォーマンス向上）
    if [[ -f "$CONFIG_CACHE" ]]; then
        local cached_value=$(grep "^${key}=" "$CONFIG_CACHE" 2>/dev/null | cut -d'=' -f2-)
        if [[ -n "$cached_value" ]]; then
            echo "$cached_value"
            return
        fi
    fi
    
    local value="$default_value"
    
    # YAML設定ファイルから取得
    if [[ -f "$YAML_CONFIG" ]]; then
        value=$(parse_yaml_value "$key" "$default_value" "$YAML_CONFIG")
    elif [[ -f "$LEGACY_CONFIG" ]]; then
        # レガシー設定ファイルフォールバック
        value=$(grep "^${key//./}=" "$LEGACY_CONFIG" 2>/dev/null | cut -d'=' -f2 || echo "$default_value")
    fi
    
    # キャッシュに保存
    mkdir -p "$(dirname "$CONFIG_CACHE")"
    echo "${key}=${value}" >> "$CONFIG_CACHE"
    
    echo "$value"
}

# === 設定値更新 ===
set_config() {
    local key="$1"
    local value="$2"
    local config_file="${3:-$YAML_CONFIG}"
    
    # YAMLファイルの更新（yq使用）
    if command -v yq >/dev/null 2>&1 && [[ -f "$config_file" ]]; then
        yq eval ".$key = \"$value\"" -i "$config_file"
    else
        # フォールバック: レガシー設定ファイルに保存
        mkdir -p "$(dirname "$LEGACY_CONFIG")"
        if grep -q "^${key//./}=" "$LEGACY_CONFIG" 2>/dev/null; then
            sed -i.bak "s/^${key//./}=.*/${key//./}=$value/" "$LEGACY_CONFIG"
        else
            echo "${key//./}=$value" >> "$LEGACY_CONFIG"
        fi
    fi
    
    # キャッシュクリア
    clear_config_cache "$key"
}

# === 設定キャッシュ管理 ===
clear_config_cache() {
    local specific_key="$1"
    
    if [[ -n "$specific_key" ]]; then
        # 特定のキーのみクリア
        if [[ -f "$CONFIG_CACHE" ]]; then
            grep -v "^${specific_key}=" "$CONFIG_CACHE" > "$CONFIG_CACHE.tmp" && mv "$CONFIG_CACHE.tmp" "$CONFIG_CACHE"
        fi
    else
        # 全キャッシュクリア
        rm -f "$CONFIG_CACHE"
    fi
}

# === 設定ファイル検証 ===
validate_config() {
    local config_file="${1:-$YAML_CONFIG}"
    
    if [[ ! -f "$config_file" ]]; then
        echo "ERROR: Configuration file not found: $config_file" >&2
        return 1
    fi
    
    # YAML構文チェック（yq使用）
    if command -v yq >/dev/null 2>&1; then
        if ! yq eval '.' "$config_file" >/dev/null 2>&1; then
            echo "ERROR: Invalid YAML syntax in $config_file" >&2
            return 1
        fi
    fi
    
    # 必須設定の確認
    local required_keys=("integration.enabled" "voice.manual.mode" "audio.system_integration")
    for key in "${required_keys[@]}"; do
        local value=$(parse_yaml_value "$key" "" "$config_file")
        if [[ -z "$value" ]]; then
            echo "WARNING: Required configuration missing: $key" >&2
        fi
    done
    
    echo "Configuration validation completed"
    return 0
}

# === 設定ファイル初期化 ===
init_config() {
    local target_dir="$CLAUDE_HOME/config"
    
    mkdir -p "$target_dir"
    
    # デフォルトYAML設定ファイルがない場合は作成
    if [[ ! -f "$YAML_CONFIG" ]]; then
        echo "Creating default YAML configuration..."
        # デフォルト設定をコピー（ここでは簡略版）
        cat > "$YAML_CONFIG" << 'EOF'
integration:
  enabled: true
  mode: "smart"
  strict_mode: false

voice:
  manual:
    mode: "brief"
    lines: 25
    model: "auto"
    voice: "Kyoko"

audio:
  system_integration: "osascript"
  fallback_behavior: "graceful"

logging:
  level: "INFO"
EOF
    fi
    
    # 設定ファイルの検証
    validate_config "$YAML_CONFIG"
}

# === デバッグ機能 ===
debug_config() {
    echo "=== Claude Voice Configuration Debug ==="
    echo "YAML Config: $YAML_CONFIG"
    echo "Legacy Config: $LEGACY_CONFIG"
    echo "Cache File: $CONFIG_CACHE"
    echo ""
    
    if [[ -f "$YAML_CONFIG" ]]; then
        echo "YAML Configuration exists: YES"
        echo "YAML file size: $(wc -c < "$YAML_CONFIG") bytes"
    else
        echo "YAML Configuration exists: NO"
    fi
    
    if [[ -f "$LEGACY_CONFIG" ]]; then
        echo "Legacy Configuration exists: YES"
    else
        echo "Legacy Configuration exists: NO"
    fi
    
    echo ""
    echo "Sample configuration values:"
    echo "  integration.enabled: $(get_config "integration.enabled" "undefined")"
    echo "  voice.manual.mode: $(get_config "voice.manual.mode" "undefined")"
    echo "  audio.system_integration: $(get_config "audio.system_integration" "undefined")"
}

# === 公開API ===
case "${1:-help}" in
    "get")
        get_config "$2" "$3"
        ;;
    "set")
        set_config "$2" "$3"
        ;;
    "validate")
        validate_config "$2"
        ;;
    "init")
        init_config
        ;;
    "clear-cache")
        clear_config_cache "$2"
        ;;
    "debug")
        debug_config
        ;;
    *)
        echo "Usage: $0 {get|set|validate|init|clear-cache|debug}"
        echo "Configuration Manager for Claude Voice"
        ;;
esac