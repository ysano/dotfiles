#!/bin/bash
# Configuration Manager Module - 設定管理機能
# 設定ファイルの表示、編集、作成、修復を担当

# === 設定管理機能 ===

# 設定ファイルの管理（表示・編集・リセット）
manage_config() {
    local action="${1:-show}"
    local config_type="${2:-legacy}"  # legacy, yaml, cache
    
    case "$config_type" in
        "legacy")
            manage_legacy_config "$action"
            ;;
        "yaml")
            manage_yaml_config "$action"
            ;;
        "cache")
            manage_config_cache "$action"
            ;;
        *)
            echo "利用可能な設定タイプ: legacy, yaml, cache"
            return 1
            ;;
    esac
}

# 従来の設定ファイル管理
manage_legacy_config() {
    local action="$1"
    local config_file="$CLAUDE_VOICE_HOME/config/claude-voice.conf"
    
    case "$action" in
        "show")
            echo "設定ファイル: $config_file"
            if [[ -f "$config_file" ]]; then
                echo ""
                cat "$config_file"
            else
                echo "設定ファイルが存在しません。デフォルト設定で動作します。"
                echo ""
                echo "作成するには: claude-voice --config reset"
            fi
            ;;
        "edit")
            edit_config_file "$config_file"
            ;;
        "reset")
            reset_config_file "$config_file" "create_default_config"
            ;;
        "validate")
            validate_legacy_config "$config_file"
            ;;
        *)
            echo "利用可能なアクション: show, edit, reset, validate"
            return 1
            ;;
    esac
}

# YAML設定ファイル管理
manage_yaml_config() {
    local action="$1"
    local config_file="$CLAUDE_VOICE_HOME/config/claude-voice.yaml"
    
    case "$action" in
        "show")
            echo "YAML設定ファイル: $config_file"
            if [[ -f "$config_file" ]]; then
                echo ""
                cat "$config_file"
            else
                echo "YAML設定ファイルが存在しません。"
                echo ""
                echo "作成するには: claude-voice --config yaml reset"
            fi
            ;;
        "edit")
            edit_config_file "$config_file"
            ;;
        "reset")
            echo "YAML設定ファイルはすでに存在します: $config_file"
            echo "上書きしますか？ (y/N)"
            read -r response
            if [[ "$response" =~ ^[yY] ]]; then
                echo "注意: YAML設定ファイルのリセットはまだサポートされていません"
                echo "手動で編集してください: $config_file"
            fi
            ;;
        "validate")
            validate_yaml_config "$config_file"
            ;;
        *)
            echo "利用可能なアクション: show, edit, reset, validate"
            return 1
            ;;
    esac
}

# 設定キャッシュ管理
manage_config_cache() {
    local action="$1"
    local cache_file="$CLAUDE_VOICE_HOME/.config_cache"
    
    case "$action" in
        "show")
            echo "設定キャッシュ: $cache_file"
            if [[ -f "$cache_file" ]]; then
                echo ""
                cat "$cache_file"
            else
                echo "設定キャッシュが存在しません。"
            fi
            ;;
        "clear")
            if [[ -f "$cache_file" ]]; then
                rm "$cache_file"
                echo "設定キャッシュを削除しました"
            else
                echo "設定キャッシュは存在しません"
            fi
            ;;
        "reset")
            manage_config_cache "clear"
            ;;
        *)
            echo "利用可能なアクション: show, clear, reset"
            return 1
            ;;
    esac
}

# === ファイル操作ユーティリティ ===

# 設定ファイルの編集
edit_config_file() {
    local config_file="$1"
    
    if [[ -f "$config_file" ]]; then
        "${EDITOR:-nano}" "$config_file"
        echo "設定ファイルを編集しました: $config_file"
    else
        echo "設定ファイルを作成しますか？ (y/N)"
        read -r response
        if [[ "$response" =~ ^[yY] ]]; then
            mkdir -p "$(dirname "$config_file")"
            
            # ファイル拡張子に基づいてデフォルト設定を作成
            case "$config_file" in
                *.yaml|*.yml)
                    echo "YAML設定ファイルの作成はサポートされていません"
                    return 1
                    ;;
                *)
                    create_default_config "$config_file"
                    ;;
            esac
            
            "${EDITOR:-nano}" "$config_file"
            echo "設定ファイルを作成・編集しました: $config_file"
        fi
    fi
}

# 設定ファイルのリセット
reset_config_file() {
    local config_file="$1"
    local create_function="$2"
    
    echo "設定ファイルをデフォルトにリセットしますか？ (y/N)"
    read -r response
    if [[ "$response" =~ ^[yY] ]]; then
        mkdir -p "$(dirname "$config_file")"
        
        # バックアップの作成
        if [[ -f "$config_file" ]]; then
            local backup_file="${config_file}.backup.$(date +%Y%m%d-%H%M%S)"
            cp "$config_file" "$backup_file"
            echo "既存の設定をバックアップしました: $backup_file"
        fi
        
        # デフォルト設定の作成
        "$create_function" "$config_file"
        echo "設定ファイルをリセットしました: $config_file"
    fi
}

# === デフォルト設定作成 ===

# デフォルト設定ファイルの作成
create_default_config() {
    local config_file="$1"
    
    cat > "$config_file" << 'EOF'
# Claude Voice Configuration File
# 設定変更後は claude-voice --config validate で検証してください

[llm]
# デフォルトモデル (auto, phi4-mini:latest, orca-mini:latest, など)
default_model=auto
# Ollama API URL
ollama.api_url=http://localhost:11434
# リクエストタイムアウト（秒）
timeout=30
# 最大再試行回数
max_retries=3
# 最大入力文字数
max_input_chars=2000

[audio]
# デフォルト音声 (auto, Kyoko, など)
default_voice=auto
# 音声速度 (macOS: 100-300, Linux: 100-200, Windows: -10-10)
speech_rate=200
# 最大音声長（文字数）
max_speech_length=500
# 通知音 (Glass, Hero, Ping, など)
notification_sound=Glass
# Do Not Disturb を尊重するか (true/false)
respect_dnd=true
# システム音量 (0-100)
volume=80

[capture]
# デフォルト取得行数
default_lines=50
# 最大文字数
max_chars=2000

[logging]
# ログレベル (DEBUG, INFO, WARN, ERROR)
level=INFO
# ログファイル
file=~/.tmux/claude/logs/claude-voice.log

[test]
# テスト時の音声再生を有効にするか (true/false)
enable_speech=false

[experimental]
# 実験的機能を有効にするか (true/false)
enabled=true
# WSL音声統合
wsl_voice_integration=true
# 音声エンジンレジストリ
voice_engine_registry=true
EOF
    
    log "INFO" "Default configuration created: $config_file"
}

# === 設定検証機能 ===

# 従来設定の検証
validate_legacy_config() {
    local config_file="$1"
    
    echo "設定ファイルの検証: $config_file"
    
    if [[ ! -f "$config_file" ]]; then
        echo "❌ 設定ファイルが存在しません"
        return 1
    fi
    
    local errors=0
    local warnings=0
    
    # セクション形式の確認
    if ! grep -q '^\[.*\]' "$config_file"; then
        echo "⚠️  警告: セクション形式が見つかりません"
        ((warnings++))
    fi
    
    # 必要なセクションの確認
    local required_sections=("llm" "audio" "capture" "logging")
    for section in "${required_sections[@]}"; do
        if ! grep -q "^\[$section\]" "$config_file"; then
            echo "❌ エラー: 必要なセクションがありません: [$section]"
            ((errors++))
        fi
    done
    
    # 値の形式確認
    check_config_value "$config_file" "timeout" '^[0-9]+$' "数値" errors
    check_config_value "$config_file" "max_retries" '^[0-9]+$' "数値" errors
    check_config_value "$config_file" "volume" '^[0-9]+$' "数値(0-100)" errors
    
    # 結果の表示
    echo ""
    if [[ $errors -eq 0 ]]; then
        if [[ $warnings -eq 0 ]]; then
            echo "✅ 設定ファイルは正常です"
        else
            echo "⚠️  設定ファイルは動作しますが、${warnings}個の警告があります"
        fi
        return 0
    else
        echo "❌ 設定ファイルに${errors}個のエラーがあります"
        echo "   修復するには: claude-voice --repair"
        return 1
    fi
}

# YAML設定の検証
validate_yaml_config() {
    local config_file="$1"
    
    echo "YAML設定ファイルの検証: $config_file"
    
    if [[ ! -f "$config_file" ]]; then
        echo "❌ YAML設定ファイルが存在しません"
        return 1
    fi
    
    # yqコマンドが利用可能な場合は詳細検証
    if command -v yq >/dev/null 2>&1; then
        if yq eval '.' "$config_file" >/dev/null 2>&1; then
            echo "✅ YAML形式は正常です"
            
            # 重要なキーの存在確認
            local required_keys=("integration.enabled" "llm.provider" "voice.manual.mode")
            local missing_keys=0
            
            for key in "${required_keys[@]}"; do
                if ! yq eval ".$key" "$config_file" >/dev/null 2>&1; then
                    echo "⚠️  警告: 推奨キーがありません: $key"
                    ((missing_keys++))
                fi
            done
            
            if [[ $missing_keys -eq 0 ]]; then
                echo "✅ YAML設定ファイルは完全です"
            else
                echo "⚠️  YAML設定ファイルは動作しますが、${missing_keys}個の推奨キーがありません"
            fi
            return 0
        else
            echo "❌ YAML形式にエラーがあります"
            return 1
        fi
    else
        echo "⚠️  yqコマンドが利用できないため、詳細検証をスキップします"
        echo "   インストール: sudo apt install yq (Ubuntu) または brew install yq (macOS)"
        return 0
    fi
}

# 設定値の確認
check_config_value() {
    local config_file="$1"
    local key="$2"
    local pattern="$3"
    local description="$4"
    local errors_var="$5"
    
    local value=$(grep "^$key=" "$config_file" 2>/dev/null | cut -d'=' -f2-)
    
    if [[ -n "$value" ]]; then
        if [[ ! "$value" =~ $pattern ]]; then
            echo "❌ エラー: $key の値が不正です (期待: $description, 実際: $value)"
            local errors=${!errors_var}
            ((errors++))
            eval "$errors_var=$errors"
        fi
    fi
}

# === 設定修復機能 ===

# 設定全体の修復
repair_configuration() {
    echo "=== Claude Voice Configuration Repair ==="
    echo ""
    
    local repairs_made=0
    
    # 1. ディレクトリ構造の修復
    echo "1. ディレクトリ構造をチェック中..."
    local dir_repairs
    dir_repairs=$(repair_directory_structure)
    repairs_made=$((repairs_made + dir_repairs))
    
    # 2. 設定ファイルの修復
    echo "2. 設定ファイルをチェック中..."
    local config_repairs
    config_repairs=$(repair_config_files)
    repairs_made=$((repairs_made + config_repairs))
    
    # 3. 実行権限の修復
    echo "3. 実行権限をチェック中..."
    local perm_repairs
    perm_repairs=$(repair_executable_permissions)
    repairs_made=$((repairs_made + perm_repairs))
    
    # 4. ログファイルの初期化
    echo "4. ログファイルを初期化中..."
    local log_repairs
    log_repairs=$(repair_log_files)
    repairs_made=$((repairs_made + log_repairs))
    
    # 5. 設定の整合性チェック
    echo "5. 設定の整合性をチェック中..."
    local consistency_repairs
    consistency_repairs=$(repair_config_consistency)
    repairs_made=$((repairs_made + consistency_repairs))
    
    echo ""
    if [[ $repairs_made -gt 0 ]]; then
        echo "✅ 設定修復完了: ${repairs_made}個の修復を実行しました"
        echo "   実行してください: claude-voice --health"
    else
        echo "✅ 修復の必要はありません - 設定は正常です"
    fi
    
    return 0
}

# ディレクトリ構造の修復
repair_directory_structure() {
    local repairs=0
    local required_dirs=(
        "$CLAUDE_VOICE_HOME/core"
        "$CLAUDE_VOICE_HOME/config" 
        "$CLAUDE_VOICE_HOME/logs"
        "$CLAUDE_VOICE_HOME/bin"
        "$CLAUDE_VOICE_HOME/os"
        "$CLAUDE_VOICE_HOME/scripts"
        "$CLAUDE_VOICE_HOME/tests"
    )
    
    for dir in "${required_dirs[@]}"; do
        if [[ ! -d "$dir" ]]; then
            echo "   ディレクトリを作成: $dir"
            mkdir -p "$dir"
            ((repairs++))
        fi
    done
    
    echo "$repairs"
}

# 設定ファイルの修復
repair_config_files() {
    local repairs=0
    
    # YAML設定ファイル
    if [[ ! -f "$CLAUDE_VOICE_HOME/config/claude-voice.yaml" ]]; then
        echo "   YAML設定ファイルはすでに存在します"
    fi
    
    # 統合設定ファイル
    if [[ ! -f "$CLAUDE_VOICE_HOME/config/integration.conf" ]]; then
        echo "   統合設定ファイルを作成中..."
        cat > "$CLAUDE_VOICE_HOME/config/integration.conf" << 'EOF'
integration.enabled=true
EOF
        ((repairs++))
    fi
    
    echo "$repairs"
}

# 実行権限の修復
repair_executable_permissions() {
    local repairs=0
    local executable_files=(
        "$CLAUDE_VOICE_HOME/bin/claude-voice"
        "$CLAUDE_VOICE_HOME/core/integration.sh"
        "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh"
        "$CLAUDE_VOICE_HOME/core/universal_voice.sh"
        "$CLAUDE_VOICE_HOME/core/wsl_voice_engine.sh"
        "$CLAUDE_VOICE_HOME/session-hook.sh"
        "$CLAUDE_VOICE_HOME/voice-trigger.sh"
    )
    
    for file in "${executable_files[@]}"; do
        if [[ -f "$file" ]] && [[ ! -x "$file" ]]; then
            echo "   実行権限を設定: $file"
            chmod +x "$file"
            ((repairs++))
        fi
    done
    
    echo "$repairs"
}

# ログファイルの修復
repair_log_files() {
    local repairs=0
    local log_files=(
        "$CLAUDE_VOICE_HOME/logs/claude-voice.log"
        "$CLAUDE_VOICE_HOME/logs/integration.log"
        "$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl"
    )
    
    for log_file in "${log_files[@]}"; do
        if [[ ! -f "$log_file" ]]; then
            echo "   ログファイルを作成: $log_file"
            touch "$log_file"
            ((repairs++))
        fi
    done
    
    echo "$repairs"
}

# 設定の整合性修復
repair_config_consistency() {
    local repairs=0
    
    # 設定キャッシュの整合性確認
    local cache_file="$CLAUDE_VOICE_HOME/.config_cache"
    if [[ -f "$cache_file" ]]; then
        # 空のキャッシュファイルをチェック
        if [[ ! -s "$cache_file" ]]; then
            echo "enabled=false" > "$cache_file"
            echo "   設定キャッシュを初期化"
            ((repairs++))
        fi
    fi
    
    echo "$repairs"
}

# === ユーティリティ関数 ===

# 設定値の取得
get_config_value() {
    local key="$1"
    local default_value="$2"
    local config_file="$CLAUDE_VOICE_HOME/config/claude-voice.conf"
    
    if [[ -f "$config_file" ]]; then
        local value=$(grep "^$key=" "$config_file" 2>/dev/null | cut -d'=' -f2-)
        echo "${value:-$default_value}"
    else
        echo "$default_value"
    fi
}

# ログ関数（簡易版）
log() {
    local level="$1"
    local message="$2"
    
    if command -v logger >/dev/null 2>&1; then
        echo "[$level] $message" >&2
    fi
}

# このモジュールが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # テスト用の環境変数設定
    CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
    
    echo "Configuration Manager Module Test"
    echo "================================="
    echo ""
    
    case "${1:-help}" in
        "show")
            manage_config "show" "${2:-legacy}"
            ;;
        "validate")
            if [[ "${2:-legacy}" == "legacy" ]]; then
                validate_legacy_config "$CLAUDE_VOICE_HOME/config/claude-voice.conf"
            else
                validate_yaml_config "$CLAUDE_VOICE_HOME/config/claude-voice.yaml"
            fi
            ;;
        "repair")
            repair_configuration
            ;;
        "create")
            echo "デフォルト設定ファイルを作成中..."
            mkdir -p "$CLAUDE_VOICE_HOME/config"
            create_default_config "$CLAUDE_VOICE_HOME/config/claude-voice.conf"
            echo "作成完了: $CLAUDE_VOICE_HOME/config/claude-voice.conf"
            ;;
        *)
            echo "Available tests:"
            echo "  show [legacy|yaml|cache] - 設定表示"
            echo "  validate [legacy|yaml]   - 設定検証"
            echo "  repair                   - 設定修復"
            echo "  create                   - デフォルト設定作成"
            ;;
    esac
fi