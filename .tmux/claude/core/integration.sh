#!/bin/bash
# Claude Voice Smart Integration Layer
# 智的な統合管理と条件判定システム

# === グローバル設定 ===
CLAUDE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
CONFIG_FILE="$CLAUDE_HOME/config/integration.conf"
LOG_FILE="$CLAUDE_HOME/logs/integration.log"

# === ログ機能 ===
log_integration() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    mkdir -p "$(dirname "$LOG_FILE")"
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    # DEBUGレベル以外はstderrにも出力
    if [[ "$level" != "DEBUG" ]]; then
        echo "[$level] $message" >&2
    fi
}

# === 設定管理（YAML対応） ===
# 設定マネージャーの読み込み
source "$CLAUDE_HOME/core/config_manager.sh" >/dev/null 2>&1

get_integration_config() {
    local key="$1"
    local default_value="$2"
    
    # 新しい設定マネージャー経由で取得
    if declare -f get_config >/dev/null 2>&1; then
        get_config "$key" "$default_value"
    else
        # フォールバック: 従来の方法
        if [[ -f "$CONFIG_FILE" ]]; then
            grep "^${key}=" "$CONFIG_FILE" 2>/dev/null | cut -d'=' -f2 | tail -1 || echo "$default_value"
        else
            echo "$default_value"
        fi
    fi
}

# === システム健全性チェック ===
check_audio_system() {
    local os_type=$(uname)
    
    case "$os_type" in
        "Darwin")
            # macOS: say コマンドとosascriptの確認
            if ! command -v say >/dev/null 2>&1; then
                log_integration "WARN" "macOS say command not available"
                return 1
            fi
            
            if ! command -v osascript >/dev/null 2>&1; then
                log_integration "WARN" "macOS osascript not available"
                return 1
            fi
            
            # 音声出力デバイスの確認
            if ! osascript -e 'get volume settings' >/dev/null 2>&1; then
                log_integration "WARN" "macOS audio system not accessible"
                return 1
            fi
            ;;
        "Linux")
            # Linux: paplay または aplay の確認
            if ! command -v paplay >/dev/null 2>&1 && ! command -v aplay >/dev/null 2>&1; then
                log_integration "WARN" "Linux audio commands not available"
                return 1
            fi
            ;;
        *)
            log_integration "WARN" "Unsupported OS for audio: $os_type"
            return 1
            ;;
    esac
    
    return 0
}

check_ollama_system() {
    local ollama_url=$(get_integration_config "ollama_url" "http://localhost:11434")
    
    # Ollamaサーバーの接続確認
    if ! curl -s --connect-timeout 3 "$ollama_url/api/tags" >/dev/null 2>&1; then
        log_integration "WARN" "Ollama server not accessible at $ollama_url"
        return 1
    fi
    
    # 利用可能モデルの確認
    local available_models=$(curl -s "$ollama_url/api/tags" 2>/dev/null | jq -r '.models[].name' 2>/dev/null)
    if [[ -z "$available_models" ]]; then
        log_integration "WARN" "No Ollama models available"
        return 1
    fi
    
    log_integration "DEBUG" "Ollama models available: $available_models"
    return 0
}

check_claude_voice_binary() {
    local claude_voice_bin="$CLAUDE_HOME/bin/claude-voice"
    
    if [[ ! -x "$claude_voice_bin" ]]; then
        log_integration "ERROR" "Claude Voice binary not found or not executable: $claude_voice_bin"
        return 1
    fi
    
    # バイナリの基本実行テスト
    if ! "$claude_voice_bin" --help >/dev/null 2>&1; then
        log_integration "ERROR" "Claude Voice binary not functional"
        return 1
    fi
    
    return 0
}

# === 包括的前提条件チェック ===
check_prerequisites() {
    local mode=$(get_integration_config "mode" "smart")
    local strict_mode=$(get_integration_config "strict_mode" "false")
    
    log_integration "INFO" "Checking prerequisites (mode: $mode, strict: $strict_mode)"
    
    local issues=()
    
    # 必須条件: Claude Voice バイナリ
    if ! check_claude_voice_binary; then
        issues+=("claude_voice_binary")
    fi
    
    # 条件付き確認: 音声システム
    if [[ "$mode" != "minimal" ]]; then
        if ! check_audio_system; then
            issues+=("audio_system")
        fi
    fi
    
    # 条件付き確認: Ollama システム
    if [[ "$mode" == "full" ]]; then
        if ! check_ollama_system; then
            issues+=("ollama_system")
        fi
    fi
    
    # 結果の評価
    if [[ ${#issues[@]} -eq 0 ]]; then
        log_integration "INFO" "All prerequisites satisfied"
        return 0
    else
        log_integration "WARN" "Prerequisites issues: ${issues[*]}"
        
        # strict_mode での厳格な評価
        if [[ "$strict_mode" == "true" ]]; then
            log_integration "ERROR" "Strict mode: Prerequisites failed"
            return 1
        else
            log_integration "INFO" "Non-strict mode: Continuing with degraded functionality"
            return 2  # 警告レベル（継続可能）
        fi
    fi
}

# === 智的実行戦略 ===
execute_voice_action_intelligently() {
    local action_type="$1"  # manual, auto_complete, auto_waiting
    local content_hint="$2"
    
    log_integration "INFO" "Executing voice action: $action_type"
    
    # 前提条件チェック
    local prereq_result
    check_prerequisites
    prereq_result=$?
    
    case $prereq_result in
        0)
            # 完全機能での実行
            execute_full_voice_action "$action_type" "$content_hint"
            ;;
        2)
            # 劣化機能での実行
            execute_degraded_voice_action "$action_type" "$content_hint"
            ;;
        *)
            # 機能無効での実行
            execute_fallback_action "$action_type" "$content_hint"
            ;;
    esac
}

execute_full_voice_action() {
    local action_type="$1"
    local content_hint="$2"
    
    # アクションタイプに基づく実行パラメータの調整
    local voice_mode="brief"
    local voice_lines="20"
    
    case "$action_type" in
        "manual")
            voice_mode=$(get_integration_config "manual_voice_mode" "brief")
            voice_lines=$(get_integration_config "manual_voice_lines" "25")
            ;;
        "auto_complete")
            voice_mode=$(get_integration_config "auto_complete_voice_mode" "brief")
            voice_lines=$(get_integration_config "auto_complete_voice_lines" "15")
            ;;
        "auto_waiting")
            voice_mode=$(get_integration_config "auto_waiting_voice_mode" "brief")
            voice_lines=$(get_integration_config "auto_waiting_voice_lines" "10")
            ;;
    esac
    
    # Claude Voice の安全な実行
    local claude_voice_bin="$CLAUDE_HOME/bin/claude-voice"
    local voice_model=$(get_integration_config "voice_model" "auto")
    
    log_integration "INFO" "Executing full voice action: $voice_mode $voice_lines lines"
    
    # バックグラウンド実行（tmux環境対応）
    if [[ -n "${TMUX:-}" ]]; then
        # tmux環境: osascript経由で実行
        osascript -e "do shell script \"$claude_voice_bin $voice_mode $voice_lines Kyoko $voice_model\"" &
    else
        # 通常環境: 直接実行
        "$claude_voice_bin" "$voice_mode" "$voice_lines" "Kyoko" "$voice_model" &
    fi
    
    log_integration "INFO" "Voice action initiated successfully"
}

execute_degraded_voice_action() {
    local action_type="$1"
    local content_hint="$2"
    
    log_integration "INFO" "Executing degraded voice action: $action_type"
    
    # 劣化機能: シンプルなテキスト読み上げのみ
    local fallback_text=""
    
    case "$action_type" in
        "manual")
            fallback_text="画面の手動確認が要求されました。"
            ;;
        "auto_complete")
            fallback_text="処理が完了しました。"
            ;;
        "auto_waiting")
            fallback_text="入力待ちです。"
            ;;
    esac
    
    # シンプルな音声出力
    if command -v say >/dev/null 2>&1; then
        echo "$fallback_text" | say &
    else
        log_integration "WARN" "Degraded voice action failed: no say command"
        execute_fallback_action "$action_type" "$content_hint"
    fi
}

execute_fallback_action() {
    local action_type="$1"
    local content_hint="$2"
    
    log_integration "INFO" "Executing fallback action: $action_type"
    
    # 最終フォールバック: 視覚的通知のみ
    local message=""
    
    case "$action_type" in
        "manual")
            message="Claude Voice: Manual check requested (audio unavailable)"
            ;;
        "auto_complete")
            message="Claude Voice: Process completed"
            ;;
        "auto_waiting")
            message="Claude Voice: Waiting for input"
            ;;
    esac
    
    # tmux通知とシステムビープ
    if command -v tmux >/dev/null 2>&1; then
        tmux display-message "$message"
    fi
    
    # システムビープ
    echo -e '\a'
    
    log_integration "INFO" "Fallback action completed: $message"
}

# === 統合状態管理 ===
get_integration_status() {
    local prereq_result
    check_prerequisites
    prereq_result=$?
    
    case $prereq_result in
        0)
            echo "fully_functional"
            ;;
        2)
            echo "degraded"
            ;;
        *)
            echo "disabled"
            ;;
    esac
}

# === 公開API ===
# 外部スクリプトから呼び出し可能な関数

# 音声アクション実行
voice_action() {
    local action_type="${1:-manual}"
    local content_hint="$2"
    
    # 統合が有効か確認
    local enabled=$(get_integration_config "enabled" "false")
    if [[ "$enabled" != "true" ]]; then
        log_integration "INFO" "Integration disabled, skipping voice action"
        return 0
    fi
    
    execute_voice_action_intelligently "$action_type" "$content_hint"
}

# 統合状態確認
status() {
    get_integration_status
}

# 設定リロード
reload_config() {
    log_integration "INFO" "Reloading integration configuration"
    # 設定の再読み込み処理（将来的にキャッシュクリアなど）
}

# === メイン実行部分 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 直接実行時の処理
    case "${1:-help}" in
        "voice_action")
            voice_action "$2" "$3"
            ;;
        "status")
            status
            ;;
        "reload")
            reload_config
            ;;
        "test")
            check_prerequisites
            echo "Prerequisites check result: $?"
            ;;
        *)
            echo "Usage: $0 {voice_action|status|reload|test}"
            echo "Smart Integration Layer for Claude Voice"
            ;;
    esac
fi