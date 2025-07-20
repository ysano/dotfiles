#!/bin/bash
# Claude Voice Core - Base Utilities
# OS非依存の基本ユーティリティ関数

CLAUDE_VOICE_VERSION="2.1.0"
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"

# ログレベル定数
readonly LOG_LEVEL_DEBUG=0
readonly LOG_LEVEL_INFO=1
readonly LOG_LEVEL_WARN=2
readonly LOG_LEVEL_ERROR=3

# 現在のログレベル（設定で変更可能）
CURRENT_LOG_LEVEL=${CLAUDE_LOG_LEVEL:-$LOG_LEVEL_INFO}

# ロギング関数
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    local level_num
    
    case "$level" in
        "DEBUG") level_num=$LOG_LEVEL_DEBUG ;;
        "INFO")  level_num=$LOG_LEVEL_INFO ;;
        "WARN")  level_num=$LOG_LEVEL_WARN ;;
        "ERROR") level_num=$LOG_LEVEL_ERROR ;;
        *) level_num=$LOG_LEVEL_INFO; level="INFO" ;;
    esac
    
    # ログレベルフィルタリング
    if [[ $level_num -lt $CURRENT_LOG_LEVEL ]]; then
        return 0
    fi
    
    # ログディレクトリの確保
    mkdir -p "$CLAUDE_VOICE_HOME/logs"
    
    # ログファイルへの出力
    echo "[$timestamp] [$level] $message" >> "$CLAUDE_VOICE_HOME/logs/claude-voice.log"
    
    # 標準エラー出力（ERROR, WARNまたはデバッグモード時）
    if [[ "$level" == "ERROR" ]] || [[ "$level" == "WARN" ]] || [[ "${DEBUG_MODE:-false}" == "true" ]]; then
        echo "[$level] $message" >&2
    fi
}

# OS検出関数（tmuxのos検出ロジックを参考）
detect_os() {
    case "$OSTYPE" in
        darwin*)
            echo "darwin"
            ;;
        linux*)
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                echo "windows"
            else
                echo "linux"
            fi
            ;;
        freebsd*)
            echo "freebsd"
            ;;
        msys*|cygwin*)
            echo "windows"
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# OS固有モジュールの読み込み
load_os_module() {
    local os_type=$(detect_os)
    local os_module="$CLAUDE_VOICE_HOME/os/${os_type}.sh"
    
    if [[ -f "$os_module" ]]; then
        source "$os_module"
        log "DEBUG" "Loaded OS module: $os_type"
        return 0
    else
        log "ERROR" "OS module not found: $os_module"
        return 1
    fi
}

# 設定値の取得（デフォルト値付き）
get_config() {
    local key="$1"
    local default_value="$2"
    local config_file="$CLAUDE_VOICE_HOME/config/claude-voice.conf"
    
    if [[ -f "$config_file" ]]; then
        local value=$(grep "^${key}=" "$config_file" 2>/dev/null | cut -d'=' -f2- | sed 's/^["'\'']\|["'\'']$//g')
        echo "${value:-$default_value}"
    else
        echo "$default_value"
    fi
}

# コマンドの存在確認
has_command() {
    command -v "$1" >/dev/null 2>&1
}

# ネットワーク接続確認
check_network_connectivity() {
    local host="${1:-8.8.8.8}"
    local timeout="${2:-3}"
    
    if has_command ping; then
        ping -c 1 -W "$timeout" "$host" >/dev/null 2>&1
    else
        # pingが利用できない場合のフォールバック
        if has_command curl; then
            curl -s --connect-timeout "$timeout" "http://$host" >/dev/null 2>&1
        else
            return 1
        fi
    fi
}

# JSON形式の検証
validate_json() {
    local json_string="$1"
    
    if [[ -z "$json_string" ]]; then
        return 1
    fi
    
    if has_command jq; then
        echo "$json_string" | jq . >/dev/null 2>&1
    else
        # jqが利用できない場合の簡易検証
        if [[ "$json_string" =~ ^\{.*\}$ ]] || [[ "$json_string" =~ ^\[.*\]$ ]]; then
            return 0
        else
            return 1
        fi
    fi
}

# 文字列の安全なエスケープ（JSONペイロード用）
escape_json_string() {
    local input="$1"
    
    # jqを使用してJSON文字列エスケープ（最も確実）
    if has_command jq; then
        printf '%s' "$input" | jq -r -R -s '.[:-1]'
    else
        # jqが利用できない場合のフォールバック
        printf '%s' "$input" | \
            sed 's/\\/\\\\/g' | \
            sed 's/"/\\"/g' | \
            sed 's/	/\\t/g'
    fi
}

# タイムスタンプ生成
get_timestamp() {
    date +%s
}

# 実行時間測定の開始
start_timer() {
    echo $(date +%s.%N)
}

# 実行時間測定の終了
end_timer() {
    local start_time="$1"
    local end_time=$(date +%s.%N)
    
    if has_command bc; then
        echo "$end_time - $start_time" | bc
    else
        # bcが利用できない場合の簡易計算
        printf "%.2f" $(echo "$end_time $start_time" | awk '{print $1 - $2}')
    fi
}

# エラーハンドリング用のトラップ設定
setup_error_handling() {
    set -eE  # エラー時に即座に終了
    trap 'handle_error $? $LINENO' ERR
}

# エラーハンドラー
handle_error() {
    local exit_code="$1"
    local line_number="$2"
    local script_name="${BASH_SOURCE[1]##*/}"
    
    log "ERROR" "Script $script_name failed at line $line_number with exit code $exit_code"
    
    # スタックトレースの出力（デバッグモード時）
    if [[ "${DEBUG_MODE:-false}" == "true" ]]; then
        log "DEBUG" "Call stack:"
        local frame=0
        while caller $frame; do
            ((frame++))
        done | while read line func file; do
            log "DEBUG" "  $file:$line in function $func"
        done
    fi
}

# 初期化関数
claude_voice_init() {
    local debug_mode="${1:-false}"
    
    # デバッグモードの設定
    if [[ "$debug_mode" == "true" ]]; then
        export DEBUG_MODE=true
        CURRENT_LOG_LEVEL=$LOG_LEVEL_DEBUG
    fi
    
    # エラーハンドリングの設定
    setup_error_handling
    
    # ログディレクトリの作成
    mkdir -p "$CLAUDE_VOICE_HOME/logs"
    
    # 初期化ログ
    log "INFO" "Claude Voice Core initialized (version: $CLAUDE_VOICE_VERSION, OS: $(detect_os))"
    
    return 0
}

# クリーンアップ関数
claude_voice_cleanup() {
    log "DEBUG" "Performing cleanup..."
    
    # 一時ファイルのクリーンアップ
    find /tmp -name "claude_voice_*" -mtime +1 -delete 2>/dev/null || true
    
    # 古いログファイルの回転（7日より古いものを削除）
    find "$CLAUDE_VOICE_HOME/logs" -name "*.log" -mtime +7 -delete 2>/dev/null || true
    
    log "DEBUG" "Cleanup completed"
}

# バージョン情報の表示
show_version() {
    echo "Claude Voice $CLAUDE_VOICE_VERSION"
    echo "OS: $(detect_os)"
    echo "Home: $CLAUDE_VOICE_HOME"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "Claude Voice Core Base - Test Mode"
    claude_voice_init true
    
    echo "Testing OS detection: $(detect_os)"
    echo "Testing command check (curl): $(has_command curl && echo "available" || echo "not available")"
    echo "Testing JSON validation: $(echo '{"test": "value"}' | validate_json && echo "valid" || echo "invalid")"
    echo "Testing timer:"
    timer=$(start_timer)
    sleep 1
    echo "Elapsed: $(end_timer $timer)s"
    
    claude_voice_cleanup
    echo "Base module test completed"
fi