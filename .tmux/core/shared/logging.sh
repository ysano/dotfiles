#!/bin/bash
# Unified Logging System - Centralized logging for all tmux components
# 統一ログシステム - 全tmuxコンポーネント用統一ログ

set -euo pipefail

readonly LOGGING_VERSION="1.0.0"

# === ログ設定 ===
readonly LOG_LEVELS=("DEBUG" "INFO" "WARN" "ERROR")
readonly DEFAULT_LOG_LEVEL="INFO"
readonly LOG_DATE_FORMAT="+%H:%M:%S"

# 環境変数でのログレベル制御
export TMUX_LOG_LEVEL="${TMUX_LOG_LEVEL:-$DEFAULT_LOG_LEVEL}"
export TMUX_LOG_DEBUG="${TMUX_LOG_DEBUG:-false}"
export TMUX_LOG_FILE="${TMUX_LOG_FILE:-}"

# === ログレベル判定 ===
_get_log_level_priority() {
    local level="$1"
    case "$level" in
        "DEBUG") echo 0 ;;
        "INFO")  echo 1 ;;
        "WARN")  echo 2 ;;
        "ERROR") echo 3 ;;
        *) echo 1 ;;
    esac
}

_should_log() {
    local message_level="$1"
    local current_level="$TMUX_LOG_LEVEL"
    
    # DEBUGフラグが有効な場合はすべてログ出力
    if [[ "$TMUX_LOG_DEBUG" == "true" ]]; then
        return 0
    fi
    
    # エラーレベルは常に出力
    if [[ "$message_level" == "ERROR" ]]; then
        return 0
    fi
    
    # レベル比較
    local message_priority=$(_get_log_level_priority "$message_level")
    local current_priority=$(_get_log_level_priority "$current_level")
    
    [[ $message_priority -ge $current_priority ]]
}

# === メインログ関数 ===
log_message() {
    local component="$1"
    local level="$2"
    shift 2
    local message="$*"
    
    # ログレベルチェック
    if ! _should_log "$level"; then
        return 0
    fi
    
    # ログフォーマット
    local timestamp=$(date "$LOG_DATE_FORMAT")
    local log_line="[$timestamp] [$component] [$level] $message"
    
    # 出力先決定
    local output_fd=2  # stderr デフォルト
    case "$level" in
        "INFO"|"DEBUG") output_fd=2 ;;
        "WARN"|"ERROR") output_fd=2 ;;
    esac
    
    # ログ出力
    echo "$log_line" >&$output_fd
    
    # ファイル出力（設定されている場合）
    if [[ -n "$TMUX_LOG_FILE" ]]; then
        echo "$(date '+%Y-%m-%d %H:%M:%S') - $log_line" >> "$TMUX_LOG_FILE" 2>/dev/null || true
    fi
}

# === コンポーネント専用ログ関数 ===
log_audio() {
    log_message "AUDIO" "$@"
}

log_voice() {
    log_message "VOICE" "$@"
}

log_ollama() {
    log_message "OLLAMA" "$@"
}

log_init() {
    log_message "INIT" "$@"
}

log_config() {
    log_message "CONFIG" "$@"
}

log_switcher() {
    log_message "SWITCHER" "$@"
}

# === ログユーティリティ ===
log_function_entry() {
    local function_name="$1"
    local component="${2:-GENERAL}"
    log_message "$component" "DEBUG" "Entering function: $function_name"
}

log_function_exit() {
    local function_name="$1"
    local exit_code="${2:-0}"
    local component="${3:-GENERAL}"
    log_message "$component" "DEBUG" "Exiting function: $function_name (exit code: $exit_code)"
}

log_performance() {
    local operation="$1"
    local duration="$2"
    local component="${3:-PERF}"
    log_message "$component" "INFO" "Performance: $operation took ${duration}ms"
}

log_error_with_context() {
    local error_msg="$1"
    local context="$2"
    local component="${3:-ERROR}"
    log_message "$component" "ERROR" "$error_msg [Context: $context]"
}

# === デバッグ支援 ===
enable_debug_logging() {
    export TMUX_LOG_DEBUG="true"
    export TMUX_LOG_LEVEL="DEBUG"
    log_message "LOGGING" "INFO" "Debug logging enabled"
}

disable_debug_logging() {
    export TMUX_LOG_DEBUG="false"
    export TMUX_LOG_LEVEL="$DEFAULT_LOG_LEVEL"
    log_message "LOGGING" "INFO" "Debug logging disabled"
}

set_log_file() {
    local log_file="$1"
    export TMUX_LOG_FILE="$log_file"
    
    # ディレクトリ作成
    local log_dir=$(dirname "$log_file")
    mkdir -p "$log_dir" 2>/dev/null || true
    
    log_message "LOGGING" "INFO" "Log file set to: $log_file"
}

# === ログ統計 ===
show_log_status() {
    echo "=== TMux Logging System Status ==="
    echo "Version: $LOGGING_VERSION"
    echo "Current Log Level: $TMUX_LOG_LEVEL"
    echo "Debug Mode: $TMUX_LOG_DEBUG"
    echo "Log File: ${TMUX_LOG_FILE:-none}"
    echo "Available Levels: ${LOG_LEVELS[*]}"
    
    if [[ -n "$TMUX_LOG_FILE" && -f "$TMUX_LOG_FILE" ]]; then
        echo "Log File Size: $(du -h "$TMUX_LOG_FILE" 2>/dev/null | cut -f1 || echo 'unknown')"
        echo "Recent Entries: $(tail -n 5 "$TMUX_LOG_FILE" 2>/dev/null | wc -l || echo '0')"
    fi
}

# === ログローテーション ===
rotate_log_file() {
    if [[ -n "$TMUX_LOG_FILE" && -f "$TMUX_LOG_FILE" ]]; then
        local backup_file="${TMUX_LOG_FILE}.$(date +%Y%m%d-%H%M%S)"
        mv "$TMUX_LOG_FILE" "$backup_file"
        log_message "LOGGING" "INFO" "Log file rotated to: $backup_file"
    fi
}

# === メイン実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-status}" in
        "status")
            show_log_status
            ;;
        "test")
            echo "Testing logging system..."
            log_message "TEST" "DEBUG" "This is a debug message"
            log_message "TEST" "INFO" "This is an info message"
            log_message "TEST" "WARN" "This is a warning message"
            log_message "TEST" "ERROR" "This is an error message"
            echo "Test completed"
            ;;
        "enable-debug")
            enable_debug_logging
            ;;
        "disable-debug")
            disable_debug_logging
            ;;
        "rotate")
            rotate_log_file
            ;;
        *)
            echo "Usage: $0 {status|test|enable-debug|disable-debug|rotate}"
            exit 1
            ;;
    esac
fi