#!/bin/bash
# TMux Scripts Core Library
# Version: 1.0.0
# 
# 基盤機能を提供する共通ライブラリ
# - ロギングシステム
# - エラーハンドリング
# - 清掃処理
# - デバッグ機能

# インポートガード（重複読み込み防止）
[[ -n "${_TMUX_CORE_LOADED}" ]] && return 0
declare -gr _TMUX_CORE_LOADED=1

# === グローバル設定 ===
declare -g TMUX_SCRIPTS_VERSION="1.0.0"
declare -g TMUX_SCRIPTS_DEBUG="${TMUX_SCRIPTS_DEBUG:-false}"
declare -g TMUX_SCRIPTS_LOG_FILE="${TMUX_SCRIPTS_LOG_FILE:-/tmp/tmux-scripts.log}"
declare -g TMUX_SCRIPTS_LOG_LEVEL="${TMUX_SCRIPTS_LOG_LEVEL:-INFO}"

# カラーコード定義（ターミナル出力用）
if [[ -t 2 ]]; then
    readonly RED='\033[0;31m'
    readonly GREEN='\033[0;32m'
    readonly YELLOW='\033[1;33m'
    readonly BLUE='\033[0;34m'
    readonly MAGENTA='\033[0;35m'
    readonly CYAN='\033[0;36m'
    readonly WHITE='\033[1;37m'
    readonly NC='\033[0m' # No Color
else
    readonly RED=''
    readonly GREEN=''
    readonly YELLOW=''
    readonly BLUE=''
    readonly MAGENTA=''
    readonly CYAN=''
    readonly WHITE=''
    readonly NC=''
fi

# === ロギングシステム ===

# ログレベル定数
readonly LOG_LEVEL_DEBUG=0
readonly LOG_LEVEL_INFO=1
readonly LOG_LEVEL_WARN=2
readonly LOG_LEVEL_ERROR=3
readonly LOG_LEVEL_FATAL=4

# 現在のログレベルを数値に変換
_get_log_level_value() {
    case "${TMUX_SCRIPTS_LOG_LEVEL}" in
        "DEBUG") echo $LOG_LEVEL_DEBUG ;;
        "INFO")  echo $LOG_LEVEL_INFO ;;
        "WARN")  echo $LOG_LEVEL_WARN ;;
        "ERROR") echo $LOG_LEVEL_ERROR ;;
        "FATAL") echo $LOG_LEVEL_FATAL ;;
        *)       echo $LOG_LEVEL_INFO ;;
    esac
}

# 内部ログ関数
_log_internal() {
    local level="$1"
    local level_value="$2"
    local color="$3"
    shift 3
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S' 2>/dev/null || echo "$(printf '%(%Y-%m-%d %H:%M:%S)T' -1)")
    local current_level=$(_get_log_level_value)
    
    # ログレベルチェック
    [[ $level_value -lt $current_level ]] && return 0
    
    # ログディレクトリ作成
    local log_dir=$(dirname "$TMUX_SCRIPTS_LOG_FILE")
    [[ ! -d "$log_dir" ]] && mkdir -p "$log_dir"
    
    # ファイルログ（カラーなし）
    echo "[$timestamp] [$level] $message" >> "$TMUX_SCRIPTS_LOG_FILE"
    
    # コンソール出力（カラー付き）
    if [[ -t 2 ]]; then
        echo -e "${color}[$timestamp] [$level]${NC} $message" >&2
    else
        echo "[$timestamp] [$level] $message" >&2
    fi
}

# パブリックログ関数
log_debug() { 
    _log_internal "DEBUG" $LOG_LEVEL_DEBUG "$BLUE" "$@"
}

log_info() { 
    _log_internal "INFO" $LOG_LEVEL_INFO "$GREEN" "$@"
}

log_warn() { 
    _log_internal "WARN" $LOG_LEVEL_WARN "$YELLOW" "$@"
}

log_error() { 
    _log_internal "ERROR" $LOG_LEVEL_ERROR "$RED" "$@"
}

log_fatal() { 
    _log_internal "FATAL" $LOG_LEVEL_FATAL "$MAGENTA" "$@"
    exit 1
}

# ログ設定関数
setup_logging() {
    local log_file="${1:-$TMUX_SCRIPTS_LOG_FILE}"
    local log_level="${2:-$TMUX_SCRIPTS_LOG_LEVEL}"
    
    export TMUX_SCRIPTS_LOG_FILE="$log_file"
    export TMUX_SCRIPTS_LOG_LEVEL="$log_level"
    
    log_debug "ログ設定: file=$log_file, level=$log_level"
}

# === エラーハンドリング ===

# エラー終了関数
error_exit() {
    local message="$1"
    local exit_code="${2:-1}"
    
    log_error "$message"
    
    # スタックトレース出力（デバッグモード時）
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        log_debug "Stack trace:"
        local frame=0
        while caller $frame; do
            ((frame++))
        done
    fi
    
    exit "$exit_code"
}

# エラートラップハンドラー
_error_trap_handler() {
    local exit_code=$?
    local line_no=$1
    local bash_lineno=$2
    local last_command=$3
    
    log_error "Command failed with exit code $exit_code at line $line_no: $last_command"
    
    # カスタムエラーハンドラーがある場合は呼び出し
    if declare -f custom_error_handler >/dev/null; then
        custom_error_handler $exit_code $line_no "$last_command"
    fi
    
    exit $exit_code
}

# エラートラップ設定
set_error_trap() {
    set -euo pipefail
    trap '_error_trap_handler $LINENO $BASH_LINENO "$BASH_COMMAND"' ERR
}

# === 清掃処理 ===

# クリーンアップアイテムの配列
declare -ga _CLEANUP_ITEMS=()
declare -ga _CLEANUP_FUNCTIONS=()

# クリーンアップアイテム登録
register_cleanup() {
    local items=("$@")
    _CLEANUP_ITEMS+=("${items[@]}")
    log_debug "クリーンアップ登録: ${items[*]}"
}

# クリーンアップ関数登録
register_cleanup_function() {
    local func="$1"
    _CLEANUP_FUNCTIONS+=("$func")
    log_debug "クリーンアップ関数登録: $func"
}

# クリーンアップハンドラー
_cleanup_handler() {
    local exit_code=$?
    
    log_debug "クリーンアップ処理開始 (exit_code=$exit_code)"
    
    # 登録されたファイル/ディレクトリの削除
    for item in "${_CLEANUP_ITEMS[@]}"; do
        if [[ -e "$item" ]]; then
            log_debug "削除中: $item"
            rm -rf "$item"
        fi
    done
    
    # 登録された関数の実行
    for func in "${_CLEANUP_FUNCTIONS[@]}"; do
        if declare -f "$func" >/dev/null; then
            log_debug "実行中: $func"
            "$func"
        fi
    done
    
    log_debug "クリーンアップ処理完了"
    
    exit $exit_code
}

# クリーンアップ有効化
enable_cleanup() {
    trap _cleanup_handler EXIT INT TERM
    log_debug "クリーンアップトラップ有効化"
}

# === デバッグ機能 ===

# デバッグモード設定
set_debug_mode() {
    local enabled="${1:-true}"
    export TMUX_SCRIPTS_DEBUG="$enabled"
    
    if [[ "$enabled" == "true" ]]; then
        set -x  # bashデバッグ出力有効化
        export TMUX_SCRIPTS_LOG_LEVEL="DEBUG"
        log_info "デバッグモード有効"
    else
        set +x  # bashデバッグ出力無効化
        log_info "デバッグモード無効"
    fi
}

# 変数ダンプ
dump_vars() {
    local prefix="${1:-TMUX_}"
    log_debug "環境変数ダンプ (prefix=$prefix):"
    
    env | grep "^$prefix" | while read -r line; do
        log_debug "  $line"
    done
}

# 実行時間測定
measure_time() {
    local start_time=$(date +%s%N)
    "$@"
    local exit_code=$?
    local end_time=$(date +%s%N)
    local duration=$(( (end_time - start_time) / 1000000 ))
    
    log_debug "実行時間: ${duration}ms - コマンド: $*"
    return $exit_code
}

# === ユーティリティ関数 ===

# コマンド存在チェック
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# 必須コマンドチェック
require_commands() {
    local commands=("$@")
    local missing=()
    
    for cmd in "${commands[@]}"; do
        if ! command_exists "$cmd"; then
            missing+=("$cmd")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        error_exit "必要なコマンドが見つかりません: ${missing[*]}"
    fi
}

# ファイル存在チェック
require_files() {
    local files=("$@")
    local missing=()
    
    for file in "${files[@]}"; do
        if [[ ! -f "$file" ]]; then
            missing+=("$file")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        error_exit "必要なファイルが見つかりません: ${missing[*]}"
    fi
}

# ディレクトリ存在チェック
require_dirs() {
    local dirs=("$@")
    local missing=()
    
    for dir in "${dirs[@]}"; do
        if [[ ! -d "$dir" ]]; then
            missing+=("$dir")
        fi
    done
    
    if [[ ${#missing[@]} -gt 0 ]]; then
        error_exit "必要なディレクトリが見つかりません: ${missing[*]}"
    fi
}

# === 初期化 ===

# ライブラリ初期化
_init_core() {
    # ログファイルディレクトリ作成
    local log_dir=$(dirname "$TMUX_SCRIPTS_LOG_FILE")
    [[ ! -d "$log_dir" ]] && mkdir -p "$log_dir"
    
    # デバッグモードチェック
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        log_debug "TMux Scripts Core Library v${TMUX_SCRIPTS_VERSION} loaded"
        log_debug "Log file: $TMUX_SCRIPTS_LOG_FILE"
        log_debug "Log level: $TMUX_SCRIPTS_LOG_LEVEL"
    fi
}

# 自動初期化
_init_core

# === エクスポート ===

# 関数のエクスポート（子プロセスで使用可能にする）
export -f log_debug log_info log_warn log_error log_fatal
export -f error_exit command_exists
export -f require_commands require_files require_dirs