#!/bin/bash
# Common Functions Library
# 共通関数ライブラリ - 複数のスクリプトで使用される関数を一元管理

set -euo pipefail

# === バージョン情報 ===
readonly COMMON_FUNCTIONS_VERSION="1.0.0"

# === カラーコード定義 ===
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly MAGENTA='\033[0;35m'
readonly CYAN='\033[0;36m'
readonly WHITE='\033[1;37m'
readonly NC='\033[0m' # No Color

# === ログレベル定数 ===
readonly LOG_LEVEL_DEBUG=0
readonly LOG_LEVEL_INFO=1
readonly LOG_LEVEL_WARN=2
readonly LOG_LEVEL_ERROR=3

# デフォルトログレベル
: ${CLAUDE_LOG_LEVEL:=$LOG_LEVEL_INFO}

# === 基本ログ関数 ===
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S' 2>/dev/null || echo "$(printf '%(%Y-%m-%d %H:%M:%S)T' -1)")
    local level_num
    local color=""

    case "$level" in
        "DEBUG")
            level_num=$LOG_LEVEL_DEBUG
            color=$BLUE
            ;;
        "INFO")
            level_num=$LOG_LEVEL_INFO
            color=$GREEN
            ;;
        "WARN")
            level_num=$LOG_LEVEL_WARN
            color=$YELLOW
            ;;
        "ERROR")
            level_num=$LOG_LEVEL_ERROR
            color=$RED
            ;;
        *)
            level_num=$LOG_LEVEL_INFO
            level="INFO"
            color=$GREEN
            ;;
    esac

    # ログレベルチェック
    if [[ $level_num -lt ${CLAUDE_LOG_LEVEL:-$LOG_LEVEL_INFO} ]]; then
        return 0
    fi

    # ログ出力
    if [[ -t 2 ]]; then
        # ターミナル出力時はカラー付き
        echo -e "${color}[$timestamp] [$level]${NC} $message" >&2
    else
        # ファイル出力時はカラーなし
        echo "[$timestamp] [$level] $message" >&2
    fi
}

# === 簡易ログ関数 ===
log_debug() { log "DEBUG" "$*"; }
log_info() { log "INFO" "$*"; }
log_warn() { log "WARN" "$*"; }
log_error() { log "ERROR" "$*"; }

# === OS検出関数 ===
detect_os() {
    local os_type="unknown"
    
    # キャッシュチェック
    if [[ -n "${CACHED_OS_TYPE:-}" ]]; then
        echo "$CACHED_OS_TYPE"
        return 0
    fi
    
    # OSTYPE変数から判定
    if [[ -n "${OSTYPE:-}" ]]; then
        case "$OSTYPE" in
            darwin*)  os_type="macos" ;;
            linux*)   os_type="linux" ;;
            msys*|cygwin*|mingw*) os_type="windows" ;;
            freebsd*) os_type="freebsd" ;;
            openbsd*) os_type="openbsd" ;;
            netbsd*)  os_type="netbsd" ;;
        esac
    fi
    
    # unameから判定（フォールバック）
    if [[ "$os_type" == "unknown" ]] && command -v uname >/dev/null 2>&1; then
        local uname_s=$(uname -s 2>/dev/null)
        case "$uname_s" in
            Darwin)  os_type="macos" ;;
            Linux)   os_type="linux" ;;
            FreeBSD) os_type="freebsd" ;;
            OpenBSD) os_type="openbsd" ;;
            NetBSD)  os_type="netbsd" ;;
            CYGWIN*|MINGW*|MSYS*) os_type="windows" ;;
        esac
    fi
    
    # WSL検出
    if [[ "$os_type" == "linux" ]]; then
        if [[ -n "${WSL_DISTRO_NAME:-}" ]] || [[ -f /proc/sys/fs/binfmt_misc/WSLInterop ]]; then
            os_type="wsl"
        fi
    fi
    
    # キャッシュ保存
    export CACHED_OS_TYPE="$os_type"
    echo "$os_type"
}

# === Claude Code検出基本関数 ===
detect_claude_status() {
    local mode="${1:-smart}"
    local claude_pid=$(pgrep -f "claude" 2>/dev/null | head -1)
    
    if [[ -z "$claude_pid" ]]; then
        echo ""
        return 0
    fi
    
    # tmuxペインの内容を取得
    local pane_text=$(tmux capture-pane -p -S -30 2>/dev/null || echo "")
    
    # パターンマッチング
    if echo "$pane_text" | grep -q "Interrupt streaming"; then
        echo "⚡"  # Busy
    elif echo "$pane_text" | grep -q "Type your message"; then
        echo "⌛"  # Waiting
    else
        echo "✅"  # Idle/Complete
    fi
}

# === エラーハンドリング ===
handle_error() {
    local exit_code=$?
    local line_no=$1
    local bash_lineno=$2
    local last_command=$3
    
    log_error "Command failed with exit code $exit_code at line $line_no: $last_command"
    
    # スタックトレース出力（デバッグモード時）
    if [[ "${CLAUDE_DEBUG:-false}" == "true" ]]; then
        echo "Stack trace:" >&2
        local frame=0
        while caller $frame; do
            ((frame++))
        done
    fi
    
    return $exit_code
}

# === クリーンアップ関数 ===
cleanup() {
    local exit_code=$?
    
    # 一時ファイルの削除
    rm -f /tmp/.claude_*_$$ 2>/dev/null || true
    
    # カスタムクリーンアップ処理（定義されている場合）
    if declare -f custom_cleanup >/dev/null; then
        custom_cleanup
    fi
    
    exit $exit_code
}

# === コマンド存在チェック ===
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# === ファイル/ディレクトリ存在チェック ===
file_exists() {
    [[ -f "$1" ]]
}

dir_exists() {
    [[ -d "$1" ]]
}

# === 実行可能チェック ===
is_executable() {
    [[ -x "$1" ]]
}

# === tmux環境チェック ===
in_tmux() {
    [[ -n "${TMUX:-}" ]]
}

# === タイムアウト付きコマンド実行 ===
run_with_timeout() {
    local timeout=$1
    shift
    
    if command_exists timeout; then
        timeout "$timeout" "$@"
    elif command_exists gtimeout; then
        gtimeout "$timeout" "$@"
    else
        # タイムアウトコマンドが使えない場合は通常実行
        "$@"
    fi
}

# === JSON処理ヘルパー ===
json_get() {
    local json="$1"
    local key="$2"
    
    if command_exists jq; then
        echo "$json" | jq -r "$key" 2>/dev/null
    else
        # jqがない場合の簡易パース（単純なケースのみ）
        echo "$json" | grep -oE "\"$key\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" | sed -E "s/.*\"$key\"[[:space:]]*:[[:space:]]*\"([^\"]*)\".*/\1/"
    fi
}

# === プロセス管理 ===
is_process_running() {
    local pid=$1
    kill -0 "$pid" 2>/dev/null
}

wait_for_process() {
    local pid=$1
    local timeout=${2:-30}
    local elapsed=0
    
    while is_process_running "$pid" && [[ $elapsed -lt $timeout ]]; do
        sleep 1
        ((elapsed++))
    done
    
    [[ $elapsed -lt $timeout ]]
}

# === テストアサーション関数 ===
assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"
    
    if [[ "$expected" != "$actual" ]]; then
        log_error "$message: expected '$expected', got '$actual'"
        return 1
    fi
    return 0
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-Assertion failed}"
    
    if [[ "$haystack" != *"$needle"* ]]; then
        log_error "$message: '$needle' not found in '$haystack'"
        return 1
    fi
    return 0
}

assert_true() {
    local condition="$1"
    local message="${2:-Assertion failed}"
    
    if ! eval "$condition"; then
        log_error "$message: condition '$condition' is false"
        return 1
    fi
    return 0
}

# === パフォーマンス測定 ===
measure_time() {
    local start_time=$(date +%s%N)
    "$@"
    local exit_code=$?
    local end_time=$(date +%s%N)
    local duration=$(( (end_time - start_time) / 1000000 ))
    
    log_debug "Command '$*' took ${duration}ms"
    return $exit_code
}

# === 標準main関数テンプレート ===
standard_main() {
    # エラーハンドリング設定
    set -euo pipefail
    trap 'handle_error $LINENO $BASH_LINENO "$BASH_COMMAND"' ERR
    trap 'cleanup' EXIT INT TERM
    
    # 引数チェック
    if [[ $# -eq 0 ]] && declare -f show_usage >/dev/null; then
        show_usage
        exit 0
    fi
    
    # カスタムmain処理（定義されている場合）
    if declare -f custom_main >/dev/null; then
        custom_main "$@"
    else
        log_warn "No custom_main function defined"
    fi
}

# === 初期化処理 ===
init_common_functions() {
    # 環境変数設定
    export COMMON_FUNCTIONS_LOADED=true
    export COMMON_FUNCTIONS_PATH="${BASH_SOURCE[0]}"
    
    log_debug "Common functions library v${COMMON_FUNCTIONS_VERSION} loaded"
}

# 自動初期化（sourceされた時）
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    init_common_functions
fi