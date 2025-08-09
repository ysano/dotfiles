#!/bin/bash
# Claude Status Unified Detection v5.0
# 統合されたステータス検出スクリプト - 複数モードをサポート
#
# Usage:
#   claude-status-unified.sh [OPTIONS]
#
# Options:
#   --mode <mode>     検出モード選択 (smart|enhanced|precision|debug|display)
#                     デフォルト: smart
#   --debug           デバッグ出力を有効化
#   --no-cache        キャッシュを無効化
#   --help            ヘルプ表示
#
# Examples:
#   claude-status-unified.sh                    # スマートモード（デフォルト）
#   claude-status-unified.sh --mode precision   # 高精度モード
#   claude-status-unified.sh --mode debug       # デバッグモード
#   claude-status-unified.sh --mode display     # 表示専用モード

set -euo pipefail

# === グローバル設定 ===
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly CACHE_FILE="/tmp/.claude_status_cache_$$"
readonly CACHE_DURATION=2
readonly STATUS_DIR="$HOME/.tmux/status"

# モード設定（デフォルト: smart）
MODE="smart"
DEBUG_MODE=false
USE_CACHE=true

# カラーコード（デバッグ用）
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# === ヘルプ関数 ===
show_help() {
    echo "Claude Status Unified Detection v5.0"
    echo ""
    echo "Usage: $(basename "$0") [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --mode <mode>     検出モード選択"
    echo "                    smart    : スマート検出（デフォルト）"
    echo "                    enhanced : 拡張3状態検出"
    echo "                    precision: 高精度検出"
    echo "                    debug    : デバッグモード"
    echo "                    display  : 表示専用モード"
    echo "  --debug           デバッグ出力を有効化"
    echo "  --no-cache        キャッシュを無効化"
    echo "  --help            このヘルプを表示"
    echo ""
    echo "Examples:"
    echo "  $(basename "$0")                    # スマートモード"
    echo "  $(basename "$0") --mode precision   # 高精度モード"
    echo "  $(basename "$0") --mode debug       # デバッグモード"
}

# === デバッグ関数 ===
debug() {
    if [[ "$DEBUG_MODE" == "true" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $*" >&2
    fi
}

# === キャッシュ管理 ===
check_cache() {
    if [[ "$USE_CACHE" == "false" ]]; then
        return 1
    fi
    
    if [[ -f "$CACHE_FILE" ]]; then
        local cache_age=$(($(date +%s) - $(stat -f%m "$CACHE_FILE" 2>/dev/null || stat -c%Y "$CACHE_FILE" 2>/dev/null || echo 0)))
        if [[ $cache_age -lt $CACHE_DURATION ]]; then
            cat "$CACHE_FILE"
            return 0
        fi
    fi
    return 1
}

save_cache() {
    if [[ "$USE_CACHE" == "true" ]]; then
        echo "$1" > "$CACHE_FILE"
    fi
}

# === Claude Code検出関数 ===
detect_claude_smart() {
    # スマート検出: バランスの取れた検出
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
        echo "✅"  # Idle
    fi
}

detect_claude_enhanced() {
    # 拡張検出: より詳細な3状態検出
    local claude_pid=$(pgrep -f "claude" 2>/dev/null | head -1)
    
    if [[ -z "$claude_pid" ]]; then
        echo ""
        return 0
    fi
    
    local pane_text=$(tmux capture-pane -p -S -50 2>/dev/null || echo "")
    
    # 詳細なパターンマッチング
    if echo "$pane_text" | grep -qE "(Interrupt streaming|Generating|Processing|Thinking)"; then
        echo "⚡"
    elif echo "$pane_text" | grep -qE "(Type your message|Waiting for input|Ready)"; then
        echo "⌛"
    elif echo "$pane_text" | grep -qE "(Complete|Done|Finished)"; then
        echo "✅"
    else
        echo "✅"
    fi
}

detect_claude_precision() {
    # 高精度検出: 複数の指標を組み合わせた検出
    local claude_pid=$(pgrep -f "claude" 2>/dev/null | head -1)
    
    if [[ -z "$claude_pid" ]]; then
        echo ""
        return 0
    fi
    
    # CPU使用率チェック
    local cpu_usage=$(ps -p "$claude_pid" -o %cpu= 2>/dev/null | tr -d ' ')
    
    # tmuxペインの内容を取得（より多くの行）
    local pane_text=$(tmux capture-pane -p -S -100 2>/dev/null || echo "")
    
    # 複合判定
    if [[ $(echo "$cpu_usage > 50" | bc -l 2>/dev/null || echo 0) -eq 1 ]]; then
        echo "⚡"
    elif echo "$pane_text" | grep -qE "(Interrupt streaming|Generating|Processing)"; then
        echo "⚡"
    elif echo "$pane_text" | grep -qE "(Type your message|Waiting for)"; then
        echo "⌛"
    else
        echo "✅"
    fi
}

detect_claude_debug() {
    # デバッグモード: 詳細情報付き検出
    echo -e "${YELLOW}=== Claude Status Debug ===${NC}" >&2
    
    local claude_pid=$(pgrep -f "claude" 2>/dev/null | head -1)
    echo -e "${GREEN}Claude PID:${NC} ${claude_pid:-Not found}" >&2
    
    if [[ -z "$claude_pid" ]]; then
        echo ""
        return 0
    fi
    
    # プロセス情報
    echo -e "${GREEN}Process Info:${NC}" >&2
    ps -p "$claude_pid" -o pid,ppid,%cpu,%mem,etime,command 2>/dev/null | head -2 >&2
    
    # tmuxペイン情報
    echo -e "${GREEN}tmux Pane Content (last 10 lines):${NC}" >&2
    tmux capture-pane -p -S -10 2>/dev/null | head -10 >&2
    
    # 通常の検出実行
    detect_claude_smart
}

display_status() {
    # 表示専用モード: ファイルから読み取るのみ
    local window_id="${TMUX_WINDOW:-1}"
    local status_file="$STATUS_DIR/window-${window_id}.status"
    
    if [[ -f "$status_file" ]]; then
        cat "$status_file"
    else
        echo ""
    fi
}

# === 引数解析 ===
while [[ $# -gt 0 ]]; do
    case "$1" in
        --mode)
            MODE="$2"
            shift 2
            ;;
        --debug)
            DEBUG_MODE=true
            shift
            ;;
        --no-cache)
            USE_CACHE=false
            shift
            ;;
        --help|-h)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            show_help
            exit 1
            ;;
    esac
done

# === メイン処理 ===
main() {
    debug "Mode: $MODE, Debug: $DEBUG_MODE, Cache: $USE_CACHE"
    
    # キャッシュチェック
    if check_cache; then
        debug "Using cached result"
        exit 0
    fi
    
    # モードに応じた検出実行
    local status=""
    case "$MODE" in
        smart)
            status=$(detect_claude_smart)
            ;;
        enhanced)
            status=$(detect_claude_enhanced)
            ;;
        precision)
            status=$(detect_claude_precision)
            ;;
        debug)
            status=$(detect_claude_debug)
            ;;
        display)
            status=$(display_status)
            ;;
        *)
            echo "Invalid mode: $MODE" >&2
            show_help
            exit 1
            ;;
    esac
    
    # 結果出力とキャッシュ保存
    echo "$status"
    save_cache "$status"
    
    debug "Detection complete: $status"
}

# クリーンアップ
trap "rm -f $CACHE_FILE" EXIT

# 実行
main