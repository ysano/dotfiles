#!/bin/bash
# Claude Status Detection v6.0 - Simplified
# tmux用Claude状態検出スクリプト（シンプル版）

set -euo pipefail

# === グローバル設定 ===
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly CACHE_FILE="/tmp/.claude_status_cache_$$"
readonly CACHE_DURATION=2
readonly STATUS_DIR="$HOME/.tmux/status"

# 設定
DEBUG_MODE=false
USE_CACHE=true

# カラーコード（デバッグ用）
readonly YELLOW='\033[1;33m'
readonly GREEN='\033[0;32m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# === ヘルプ関数 ===
show_help() {
    echo "Claude Status Detection v6.0"
    echo ""
    echo "Usage: $(basename "$0") [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --debug           デバッグ出力を有効化"
    echo "  --no-cache        キャッシュを無効化"
    echo "  --help            このヘルプを表示"
    echo ""
    echo "Examples:"
    echo "  $(basename "$0")           # 標準実行"
    echo "  $(basename "$0") --debug   # デバッグモード"
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

# === Claude Code検出関数（詳細パターンマッチング） ===
detect_claude() {
    local claude_pid=$(pgrep -f "claude " 2>/dev/null | head -1)
    
    if [[ -z "$claude_pid" ]]; then
        echo ""
        return 0
    fi
    
    # tmuxペインの内容を取得（50行）
    local pane_text=$(tmux capture-pane -p -S -50 2>/dev/null || echo "")
    
    # 詳細なパターンマッチング
    if echo "$pane_text" | grep -qE "(Interrupt streaming|Generating|Processing|Thinking|Analyzing|Writing|Reading|Searching|Executing|Running)"; then
        echo "⚡"  # 実行中
    elif echo "$pane_text" | grep -qE "(Type your message|Waiting for input|Ready|Human:)"; then
        echo "⌛"  # 待機中
    elif echo "$pane_text" | grep -qE "(Complete|Done|Finished|Successfully|Created|Updated|Deleted|Modified)"; then
        echo "✅"  # 完了
    else
        echo "✅"  # デフォルト: 完了
    fi
}

# === デバッグモード検出 ===
detect_claude_debug() {
    echo -e "${YELLOW}=== Claude Status Debug ===${NC}" >&2
    
    local claude_pid=$(pgrep -f "claude " 2>/dev/null | head -1)
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
    detect_claude
}

# === 引数解析 ===
while [[ $# -gt 0 ]]; do
    case "$1" in
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
    debug "Debug: $DEBUG_MODE, Cache: $USE_CACHE"
    
    # キャッシュチェック
    if check_cache; then
        debug "Using cached result"
        exit 0
    fi
    
    # 検出実行
    local status=""
    if [[ "$DEBUG_MODE" == "true" ]]; then
        status=$(detect_claude_debug)
    else
        status=$(detect_claude)
    fi
    
    # 結果出力とキャッシュ保存
    echo "$status"
    save_cache "$status"
    
    debug "Detection complete: $status"
}

# クリーンアップ
trap "rm -f $CACHE_FILE" EXIT

# 実行
main