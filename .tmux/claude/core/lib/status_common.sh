#!/bin/bash
# Claude Voice Status Detection Common Library
# ステータス検出に関する共通関数とコンスタント

# ステータス定数
readonly STATUS_BUSY="⚡"
readonly STATUS_WAITING="⌛"
readonly STATUS_IDLE="✅"
readonly STATUS_EMPTY=""

# ステータス名定数（内部処理用）
readonly STATUS_NAME_BUSY="Busy"
readonly STATUS_NAME_WAITING="Waiting"
readonly STATUS_NAME_IDLE="Idle"
readonly STATUS_NAME_EMPTY="Empty"

# ステータスディレクトリ設定
readonly STATUS_DIR="${STATUS_DIR:-$HOME/.tmux/status}"
readonly STATUS_CACHE_TTL=2  # キャッシュ有効期限（秒）

# ステータスファイルパスを取得
get_status_file_path() {
    local window_id="${1:-$(tmux display-message -p '#I' 2>/dev/null)}"
    echo "$STATUS_DIR/window-${window_id}.status"
}

# ステータスをファイルに保存
save_window_status() {
    local window_id="$1"
    local status="$2"
    local status_file=$(get_status_file_path "$window_id")
    
    # ディレクトリ作成
    [[ ! -d "$STATUS_DIR" ]] && mkdir -p "$STATUS_DIR"
    
    # ステータスを保存（空の場合も明示的に保存）
    echo "$status" > "$status_file" 2>/dev/null
}

# 保存されたステータスを読み込み
load_window_status() {
    local window_id="$1"
    local status_file=$(get_status_file_path "$window_id")
    
    if [[ -f "$status_file" ]]; then
        cat "$status_file" 2>/dev/null || echo ""
    else
        echo ""
    fi
}

# ステータスアイコンを名前に変換
status_icon_to_name() {
    local icon="$1"
    case "$icon" in
        "$STATUS_BUSY") echo "$STATUS_NAME_BUSY" ;;
        "$STATUS_WAITING") echo "$STATUS_NAME_WAITING" ;;
        "$STATUS_IDLE") echo "$STATUS_NAME_IDLE" ;;
        *) echo "$STATUS_NAME_EMPTY" ;;
    esac
}

# ステータス名をアイコンに変換
status_name_to_icon() {
    local name="$1"
    case "$name" in
        "$STATUS_NAME_BUSY") echo "$STATUS_BUSY" ;;
        "$STATUS_NAME_WAITING") echo "$STATUS_WAITING" ;;
        "$STATUS_NAME_IDLE") echo "$STATUS_IDLE" ;;
        *) echo "$STATUS_EMPTY" ;;
    esac
}

# プロセスツリーからClaude Codeを検出
detect_claude_process() {
    local pane_pid="$1"
    
    [[ -z "$pane_pid" ]] && return 1
    
    # pstreeでclaude processを確認
    if command -v pstree >/dev/null 2>&1; then
        pstree -p "$pane_pid" 2>/dev/null | grep -q 'claude' && return 0
    fi
    
    # フォールバック: psでプロセスを確認
    if command -v ps >/dev/null 2>&1; then
        ps -p "$pane_pid" -o comm= 2>/dev/null | grep -q 'claude' && return 0
    fi
    
    return 1
}

# ペインのコンテンツからステータスを判定
analyze_pane_content() {
    local content="$1"
    
    # Busy状態のパターン
    if echo "$content" | grep -qE "(esc to interrupt|Running…|Whirring…|Thinking…|Scheming…|Puzzling…|Concocting…|tokens.*esc to interrupt)" 2>/dev/null; then
        echo "$STATUS_NAME_BUSY"
        return
    fi
    
    # Waiting状態のパターン
    if echo "$content" | grep -qE "(Do you want to proceed\?|❯ 1|❯ 2|❯ 3|tell Claude what|Should I|Would you like|Yes, and|No, keep|Choose an option|Continue\?|Proceed\?|Error:|Failed:|Exception:)" 2>/dev/null; then
        echo "$STATUS_NAME_WAITING"
        return
    fi
    
    # デフォルトはIdle
    echo "$STATUS_NAME_IDLE"
}

# 古いステータスファイルのクリーンアップ
cleanup_status_files() {
    local cleanup_marker="$STATUS_DIR/.last_cleanup"
    local now=$(date +%s)
    local last_cleanup=0
    local cleanup_interval=3600  # 1時間ごと
    
    [[ -f "$cleanup_marker" ]] && last_cleanup=$(cat "$cleanup_marker" 2>/dev/null || echo 0)
    
    # クリーンアップ間隔をチェック
    if [[ $((now - last_cleanup)) -lt $cleanup_interval ]]; then
        return
    fi
    
    # 存在しないウィンドウのステータスファイルを削除
    for status_file in "$STATUS_DIR"/window-*.status; do
        [[ ! -f "$status_file" ]] && continue
        
        local window_id=$(basename "$status_file" | sed 's/window-\([0-9]*\)\.status/\1/')
        
        # tmuxウィンドウが存在するか確認
        if ! tmux list-windows -F '#I' 2>/dev/null | grep -q "^${window_id}$"; then
            rm -f "$status_file"
        fi
    done
    
    # 24時間以上古いファイルを削除
    find "$STATUS_DIR" -name "window-*.status" -mtime +1 -delete 2>/dev/null
    
    echo "$now" > "$cleanup_marker"
}