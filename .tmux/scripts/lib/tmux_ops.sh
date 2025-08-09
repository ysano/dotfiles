#!/bin/bash
# TMux Scripts TMux Operations Library
# Version: 1.0.0
#
# tmux操作関連のユーティリティ関数
# - セッション/ウィンドウ/ペイン操作
# - ステータス管理
# - 環境変数操作

# インポートガード
[[ -n "${_TMUX_OPS_LOADED}" ]] && return 0
declare -gr _TMUX_OPS_LOADED=1

# 依存ライブラリ
source "$(dirname "${BASH_SOURCE[0]}")/core.sh"
source "$(dirname "${BASH_SOURCE[0]}")/platform.sh"

# === 定数定義 ===
readonly TMUX_STATUS_DIR="${TMUX_STATUS_DIR:-$HOME/.tmux/status}"
readonly TMUX_CACHE_DIR="${TMUX_CACHE_DIR:-$HOME/.tmux/cache}"

# === tmux環境チェック ===

# tmux内で実行されているかチェック
in_tmux() {
    [[ -n "${TMUX:-}" ]]
}

# tmuxが利用可能かチェック
tmux_available() {
    command_exists tmux
}

# tmuxバージョン取得
get_tmux_version() {
    if tmux_available; then
        tmux -V 2>/dev/null | cut -d' ' -f2
    else
        echo ""
    fi
}

# tmuxバージョン比較
tmux_version_ge() {
    local required="$1"
    local current=$(get_tmux_version)
    
    if [[ -z "$current" ]]; then
        return 1
    fi
    
    # バージョン比較（簡易版）
    [[ "$(printf '%s\n' "$required" "$current" | sort -V | head -n1)" == "$required" ]]
}

# === セッション操作 ===

# 現在のセッション名取得
get_current_session() {
    if in_tmux; then
        tmux display-message -p '#{session_name}' 2>/dev/null
    else
        echo ""
    fi
}

# セッション一覧取得
list_sessions() {
    if tmux_available; then
        tmux list-sessions -F '#{session_name}' 2>/dev/null
    fi
}

# セッション存在チェック
session_exists() {
    local session="$1"
    tmux has-session -t "$session" 2>/dev/null
}

# 新規セッション作成
create_session() {
    local session="${1:-default}"
    local window="${2:-}"
    local command="${3:-}"
    
    if session_exists "$session"; then
        log_warn "セッション '$session' は既に存在します"
        return 1
    fi
    
    local cmd="tmux new-session -d -s '$session'"
    [[ -n "$window" ]] && cmd="$cmd -n '$window'"
    [[ -n "$command" ]] && cmd="$cmd '$command'"
    
    eval "$cmd"
}

# セッションにアタッチ
attach_session() {
    local session="${1:-}"
    
    if [[ -z "$session" ]]; then
        # デフォルトセッションにアタッチ
        tmux attach-session 2>/dev/null || tmux new-session
    else
        if session_exists "$session"; then
            tmux attach-session -t "$session"
        else
            log_error "セッション '$session' が見つかりません"
            return 1
        fi
    fi
}

# === ウィンドウ操作 ===

# 現在のウィンドウID取得
get_current_window() {
    if in_tmux; then
        tmux display-message -p '#{window_id}' 2>/dev/null
    else
        echo ""
    fi
}

# 現在のウィンドウインデックス取得
get_current_window_index() {
    if in_tmux; then
        tmux display-message -p '#{window_index}' 2>/dev/null
    else
        echo ""
    fi
}

# ウィンドウ一覧取得
list_windows() {
    local session="${1:-}"
    
    if [[ -n "$session" ]]; then
        tmux list-windows -t "$session" -F '#{window_id}:#{window_index}:#{window_name}' 2>/dev/null
    else
        tmux list-windows -F '#{window_id}:#{window_index}:#{window_name}' 2>/dev/null
    fi
}

# 新規ウィンドウ作成
create_window() {
    local name="${1:-}"
    local command="${2:-}"
    local session="${3:-}"
    
    local cmd="tmux new-window"
    [[ -n "$session" ]] && cmd="$cmd -t '$session'"
    [[ -n "$name" ]] && cmd="$cmd -n '$name'"
    [[ -n "$command" ]] && cmd="$cmd '$command'"
    
    eval "$cmd"
}

# === ペイン操作 ===

# 現在のペインID取得
get_current_pane() {
    if in_tmux; then
        tmux display-message -p '#{pane_id}' 2>/dev/null
    else
        echo ""
    fi
}

# ペイン一覧取得
list_panes() {
    local window="${1:-}"
    
    if [[ -n "$window" ]]; then
        tmux list-panes -t "$window" -F '#{pane_id}:#{pane_index}' 2>/dev/null
    else
        tmux list-panes -F '#{pane_id}:#{pane_index}' 2>/dev/null
    fi
}

# ペインの内容をキャプチャ
capture_pane() {
    local pane="${1:-}"
    local lines="${2:-30}"  # デフォルト30行
    local start="${3:--$lines}"  # デフォルトは最後のN行
    
    local cmd="tmux capture-pane -p"
    [[ -n "$pane" ]] && cmd="$cmd -t '$pane'"
    cmd="$cmd -S '$start'"
    
    eval "$cmd" 2>/dev/null
}

# ペインにテキスト送信
send_to_pane() {
    local text="$1"
    local pane="${2:-}"
    
    local cmd="tmux send-keys"
    [[ -n "$pane" ]] && cmd="$cmd -t '$pane'"
    cmd="$cmd '$text'"
    
    eval "$cmd"
}

# === ステータス管理 ===

# ステータスディレクトリ初期化
init_status_dir() {
    if [[ ! -d "$TMUX_STATUS_DIR" ]]; then
        mkdir -p "$TMUX_STATUS_DIR"
        log_debug "ステータスディレクトリ作成: $TMUX_STATUS_DIR"
    fi
}

# ウィンドウステータス取得
get_window_status() {
    local window_id="${1:-$(get_current_window_index)}"
    local status_file="$TMUX_STATUS_DIR/window-${window_id}.status"
    
    if [[ -f "$status_file" ]]; then
        cat "$status_file"
    else
        echo ""
    fi
}

# ウィンドウステータス設定
set_window_status() {
    local status="$1"
    local window_id="${2:-$(get_current_window_index)}"
    
    init_status_dir
    
    local status_file="$TMUX_STATUS_DIR/window-${window_id}.status"
    echo "$status" > "$status_file"
    
    log_debug "ステータス設定: window=$window_id, status=$status"
}

# ステータスファイルクリーンアップ
cleanup_status_files() {
    local max_age="${1:-86400}"  # デフォルト24時間
    
    if [[ -d "$TMUX_STATUS_DIR" ]]; then
        find "$TMUX_STATUS_DIR" -type f -name "*.status" -mtime +$((max_age / 86400)) -delete
        log_debug "古いステータスファイルをクリーンアップしました"
    fi
}

# === 環境変数操作 ===

# tmux環境変数取得
get_tmux_env() {
    local var="$1"
    
    if tmux_available; then
        tmux show-environment "$var" 2>/dev/null | cut -d= -f2
    else
        echo ""
    fi
}

# tmux環境変数設定
set_tmux_env() {
    local var="$1"
    local value="$2"
    local global="${3:-false}"
    
    if tmux_available; then
        if [[ "$global" == "true" ]]; then
            tmux set-environment -g "$var" "$value"
        else
            tmux set-environment "$var" "$value"
        fi
        log_debug "tmux環境変数設定: $var=$value (global=$global)"
    fi
}

# tmux環境変数削除
unset_tmux_env() {
    local var="$1"
    local global="${2:-false}"
    
    if tmux_available; then
        if [[ "$global" == "true" ]]; then
            tmux set-environment -gu "$var"
        else
            tmux set-environment -u "$var"
        fi
        log_debug "tmux環境変数削除: $var (global=$global)"
    fi
}

# === ステータスバー操作 ===

# ステータスバー更新
refresh_status_bar() {
    if tmux_available; then
        tmux refresh-client -S
        log_debug "ステータスバーを更新しました"
    fi
}

# ステータスバーメッセージ表示
show_message() {
    local message="$1"
    local duration="${2:-3000}"  # デフォルト3秒
    
    if tmux_available; then
        tmux display-message -d "$duration" "$message"
    else
        echo "$message"
    fi
}

# === フック管理 ===

# フック登録
register_hook() {
    local hook="$1"
    local command="$2"
    local global="${3:-true}"
    
    if tmux_available; then
        if [[ "$global" == "true" ]]; then
            tmux set-hook -g "$hook" "$command"
        else
            tmux set-hook "$hook" "$command"
        fi
        log_debug "フック登録: $hook -> $command"
    fi
}

# フック削除
unregister_hook() {
    local hook="$1"
    local global="${2:-true}"
    
    if tmux_available; then
        if [[ "$global" == "true" ]]; then
            tmux set-hook -gu "$hook"
        else
            tmux set-hook -u "$hook"
        fi
        log_debug "フック削除: $hook"
    fi
}

# === オプション操作 ===

# tmuxオプション取得
get_tmux_option() {
    local option="$1"
    local window="${2:-}"
    local global="${3:-false}"
    
    local cmd="tmux show-options"
    [[ "$global" == "true" ]] && cmd="$cmd -g"
    [[ -n "$window" ]] && cmd="$cmd -w -t '$window'"
    cmd="$cmd '$option'"
    
    eval "$cmd" 2>/dev/null | cut -d' ' -f2
}

# tmuxオプション設定
set_tmux_option() {
    local option="$1"
    local value="$2"
    local window="${3:-}"
    local global="${4:-false}"
    
    local cmd="tmux set-option"
    [[ "$global" == "true" ]] && cmd="$cmd -g"
    [[ -n "$window" ]] && cmd="$cmd -w -t '$window'"
    cmd="$cmd '$option' '$value'"
    
    eval "$cmd"
    log_debug "オプション設定: $option=$value"
}

# === レイアウト操作 ===

# 現在のレイアウト取得
get_layout() {
    local window="${1:-}"
    
    local cmd="tmux list-windows"
    [[ -n "$window" ]] && cmd="$cmd -t '$window'"
    cmd="$cmd -F '#{window_layout}'"
    
    eval "$cmd" 2>/dev/null | head -1
}

# レイアウト設定
set_layout() {
    local layout="$1"
    local window="${2:-}"
    
    local cmd="tmux select-layout"
    [[ -n "$window" ]] && cmd="$cmd -t '$window'"
    cmd="$cmd '$layout'"
    
    eval "$cmd"
    log_debug "レイアウト設定: $layout"
}

# === ユーティリティ ===

# tmuxコマンド実行（エラーハンドリング付き）
run_tmux_command() {
    local command="$@"
    
    if ! tmux_available; then
        log_error "tmuxが利用できません"
        return 1
    fi
    
    if ! eval "tmux $command" 2>/dev/null; then
        log_error "tmuxコマンド実行失敗: tmux $command"
        return 1
    fi
}

# tmux設定再読み込み
reload_tmux_config() {
    local config="${1:-$HOME/.tmux.conf}"
    
    if [[ -f "$config" ]]; then
        run_tmux_command source-file "'$config'"
        log_info "tmux設定を再読み込みしました: $config"
    else
        log_error "設定ファイルが見つかりません: $config"
        return 1
    fi
}

# === 初期化 ===

# ライブラリ初期化
_init_tmux_ops() {
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        log_debug "TMux Scripts TMux Operations Library loaded"
        
        if tmux_available; then
            log_debug "tmux version: $(get_tmux_version)"
            
            if in_tmux; then
                log_debug "Running inside tmux"
                log_debug "Session: $(get_current_session)"
                log_debug "Window: $(get_current_window)"
                log_debug "Pane: $(get_current_pane)"
            else
                log_debug "Running outside tmux"
            fi
        else
            log_debug "tmux is not available"
        fi
    fi
    
    # ステータスディレクトリ初期化
    init_status_dir
}

# 自動初期化
_init_tmux_ops