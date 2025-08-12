#!/bin/bash
# ファイル名: polling_monitor.sh  
# 説明: Polling方式による軽量Claude Voice監視スクリプト
# 用途: tmux status-right から5秒間隔で呼び出される1回実行型スクリプト

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 設定とヘルパー関数を読み込み
source "$SCRIPT_DIR/functions.sh"

# 設定読み込み（silent mode）
load_configuration() {
    # システム有効/無効チェック
    CLAUDE_VOICE_ENABLED=$(tmux show-option -gqv @claude_voice_enabled 2>/dev/null)
    if [[ "$CLAUDE_VOICE_ENABLED" != "true" ]]; then
        return 1 # システム無効時は何もしない
    fi
    
    # パターン設定
    CLAUDE_VOICE_WINDOW_PATTERN=$(tmux show-option -gqv @claude_voice_window_pattern 2>/dev/null)
    CLAUDE_VOICE_WINDOW_PATTERN="${CLAUDE_VOICE_WINDOW_PATTERN:-Claude|claude|CLAUDE}"
    
    return 0
}

# メイン処理：ポーリング監視（1回実行）
polling_monitor_main() {
    # 設定読み込み
    if ! load_configuration; then
        return 0 # システム無効時は静かに終了
    fi
    
    # tmuxセッションが存在するかチェック
    if ! tmux list-sessions &>/dev/null; then
        return 0 # tmuxセッションがない場合は終了
    fi
    
    # Claude Codeプロセスを検出（Window名パターンは使用しない）
    local claude_windows
    claude_windows=$(detect_claude_windows)
    
    if [[ -z "$claude_windows" ]]; then
        # Claude Codeが検出されない場合、すべてのウィンドウのアイコンをクリア
        clear_all_claude_icons
        return 0
    fi
    
    # 各Claudeウィンドウを処理
    while IFS= read -r session_window; do
        if [[ -n "$session_window" ]]; then
            process_claude_window_polling "$session_window"
        fi
    done <<< "$claude_windows"
}

# ウィンドウごとのポーリング処理
process_claude_window_polling() {
    local session_window="$1"
    
    # 現在のステータスを分析
    local current_status
    current_status=$(analyze_pane_content "$session_window")
    
    if [[ -z "$current_status" ]]; then
        return 1
    fi
    
    # 前回の状態を取得
    local previous_status
    previous_status=$(get_previous_status "$session_window")
    
    # 状態変化検出
    if [[ "$current_status" != "$previous_status" ]]; then
        # 状態変化を記録
        log_info "ステータス変化を検出: $session_window ($previous_status -> $current_status)"
        
        # 状態を保存
        save_current_status "$session_window" "$current_status"
        
        # Claude Codeステータスアイコンを更新
        update_claude_status_icon "$session_window" "$current_status"
        
        # 音声フィードバック（非同期実行）
        trigger_audio_feedback "$session_window" "$current_status" &
    else
        # 状態変化なし：アイコン更新のみ（念のため）
        update_claude_status_icon "$session_window" "$current_status"
    fi
}

# Claude Codeステータスアイコン更新（tmux変数方式）
update_claude_status_icon() {
    local session_window="$1"
    local status="$2"
    
    local icon=""
    case "$status" in
        "Busy")   icon="⚡" ;;
        "Waiting") icon="⌛" ;;
        *)        icon="✅" ;;  # その他はすべてIdle判定
    esac
    
    # ウィンドウインデックスを取得
    local window_index
    window_index=$(echo "$session_window" | cut -d':' -f2)
    
    # tmux変数にアイコンを保存
    tmux set-option -g "@claude_voice_icon_$window_index" "$icon" 2>/dev/null
}

# すべてのウィンドウのClaudeアイコンをクリア
clear_all_claude_icons() {
    # すべてのウィンドウのアイコン変数をクリア
    local windows
    windows=$(tmux list-windows -F "#{window_index}" 2>/dev/null)
    
    while IFS= read -r window_index; do
        if [[ -n "$window_index" ]]; then
            tmux set-option -g "@claude_voice_icon_$window_index" "" 2>/dev/null
        fi
    done <<< "$windows"
    
    log_debug "すべてのClaudeアイコンをクリアしました"
}

# 音声フィードバック（バックグラウンド実行）
trigger_audio_feedback() {
    local session_window="$1"
    local status="$2"
    
    # 音声機能有効チェック
    local sound_enabled
    sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    
    if [[ "$sound_enabled" == "true" ]]; then
        # 音声ファイルが存在すればバックグラウンドで実行
        if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
            case "$status" in
                "Busy")   "$SCRIPT_DIR/sound_utils.sh" play start >/dev/null 2>&1 & ;;
                "Waiting") "$SCRIPT_DIR/sound_utils.sh" play waiting >/dev/null 2>&1 & ;;
                "Idle")   "$SCRIPT_DIR/sound_utils.sh" play complete >/dev/null 2>&1 & ;;
            esac
        fi
    fi
}

# メイン実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 一回だけ実行してサイレント終了
    polling_monitor_main 2>/dev/null
fi