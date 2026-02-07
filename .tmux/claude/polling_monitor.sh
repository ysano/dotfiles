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

    # Claude Codeペインを検出（ペインレベル）
    local claude_panes
    claude_panes=$(detect_claude_panes)

    if [[ -z "$claude_panes" ]]; then
        # Claude Codeが検出されない場合、すべてのウィンドウのアイコンをクリア
        clear_all_claude_icons
        return 0
    fi

    # 各Claudeペインを処理
    while IFS= read -r pane_target; do
        if [[ -n "$pane_target" ]]; then
            process_claude_pane_polling "$pane_target"
        fi
    done <<< "$claude_panes"
}

# ペインごとのポーリング処理
process_claude_pane_polling() {
    local pane_target="$1"

    # 現在のステータスを分析
    local current_status
    current_status=$(analyze_pane_content "$pane_target")

    if [[ -z "$current_status" ]]; then
        return 1
    fi

    # 前回の状態を取得
    local previous_status
    previous_status=$(get_previous_status "$pane_target")

    # 状態変化検出
    if [[ "$current_status" != "$previous_status" ]]; then
        # 状態変化を記録
        log_info "ステータス変化を検出: $pane_target ($previous_status -> $current_status)"

        # 状態を保存
        save_current_status "$pane_target" "$current_status"

        # Claude Codeステータスアイコンを更新（ペインレベル → ウィンドウレベルに集約）
        update_claude_status_icon "$pane_target" "$current_status"

        # 音声フィードバック（非同期実行）
        trigger_audio_feedback "$pane_target" "$current_status" &
    else
        # 状態変化なし：アイコン更新のみ（念のため）
        update_claude_status_icon "$pane_target" "$current_status"
    fi
}

# Claude Codeステータスアイコン更新（ペインレベル状態をウィンドウレベルに集約）
# 優先度: Waiting(⌛) > Busy(⚡) > Idle(✅)
update_claude_status_icon() {
    local pane_target="$1"  # session:window.pane
    local status="$2"
    local session_window="${pane_target%.*}"
    local window_index="${session_window#*:}"

    # ペインごとの状態をtmux変数に保存
    local pane_key="@claude_voice_pane_status_${pane_target//[:\.]/_}"
    tmux set-option -g "$pane_key" "$status" 2>/dev/null

    # ウィンドウ内の全ペイン状態を集約して最優先アイコンを決定
    local all_statuses
    all_statuses=$(tmux show-options -g 2>/dev/null | grep "^@claude_voice_pane_status_" | grep "_${window_index}_" | awk '{print $2}' | tr -d '"')

    local icon="✅"
    if echo "$all_statuses" | grep -q "Busy"; then
        icon="⚡"
    fi
    if echo "$all_statuses" | grep -q "Waiting"; then
        icon="⌛"
    fi

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
    local pane_target="$1"
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

# 全Claude Codeペインのステータスを表示
show_pane_status() {
    local claude_panes
    claude_panes=$(detect_claude_panes)

    if [[ -z "$claude_panes" ]]; then
        echo "Claude Codeペインが検出されませんでした"
        return 0
    fi

    echo "=== Claude Code ペインステータス ==="
    while IFS= read -r pane_target; do
        if [[ -n "$pane_target" ]]; then
            local status
            status=$(analyze_pane_content "$pane_target" 2>/dev/null || echo "Unknown")
            local icon="?"
            case "$status" in
                "Busy")    icon="⚡" ;;
                "Waiting") icon="⌛" ;;
                "Idle")    icon="✅" ;;
            esac
            echo "  $icon $pane_target: $status"
        fi
    done <<< "$claude_panes"
}

# メイン実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-run}" in
        "status")
            show_pane_status
            ;;
        *)
            # 一回だけ実行してサイレント終了
            polling_monitor_main 2>/dev/null
            ;;
    esac
fi