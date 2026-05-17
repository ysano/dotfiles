#!/bin/bash
# ファイル名: dialog_detector.sh
# 説明: Claude Code の AskUserQuestion ダイアログをペイン内容スキャンで検出し
#       サウンドを発火する軽量モジュール。
# 用途: polling_monitor.sh から status-right (5s 間隔) ごとに呼ばれる。
#
# 経緯: Claude Code 1.x の AskUserQuestion は Notification hook を emit しない
#       (検証で hook log が 0 行と確認済み) ため、refactor で撤去された
#       capture-pane 経路を「ダイアログ検出限定」で限定復活させた経路。
#       ADR 0004 の AI-DLC ライト運用方針の範囲内で、機能追加は spec 化せず
#       直接実装している。
#
# 検出パターン:
#   AskUserQuestion ダイアログのフッター固定文字列
#   "Enter to select · ↑/↓ to navigate · Esc to cancel"
#   これは Claude Code 1.x の選択型ダイアログ専用 signature

set -u

# このスクリプト単体実行用ガード (通常は polling_monitor.sh から source される)
[[ -z "${_DIALOG_DETECTOR_LOADED:-}" ]] || return 0
_DIALOG_DETECTOR_LOADED=1

DIALOG_DETECTOR_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# AskUserQuestion ダイアログの固定 footer (これが見えていればダイアログ表示中)
readonly DIALOG_FOOTER_PATTERN="Enter to select"

# 検出無効化フラグ
# TMUX_CLAUDE_DIALOG_DETECT_DISABLED=true で全停止
detect_dialogs() {
    [[ "${TMUX_CLAUDE_DIALOG_DETECT_DISABLED:-false}" == "true" ]] && return 0

    # サウンドが有効でなければスキップ (CPU 節約)
    local sound_enabled
    sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    [[ "$sound_enabled" != "true" ]] && return 0

    # 全 Claude Code ペインを列挙 (current_command で検出。title は会話トピックで
    # 上書きされるため信頼できない)
    local all_panes
    all_panes=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}	#{pane_current_command}" 2>/dev/null)
    [[ -z "$all_panes" ]] && return 0

    while IFS=$'\t' read -r pane_target cmd; do
        [[ -z "$pane_target" ]] && continue
        # claude / claude.exe (macOS Ghostty 上は claude.exe で起動される)
        [[ "$cmd" != claude* ]] && continue

        # 現在 visible なペイン内容のみ確認 (-S なしで visible buffer)
        local content
        content=$(tmux capture-pane -t "$pane_target" -p 2>/dev/null)

        local pane_key
        pane_key=$(encode_pane_key "$pane_target")
        local state_key="@claude_voice_dialog_active_${pane_key}"
        local prev_state
        prev_state=$(tmux show-option -gqv "$state_key" 2>/dev/null)

        if echo "$content" | grep -qF "$DIALOG_FOOTER_PATTERN"; then
            # ダイアログ表示中
            if [[ "$prev_state" != "true" ]]; then
                # 新規出現 → サウンド発火 (Funk = waiting)
                if [[ -x "$DIALOG_DETECTOR_DIR/sound_utils.sh" ]]; then
                    "$DIALOG_DETECTOR_DIR/sound_utils.sh" play waiting "$pane_target" >/dev/null 2>&1 &
                fi
                tmux set-option -g "$state_key" "true" 2>/dev/null
                log_debug "Dialog detected: $pane_target"
            fi
        else
            # ダイアログ無し
            if [[ "$prev_state" == "true" ]]; then
                # 消失 → 状態リセット
                tmux set-option -gu "$state_key" 2>/dev/null
                log_debug "Dialog dismissed: $pane_target"
            fi
        fi
    done <<< "$all_panes"
}

# 停止した CC ペインの dialog state を掃除 (polling_monitor の liveness check と連動)
cleanup_dialog_state_for_pane() {
    local pane_key="$1"
    [[ -z "$pane_key" ]] && return 0
    tmux set-option -gu "@claude_voice_dialog_active_${pane_key}" 2>/dev/null
}
