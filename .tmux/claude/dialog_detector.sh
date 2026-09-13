#!/bin/bash
# ファイル名: dialog_detector.sh
# 説明: Claude Code の AskUserQuestion ダイアログをペイン内容スキャンで検出し
#       サウンド発火 + 質問内容の読み上げを行う軽量モジュール。
# 用途: polling_monitor.sh から status-right (5s 間隔) ごとに呼ばれる。
#
# 経緯: Claude Code 1.x の AskUserQuestion は Notification hook を emit しない
#       (検証で hook log が 0 行と確認済み) ため、refactor で撤去された
#       capture-pane 経路を「ダイアログ検出限定」で限定復活させた経路。
#       ADR 0004 の AI-DLC ライト運用方針の範囲内で、機能追加は spec 化せず
#       直接実装している。詳細は ADR 0005 参照。
#
# 検出パターン:
#   AskUserQuestion ダイアログのフッター固定文字列
#   "Enter to select · ↑/↓ to navigate · Esc to cancel"
#   これは Claude Code 1.x の選択型ダイアログ専用 signature
#
# フィードバック:
#   - @claude_voice_sound_enabled=true   → Funk (waiting 音) を再生
#   - @claude_voice_summary_enabled=true → 質問テキストを抽出して読み上げ

set -u

# このスクリプト単体実行用ガード (通常は polling_monitor.sh から source される)
[[ -z "${_DIALOG_DETECTOR_LOADED:-}" ]] || return 0
_DIALOG_DETECTOR_LOADED=1

DIALOG_DETECTOR_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# AskUserQuestion ダイアログの固定 footer (これが見えていればダイアログ表示中)
readonly DIALOG_FOOTER_PATTERN="Enter to select"

# ダイアログ本文から質問テキストを抽出する。
# ダイアログ構造:
#   ☐ <header>          ← チェックボックス + 短いヘッダ
#   <question text>      ← 読み上げたい本文 (1〜2 行)
#   ❯ 1. <option>        ← 選択肢 (❯ は現在ハイライト)
#   ...
# ☐ 行の次から、最初の選択肢行 (❯ または "N." 始まり) の手前までを質問とみなす。
extract_dialog_question() {
    local content="$1"
    local q
    q=$(printf '%s\n' "$content" | awk '
        /☐/ { cap=1; next }
        cap && /^[[:space:]]*❯/        { exit }
        cap && /^[[:space:]]*[0-9]+\./ { exit }
        cap && /[^[:space:]]/          { print; n++ }
        n >= 2 { exit }
    ')
    # 改行を空白化 → 連続空白圧縮 → 前後 trim → 150 文字で切り詰め
    echo "$q" | tr '\n' ' ' | sed 's/  */ /g; s/^ //; s/ $//' | cut -c1-150
}

# 検出無効化フラグ
# TMUX_CLAUDE_DIALOG_DETECT_DISABLED=true で全停止
detect_dialogs() {
    [[ "${TMUX_CLAUDE_DIALOG_DETECT_DISABLED:-false}" == "true" ]] && return 0

    # 表示の検出は通知設定とは独立して行う。
    local sound_enabled summary_enabled
    sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    summary_enabled=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)

    # プロセス親子関係から node wrapper / claude.exe を含めて列挙。
    local all_panes
    all_panes=$(detect_claude_panes)
    [[ -z "$all_panes" ]] && return 0

    while IFS= read -r pane_target; do
        [[ -z "$pane_target" ]] && continue

        # 現在 visible なペイン内容のみ確認 (-S なしで visible buffer)
        local content
        content=$(tmux capture-pane -t "$pane_target" -p 2>/dev/null)

        local state_key="@claude_dialog_active"
        local prev_state
        prev_state=$(tmux show-option -pqv -t "$pane_target" "$state_key" 2>/dev/null)

        if echo "$content" | grep -qF "$DIALOG_FOOTER_PATTERN"; then
            # ダイアログ表示中
            local changed
            changed=$(update_claude_evidence "$pane_target" dialog "Enter to select")
            tmux set-option -p -t "$pane_target" "$state_key" "true" 2>/dev/null
            if [[ "$prev_state" != "true" && "$changed" == "changed" ]] && claude_notifications_enabled; then
                # 新規出現 → フィードバック発火
                # (1) Funk 音 (waiting)
                if [[ "$sound_enabled" == "true" && -x "$DIALOG_DETECTOR_DIR/sound_utils.sh" ]]; then
                    "$DIALOG_DETECTOR_DIR/sound_utils.sh" play waiting "$pane_target" >/dev/null 2>&1 &
                fi
                # (2) 質問テキストの読み上げ
                if [[ "$summary_enabled" == "true" ]]; then
                    local question
                    question=$(extract_dialog_question "$content")
                    (
                        source "$DIALOG_DETECTOR_DIR/sound_utils.sh" 2>/dev/null
                        speak_text "質問です。${question:-クロードが選択を求めています}" "$pane_target"
                    ) >/dev/null 2>&1 &
                fi
                # ペイン状態を Question にしてウィンドウアイコンを ❓ に
                # Native status and legacy icon were updated atomically above.
                log_debug "Dialog detected: $pane_target"
            fi
        else
            # ダイアログ無し
            if [[ "$prev_state" == "true" ]]; then
                # 消失 → authoritative hook の状態を復元。
                tmux set-option -pu -t "$pane_target" "$state_key" 2>/dev/null
                update_claude_evidence "$pane_target" dialog "" >/dev/null
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
