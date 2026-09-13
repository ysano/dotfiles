#!/bin/bash
# Only called for a state transition by claude_status.py. Existing voice policy
# (sound source, TTS, summary, focus filtering and panning) stays in sound_utils.
set -u
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$SCRIPT_DIR/functions.sh"
claude_notifications_enabled || exit 0
PANE_TARGET="$1"
NEW_STATUS="$2"
SOUND="$3"
NOTIFICATION_MSG="${4:-}"
_log() { log_debug "$2"; }
if [[ "$(tmux show-option -gqv @claude_voice_sound_enabled)" == "true" ]]; then
    "$SCRIPT_DIR/sound_utils.sh" play "$SOUND" "$PANE_TARGET" >/dev/null 2>&1 &
fi
# --- 10. TTS 読み上げフィードバック（バックグラウンド実行） ---
SUMMARY_ENABLED=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
if [[ "$SUMMARY_ENABLED" == "true" ]]; then
    case "$NEW_STATUS" in
        "Permission")
            # ツール承認待ちの通知メッセージを読み上げ
            (
                source "$SCRIPT_DIR/sound_utils.sh" 2>/dev/null
                speak_text "${NOTIFICATION_MSG:-ツール実行の許可を求めています}"
            ) >/dev/null 2>&1 &
            _log "DEBUG" "TTS: permission 読み上げ開始"
            ;;
        "Idle")
            # タスク完了: ペイン内容を要約して読み上げ
            (
                source "$SCRIPT_DIR/sound_utils.sh" 2>/dev/null
                # 読み上げ冒頭にどの window/pane か分かるラベルを付ける（複数pane時のみpane番号）
                label=$(pane_speech_label "$PANE_TARGET")
                pane_content=$(tmux capture-pane -t "$PANE_TARGET" -p -S -30 2>/dev/null)
                if [[ -n "$pane_content" ]]; then
                    # Ollama で要約を試行
                    summary=""
                    if [[ -f "$SCRIPT_DIR/ollama_utils.sh" ]]; then
                        source "$SCRIPT_DIR/ollama_utils.sh" 2>/dev/null
                        summary=$(summarize_with_ollama "$pane_content" 2>/dev/null) || summary=""
                    fi
                    speak_text "${label:+${label}、}${summary:-タスクが完了しました}"
                else
                    speak_text "${label:+${label}、}タスクが完了しました"
                fi
            ) >/dev/null 2>&1 &
            _log "DEBUG" "TTS: 完了要約読み上げ開始"
            ;;
    esac
else
    # ADR 0008: TTS が動かない原因をサイレント失敗にしないためにログを残す。
    # Permission / Idle のみ記録 (Busy/Question 等は元々 TTS 対象外)。
    case "$NEW_STATUS" in
        "Permission"|"Idle")
            _log "INFO" "TTS スキップ: summary_enabled=${SUMMARY_ENABLED:-unset} (status=$NEW_STATUS, pane=$PANE_TARGET)"
            ;;
    esac
fi

exit 0
