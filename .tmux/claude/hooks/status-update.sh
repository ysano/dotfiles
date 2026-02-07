#!/bin/bash
# ファイル名: hooks/status-update.sh
# 説明: Claude Code hooks イベントの共通エントリポイント
# 用途: Claude Code の hooks システムから呼び出され、ペインステータスをリアルタイム更新する
#
# 入力: stdin に JSON（hook_event_name, notification_type 等）
# ペイン特定: $TMUX_PANE 環境変数 → tmux list-panes で逆引き
# 出力: tmux option にステータスを保存、アイコン集約、音声フィードバック

set -euo pipefail

# スクリプトのディレクトリを取得（hooks/ の親 = claude/）
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# ログ機能（functions.sh の読み込みが失敗してもログ出力できるように最低限のフォールバック）
CLAUDE_VOICE_LOG_FILE="${TMPDIR:-/tmp}/tmux-claude-voice.log"

_log() {
    local level="$1" msg="$2"
    echo "[$level] $(date '+%Y-%m-%d %H:%M:%S') [hooks] $msg" >> "$CLAUDE_VOICE_LOG_FILE"
}

# --- 1. stdin から JSON を読み取り ---
INPUT=$(cat)
if [[ -z "$INPUT" ]]; then
    _log "ERROR" "stdin が空です"
    exit 0
fi

# --- 2. jq でイベント種別を取得 ---
if ! command -v jq >/dev/null 2>&1; then
    _log "ERROR" "jq が見つかりません"
    exit 0
fi

HOOK_EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // empty' 2>/dev/null)
if [[ -z "$HOOK_EVENT" ]]; then
    _log "ERROR" "hook_event_name を取得できません: $INPUT"
    exit 0
fi

NOTIFICATION_TYPE=$(echo "$INPUT" | jq -r '.notification_type // empty' 2>/dev/null)
NOTIFICATION_MSG=$(echo "$INPUT" | jq -r '.message // empty' 2>/dev/null)

_log "DEBUG" "イベント受信: event=$HOOK_EVENT notification_type=$NOTIFICATION_TYPE"

# --- 3. $TMUX_PANE からペインターゲットを解決 ---
resolve_pane_target() {
    # $TMUX_PANE が設定されている場合（例: %1）
    if [[ -n "${TMUX_PANE:-}" ]]; then
        local pane_info
        pane_info=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index} #{pane_id}" 2>/dev/null \
            | grep " ${TMUX_PANE}$" \
            | head -1 \
            | awk '{print $1}')

        if [[ -n "$pane_info" ]]; then
            echo "$pane_info"
            return 0
        fi
    fi

    # フォールバック: /proc/$PPID/stat からプロセスツリーを逆引き
    if [[ -f "/proc/$PPID/stat" ]]; then
        local parent_pid=$PPID
        # tmux ペインの PID を探してマッチさせる
        local panes_list
        panes_list=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index} #{pane_pid}" 2>/dev/null)
        while [[ $parent_pid -gt 1 ]]; do
            local match
            match=$(echo "$panes_list" | grep " ${parent_pid}$" | head -1 | awk '{print $1}')
            if [[ -n "$match" ]]; then
                echo "$match"
                return 0
            fi
            # 親プロセスを辿る
            parent_pid=$(awk '{print $4}' "/proc/$parent_pid/stat" 2>/dev/null) || break
        done
    fi

    _log "ERROR" "ペインターゲットを解決できません (TMUX_PANE=${TMUX_PANE:-unset})"
    return 1
}

PANE_TARGET=$(resolve_pane_target) || exit 0

_log "DEBUG" "ペイン解決: $PANE_TARGET (TMUX_PANE=${TMUX_PANE:-unset})"

# --- 4. イベント種別 → ステータス変換 ---
determine_status() {
    local event="$1"
    local notif_type="$2"

    case "$event" in
        UserPromptSubmit)
            echo "Busy"
            ;;
        PreToolUse)
            echo "Busy"
            ;;
        Notification)
            # idle_prompt または permission_prompt の場合のみ Waiting
            if [[ "$notif_type" =~ ^(idle_prompt|permission_prompt)$ ]]; then
                echo "Waiting"
            else
                # その他の通知はステータス変更しない（タイムスタンプのみ更新）
                echo ""
            fi
            ;;
        Stop)
            echo "Idle"
            ;;
        SessionStart)
            echo "Idle"
            ;;
        SessionEnd)
            echo "CLEAR"
            ;;
        *)
            _log "DEBUG" "未知のイベント: $event"
            echo ""
            ;;
    esac
}

NEW_STATUS=$(determine_status "$HOOK_EVENT" "$NOTIFICATION_TYPE")

# ステータスが空の場合はタイムスタンプだけ更新して終了
PANE_KEY="${PANE_TARGET//[:\.]/_}"
NOW=$(date +%s)

# --- 5. ウィンドウレベルのアイコン集約（他ステップから呼び出されるため先に定義） ---
aggregate_window_icon() {
    local pane_target="$1"
    local session_window="${pane_target%.*}"
    local window_index="${session_window#*:}"

    # ウィンドウ内の全ペイン状態を集約して最優先アイコンを決定
    local all_statuses
    all_statuses=$(tmux show-options -g 2>/dev/null \
        | grep "^@claude_voice_pane_status_" \
        | grep "_${window_index}_" \
        | awk '{print $2}' \
        | tr -d '"')

    local icon=""
    if [[ -n "$all_statuses" ]]; then
        icon="✅"
        if echo "$all_statuses" | grep -q "Busy"; then
            icon="⚡"
        fi
        if echo "$all_statuses" | grep -q "Waiting"; then
            icon="⌛"
        fi
    fi

    tmux set-option -g "@claude_voice_icon_$window_index" "$icon" 2>/dev/null
    _log "DEBUG" "アイコン集約: window=$window_index icon=$icon"
}

if [[ -z "$NEW_STATUS" ]]; then
    tmux set-option -g "@claude_voice_hooks_ts_${PANE_KEY}" "$NOW" 2>/dev/null
    _log "DEBUG" "タイムスタンプのみ更新: $PANE_TARGET"
    exit 0
fi

# --- 6. SessionEnd: ペイン状態をクリア ---
if [[ "$NEW_STATUS" == "CLEAR" ]]; then
    tmux set-option -g -u "@claude_voice_status_${PANE_KEY}" 2>/dev/null
    tmux set-option -g -u "@claude_voice_pane_status_${PANE_KEY}" 2>/dev/null
    tmux set-option -g -u "@claude_voice_hooks_ts_${PANE_KEY}" 2>/dev/null
    _log "INFO" "セッション終了: ペイン状態をクリア $PANE_TARGET"

    # アイコン集約を再実行
    aggregate_window_icon "$PANE_TARGET"
    exit 0
fi

# --- 7. 前回ステータスと比較 ---
PREV_STATUS=$(tmux show-option -gqv "@claude_voice_pane_status_${PANE_KEY}" 2>/dev/null)

# タイムスタンプは常に更新
tmux set-option -g "@claude_voice_hooks_ts_${PANE_KEY}" "$NOW" 2>/dev/null

if [[ "$NEW_STATUS" == "$PREV_STATUS" ]]; then
    _log "DEBUG" "ステータス変化なし: $PANE_TARGET ($NEW_STATUS)"
    exit 0
fi

# --- 8. ステータス更新 ---
tmux set-option -g "@claude_voice_status_${PANE_KEY}" "$NEW_STATUS" 2>/dev/null
tmux set-option -g "@claude_voice_pane_status_${PANE_KEY}" "$NEW_STATUS" 2>/dev/null

_log "INFO" "ステータス変化: $PANE_TARGET ($PREV_STATUS -> $NEW_STATUS)"

aggregate_window_icon "$PANE_TARGET"

# --- 9. 音声フィードバック（バックグラウンド実行） ---
if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
    SOUND_ENABLED=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    if [[ "$SOUND_ENABLED" == "true" ]]; then
        case "$NEW_STATUS" in
            "Busy")    "$SCRIPT_DIR/sound_utils.sh" play start   >/dev/null 2>&1 & ;;
            "Waiting") "$SCRIPT_DIR/sound_utils.sh" play waiting >/dev/null 2>&1 & ;;
            "Idle")    "$SCRIPT_DIR/sound_utils.sh" play complete >/dev/null 2>&1 & ;;
        esac
    fi
fi

# --- 10. TTS 読み上げフィードバック（バックグラウンド実行） ---
if [[ "$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)" == "true" ]]; then
    case "$NEW_STATUS" in
        "Waiting")
            # permission_prompt のみ読み上げ（idle_prompt は Stop の完了要約でカバー）
            if [[ "$NOTIFICATION_TYPE" == "permission_prompt" ]]; then
                (
                    source "$SCRIPT_DIR/sound_utils.sh" 2>/dev/null
                    speak_text "${NOTIFICATION_MSG:-ツール実行の許可を求めています}"
                ) >/dev/null 2>&1 &
                _log "DEBUG" "TTS: permission_prompt 読み上げ開始"
            fi
            ;;
        "Idle")
            # タスク完了: ペイン内容を要約して読み上げ
            (
                source "$SCRIPT_DIR/sound_utils.sh" 2>/dev/null
                pane_content=$(tmux capture-pane -t "$PANE_TARGET" -p -S -30 2>/dev/null)
                if [[ -n "$pane_content" ]]; then
                    # Ollama で要約を試行
                    summary=""
                    if [[ -f "$SCRIPT_DIR/ollama_utils.sh" ]]; then
                        source "$SCRIPT_DIR/ollama_utils.sh" 2>/dev/null
                        summary=$(summarize_with_ollama "$pane_content" 2>/dev/null) || summary=""
                    fi
                    speak_text "${summary:-タスクが完了しました}"
                else
                    speak_text "タスクが完了しました"
                fi
            ) >/dev/null 2>&1 &
            _log "DEBUG" "TTS: 完了要約読み上げ開始"
            ;;
    esac
fi

exit 0
