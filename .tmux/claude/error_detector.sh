#!/bin/bash
# ファイル名: error_detector.sh
# 説明: Claude Code の継続不能エラー (API障害 / Usage超過 / Usage Policy違反) を
#       ペイン内容スキャンで検出し、ステータスを Error(⚠) にするモジュール。
# 用途: polling_monitor.sh から status-right (5s 間隔) ごとに呼ばれる。
#
# 経緯: 5 状態モデル (ADR 0007) の ⚠ Error は「時間経過によるハング」ではなく
#       「Claude Code が返す継続不能エラー」を表す。エラー文言は実画面から採取:
#         - API Error: 500 {"type":"api_error",...}        (API障害)
#         - Claude Code is unable to respond ... Usage Policy (Policy違反)
#         - Claude usage limit reached / monthly usage limit (Usage超過)
#
# status page 裏取り: API障害型のみ status.claude.com を参照し、Anthropic 側
#       インシデントの有無を通知に反映する (5分キャッシュ)。
#
# TMUX_CLAUDE_ERROR_DETECT_DISABLED=true で無効化可能。

set -u

[[ -z "${_ERROR_DETECTOR_LOADED:-}" ]] || return 0
_ERROR_DETECTOR_LOADED=1

ERROR_DETECTOR_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# エラー文言パターン (実画面から採取)
readonly ERR_PAT_API='API Error: [0-9]'
readonly ERR_PAT_USAGE='Claude usage limit reached|monthly usage limit'
readonly ERR_PAT_POLICY='unable to respond to this request'

# Anthropic status page キャッシュ
ERROR_STATUS_CACHE="${TMPDIR:-/tmp}/claude-anthropic-status.json"
readonly ERROR_STATUS_CACHE_TTL=300  # 5 分

# Anthropic status page の indicator を返す (none/minor/major/critical)。
# 5 分キャッシュ。取得失敗時は空文字。
check_anthropic_status() {
    if [[ -f "$ERROR_STATUS_CACHE" ]]; then
        local mtime age
        mtime=$(stat -f %m "$ERROR_STATUS_CACHE" 2>/dev/null \
                || stat -c %Y "$ERROR_STATUS_CACHE" 2>/dev/null || echo 0)
        age=$(( $(date +%s) - mtime ))
        [[ $age -ge $ERROR_STATUS_CACHE_TTL ]] && rm -f "$ERROR_STATUS_CACHE"
    fi
    if [[ ! -f "$ERROR_STATUS_CACHE" ]]; then
        curl -s --max-time 5 \
            "https://status.claude.com/api/v2/summary.json" \
            -o "$ERROR_STATUS_CACHE" 2>/dev/null || return 1
    fi
    if command -v jq >/dev/null 2>&1; then
        jq -r '.status.indicator // ""' "$ERROR_STATUS_CACHE" 2>/dev/null
    else
        grep -o '"indicator":"[^"]*"' "$ERROR_STATUS_CACHE" 2>/dev/null \
            | head -1 | cut -d'"' -f4
    fi
}

# エラー検出時の通知 (音 + TTS)。バックグラウンド実行前提。
# 引数: pane_target, errtype (api/usage/policy)
notify_error() {
    local pane_target="$1" errtype="$2"

    # エラー音 (Sosumi)
    local sound_enabled
    sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    if [[ "$sound_enabled" == "true" && -x "$ERROR_DETECTOR_DIR/sound_utils.sh" ]]; then
        "$ERROR_DETECTOR_DIR/sound_utils.sh" play error "$pane_target" >/dev/null 2>&1 &
    fi

    # TTS 読み上げ
    local summary_enabled
    summary_enabled=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
    [[ "$summary_enabled" != "true" ]] && return 0

    local msg
    case "$errtype" in
        api)
            local ind
            ind=$(check_anthropic_status)
            case "$ind" in
                none)
                    msg="API 障害です。Anthropic ステータスは正常表示のため一時的な可能性があります。" ;;
                minor|major|critical)
                    msg="API 障害です。Anthropic 側でインシデントが発生しています。" ;;
                *)
                    msg="API 障害が発生しています。" ;;
            esac
            ;;
        usage)  msg="利用上限に達しました。継続できません。" ;;
        policy) msg="リクエストが Usage Policy 違反と判定されました。" ;;
        *)      msg="継続不能なエラーが発生しています。" ;;
    esac

    (
        source "$ERROR_DETECTOR_DIR/sound_utils.sh" 2>/dev/null
        speak_text "$msg" "$pane_target"
    ) >/dev/null 2>&1 &
}

# 全 Claude Code ペインの継続不能エラーを検出。
# - エラー検出 → ペイン状態を Error に、新規検出時は notify_error
# - エラー解消 → Error だったら Idle に戻す (title/hook 補正に委ねる)
detect_error_state() {
    [[ "${TMUX_CLAUDE_ERROR_DETECT_DISABLED:-false}" == "true" ]] && return 0

    local panes
    panes=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}	#{pane_current_command}" 2>/dev/null)
    [[ -z "$panes" ]] && return 0

    while IFS=$'\t' read -r pane_target cmd; do
        [[ -z "$pane_target" ]] && continue
        [[ "$cmd" == claude* ]] || continue

        # 可視範囲の末尾 20 行をエラーパターンで検査
        local content
        content=$(tmux capture-pane -t "$pane_target" -p 2>/dev/null | tail -20)

        local errtype=""
        if   echo "$content" | grep -qE "$ERR_PAT_API";   then errtype="api"
        elif echo "$content" | grep -qE "$ERR_PAT_USAGE"; then errtype="usage"
        elif echo "$content" | grep -qF "$ERR_PAT_POLICY"; then errtype="policy"
        fi

        local pane_key cur
        pane_key=$(encode_pane_key "$pane_target")
        cur=$(tmux show-option -gqv "@claude_voice_pane_status_${pane_key}" 2>/dev/null)

        if [[ -n "$errtype" ]]; then
            if [[ "$cur" != "Error" ]]; then
                tmux set-option -g "@claude_voice_pane_status_${pane_key}" "Error" 2>/dev/null
                aggregate_window_icon "$pane_target"
                log_debug "エラー検出: $pane_target ($errtype) ${cur:-未登録} -> Error"
                notify_error "$pane_target" "$errtype" &
            fi
        else
            if [[ "$cur" == "Error" ]]; then
                tmux set-option -g "@claude_voice_pane_status_${pane_key}" "Idle" 2>/dev/null
                aggregate_window_icon "$pane_target"
                log_debug "エラー解消: $pane_target Error -> Idle"
            fi
        fi
    done <<< "$panes"
}
