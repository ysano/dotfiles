#!/bin/bash
# ファイル名: polling_monitor.sh
# 説明: Hooks駆動 + 軽量Liveness Checkによる Claude Voice 監視スクリプト
# 用途: tmux status-right から定期呼び出し。Hooks が全ステータス遷移を管理し、
#       本スクリプトは登録済みペインの生存確認と古い状態のクリーンアップのみ行う。

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 設定とヘルパー関数を読み込み
source "$SCRIPT_DIR/functions.sh"

# Dialog detector を source (検出関数 detect_dialogs を提供)
[[ -f "$SCRIPT_DIR/dialog_detector.sh" ]] && source "$SCRIPT_DIR/dialog_detector.sh"

# Error detector を source (検出関数 detect_error_state を提供)
[[ -f "$SCRIPT_DIR/error_detector.sh" ]] && source "$SCRIPT_DIR/error_detector.sh"

# 設定読み込み（silent mode）
load_configuration() {
    # システム有効/無効チェック
    CLAUDE_VOICE_ENABLED=$(tmux show-option -gqv @claude_voice_enabled 2>/dev/null)
    if [[ "$CLAUDE_VOICE_ENABLED" != "true" ]]; then
        return 1 # システム無効時は何もしない
    fi

    return 0
}

# pane_title から Claude Code の状態を推定する (hook 不発セッション対策)
#   Busy: 先頭が点字スピナー (U+2800-U+28FF / UTF-8 で e2a0xx-e2a3xx)
#   Idle: 先頭が ✳ (U+2733 / UTF-8 で e29cb3)
#   不明: それ以外 → 空文字 (呼び出し側は登録ステータスを変更しない)
infer_state_from_title() {
    local title="$1"
    [[ -z "$title" ]] && return 0
    local hex
    hex=$(printf '%s' "$title" | head -c3 | od -An -tx1 2>/dev/null | tr -d ' \n')
    case "$hex" in
        e2a0??|e2a1??|e2a2??|e2a3??) echo "Busy" ;;
        e29cb3)                       echo "Idle" ;;
        *)                            echo "" ;;
    esac
}

# pane_title による状態補正。
# hook を発火しない CC セッションでもアイコンが実状態を反映するようにする。
# - title スピナー → Busy (確実に作業中なので常に上書き)
# - title ✳ かつ登録が Busy → Idle (古い Busy を補正。Idle/Waiting は hook 判定を尊重)
# TMUX_CLAUDE_TITLE_DETECT_DISABLED=true で無効化可能。
correct_status_from_title() {
    [[ "${TMUX_CLAUDE_TITLE_DETECT_DISABLED:-false}" == "true" ]] && return 0
    local panes
    panes=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}	#{pane_current_command}	#{pane_title}" 2>/dev/null)
    [[ -z "$panes" ]] && return 0
    while IFS=$'\t' read -r pane_target cmd title; do
        [[ -z "$pane_target" ]] && continue
        [[ "$cmd" == claude* ]] || continue
        local inferred
        inferred=$(infer_state_from_title "$title")
        [[ -z "$inferred" ]] && continue
        local pane_key cur
        pane_key=$(encode_pane_key "$pane_target")
        cur=$(tmux show-option -gqv "@claude_voice_pane_status_${pane_key}" 2>/dev/null)
        # Permission / Question / Error は専用検出 (hook / dialog_detector /
        # エラー検出) が管理するため title 補正の対象外。Busy <-> Idle のみ扱う。
        case "$cur" in
            Permission|Question|Error) continue ;;
        esac
        if [[ "$inferred" == "Busy" && "$cur" != "Busy" ]]; then
            tmux set-option -g "@claude_voice_pane_status_${pane_key}" "Busy" 2>/dev/null
            aggregate_window_icon "$pane_target"
            log_debug "title 補正: $pane_target ${cur:-未登録} -> Busy"
            # hook 不発セッションでも通知音が鳴るよう polling 側からも発火
            # hook 駆動セッションでは hook 側が先に状態を Busy にしているため
            # この分岐に来ず、二重発火しない
            if [[ "$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)" == "true" ]] \
               && [[ -x "$SCRIPT_DIR/sound_utils.sh" ]]; then
                "$SCRIPT_DIR/sound_utils.sh" play start "$pane_target" >/dev/null 2>&1 &
            fi
        elif [[ "$inferred" == "Idle" && "$cur" == "Busy" ]]; then
            tmux set-option -g "@claude_voice_pane_status_${pane_key}" "Idle" 2>/dev/null
            aggregate_window_icon "$pane_target"
            log_debug "title 補正: $pane_target Busy -> Idle"
            if [[ "$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)" == "true" ]] \
               && [[ -x "$SCRIPT_DIR/sound_utils.sh" ]]; then
                "$SCRIPT_DIR/sound_utils.sh" play complete "$pane_target" >/dev/null 2>&1 &
            fi
        fi
    done <<< "$panes"
}

# メイン処理：Liveness Check（1回実行）
# Hooks がステータス遷移の主経路だが、hook を発火しないセッションも存在するため
# ポーリングで以下を補完する:
#   1. 古い状態のクリーンアップ（CC 終了ペイン）
#   2. 未登録 CC ペインの自動登録（セーフティネット）
#   2.5. pane_title による状態補正（hook 不発セッション対策）
#   3. AskUserQuestion 等ダイアログ検出
polling_monitor_main() {
    # 設定読み込み
    if ! load_configuration; then
        return 0 # システム無効時は静かに終了
    fi

    # tmuxセッションが存在するかチェック
    if ! tmux list-sessions &>/dev/null; then
        return 0
    fi

    # 現在の全ペインの current_command を取得（1回の tmux 呼び出しで完了）
    # 注: pane_title は会話トピック名で上書きされるため CC 検出には使えない。
    #     current_command (claude / claude.exe) で識別する。
    local all_panes
    all_panes=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}	#{pane_current_command}" 2>/dev/null)

    # 現在アクティブな CC ペインを抽出
    local active_cc_panes=""
    while IFS=$'\t' read -r pane_target cmd; do
        [[ -z "$pane_target" ]] && continue
        if [[ "$cmd" == claude* ]]; then
            if [[ -z "$active_cc_panes" ]]; then
                active_cc_panes="$pane_target"
            else
                active_cc_panes="${active_cc_panes}"$'\n'"${pane_target}"
            fi
        fi
    done <<< "$all_panes"

    # 登録済みペインステータスを取得
    local registered_keys
    registered_keys=$(tmux show-options -g 2>/dev/null | grep "^@claude_voice_pane_status_" | awk '{print $1}')

    if [[ -z "$active_cc_panes" && -z "$registered_keys" ]]; then
        # CC ペインなし、登録もなし → 何もしない
        return 0
    fi

    # --- 1. 古い登録のクリーンアップ ---
    # 登録済みだが CC が動いていないペインをクリア
    while IFS= read -r key; do
        [[ -z "$key" ]] && continue
        local pane_id="${key#@claude_voice_pane_status_}"

        # decode_pane_key で安全に逆引き（アンダースコア含むセッション名対応）
        local pane_target
        pane_target=$(decode_pane_key "$pane_id")
        local session="${pane_target%%:*}"
        local session_window="${pane_target%.*}"
        local window="${session_window#*:}"

        if [[ -z "$active_cc_panes" ]] || ! echo "$active_cc_panes" | grep -qF "$pane_target"; then
            # CC が動いていない → クリア
            tmux set-option -g -u "$key" 2>/dev/null
            tmux set-option -g -u "@claude_voice_status_${pane_id}" 2>/dev/null
            tmux set-option -g -u "@claude_voice_hooks_ts_${pane_id}" 2>/dev/null
            # ダイアログ検出状態もクリア
            type cleanup_dialog_state_for_pane >/dev/null 2>&1 \
                && cleanup_dialog_state_for_pane "$pane_id"
            log_debug "Liveness check: ステータスクリア $pane_target"

            # ウィンドウのアイコンを再集約（統一関数）
            aggregate_window_icon "$pane_target"
        fi
    done <<< "$registered_keys"

    # --- 2. 未登録 CC ペインの自動登録（セーフティネット） ---
    # Hooks が発火していない CC ペインを Idle で登録
    if [[ -n "$active_cc_panes" ]]; then
        while IFS= read -r pane_target; do
            [[ -z "$pane_target" ]] && continue
            local pane_key
            pane_key=$(encode_pane_key "$pane_target")
            local existing
            existing=$(tmux show-option -gqv "@claude_voice_pane_status_${pane_key}" 2>/dev/null)
            if [[ -z "$existing" ]]; then
                # 未登録 → Idle で登録
                tmux set-option -g "@claude_voice_pane_status_${pane_key}" "Idle" 2>/dev/null
                log_debug "Liveness check: 未登録 CC ペインを自動登録 $pane_target (Idle)"

                # アイコンを更新（統一関数）
                aggregate_window_icon "$pane_target"
            fi
        done <<< "$active_cc_panes"
    fi

    # --- 2.5. pane_title による状態補正 (hook 不発セッション対策) ---
    correct_status_from_title

    # --- 2.7. 継続不能エラー検出 (API障害 / Usage超過 / Policy違反) ---
    type detect_error_state >/dev/null 2>&1 && detect_error_state

    # --- 3. AskUserQuestion 等ダイアログ検出 (Hooks では捕捉不可な領域) ---
    type detect_dialogs >/dev/null 2>&1 && detect_dialogs
}

# 全Claude Codeペインのステータスを表示（診断用）
show_pane_status() {
    local claude_panes
    claude_panes=$(detect_claude_panes)

    if [[ -z "$claude_panes" ]]; then
        echo "Claude Codeペインが検出されませんでした"
        return 0
    fi

    echo "=== Claude Code ペインステータス ==="

    # Hooks 登録状態も表示
    while IFS= read -r pane_target; do
        if [[ -n "$pane_target" ]]; then
            local pane_key
            pane_key=$(encode_pane_key "$pane_target")
            local hooks_status
            hooks_status=$(tmux show-option -gqv "@claude_voice_pane_status_${pane_key}" 2>/dev/null)
            local hooks_ts
            hooks_ts=$(tmux show-option -gqv "@claude_voice_hooks_ts_${pane_key}" 2>/dev/null)

            local status="${hooks_status:-Unregistered}"
            local icon="?"
            case "$status" in
                "Busy")         icon="⚡" ;;
                "Permission")   icon="⌛" ;;
                "Question")     icon="❓" ;;
                "Error")        icon="⚠" ;;
                "Idle")         icon="✅" ;;
                "Unregistered") icon="?" ;;
            esac

            local ts_info=""
            if [[ -n "$hooks_ts" ]]; then
                local now=$(date +%s)
                local age=$(( now - hooks_ts ))
                ts_info=" (hooks: ${age}s ago)"
            fi

            echo "  $icon $pane_target: $status$ts_info"
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
