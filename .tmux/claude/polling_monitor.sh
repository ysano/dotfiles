#!/bin/bash
# ファイル名: polling_monitor.sh
# 説明: Hooks駆動 + 軽量Liveness Checkによる Claude Voice 監視スクリプト
# 用途: tmux status-right から定期呼び出し。Hooks が全ステータス遷移を管理し、
#       本スクリプトは登録済みペインの生存確認と古い状態のクリーンアップのみ行う。

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

# メイン処理：Liveness Check（1回実行）
# Hooks が全ステータス遷移を管理するため、ポーリングは以下のみ行う:
#   1. 登録済みペインの生存確認（pane_title チェック）
#   2. 古い状態のクリーンアップ
#   3. 未登録 CC ペインの自動登録（セーフティネット）
polling_monitor_main() {
    # 設定読み込み
    if ! load_configuration; then
        return 0 # システム無効時は静かに終了
    fi

    # tmuxセッションが存在するかチェック
    if ! tmux list-sessions &>/dev/null; then
        return 0
    fi

    # 現在の全ペインのタイトルを取得（1回の tmux 呼び出しで完了）
    local all_panes
    all_panes=$(tmux list-panes -a -F "#{session_name}:#{window_index}.#{pane_index}	#{pane_title}" 2>/dev/null)

    # 現在アクティブな CC ペインをタイトルから抽出
    local active_cc_panes=""
    while IFS=$'\t' read -r pane_target title; do
        [[ -z "$pane_target" ]] && continue
        if [[ "$title" == *"Claude Code"* ]]; then
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
                "Waiting")      icon="⌛" ;;
                "Idle")         icon="✅" ;;
                "Unregistered") icon="❓" ;;
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
