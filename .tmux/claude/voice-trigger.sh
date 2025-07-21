#!/bin/bash
# Claude Voice Trigger - Smart Voice Action Handler
# キーバインド経由での音声機能トリガー

# 智的統合層の読み込み
source "$HOME/.tmux/claude/core/integration.sh" 2>/dev/null || {
    echo "Smart integration layer not available" >&2
    exit 1
}

# 設定値取得関数
get_config_value() {
    local key="$1"
    local default_value="$2"
    local config_file="$HOME/.tmux/claude/config/integration.conf"

    if [[ -f "$config_file" ]]; then
        grep "^${key}=" "$config_file" 2>/dev/null | cut -d'=' -f2 || echo "$default_value"
    else
        echo "$default_value"
    fi
}

# Graceful Degradation（機能劣化対応）
graceful_degradation() {
    local reason="$1"

    case "$reason" in
        "audio_unavailable")
            tmux display-message "Audio unavailable - Check system settings"
            ;;
        "integration_disabled")
            tmux display-message "Claude integration disabled"
            ;;
        "claude_voice_error")
            tmux display-message "Claude Voice error - Check logs"
            ;;
        *)
            tmux display-message "Claude integration temporarily unavailable"
            ;;
    esac

    # フォールバック: 基本的なビープ音
    echo -e '\a'
}

# 安全な音声実行
execute_voice_action_safely() {
    local voice_mode="$(get_config_value "voice_mode" "brief")"
    local voice_lines="$(get_config_value "voice_lines" "20")"
    local voice_model="$(get_config_value "voice_model" "auto")"

    # tmux環境情報の収集
    local window_id=$(tmux display-message -p '#I' 2>/dev/null || echo "1")
    local pane_id=$(tmux display-message -p '#P' 2>/dev/null || echo "0")

    # Claude Voice実行ログ
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Manual voice trigger: window=$window_id, pane=$pane_id" >> \
        "$HOME/.tmux/claude/logs/voice-actions.log"

    # 音声処理の安全な実行
    if "$HOME/.tmux/claude/bin/claude-voice" "$voice_mode" "$voice_lines" "Kyoko" "$voice_model" 2>/dev/null; then
        # 成功時の処理
        tmux display-message "Claude Voice: 要約完了"
    else
        # エラー時の処理
        graceful_degradation "claude_voice_error"
        return 1
    fi
}

# メイン処理（智的統合層を使用）
main() {
    # ログディレクトリの確保
    mkdir -p "$HOME/.tmux/claude/logs"

    # 智的統合層による音声アクション実行
    voice_action "manual" "user_triggered"
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
