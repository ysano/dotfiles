#!/bin/bash
# Claude Code Notification Mode Toggle Script
# システムBEEPモードと通知音+音声合成モードを切り替え

NOTIFICATION_MODE_FILE="$HOME/.tmux/claude/notification_mode"

# 現在のモードを取得
get_current_mode() {
    if [ -f "$NOTIFICATION_MODE_FILE" ]; then
        cat "$NOTIFICATION_MODE_FILE" 2>/dev/null || echo "sound"
    else
        echo "sound"
    fi
}

# モードを設定
set_mode() {
    local mode="$1"
    mkdir -p "$(dirname "$NOTIFICATION_MODE_FILE")"
    echo "$mode" > "$NOTIFICATION_MODE_FILE"
    
    # tmux環境変数を更新
    case "$mode" in
        "beep")
            tmux set-environment -g CLAUDE_NOTIFICATION_MODE "beep"
            tmux set-environment -g CLAUDE_VOICE_AUTO_SUMMARY "false"
            ;;
        "sound")
            tmux set-environment -g CLAUDE_NOTIFICATION_MODE "sound"
            tmux set-environment -g CLAUDE_VOICE_AUTO_SUMMARY "true"
            ;;
    esac
}

# モードを表示
show_mode() {
    local current_mode
    current_mode=$(get_current_mode)
    case "$current_mode" in
        "beep")
            echo "🔕 BEEP Mode (システムベル通知)"
            ;;
        "sound")
            echo "🔊 Sound Mode (通知音+音声合成)"
            ;;
    esac
}

# モードをトグル
toggle_mode() {
    local current_mode
    local new_mode
    current_mode=$(get_current_mode)
    
    if [ "$current_mode" = "beep" ]; then
        new_mode="sound"
    else
        new_mode="beep"
    fi
    
    set_mode "$new_mode"
    
    # 結果を表示
    echo "通知モードを切り替えました:"
    show_mode
    
    # tmuxステータスバーを更新
    tmux refresh-client -S 2>/dev/null || true
}

# 使用方法を表示
show_help() {
    cat << EOF
Claude Code Notification Mode Toggle

使用方法:
  $(basename "$0") [COMMAND]

コマンド:
  toggle, t     - モードをトグル切り替え
  status, s     - 現在のモードを表示
  beep, b       - BEEPモードに設定
  sound, so     - 音声モードに設定
  help, h       - このヘルプを表示

モード:
  🔕 BEEP Mode  - システムベル通知のみ
  🔊 Sound Mode - 通知音+音声合成

例:
  $(basename "$0") toggle    # モード切り替え
  $(basename "$0") status    # 現在のモード確認
EOF
}

# メイン処理
case "${1:-toggle}" in
    "toggle"|"t")
        toggle_mode
        ;;
    "status"|"s")
        show_mode
        ;;
    "beep"|"b")
        set_mode "beep"
        echo "BEEPモードに設定しました:"
        show_mode
        ;;
    "sound"|"so")
        set_mode "sound"
        echo "音声モードに設定しました:"
        show_mode
        ;;
    "help"|"h"|"--help")
        show_help
        ;;
    *)
        echo "Unknown command: $1"
        show_help
        exit 1
        ;;
esac