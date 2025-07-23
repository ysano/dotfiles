#!/bin/bash
# Claude Code Notification Mode Status Display for tmux status bar

NOTIFICATION_MODE_FILE="$HOME/.tmux/claude/notification_mode"

# 現在のモードを取得
get_current_mode() {
    if [ -f "$NOTIFICATION_MODE_FILE" ]; then
        cat "$NOTIFICATION_MODE_FILE" 2>/dev/null || echo "sound"
    else
        echo "sound"
    fi
}

# ステータスバー用の短い表示
current_mode=$(get_current_mode)
case "$current_mode" in
    "beep")
        echo "🔕"
        ;;
    "sound")
        echo "🔊"
        ;;
    *)
        echo "🔊"
        ;;
esac