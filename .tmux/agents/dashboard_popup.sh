#!/bin/sh
# Prefix+w の起動元paneを保存したままdashboard popupを開く。
# display-popupのshell command内ではtmux formatが展開されないため、
# run-shellから渡されたsession/paneを明示的に使う。
set -eu

session=${1:-}
origin=${2:-}
client=${3:-}

case "$session" in
    \$[0-9]*) ;;
    *) exit 1 ;;
esac
case "$origin" in
    %[0-9]*) ;;
    *) exit 1 ;;
esac
case "$client" in
    /dev/*) ;;
    *) exit 1 ;;
esac

cwd=$(tmux display-message -p -t "$origin" '#{pane_current_path}') || exit 1

if command -v python3 >/dev/null 2>&1; then
    exec tmux display-popup -c "$client" -t "$origin" -E -w 90% -h 80% -d "$cwd" \
        "python3 ~/.tmux/agents/dashboard.py --session '$session' --pane '$origin'"
fi

exec tmux display-popup -c "$client" -t "$origin" -E -w 90% -h 80% -d "$cwd" \
    "~/.tmux/claude/worktree_launch.sh popup"
