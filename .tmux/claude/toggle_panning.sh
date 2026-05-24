#!/bin/bash
# ファイル名: toggle_panning.sh
# 説明: パンニング機能 (panning_enabled) の ON/OFF を切り替え
# 用途: Prefix + v + p から呼び出される。display-message に加えて say で
#       気付ける音声フィードバックを出す (ADR 0008 サイレントトグル対策)。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

current=$(tmux show-option -gqv @claude_voice_panning_enabled 2>/dev/null)
if [[ "$current" == "true" ]]; then
    new="false"
    label="オフ"
else
    new="true"
    label="オン"
fi

tmux set-option -g @claude_voice_panning_enabled "$new" 2>/dev/null
tmux display-message "Claude Voice パンニング: $new"

if command -v say >/dev/null 2>&1; then
    voice=$(tmux show-option -gqv @claude_voice_macos_voice 2>/dev/null)
    voice="${voice:-Kyoko}"
    say -v "$voice" -r 240 "パンニング${label}" >/dev/null 2>&1 &
fi

exit 0
