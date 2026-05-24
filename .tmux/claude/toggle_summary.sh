#!/bin/bash
# ファイル名: toggle_summary.sh
# 説明: 完了要約読み上げ (summary_enabled) の ON/OFF を切り替え
# 用途: Prefix + v + v から呼び出される。display-message に加えて say で
#       気付ける音声フィードバックを出す (ADR 0008 サイレントトグル対策)。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

current=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
if [[ "$current" == "true" ]]; then
    new="false"
    label="オフ"
else
    new="true"
    label="オン"
fi

tmux set-option -g @claude_voice_summary_enabled "$new" 2>/dev/null
tmux display-message "Claude Voice 要約: $new"

# 短い音声フィードバックでトグルを耳で確認可能にする
# (Kyoko が無い環境では系既定の声にフォールバック)
if command -v say >/dev/null 2>&1; then
    voice=$(tmux show-option -gqv @claude_voice_macos_voice 2>/dev/null)
    voice="${voice:-Kyoko}"
    say -v "$voice" -r 240 "要約${label}" >/dev/null 2>&1 &
fi

exit 0
