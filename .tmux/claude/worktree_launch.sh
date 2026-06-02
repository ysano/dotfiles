#!/bin/bash
# ファイル名: worktree_launch.sh
# 説明: git worktree を外部配置で作成し Claude Code を tmux 上に並列起動する
#       ランチャー。prefix + w の display-popup から呼ばれる。
# 用途: 純粋ヘルパは source して単体テスト可能。実行時は `worktree_launch.sh popup`。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 名前を sanitize して stdout に出す。無効（結果が空）なら rc 1。正常時 rc 0。
# ロケール非依存にするため tr/sed は LC_ALL=C で実行する。
validate_name() {
    local raw="${1:-}" s
    s="$(printf '%s' "$raw" \
        | LC_ALL=C tr ' /' '--' \
        | LC_ALL=C sed -E 's/[^A-Za-z0-9._-]/-/g; s/-+/-/g; s/^-+//; s/-+$//')"
    [[ -n "$s" ]] || return 1
    printf '%s' "$s"
}
