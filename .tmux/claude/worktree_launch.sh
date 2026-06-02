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

# worktree の配置パス（gwt と同じ外部規約）を stdout に出す。
worktree_path() {
    local name="$1" root parent repo
    root="$(git rev-parse --show-toplevel)" || return 1
    parent="$(dirname "$root")"
    repo="$(basename "$root")"
    printf '%s/worktrees/%s-%s' "$parent" "$repo" "$name"
}

# worktree 用ブランチ名（公式 -w に合わせ worktree- プレフィックス）。
branch_name() {
    printf 'worktree-%s' "$1"
}

# base ref を解決。引数なしは HEAD（手元の最新を起点に）。
resolve_base() {
    if [[ -n "${1:-}" ]]; then
        printf '%s' "$1"
    else
        printf 'HEAD'
    fi
}

# 起動する claude コマンド文字列を組み立てる。
#   supervised   : 対話。表示名のみ。
#   unsupervised : headless。許可は読取+編集系に厳選（Bash は既定で渡さない）。
#   name/task/allowedTools は %q (bash 組み込み printf) でエスケープし、
#   後段の eval で単語分割・注入が起きないようにする。
build_claude_cmd() {
    local mode="$1" name="$2" task="${3:-}"
    case "$mode" in
        supervised)
            printf 'claude -n %q' "$name"
            ;;
        unsupervised)
            printf 'claude -p %q --allowedTools %q' "$task" "Read Edit Write Grep Glob"
            ;;
        *)
            return 1
            ;;
    esac
}

# WT_DRY_RUN が非空ならコマンド文字列を echo（テスト用）、空なら実行する。
_emit() {
    if [[ -n "${WT_DRY_RUN:-}" ]]; then
        printf '%s\n' "$*"
    else
        eval "$*"
    fi
}

# worktree を外部配置に作成（base 既定 HEAD）。
wt_create() {
    local name="$1" base path br
    base="$(resolve_base "${2:-}")"
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git worktree add -b $br $path $base"
}

# tmux window を作って claude を起動し、@cc_worktree に worktree パスを記録。
#   supervised   : 現セッションに new-window（フォアグラウンド）
#   unsupervised : new-window -d（detached, headless）
wt_spawn() {
    local mode="$1" name="$2" task="${3:-}" path cmd flags
    path="$(worktree_path "$name")"
    cmd="$(build_claude_cmd "$mode" "$name" "$task")" || return 1
    if [[ "$mode" == "unsupervised" ]]; then
        flags="-d"
    else
        flags=""
    fi
    _emit "tmux new-window $flags -c $path -n $name $cmd ; tmux set-window-option @cc_worktree $path"
}

# worktree とブランチを削除。
wt_remove() {
    local name="$1" path br
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git worktree remove $path ; git branch -D $br"
}
