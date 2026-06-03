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

# worktree を外部配置に作成（base 既定 HEAD）。パス/ベースは %q でエスケープ。
wt_create() {
    local name="$1" base path br
    base="$(resolve_base "${2:-}")"
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git worktree add -b $(printf '%q' "$br") $(printf '%q' "$path") $(printf '%q' "$base")"
}

# tmux window を作って claude を起動し、@cc_worktree に worktree パスを記録。
#   supervised   : 現セッションに new-window（フォアグラウンド）
#   unsupervised : new-window -d（detached, headless）
# 新 window の id を -P -F で捕捉し -t で確実にその window へオプションを付ける
# (-d 時は新 window が current にならないため -t なしでは誤った window に付く)。
wt_spawn() {
    local mode="$1" name="$2" task="${3:-}" path cmd flags
    path="$(worktree_path "$name")"
    cmd="$(build_claude_cmd "$mode" "$name" "$task")" || return 1
    if [[ "$mode" == "unsupervised" ]]; then
        flags="-d "
    else
        flags=""
    fi
    _emit "_wt_win=\$(tmux new-window ${flags}-c $(printf '%q' "$path") -n $(printf '%q' "$name") -P -F '#{window_id}' $cmd) ; tmux set-window-option -t \"\$_wt_win\" @cc_worktree $(printf '%q' "$path")"
}

# worktree とブランチを削除。
wt_remove() {
    local name="$1" path br
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git worktree remove $(printf '%q' "$path") ; git branch -D $(printf '%q' "$br")"
}

# 行選択 UI。fzf があれば fzf、無ければ read にフォールバック（グレースフル劣化）。
_pick() {
    local prompt="$1"
    if command -v fzf >/dev/null 2>&1; then
        fzf --prompt="$prompt " --height=40% --reverse
    else
        printf '%s' "$prompt" >&2
        local line; IFS= read -r line; printf '%s' "$line"
    fi
}

# fzf --print-query の結果を解釈する。
#   $1: fzf の終了コード  $2: fzf 出力 (1 行目=query, 2 行目=選択行)
#   stdout: "select\t<name>"(既存選択) / "new\t<query>"(新規名) / "abort"(中止)
# fzf は「選択=rc0」「該当なしで Enter=rc1(query を採用)」「ESC=rc130」を返す。
classify_pick() {
    local rc="$1" out="$2" query selection
    query="$(printf '%s\n' "$out" | sed -n '1p')"
    selection="$(printf '%s\n' "$out" | sed -n '2p')"
    case "$rc" in
        0) if [[ -n "$selection" ]]; then printf 'select\t%s' "$selection"; else printf 'abort'; fi ;;
        1) if [[ -n "$query" ]]; then printf 'new\t%s' "$query"; else printf 'abort'; fi ;;
        *) printf 'abort' ;;
    esac
}

# 既存 worktree 名の一覧。外部 worktrees ディレクトリの完全パス前方一致で名前を取り出す
# (リポジトリ名を正規表現に流さないよう awk のリテラル一致を使う)。
_list_worktrees() {
    local root parent repo prefix
    root="$(git rev-parse --show-toplevel)" || return 1
    parent="$(dirname "$root")"
    repo="$(basename "$root")"
    prefix="${parent}/worktrees/${repo}-"
    git worktree list --porcelain \
        | awk '/^worktree /{print $2}' \
        | awk -v p="$prefix" 'index($0, p) == 1 { print substr($0, length(p) + 1) }'
}

# 既存 worktree に対応する window があれば前面化、無ければ claude -c で継続。
# 新規 window は id を捕捉し -t で @cc_worktree を確実に付与する。
wt_open() {
    local name="$1" path win
    path="$(worktree_path "$name")"
    win="$(tmux list-windows -a -F '#{window_id} #{@cc_worktree}' 2>/dev/null \
        | awk -v p="$path" '$2==p{print $1; exit}')"
    if [[ -n "$win" ]]; then
        tmux select-window -t "$win"
    else
        win="$(tmux new-window -c "$path" -n "$name" -P -F '#{window_id}' 'claude -c')"
        tmux set-window-option -t "$win" @cc_worktree "$path"
    fi
}

# popup のメインフロー。
cmd_popup() {
    if ! git rev-parse --show-toplevel >/dev/null 2>&1; then
        echo "git リポジトリ内で実行してください"; sleep 1; return 1
    fi
    # 既存 worktree を選ぶか、リストに無い名前を入力して新規作成するかを 1 画面で受ける。
    # fzf: 入力語で絞り込み、該当なしで Enter ならその語を新規名として採用 (--print-query)。
    # fzf 不在時は read 入力 → 既存名なら選択、それ以外は新規名。
    local verb name
    if command -v fzf >/dev/null 2>&1; then
        local out rc decision
        out="$(_list_worktrees | fzf --print-query --reverse --height=40% \
            --prompt='worktree (既存を選択 / 新規名を入力)> ')"
        rc=$?
        decision="$(classify_pick "$rc" "$out")"
        verb="${decision%%$'\t'*}"
        name="${decision#*$'\t'}"
        [[ "$verb" == "abort" ]] && return 0
    else
        printf 'worktree (既存名 or 新規名): ' >&2
        local raw; IFS= read -r raw || return 0
        [[ -n "$raw" ]] || return 0
        if _list_worktrees | grep -qxF -- "$raw"; then verb="select"; else verb="new"; fi
        name="$raw"
    fi

    if [[ "$verb" == "select" ]]; then
        local action; action="$(printf '%s\n' "開く / 前面化" "削除" | _pick "action>")"
        case "$action" in
            開く*) wt_open "$name" ;;
            削除)  wt_remove "$name" ;;
        esac
        return 0
    fi

    # 新規作成 (verb == new): 入力語を sanitize してブランチ名にする。
    local branch base mode task
    branch="$(validate_name "$name")" || { echo "名前が不正です"; sleep 1; return 1; }
    printf 'base ref [HEAD]: ' >&2; IFS= read -r base
    wt_create "$branch" "$base" || { echo "worktree 作成に失敗"; sleep 1; return 1; }
    mode="$(printf '%s\n' "監視あり (supervised)" "監視なし (unsupervised)" | _pick "mode>")"
    case "$mode" in
        *unsupervised*) printf 'タスク: ' >&2; IFS= read -r task
                        wt_spawn unsupervised "$branch" "$task" ;;
        *supervised*)   wt_spawn supervised "$branch" ;;
    esac
}

main() {
    case "${1:-popup}" in
        popup) cmd_popup ;;
        *) echo "usage: worktree_launch.sh popup" >&2; return 2 ;;
    esac
}

# source 時は関数定義のみ。直接実行時のみ main を呼ぶ。
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
