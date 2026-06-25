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

# メインリポジトリのルートを stdout に出す（リンク worktree 内から呼んでも一定）。
# 共有 .git (git-common-dir) の親 = メイン作業ツリーのルート。show-toplevel だと
# リンク worktree 内では worktree 自身を指し、配置が worktrees/ にネストしてしまう。
_repo_root() {
    local cdir
    cdir="$(git rev-parse --git-common-dir 2>/dev/null)" || return 1
    cdir="$(cd "$cdir" && pwd -P)" || return 1
    dirname "$cdir"
}

# worktree の配置パス（gwt と同じ外部規約）を stdout に出す。配置は常にメイン基準。
worktree_path() {
    local name="$1" root parent repo
    root="$(_repo_root)" || return 1
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

# src 作業ツリーの *.local.md / *.local.json を相対パスを保って dest へ再帰複製する。
# git 管理外のローカルメモやローカル設定（.claude/settings.local.json 等）を新 worktree に
# 引き継ぐ用途。.git と node_modules は除外（生成物を拾わない）。
# BSD/GNU 両 find 対応の構文を使う。
_copy_local_files() {
    local src="$1" dest="$2" f rel
    while IFS= read -r -d '' f; do
        rel="${f#"$src"/}"
        mkdir -p "$dest/$(dirname "$rel")"
        cp -p "$f" "$dest/$rel"
    done < <(find "$src" \( -name .git -o -name node_modules \) -prune \
        -o -type f \( -name '*.local.md' -o -name '*.local.json' \) -print0 2>/dev/null)
}

# worktree を外部配置に作成（base 既定 HEAD）。パス/ベースは %q でエスケープ。
# 作成後、現作業ツリーの *.local.md / *.local.json を新 worktree に複製（dry-run では行わない）。
wt_create() {
    local name="$1" base path br src
    base="$(resolve_base "${2:-}")"
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git worktree add -b $(printf '%q' "$br") $(printf '%q' "$path") $(printf '%q' "$base")" || return 1
    if [[ -z "${WT_DRY_RUN:-}" ]]; then
        src="$(git rev-parse --show-toplevel 2>/dev/null)" && _copy_local_files "$src" "$path"
    fi
}

# claude を起動し、@cc_worktree(pane オプション)に worktree パスを記録。
#   supervised   : 現 window を split-window で分割した pane（向きは tmux 既定）
#   unsupervised : new-window -d（detached, headless。視界を奪わない背景実行）
# どちらも新 pane の id を -P -F '#{pane_id}' で捕捉し、-p -t でその pane へ
# オプションを付ける（識別は pane 単位に統一。wt_open は list-panes で探索）。
wt_spawn() {
    local mode="$1" name="$2" task="${3:-}" path cmd panecmd
    path="$(worktree_path "$name")"
    cmd="$(build_claude_cmd "$mode" "$name" "$task")" || return 1
    if [[ "$mode" == "unsupervised" ]]; then
        _emit "_wt_p=\$(tmux new-window -d -c $(printf '%q' "$path") -n $(printf '%q' "$name") -P -F '#{pane_id}' $cmd) ; tmux set-option -p -t \"\$_wt_p\" @cc_worktree $(printf '%q' "$path")"
    else
        # claude 終了後は worktree の cwd でシェルに落として pane を残す
        # （オプションを付け直して claude -c で再起動できるように）。
        # pane コマンド全体を 1 トークンとして tmux に渡し sh -c 実行させる。
        panecmd="$cmd ; exec $(printf '%q' "${SHELL:-/bin/sh}")"
        _emit "_wt_p=\$(tmux split-window -c $(printf '%q' "$path") -P -F '#{pane_id}' $(printf '%q' "$panecmd")) ; tmux set-option -p -t \"\$_wt_p\" @cc_worktree $(printf '%q' "$path")"
    fi
}

# worktree を安全に削除（未コミット変更があれば git が拒否）。
# git -C <main-root> で操作するため、対象 worktree 内が cwd でも安全に消せ、
# 削除後に cwd 消滅で次コマンドが落ちる問題も起きない。ブランチは安全側 -d
# （未マージなら残す）。worktree 削除に失敗したら rc 1 を返す（呼び出し側が強制を提示）。
wt_remove() {
    local name root path br
    name="$1"
    root="$(_repo_root)" || return 1
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git -C $(printf '%q' "$root") worktree remove $(printf '%q' "$path")" || return 1
    _emit "git -C $(printf '%q' "$root") branch -d $(printf '%q' "$br")" || true
}

# worktree を強制削除（未コミット変更を破棄）。ブランチも強制削除 -D。
wt_remove_force() {
    local name root path br
    name="$1"
    root="$(_repo_root)" || return 1
    path="$(worktree_path "$name")"
    br="$(branch_name "$name")"
    _emit "git -C $(printf '%q' "$root") worktree remove --force $(printf '%q' "$path")" || return 1
    _emit "git -C $(printf '%q' "$root") branch -D $(printf '%q' "$br")" || true
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
    root="$(_repo_root)" || return 1
    parent="$(dirname "$root")"
    repo="$(basename "$root")"
    prefix="${parent}/worktrees/${repo}-"
    git worktree list --porcelain \
        | awk '/^worktree /{print $2}' \
        | awk -v p="$prefix" 'index($0, p) == 1 { print substr($0, length(p) + 1) }'
}

# 既存 worktree に対応する pane があれば前面化、無ければ split-window で claude -c。
# pane を id で捕捉し -p -t で @cc_worktree を確実に付与する。
wt_open() {
    local name="$1" path pane
    path="$(worktree_path "$name")"
    pane="$(tmux list-panes -a -F '#{pane_id} #{@cc_worktree}' 2>/dev/null \
        | awk -v p="$path" '$2==p{print $1; exit}')"
    if [[ -n "$pane" ]]; then
        tmux select-window -t "$pane"
        tmux select-pane -t "$pane"
    else
        pane="$(tmux split-window -c "$path" -P -F '#{pane_id}' "claude -c ; exec ${SHELL:-/bin/sh}")"
        tmux set-option -p -t "$pane" @cc_worktree "$path"
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
            削除)
                if wt_remove "$name"; then
                    echo "削除しました: $name"; sleep 1
                else
                    echo "削除できません（未コミット/未追跡の変更がある可能性）" >&2
                    printf '強制削除しますか？ 変更は失われます [y/N]: ' >&2
                    local yn; IFS= read -r yn
                    case "$yn" in
                        [yY]*) if wt_remove_force "$name"; then echo "強制削除しました: $name"; else echo "強制削除に失敗しました"; fi; sleep 2 ;;
                        *) echo "中止しました"; sleep 1 ;;
                    esac
                fi
                ;;
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
