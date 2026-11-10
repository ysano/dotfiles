#!/bin/bash
# ファイル名: test_worktree_launch.sh
# 説明: worktree_launch.sh の純粋ヘルパと dry-run 出力を検証する単体テスト。
# 用途: .tmux/ci.sh および手動 (bash test_worktree_launch.sh) から実行。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=worktree_launch.sh
source "$SCRIPT_DIR/worktree_launch.sh"

fails=0

assert_eq() { # $1 actual  $2 expected  $3 msg
    if [[ "$1" == "$2" ]]; then
        echo "[PASS] $3"
    else
        echo "[FAIL] $3: got '$1' want '$2'"; fails=$((fails + 1))
    fi
}
assert_contains() { # $1 haystack  $2 needle  $3 msg
    case "$1" in
        *"$2"*) echo "[PASS] $3" ;;
        *) echo "[FAIL] $3: '$1' に '$2' が含まれない"; fails=$((fails + 1)) ;;
    esac
}
assert_not_contains() { # $1 haystack  $2 needle  $3 msg
    case "$1" in
        *"$2"*) echo "[FAIL] $3: '$1' に '$2' が含まれている"; fails=$((fails + 1)) ;;
        *) echo "[PASS] $3" ;;
    esac
}
assert_rc() { # $1 actual_rc  $2 expected_rc  $3 msg
    if [[ "$1" == "$2" ]]; then
        echo "[PASS] $3"
    else
        echo "[FAIL] $3: rc=$1 want=$2"; fails=$((fails + 1))
    fi
}

echo "=== validate_name ==="
assert_eq "$(validate_name 'feat/login')" "feat-login" "スラッシュをハイフン化"
assert_eq "$(validate_name 'a b')" "a-b" "空白をハイフン化"
validate_name "" >/dev/null 2>&1; assert_rc "$?" "1" "空文字は無効"
validate_name "   " >/dev/null 2>&1; assert_rc "$?" "1" "空白のみは無効"

echo ""
echo "=== worktree_path / branch_name / resolve_base ==="
_root="$(git worktree list --porcelain | sed -n '1s/^worktree //p')"
_expected="$(dirname "$_root")/worktrees/$(basename "$_root")-foo"
assert_eq "$(worktree_path foo)" "$_expected" "外部 worktrees 配置を返す"
assert_eq "$(branch_name foo)" "worktree-foo" "ブランチ名に worktree- を付与"
assert_eq "$(resolve_base)" "HEAD" "base 既定は HEAD"
assert_eq "$(resolve_base origin/master)" "origin/master" "base 上書き"

echo ""
echo "=== build_claude_cmd ==="
assert_eq "$(build_claude_cmd supervised login)" "claude -n login" "監視ありは -n 表示名"
_uns="$(build_claude_cmd unsupervised login 'fix tests')"
assert_contains "$_uns" "claude -p" "監視なしは -p"
assert_contains "$_uns" "--allowedTools" "監視なしは allowedTools を厳選"
# %q エスケープにより、task がスペース込みで 1 引数として安全に復元されることを検証
# (eval で位置パラメータに展開し、-p の次の引数が 'fix tests' 1 個であること)
eval "set -- $_uns"
_task_arg=""
while [[ $# -gt 0 ]]; do
    if [[ "$1" == "-p" ]]; then _task_arg="${2:-}"; break; fi
    shift
done
assert_eq "$_task_arg" "fix tests" "監視なしは task を 1 引数として安全に保持 (%q)"
build_claude_cmd bogus x >/dev/null 2>&1; assert_rc "$?" "1" "未知 mode は rc 1"

echo ""
echo "=== dry-run 副作用関数 (WT_DRY_RUN=1) ==="
export WT_DRY_RUN=1
_c="$(wt_create foo)"
assert_contains "$_c" "git worktree add -b worktree-foo" "create: ブランチ指定"
assert_contains "$_c" "$_expected" "create: 外部配置"
assert_contains "$_c" " HEAD" "create: 既定 base は HEAD"
assert_contains "$(wt_create foo origin/master)" " origin/master" "create: base 上書き"
_s="$(wt_spawn supervised login)"
assert_contains "$_s" "split-window" "spawn 監視あり: 現 window に pane 分割"
assert_not_contains "$_s" "new-window" "spawn 監視あり: window は作らない"
assert_contains "$_s" "@cc_worktree" "spawn: pane option で同一性を記録"
assert_contains "$_s" "-P -F '#{pane_id}'" "spawn: 新 pane id を捕捉"
assert_contains "$_s" "set-option -p -t" "spawn: @cc_worktree を対象 pane へ -p -t 指定"
assert_contains "$_s" "exec" "spawn 監視あり: claude 終了後はシェルに落ちて pane を残す"
export TMUX_WORKTREE_ORIGIN=%42
_origin_spawn="$(wt_spawn supervised login)"
assert_contains "$_origin_spawn" "split-window -t %42" "spawn: 渡されたorigin paneを明示targetにする"
unset TMUX_WORKTREE_ORIGIN
_u="$(wt_spawn unsupervised login 'fix tests')"
assert_contains "$_u" "new-window -d" "spawn 監視なしは detached window"
assert_contains "$_u" "-P -F '#{pane_id}'" "spawn 監視なし: 新 pane id を捕捉"
assert_contains "$_u" "set-option -p -t" "spawn 監視なし: pane option で記録"
_r="$(wt_remove foo)"
assert_contains "$_r" "git -C " "remove: メインルートから操作 (git -C)"
assert_contains "$_r" "worktree remove " "remove: worktree 削除（安全・--force なし）"
assert_not_contains "$_r" "worktree remove --force" "remove: 既定では --force しない"
assert_contains "$_r" "branch -d worktree-foo" "remove: ブランチは安全削除 -d"
_rf="$(wt_remove_force foo)"
assert_contains "$_rf" "worktree remove --force" "force: worktree を強制削除"
assert_contains "$_rf" "branch -D worktree-foo" "force: ブランチも強制削除 -D"
unset WT_DRY_RUN

echo ""
echo "=== wt_open: popup origin target ==="
_open_tmp="$(mktemp -d)"
_open_log="$_open_tmp/tmux.log"
cat > "$_open_tmp/tmux" <<'EOF'
#!/bin/sh
printf '%s\n' "$@" >> "$WT_OPEN_LOG"
if [ "$1" = "split-window" ]; then printf '%%99\n'; fi
EOF
chmod +x "$_open_tmp/tmux"
PATH="$_open_tmp:$PATH" WT_OPEN_LOG="$_open_log" TMUX_WORKTREE_ORIGIN=%42 wt_open login
_open_call="$(tr '\n' ' ' < "$_open_log")"
assert_contains "$_open_call" "split-window -t %42" "open: 渡されたorigin paneを明示targetにする"
rm -rf "$_open_tmp"

echo ""
echo "=== classify_pick (fzf --print-query の解釈) ==="
assert_eq "$(classify_pick 0 "$(printf 'tf\nlogin')")" "$(printf 'select\tlogin')" "rc0+選択行: 既存を select"
assert_eq "$(classify_pick 1 "$(printf 'tf')")" "$(printf 'new\ttf')" "rc1+query: 新規名として new"
assert_eq "$(classify_pick 1 "$(printf '')")" "abort" "rc1+空 query: abort"
assert_eq "$(classify_pick 130 "$(printf 'tf')")" "abort" "rc130(ESC): abort"
assert_eq "$(classify_pick 0 "$(printf 'tf')")" "abort" "rc0+選択行なし: abort"

echo ""
echo "=== _pick: fzf が無ければ番号で選ぶ（番号は端末から読む） ==="
_nofzf="/usr/bin:/bin"
assert_eq "$(printf 'a\nb\nc\n' | PATH="$_nofzf" WT_TTY=<(printf '2\n') _pick 'x>' 2>/dev/null)" "b" "pick: 番号の選択肢を返す（先頭行に決め打ちしない）"
assert_eq "$(printf 'a\nb\n' | PATH="$_nofzf" WT_TTY=<(printf '9\n') _pick 'x>' 2>/dev/null)" "" "pick: 範囲外の番号は選ばない"
assert_eq "$(printf 'a\nb\n' | PATH="$_nofzf" WT_TTY=<(printf 'a\n') _pick 'x>' 2>/dev/null)" "" "pick: 数字以外は選ばない"

echo ""
echo "=== worktree_path: リンク worktree 内から呼んでも main 基準で配置 ==="
_mroot="$_root"
_probe="$(dirname "$_mroot")/worktrees/$(basename "$_mroot")-_axisprobe"
if git worktree add -q "$_probe" -b _axisprobe HEAD 2>/dev/null; then
    _got="$(cd "$_probe" && worktree_path foo)"
    git worktree remove --force "$_probe" >/dev/null 2>&1
    git branch -D _axisprobe >/dev/null 2>&1
    assert_eq "$_got" "$(dirname "$_mroot")/worktrees/$(basename "$_mroot")-foo" "別 worktree 内からでも main 基準で配置（ネストしない）"
else
    echo "[SKIP] worktree 作成不可のためスキップ"
fi

echo ""
echo "=== _copy_local_files (*.local.md / *.local.json を再帰複製・生成物は除外) ==="
_csrc="$(mktemp -d)"; _cdst="$(mktemp -d)"
mkdir -p "$_csrc/sub" "$_csrc/.claude" "$_csrc/node_modules/pkg" "$_csrc/.git"
echo a > "$_csrc/a.local.md"
echo b > "$_csrc/sub/b.local.md"
echo s > "$_csrc/.claude/settings.local.json"
echo c > "$_csrc/sub/c.local.json"
echo n > "$_csrc/normal.md"
echo o > "$_csrc/normal.json"
echo j > "$_csrc/node_modules/pkg/junk.local.md"
echo k > "$_csrc/node_modules/pkg/junk.local.json"
echo g > "$_csrc/.git/x.local.md"
_copy_local_files "$_csrc" "$_cdst"
[ -f "$_cdst/a.local.md" ];                      assert_rc "$?" "0" "copy: ルートの *.local.md"
[ -f "$_cdst/sub/b.local.md" ];                  assert_rc "$?" "0" "copy: サブ階層も再帰複製（相対パス維持）"
[ -f "$_cdst/.claude/settings.local.json" ];     assert_rc "$?" "0" "copy: .claude/settings.local.json も複製"
[ -f "$_cdst/sub/c.local.json" ];                assert_rc "$?" "0" "copy: サブ階層の *.local.json も再帰複製"
[ -f "$_cdst/normal.md" ];                       assert_rc "$?" "1" "copy: *.local.md 以外の .md は複製しない"
[ -f "$_cdst/normal.json" ];                     assert_rc "$?" "1" "copy: *.local.json 以外の .json は複製しない"
[ -f "$_cdst/node_modules/pkg/junk.local.md" ];  assert_rc "$?" "1" "copy: node_modules は除外 (.md)"
[ -f "$_cdst/node_modules/pkg/junk.local.json" ];assert_rc "$?" "1" "copy: node_modules は除外 (.json)"
[ -f "$_cdst/.git/x.local.md" ];                 assert_rc "$?" "1" "copy: .git は除外"
rm -rf "$_csrc" "$_cdst"

echo ""
echo "=== 前に消した worktree のブランチが残っているとき（実リポジトリ） ==="
# 空白・日本語を含む場所でも、使用中の worktree を見つける
_er="$(mktemp -d)"
_erepo="$_er/作業 dir/repo"
git init -q -b main "$_erepo"
git -C "$_erepo" config user.email test@example.invalid; git -C "$_erepo" config user.name Test
git -C "$_erepo" commit -q --allow-empty -m init
_back="$PWD"; cd "$_erepo" || exit 1
_ep="$(worktree_path foo)"
assert_eq "$(branch_state worktree-foo)" "none" "branch_state: 無ければ none"
wt_create foo >/dev/null 2>&1; assert_rc "$?" "0" "existing: 最初の作成"
assert_eq "$(branch_state worktree-foo)" "$(printf 'checked-out\t%s' "$(cd "$_ep" && pwd -P)")" "branch_state: worktree で使用中なら checked-out と場所"
git -C "$_ep" commit -q --allow-empty -m "wip on foo"
WT_NOTICE=""
wt_remove foo >/dev/null 2>&1; assert_rc "$?" "0" "existing: worktree は消せる（未 merge のブランチは残る）"
if [ -d "$_ep" ]; then _rc=0; else _rc=1; fi; assert_rc "$_rc" "1" "existing: worktree のディレクトリは消えている"
assert_contains "$WT_NOTICE" "worktree-foo" "remove: 残したブランチの名前を知らせる"
assert_contains "$WT_NOTICE" "残しました" "remove: ブランチを残したことを黙らない"
assert_eq "$(branch_state worktree-foo)" "free" "branch_state: 残ったブランチは free"
_sum="$(branch_summary worktree-foo)"
assert_contains "$_sum" "wip on foo" "summary: 先頭の commit を見せる"
assert_contains "$_sum" "1 件" "summary: HEAD に無い commit の件数を見せる"
wt_create foo >/dev/null 2>&1; assert_rc "$?" "1" "existing: -b では作れない（git が断る）"
wt_create foo "" reuse >/dev/null 2>&1; assert_rc "$?" "0" "existing: 既存のブランチで作れる"
assert_eq "$(git -C "$_ep" symbolic-ref --short HEAD 2>/dev/null)" "worktree-foo" "reuse: 既存のブランチを使う"
assert_eq "$(git -C "$_ep" log -1 --format=%s 2>/dev/null)" "wip on foo" "reuse: 残っていた commit から続ける"
wt_remove_force foo >/dev/null 2>&1
# resolve_existing: 選択（_pick）と確認（標準入力）で、作り方を決める
# shellcheck disable=SC2329  # resolve_existing の中から呼ばれる
_pick() { cat >/dev/null; printf '%s' "$_PICKED"; }
assert_eq "$(resolve_existing worktree-foo 2>/dev/null </dev/null)" "new" "resolve: ブランチが無ければ new"
git -C "$_erepo" branch worktree-foo HEAD; git -C "$_erepo" commit -q --allow-empty -m other
git -C "$_erepo" branch -f worktree-foo HEAD; git -C "$_erepo" reset -q --hard HEAD~1
assert_eq "$(_PICKED="このブランチで作る" resolve_existing worktree-foo 2>/dev/null </dev/null)" "reuse" "resolve: このブランチで作る"
_out="$(_PICKED="中止" resolve_existing worktree-foo 2>/dev/null </dev/null)"; assert_rc "$?" "2" "resolve: 中止（rc 2）"
_out="$(_PICKED="消して作り直す" resolve_existing worktree-foo 2>/dev/null <<< "n")"; assert_rc "$?" "2" "resolve: 消す確認で n なら中止（rc 2）"
assert_eq "$(branch_state worktree-foo)" "free" "resolve: n ならブランチは残る"
assert_eq "$(_PICKED="消して作り直す" resolve_existing worktree-foo 2>/dev/null <<< "y")" "new" "resolve: 消す確認で y なら new"
assert_eq "$(branch_state worktree-foo)" "none" "resolve: y ならブランチを消す"
wt_create foo >/dev/null 2>&1
_err="$(_PICKED="このブランチで作る" resolve_existing worktree-foo 2>&1 >/dev/null </dev/null)"; _rc=$?
assert_rc "$_rc" "1" "resolve: 別の worktree で使用中なら作らない"
assert_contains "$_err" "$(cd "$_ep" && pwd -P)" "resolve: 使用中の場所を見せる"
wt_remove_force foo >/dev/null 2>&1
unset -f _pick
cd "$_back" || exit 1
rm -rf "$_er"

echo ""
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
