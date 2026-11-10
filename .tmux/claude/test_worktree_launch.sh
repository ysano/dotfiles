#!/bin/bash
# ファイル名: test_worktree_launch.sh
# 説明: worktree_launch.sh の純粋ヘルパと dry-run 出力を検証する単体テスト。
# 用途: .tmux/ci.sh および手動 (bash test_worktree_launch.sh) から実行。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=worktree_launch.sh
source "$SCRIPT_DIR/worktree_launch.sh"
# 稼働中の tmux（karin 等のプロファイル）に結果が左右されないよう、既定は「決まらない」。
# 実物は「claude のプロファイル」の節で確かめる。
claude_profile_env() { :; }

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
echo "=== claude のプロファイル (CLAUDE_CONFIG_DIR) ==="
_profile_tmp="$(mktemp -d)"
_profile_dir="$_profile_tmp/profile karin"
claude_profile_env() { printf '%s' "$_profile_dir"; }
export WT_DRY_RUN=1
_ps="$(wt_spawn supervised login)"
assert_contains "$_ps" "split-window -e $(printf '%q' "CLAUDE_CONFIG_DIR=$_profile_dir")" "spawn 監視あり: 同じリポジトリのプロファイルを pane の環境に渡す"
_pu="$(wt_spawn unsupervised login 'fix tests')"
assert_contains "$_pu" "new-window -d -e $(printf '%q' "CLAUDE_CONFIG_DIR=$_profile_dir")" "spawn 監視なし: 同じリポジトリのプロファイルを渡す"
unset WT_DRY_RUN
_profile_log="$_profile_tmp/tmux.log"
cat > "$_profile_tmp/tmux" <<'EOF'
#!/bin/sh
printf '%s\n' "$@" >> "$WT_OPEN_LOG"
if [ "$1" = "split-window" ]; then printf '%%99\n'; fi
EOF
chmod +x "$_profile_tmp/tmux"
PATH="$_profile_tmp:$PATH" WT_OPEN_LOG="$_profile_log" TMUX_WORKTREE_ORIGIN=%42 wt_open login
assert_contains "$(tr '\n' '|' < "$_profile_log")" "|-e|CLAUDE_CONFIG_DIR=$_profile_dir|" "open: 同じリポジトリのプロファイルで claude -c を起動"
claude_profile_env() { :; }
export WT_DRY_RUN=1
assert_not_contains "$(wt_spawn supervised login)" "CLAUDE_CONFIG_DIR" "spawn: プロファイルが決まらなければ素の claude"
unset WT_DRY_RUN
: > "$_profile_log"
PATH="$_profile_tmp:$PATH" WT_OPEN_LOG="$_profile_log" TMUX_WORKTREE_ORIGIN=%42 wt_open login
assert_not_contains "$(tr '\n' '|' < "$_profile_log")" "CLAUDE_CONFIG_DIR" "open: プロファイルが決まらなければ素の claude"
# 実物: worktrees.py に worktree と起動元の pane を渡し、その出力を使う
unset -f claude_profile_env
eval "$(sed -n '/^claude_profile_env() {/,/^}/p' "$SCRIPT_DIR/worktree_launch.sh")"
cat > "$_profile_tmp/python3" <<'EOF'
#!/bin/sh
printf '%s|' "$@" > "$WT_PY_LOG"
printf '/p/karin'
EOF
chmod +x "$_profile_tmp/python3"
_py_out="$(PATH="$_profile_tmp:$PATH" WT_PY_LOG="$_profile_tmp/py.log" TMUX_WORKTREE_ORIGIN=%42 claude_profile_env "/w/a b")"
assert_eq "$_py_out" "/p/karin" "profile: worktrees.py の出力をそのまま使う"
assert_contains "$(cat "$_profile_tmp/py.log")" "worktrees.py|claude-config-dir|/w/a b|%42|" "profile: worktree と起動元の pane を渡す"
printf '#!/bin/sh\nexit 1\n' > "$_profile_tmp/python3"
_py_out="$(PATH="$_profile_tmp:$PATH" claude_profile_env "/w/a b")"; _py_rc=$?
assert_eq "$_py_out|$_py_rc" "|0" "profile: worktrees.py が失敗しても素の claude で続ける"
claude_profile_env() { :; }
rm -rf "$_profile_tmp"

echo ""
echo "=== classify_pick (fzf --print-query の解釈) ==="
assert_eq "$(classify_pick 0 "$(printf 'tf\nlogin')")" "$(printf 'select\tlogin')" "rc0+選択行: 既存を select"
assert_eq "$(classify_pick 1 "$(printf 'tf')")" "$(printf 'new\ttf')" "rc1+query: 新規名として new"
assert_eq "$(classify_pick 1 "$(printf '')")" "abort" "rc1+空 query: abort"
assert_eq "$(classify_pick 130 "$(printf 'tf')")" "abort" "rc130(ESC): abort"
assert_eq "$(classify_pick 0 "$(printf 'tf')")" "abort" "rc0+選択行なし: abort"

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
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
