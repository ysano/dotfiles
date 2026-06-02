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
_root="$(git rev-parse --show-toplevel)"
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
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
