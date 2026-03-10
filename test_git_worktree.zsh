#!/usr/bin/env zsh
# Git Worktree (gwt) テスト
# Usage: ./test_git_worktree.zsh

SCRIPT_DIR="${0:a:h}"
TEST_DIR=$(mktemp -d)
trap "rm -rf '$TEST_DIR'" EXIT

PASS=0
FAIL=0

assert_ok() {
    local label="$1"; shift
    if "$@" >/dev/null 2>&1; then
        echo "✅ $label"; PASS=$((PASS + 1))
    else
        echo "❌ $label (exit: $?)"; FAIL=$((FAIL + 1))
    fi
}

assert_fail() {
    local label="$1"; shift
    if ! "$@" >/dev/null 2>&1; then
        echo "✅ $label"; PASS=$((PASS + 1))
    else
        echo "❌ $label (expected failure)"; FAIL=$((FAIL + 1))
    fi
}

assert_contains() {
    local label="$1" expected="$2" actual="$3"
    if [[ "$actual" == *"$expected"* ]]; then
        echo "✅ $label"; PASS=$((PASS + 1))
    else
        echo "❌ $label"; echo "   expected to contain: $expected"; echo "   actual: $actual"; FAIL=$((FAIL + 1))
    fi
}

echo "=== Git Worktree (gwt) テスト ==="
echo ""

# テスト用gitリポジトリ作成
cd "$TEST_DIR"
git init test-repo >/dev/null 2>&1
cd test-repo
git commit --allow-empty -m "initial commit" >/dev/null 2>&1

# gwt読み込み
source "$SCRIPT_DIR/.zsh/git-worktree.zsh"

echo "--- 関数存在チェック ---"
assert_ok "gwt関数" type gwt
assert_ok "_gwt_create関数" type _gwt_create
assert_ok "_gwt_list関数" type _gwt_list
assert_ok "_gwt_switch関数" type _gwt_switch
assert_ok "_gwt_remove関数" type _gwt_remove
assert_ok "_gwt_clean関数" type _gwt_clean
assert_ok "_gwt_help関数" type _gwt_help

echo ""
echo "--- gwt help ---"
output=$(gwt help 2>&1)
assert_contains "helpにcreateが含まれる" "create" "$output"
assert_contains "helpにエイリアスが含まれる" "gwsw" "$output"

echo ""
echo "--- gwt create ---"
assert_fail "ブランチ名なしで失敗" gwt create
assert_ok "worktree作成" gwt create test-branch

# worktreeディレクトリの存在確認
wt_dir="$TEST_DIR/worktrees/test-repo-test-branch"
if [[ -d "$wt_dir" ]]; then
    echo "✅ worktreeディレクトリが存在: $(basename $wt_dir)"; PASS=$((PASS + 1))
else
    echo "❌ worktreeディレクトリが存在しない: $wt_dir"; FAIL=$((FAIL + 1))
fi

# スラッシュ付きブランチ名
assert_ok "スラッシュ付きブランチ作成" gwt create feature/slash-test
wt_dir2="$TEST_DIR/worktrees/test-repo-feature-slash-test"
if [[ -d "$wt_dir2" ]]; then
    echo "✅ スラッシュがダッシュに変換される"; PASS=$((PASS + 1))
else
    echo "❌ スラッシュ付きworktreeが見つからない: $wt_dir2"; FAIL=$((FAIL + 1))
fi

echo ""
echo "--- gwt list ---"
output=$(gwt list 2>&1)
assert_ok "listが成功" gwt list
assert_contains "listにtest-branchが含まれる" "test-branch" "$output"

echo ""
echo "--- gwt remove ---"
assert_fail "名前なしで失敗" gwt remove
assert_ok "worktree削除" gwt remove test-branch

if [[ ! -d "$wt_dir" ]]; then
    echo "✅ worktreeディレクトリが削除された"; PASS=$((PASS + 1))
else
    echo "❌ worktreeディレクトリが残っている"; FAIL=$((FAIL + 1))
fi

# -d オプション: ブランチも削除
assert_ok "ブランチ付き削除" gwt remove -d feature/slash-test
if ! git show-ref --verify --quiet "refs/heads/feature/slash-test" 2>/dev/null; then
    echo "✅ ブランチも削除された"; PASS=$((PASS + 1))
else
    echo "❌ ブランチが残っている"; FAIL=$((FAIL + 1))
fi

echo ""
echo "--- gwt clean ---"
assert_ok "cleanが成功" gwt clean

echo ""
echo "--- リポジトリ外での実行 ---"
cd "$TEST_DIR"
assert_fail "リポジトリ外でcreateが失敗" gwt create outside-branch
assert_fail "リポジトリ外でlistが失敗" gwt list

echo ""
echo "=== 結果: ✅ $PASS passed, ❌ $FAIL failed ==="
[[ $FAIL -eq 0 ]] && exit 0 || exit 1
