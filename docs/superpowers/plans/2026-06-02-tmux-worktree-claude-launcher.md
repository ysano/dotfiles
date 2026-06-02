# tmux × Claude worktree ランチャー Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** tmux の `prefix + w` で開く popup から、git worktree を外部配置で作成し、Claude Code を監視あり（対話 window）／監視なし（headless）で並列起動・切替・削除できるランチャーを実装する。

**Architecture:** コアスクリプト `worktree_launch.sh` に「副作用なしの純粋ヘルパ（名前検証・パス構築・base 解決・claude コマンド組立）」と「`WT_DRY_RUN` で出力に切替わる副作用関数（git/tmux 実行）」を分離。tmux キーバインドは新規 `claude-worktree.conf` から popup でコアを呼ぶ。worktree は gwt と同じ外部規約 `../worktrees/<repo>-<name>` に素の `git worktree` で作る（gwt 関数へのランタイム依存なし）。

**Tech Stack:** bash, tmux 3.2+（display-popup / set-window-option）, git worktree, fzf（任意・無ければ command-prompt にフォールバック）

参照 spec: `docs/superpowers/specs/2026-06-02-tmux-worktree-claude-launcher-design.md`

---

## File Structure

| ファイル | 責務 |
|---|---|
| Create: `.tmux/claude/worktree_launch.sh` | コア。純粋ヘルパ＋ DRY_RUN 対応副作用関数＋ popup ディスパッチャ。source 可能（実行ガード付き） |
| Create: `.tmux/claude/test_worktree_launch.sh` | 単体テスト。コアを source し純粋ヘルパと dry-run 出力を検証 |
| Create: `.tmux/claude-worktree.conf` | `prefix + w` → `display-popup` でコアを起動 |
| Modify: `.tmux.conf` | guarded `source-file ~/.tmux/claude-worktree.conf` を追記 |
| Modify: `.tmux/ci.sh` | worktree ランチャー単体テストを実行するブロックを追加 |

関数シグネチャ（全タスク共通・命名固定）:
- `validate_name <name>` → sanitize 済み名を stdout、無効/空なら rc 1
- `worktree_path <name>` → `<repo親>/worktrees/<repo名>-<name>` を stdout
- `branch_name <name>` → `worktree-<name>` を stdout
- `resolve_base [ref]` → 引数なし=`HEAD`、あり=その ref
- `build_claude_cmd <mode> <name> [task]` → 起動コマンド文字列を stdout（mode: `supervised`|`unsupervised`）
- `wt_create <name> [base]` / `wt_spawn <mode> <name> [task]` / `wt_remove <name>` → `WT_DRY_RUN` 非空でコマンド文字列を echo、空なら実行

---

## Task 1: コア骨格 + `validate_name`（TDD）

**Files:**
- Create: `.tmux/claude/worktree_launch.sh`
- Test: `.tmux/claude/test_worktree_launch.sh`

- [ ] **Step 1: 失敗するテストを書く**

`.tmux/claude/test_worktree_launch.sh` を作成:

```bash
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
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
```

`.tmux/claude/worktree_launch.sh` を骨格のみで作成（`validate_name` はまだ無い）:

```bash
#!/bin/bash
# ファイル名: worktree_launch.sh
# 説明: git worktree を外部配置で作成し Claude Code を tmux 上に並列起動する
#       ランチャー。prefix + w の display-popup から呼ばれる。
# 用途: 純粋ヘルパは source して単体テスト可能。実行時は `worktree_launch.sh popup`。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
```

- [ ] **Step 2: テストを実行して失敗を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: `validate_name` 各行が FAIL（`validate_name: command not found` 由来）、最終 exit 1

- [ ] **Step 3: 最小実装**

`.tmux/claude/worktree_launch.sh` の `SCRIPT_DIR=...` 行の下に追記:

```bash

# 名前を sanitize して stdout に出す。無効（結果が空）なら rc 1。
validate_name() {
    local raw="${1:-}" s
    s="$(printf '%s' "$raw" \
        | tr ' /' '--' \
        | sed -E 's/[^A-Za-z0-9._-]/-/g; s/-+/-/g; s/^-+//; s/-+$//')"
    [[ -n "$s" ]] || return 1
    printf '%s' "$s"
}
```

- [ ] **Step 4: テストを実行して PASS を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: `validate_name` 4 行すべて `[PASS]`、`0 失敗`、exit 0

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude/worktree_launch.sh .tmux/claude/test_worktree_launch.sh
git commit -m "feat(tmux-worktree): コア骨格と validate_name を追加"
```

---

## Task 2: `worktree_path` / `branch_name` / `resolve_base`（TDD）

**Files:**
- Modify: `.tmux/claude/worktree_launch.sh`
- Test: `.tmux/claude/test_worktree_launch.sh`

- [ ] **Step 1: 失敗するテストを書く**

`test_worktree_launch.sh` の `echo "=== 結果` 行の直前に追記:

```bash
echo ""
echo "=== worktree_path / branch_name / resolve_base ==="
_root="$(git rev-parse --show-toplevel)"
_expected="$(dirname "$_root")/worktrees/$(basename "$_root")-foo"
assert_eq "$(worktree_path foo)" "$_expected" "外部 worktrees 配置を返す"
assert_eq "$(branch_name foo)" "worktree-foo" "ブランチ名に worktree- を付与"
assert_eq "$(resolve_base)" "HEAD" "base 既定は HEAD"
assert_eq "$(resolve_base origin/master)" "origin/master" "base 上書き"
```

- [ ] **Step 2: テストを実行して失敗を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 新 4 行が FAIL（関数未定義）

- [ ] **Step 3: 最小実装**

`worktree_launch.sh` の `validate_name` 定義の下に追記:

```bash

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
```

- [ ] **Step 4: テストを実行して PASS を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 全行 `[PASS]`、exit 0

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude/worktree_launch.sh .tmux/claude/test_worktree_launch.sh
git commit -m "feat(tmux-worktree): worktree_path/branch_name/resolve_base を追加"
```

---

## Task 3: `build_claude_cmd`（TDD）

**Files:**
- Modify: `.tmux/claude/worktree_launch.sh`
- Test: `.tmux/claude/test_worktree_launch.sh`

- [ ] **Step 1: 失敗するテストを書く**

`test_worktree_launch.sh` の `echo "=== 結果` 行の直前に追記:

```bash
echo ""
echo "=== build_claude_cmd ==="
assert_eq "$(build_claude_cmd supervised login)" "claude -n login" "監視ありは -n 表示名"
_uns="$(build_claude_cmd unsupervised login 'fix tests')"
assert_contains "$_uns" "claude -p" "監視なしは -p"
assert_contains "$_uns" "fix tests" "監視なしは task を含む"
assert_contains "$_uns" "--allowedTools" "監視なしは allowedTools を厳選"
build_claude_cmd bogus x >/dev/null 2>&1; assert_rc "$?" "1" "未知 mode は rc 1"
```

- [ ] **Step 2: テストを実行して失敗を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 新行が FAIL（関数未定義）

- [ ] **Step 3: 最小実装**

`worktree_launch.sh` の `resolve_base` 定義の下に追記:

```bash

# 起動する claude コマンド文字列を組み立てる。
#   supervised   : 対話。表示名のみ。
#   unsupervised : headless。許可は読取+編集系に厳選（Bash は既定で渡さない）。
build_claude_cmd() {
    local mode="$1" name="$2" task="${3:-}"
    case "$mode" in
        supervised)
            printf 'claude -n %s' "$name"
            ;;
        unsupervised)
            printf 'claude -p %q --allowedTools %q' "$task" "Read Edit Write Grep Glob"
            ;;
        *)
            return 1
            ;;
    esac
}
```

- [ ] **Step 4: テストを実行して PASS を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 全行 `[PASS]`、exit 0

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude/worktree_launch.sh .tmux/claude/test_worktree_launch.sh
git commit -m "feat(tmux-worktree): build_claude_cmd を追加"
```

---

## Task 4: dry-run 副作用関数 `wt_create` / `wt_spawn` / `wt_remove`（TDD）

**Files:**
- Modify: `.tmux/claude/worktree_launch.sh`
- Test: `.tmux/claude/test_worktree_launch.sh`

- [ ] **Step 1: 失敗するテストを書く**

`test_worktree_launch.sh` の `echo "=== 結果` 行の直前に追記:

```bash
echo ""
echo "=== dry-run 副作用関数 (WT_DRY_RUN=1) ==="
export WT_DRY_RUN=1
_c="$(wt_create foo)"
assert_contains "$_c" "git worktree add -b worktree-foo" "create: ブランチ指定"
assert_contains "$_c" "/worktrees/$(basename "$(git rev-parse --show-toplevel)")-foo" "create: 外部配置"
assert_contains "$_c" " HEAD" "create: 既定 base は HEAD"
assert_contains "$(wt_create foo origin/master)" " origin/master" "create: base 上書き"
_s="$(wt_spawn supervised login)"
assert_contains "$_s" "new-window" "spawn: tmux new-window"
assert_contains "$_s" "@cc_worktree" "spawn: window option で同一性を記録"
assert_contains "$(wt_spawn unsupervised login 'fix tests')" "new-window -d" "spawn 監視なしは detached"
assert_contains "$(wt_remove foo)" "git worktree remove" "remove: worktree 削除"
assert_contains "$(wt_remove foo)" "git branch -D worktree-foo" "remove: ブランチ削除"
unset WT_DRY_RUN
```

- [ ] **Step 2: テストを実行して失敗を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: dry-run 行が FAIL（関数未定義）

- [ ] **Step 3: 最小実装**

`worktree_launch.sh` の `build_claude_cmd` 定義の下に追記:

```bash

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
```

- [ ] **Step 4: テストを実行して PASS を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 全行 `[PASS]`、exit 0

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude/worktree_launch.sh .tmux/claude/test_worktree_launch.sh
git commit -m "feat(tmux-worktree): dry-run 対応の create/spawn/remove を追加"
```

---

## Task 5: popup ディスパッチャ + 実行ガード（対話部・手動検証）

対話 fzf フローは単体テスト対象外。テスト済みプリミティブの上に組み、`bash -n` と手動で検証する。

**Files:**
- Modify: `.tmux/claude/worktree_launch.sh`

- [ ] **Step 1: popup フローと main を実装**

`worktree_launch.sh` の `wt_remove` 定義の下に追記:

```bash

# 行選択 UI。fzf があれば fzf、無ければ tmux command-prompt 相当の read にフォールバック。
_pick() {
    local prompt="$1"
    if command -v fzf >/dev/null 2>&1; then
        fzf --prompt="$prompt " --height=40% --reverse
    else
        # フォールバック: 1 行入力（先頭行を採用）
        printf '%s' "$prompt" >&2
        local line; IFS= read -r line; printf '%s' "$line"
    fi
}

# 既存 worktree 名の一覧（リポジトリ親の worktrees/<repo>- プレフィックスを剥がす）。
_list_worktrees() {
    local root repo
    root="$(git rev-parse --show-toplevel)" || return 1
    repo="$(basename "$root")"
    git worktree list --porcelain \
        | awk '/^worktree /{print $2}' \
        | sed -n "s#.*/worktrees/${repo}-##p"
}

# 既存 worktree に対応する window があれば前面化、無ければ claude -c で継続。
wt_open() {
    local name="$1" path win
    path="$(worktree_path "$name")"
    win="$(tmux list-windows -a -F '#{window_id} #{@cc_worktree}' 2>/dev/null \
        | awk -v p="$path" '$2==p{print $1; exit}')"
    if [[ -n "$win" ]]; then
        tmux select-window -t "$win"
    else
        tmux new-window -c "$path" -n "$name" "claude -c"
        tmux set-window-option @cc_worktree "$path"
    fi
}

# popup のメインフロー。
cmd_popup() {
    if ! git rev-parse --show-toplevel >/dev/null 2>&1; then
        echo "git リポジトリ内で実行してください"; sleep 1; return 1
    fi
    local choice
    choice="$( { echo "＋ 新規作成"; _list_worktrees; } | _pick "worktree>" )"
    [[ -n "$choice" ]] || return 0

    if [[ "$choice" == "＋ 新規作成" ]]; then
        printf 'ブランチ名: ' >&2; local raw; IFS= read -r raw
        local name; name="$(validate_name "$raw")" || { echo "名前が不正です"; sleep 1; return 1; }
        printf 'base ref [HEAD]: ' >&2; local base; IFS= read -r base
        wt_create "$name" "$base" || { echo "worktree 作成に失敗"; sleep 1; return 1; }
        local mode; mode="$(printf '%s\n' "監視あり (supervised)" "監視なし (unsupervised)" | _pick "mode>")"
        case "$mode" in
            *supervised*)   wt_spawn supervised "$name" ;;
            *unsupervised*) printf 'タスク: ' >&2; local task; IFS= read -r task
                            wt_spawn unsupervised "$name" "$task" ;;
        esac
    else
        local action; action="$(printf '%s\n' "開く / 前面化" "削除" | _pick "action>")"
        case "$action" in
            開く*) wt_open "$choice" ;;
            削除)  wt_remove "$choice" ;;
        esac
    fi
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
```

- [ ] **Step 2: bash 構文チェック**

Run: `bash -n .tmux/claude/worktree_launch.sh && echo OK`
Expected: `OK`

- [ ] **Step 3: source しても副作用が出ないこと（既存テストが通る）を確認**

Run: `bash .tmux/claude/test_worktree_launch.sh`
Expected: 全 `[PASS]`、exit 0（main は実行されない）

- [ ] **Step 4: 実行権限を付与**

Run: `chmod +x .tmux/claude/worktree_launch.sh && ls -l .tmux/claude/worktree_launch.sh`
Expected: `-rwxr-xr-x` 等、実行可能

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude/worktree_launch.sh
git commit -m "feat(tmux-worktree): popup ディスパッチャと実行ガードを追加"
```

---

## Task 6: キーバインド conf + `.tmux.conf` source + 衝突チェック

**Files:**
- Create: `.tmux/claude-worktree.conf`
- Modify: `.tmux.conf`（Claude Voice 統合ブロックの直後）

- [ ] **Step 1: キーバインド conf を作成**

`.tmux/claude-worktree.conf`:

```tmux
# =============================================================================
# Claude worktree ランチャー
# prefix + w で popup を開き、worktree の作成/切替/削除と claude 起動を行う。
# コア実装: ~/.tmux/claude/worktree_launch.sh
# =============================================================================
bind-key w display-popup -E -w 80% -h 60% "~/.tmux/claude/worktree_launch.sh popup"
```

- [ ] **Step 2: `.tmux.conf` に guarded source を追記**

`.tmux.conf` の以下のブロック:

```tmux
# === Claude Voice統合（条件付き） ===
if-shell '[ "${CLAUDE_VOICE_ENABLED:-true}" = "true" ] && [ -f ~/.tmux/claude.conf ]' \
  'source-file ~/.tmux/claude.conf'
```

の直後に追記:

```tmux

# === Claude worktree ランチャー ===
if-shell '[ -f ~/.tmux/claude-worktree.conf ]' \
  'source-file ~/.tmux/claude-worktree.conf'
```

- [ ] **Step 3: キー衝突チェック**

Run: `bash .claude/skills/tmux-config/scripts/check_conflicts.sh`
Expected: `prefix + w` 関連の `[FAIL]` が無いこと（既存バインドと重複しない）

- [ ] **Step 4: tmux 設定の読み込み確認（tmux 内のみ）**

Run: `tmux source-file ~/.tmux.conf && tmux list-keys | grep -E "bind.* w " | grep -i popup`
Expected: `prefix + w` が `display-popup ... worktree_launch.sh popup` にバインドされている1行

> 注: symlink デプロイ前にローカルで試す場合は `tmux source-file ./.tmux/claude-worktree.conf` でも可。

- [ ] **Step 5: コミット**

```bash
git add .tmux/claude-worktree.conf .tmux.conf
git commit -m "feat(tmux-worktree): prefix + w でランチャー popup を起動"
```

---

## Task 7: CI にランチャーテストを統合

**Files:**
- Modify: `.tmux/ci.sh`（`# 4. Run conflict check if available` ブロックの直前）

- [ ] **Step 1: ci.sh にテスト実行ブロックを追加**

`.tmux/ci.sh` の以下の行:

```bash
# 4. Run conflict check if available
```

の直前に追記:

```bash
# 3.5 Worktree launcher unit tests
TEST_SCRIPT="$SCRIPT_DIR/claude/test_worktree_launch.sh"
if [ -f "$TEST_SCRIPT" ]; then
  echo "--- Worktree Launcher Tests ---"
  if bash "$TEST_SCRIPT"; then
    echo "[PASS] worktree launcher tests"
  else
    echo "[FAIL] worktree launcher tests"
    errors=$((errors + 1))
  fi
  echo ""
fi

```

- [ ] **Step 2: CI 全体を実行して PASS を確認**

Run: `bash .tmux/ci.sh`
Expected: `--- Worktree Launcher Tests ---` 配下が全 `[PASS]`、末尾 `=== CI Complete (errors: 0) ===`、exit 0

- [ ] **Step 3: コミット**

```bash
git add .tmux/ci.sh
git commit -m "test(tmux-worktree): CI に worktree ランチャーの単体テストを統合"
```

---

## Task 8: 実 worktree でのスモークテスト（即クリーンアップ）

dry-run ではなく実際に worktree を1つ作って起動・削除まで通し、副作用が残らないことを確認する。

**Files:** なし（手動検証のみ）

- [ ] **Step 1: 実作成（dry-run 無効）して配置を確認**

```bash
cd /Users/yoshiaki_sano/dotfiles
source .tmux/claude/worktree_launch.sh
wt_create smoke
git worktree list | grep smoke
```
Expected: `<repo親>/worktrees/<repo>-smoke` が一覧に出る（リポジトリ内 `.claude/worktrees/` ではない＝汚染なし）

- [ ] **Step 2: main リポジトリが汚染されていないことを確認**

Run: `git status --short`
Expected: `worktrees/` 由来の `??` が出ない（外部配置のため）

- [ ] **Step 3: 削除して残骸ゼロを確認**

```bash
wt_remove smoke
git worktree list
git branch | grep smoke || echo "ブランチ残骸なし"
git status --short || echo clean
```
Expected: smoke worktree もブランチも消え、`git status` clean

- [ ] **Step 4: コミットなし（検証のみ）**

副作用が残っていないことを確認したら完了。コミット対象の変更は無い。

---

## Self-Review メモ

- **spec カバレッジ**: D1(resolve_base/Task2,4) D2(worktree_path 外部配置/Task2,8) D3(wt_spawn supervised=現セッション new-window/Task4) D4(build_claude_cmd unsupervised + -d/Task3,4) D5(@cc_worktree/Task4,5) D6(claude-worktree.conf + guarded source/Task6) D7(wt_remove/Task4,5) をそれぞれタスクで実装。
- **エラー処理/グレースフル劣化**: git 外チェック(Task5 cmd_popup)、fzf 不在フォールバック(_pick/Task5)、不正名 rc(Task1,5)。
- **環境非依存**: テストは `git rev-parse` から期待値を導出し絶対パスをハードコードしない（Task2,4）。CI 動作は Task7 で担保。
- **未テスト部**: 対話 popup フロー（fzf/read）は性質上単体テスト外 → bash -n + 手動(Task5,6,8)で検証。
