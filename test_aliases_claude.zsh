#!/usr/bin/env zsh
# Claude Code Bash ツールでの標準コマンド維持テスト
# aliases.zsh が CLAUDECODE 環境下で「標準コマンドを上書きする alias」を
# 定義しない（＝スナップショット経由でツール実行を壊さない）ことを検証する。
# Usage: ./test_aliases_claude.zsh

SCRIPT_DIR="${0:a:h}"
PASS=0
FAIL=0

# 指定の CLAUDECODE 状態で aliases.zsh を単体 source し、alias 定義の有無を返す。
# $1: set|unset  $2: alias名   -> alias が定義されていれば exit 0
alias_defined() {
    local mode="$1" name="$2"
    local body="source '$SCRIPT_DIR/.zsh/utils.zsh' 2>/dev/null; source '$SCRIPT_DIR/.zsh/aliases.zsh' 2>/dev/null; alias '$name' >/dev/null 2>&1"
    if [[ "$mode" == "set" ]]; then
        CLAUDECODE=1 zsh -c "$body"
    else
        env -u CLAUDECODE zsh -c "$body"
    fi
}

# CLAUDECODE=1 のとき標準コマンドである（alias 未定義）こと
assert_standard_under_claude() {
    local name="$1"
    if ! alias_defined set "$name"; then
        echo "✅ CLAUDECODE=1: '$name' は標準コマンド（alias 未定義）"; PASS=$((PASS + 1))
    else
        echo "❌ CLAUDECODE=1: '$name' が alias 化されている（ツール実行を壊す）"; FAIL=$((FAIL + 1))
    fi
}

# 通常シェル（CLAUDECODE 未設定）では alias が温存されること
assert_aliased_when_interactive() {
    local name="$1"
    if alias_defined unset "$name"; then
        echo "✅ 通常シェル: '$name' alias 温存"; PASS=$((PASS + 1))
    else
        echo "❌ 通常シェル: '$name' alias が失われた（UX 劣化）"; FAIL=$((FAIL + 1))
    fi
}

# CLAUDECODE=1 でも無害な alias は温存されること（過剰除去の回帰防止）
assert_preserved_under_claude() {
    local name="$1"
    if alias_defined set "$name"; then
        echo "✅ CLAUDECODE=1: '$name' は温存（過剰除去なし）"; PASS=$((PASS + 1))
    else
        echo "❌ CLAUDECODE=1: '$name' まで除去された"; FAIL=$((FAIL + 1))
    fi
}

echo "=== Claude Code 標準コマンド維持テスト ==="
echo ""

echo "--- CLAUDECODE=1: 標準コマンドが維持される（破壊的・出力整形 alias を除去） ---"
for c in rm cp mv mkdir ls ll la lt lta cat du df top ps help; do
    assert_standard_under_claude "$c"
done

echo ""
echo "--- 通常シェル: 対話 UX 用 alias は温存される ---"
for c in rm cp mv mkdir; do
    assert_aliased_when_interactive "$c"
done

echo ""
echo "--- CLAUDECODE=1: 無害な alias は温存（過剰除去の回帰防止） ---"
# git ショートカット（git 存在時に定義される）は Claude 実行時も残す
assert_preserved_under_claude gs

echo ""
echo "=== 結果: PASS=$PASS FAIL=$FAIL ==="
[[ $FAIL -eq 0 ]]
