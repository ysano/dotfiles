#!/bin/bash
# ファイル名: test_emoji_id.sh
# 説明: bin/emoji-id の決定論性・判別性・異常系を検証する単体テスト。
# 用途: 手動 (bash test_emoji_id.sh) および CI から実行。
#       emoji-id は source 可能（BASH_SOURCE ガード）なので関数を直接呼ぶ。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=bin/emoji-id
source "$SCRIPT_DIR/bin/emoji-id"

fails=0
assert_eq() { # $1 actual $2 expected $3 msg
    if [[ "$1" == "$2" ]]; then echo "[PASS] $3"; else echo "[FAIL] $3: got '$1' want '$2'"; fails=$((fails + 1)); fi
}
assert_ne() { # $1 a $2 b $3 msg
    if [[ "$1" != "$2" ]]; then echo "[PASS] $3"; else echo "[FAIL] $3: 両方 '$1'"; fails=$((fails + 1)); fi
}
assert_rc() { # $1 actual_rc $2 expected_rc $3 msg
    if [[ "$1" == "$2" ]]; then echo "[PASS] $3"; else echo "[FAIL] $3: rc=$1 want=$2"; fails=$((fails + 1)); fi
}

echo "=== パレット ==="
assert_eq "${#PALETTE[@]}" "64" "パレットはちょうど64要素"

echo ""
echo "=== 決定論 ==="
_a="$(emoji_id 1234)"; _b="$(emoji_id 1234)"
assert_eq "$_a" "$_b" "同一入力は同一出力"
_ra="$(emoji_id dotfiles 1234)"; _rb="$(emoji_id dotfiles 1234)"
assert_eq "$_ra" "$_rb" "repo+番号も同一入力は同一出力"

echo ""
echo "=== 近接番号の判別（雪崩効果）==="
_e1="$(emoji_id 1234)"; _e2="$(emoji_id 1235)"; _e3="$(emoji_id 1236)"
assert_ne "$_e1" "$_e2" "1234 ≠ 1235"
assert_ne "$_e2" "$_e3" "1235 ≠ 1236"
assert_ne "$_e1" "$_e3" "1234 ≠ 1236"

echo ""
echo "=== リポジトリ跨ぎの区別 ==="
assert_ne "$(emoji_id a 1)" "$(emoji_id b 1)" "a#1 ≠ b#1"

echo ""
echo "=== フォールバック（番号のみ）==="
_f="$(emoji_id 1)"
[[ -n "$_f" ]]; assert_rc "$?" "0" "番号のみで出力あり"

echo ""
echo "=== 既知値（リグレッション・実装後に確定）==="
assert_eq "$(emoji_id 1234)" "🦁🔑" "既知値: emoji_id 1234"
assert_eq "$(emoji_id dotfiles 1234)" "🐢🧭" "既知値: emoji_id dotfiles 1234"

echo ""
echo "=== 異常系 ==="
emoji_id >/dev/null 2>&1; assert_rc "$?" "1" "引数なしは rc1"
emoji_id a b c >/dev/null 2>&1; assert_rc "$?" "1" "引数過多は rc1"

echo ""
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
