#!/bin/bash
# ファイル名: test_functions.sh
# 説明: functions.sh の純関数テスト。まずは読み上げラベル書式 format_pane_label を検証。
#       window 名 +（複数 pane の window のみ pane 番号）というルールを固定する。
# 用途: .tmux/ci.sh および手動 (bash test_functions.sh) から実行。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/core/logging_utils.sh" 2>/dev/null
source "$SCRIPT_DIR/functions.sh" 2>/dev/null

fails=0
assert_eq() { # $1 actual $2 expected $3 msg
    if [[ "$1" == "$2" ]]; then echo "[PASS] $3"; else echo "[FAIL] $3: got '$1' want '$2'"; fails=$((fails + 1)); fi
}

echo "=== format_pane_label (window名 + 複数pane時のみpane番号) ==="
assert_eq "$(format_pane_label 'ml-proposals' 1 1)" "ml-proposals"       "単一 pane は window 名のみ"
assert_eq "$(format_pane_label 'dotfiles' 2 2)"     "dotfiles ペイン2"   "複数 pane は pane 番号を付加"
assert_eq "$(format_pane_label 'x' 3 1)"            "x ペイン1"          "3 pane 中の pane1 も付加"
assert_eq "$(format_pane_label '' 1 1)"             ""                   "window 名が空ならラベルなし"

echo ""
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
