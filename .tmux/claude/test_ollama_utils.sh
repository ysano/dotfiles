#!/bin/bash
# ファイル名: test_ollama_utils.sh
# 説明: sanitize_summary_for_speech（要約の読み上げ向け整形）の単体テスト。
#       ollama 非依存の純関数。記号除去・改行畳み込み・一文化（ぶつ切り防止）を検証。
# 用途: .tmux/ci.sh および手動 (bash test_ollama_utils.sh) から実行。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/core/logging_utils.sh" 2>/dev/null
source "$SCRIPT_DIR/ollama_utils.sh" 2>/dev/null

fails=0
assert_eq() { # $1 actual $2 expected $3 msg
    if [[ "$1" == "$2" ]]; then echo "[PASS] $3"; else echo "[FAIL] $3: got '$1' want '$2'"; fails=$((fails + 1)); fi
}

echo "=== sanitize_summary_for_speech ==="
assert_eq "$(sanitize_summary_for_speech 'ログ処理の修正完了。次のステップを尋ねています。')" \
          "ログ処理の修正完了。" "最初の一文だけ採用（ぶつ切りしない）"
assert_eq "$(sanitize_summary_for_speech '**修正**完了')" \
          "修正完了" "Markdown 強調記号を除去"
assert_eq "$(sanitize_summary_for_speech '`code` と # 見出し')" \
          "code と 見出し" "コード/見出し記号を除去し空白を圧縮"
assert_eq "$(sanitize_summary_for_speech $'複数\n行\nの要約')" \
          "複数 行 の要約" "改行を空白に畳み込む"
assert_eq "$(sanitize_summary_for_speech '記号なし一文です')" \
          "記号なし一文です" "句点なしはそのまま返す"

echo ""
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
