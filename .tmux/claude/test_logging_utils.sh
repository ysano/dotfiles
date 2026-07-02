#!/bin/bash
# ファイル名: test_logging_utils.sh
# 説明: core/logging_utils.sh の set -u 互換性回帰テスト（ADR 0002）。
#       呼び出し側が `set -euo pipefail` でも、環境変数未設定でログ関数が abort しないこと。
#       回帰の実害: hooks の TTS サブシェルが log_debug で中断し、読み上げが無音になる。
# 用途: .tmux/ci.sh および手動 (bash test_logging_utils.sh) から実行。

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB="$SCRIPT_DIR/core/logging_utils.sh"

fails=0
assert_ok() { # $1 rc  $2 msg
    if [[ "$1" -eq 0 ]]; then echo "[PASS] $2"; else echo "[FAIL] $2 (rc=$1)"; fails=$((fails + 1)); fi
}

echo "=== set -u 互換性（TMUX_CLAUDE_VOICE_DEBUG 未設定）==="
# 呼び出し側と同じ set -euo pipefail 下で、デバッグ変数を未設定にして log_debug を呼ぶ。
# :- ガードが無いと unbound variable → abort（実 hooks の TTS 中断と同じ経路）。
(
    set -euo pipefail
    unset TMUX_CLAUDE_VOICE_DEBUG 2>/dev/null || true
    source "$LIB"
    log_debug "regression check"
) >/dev/null 2>&1
assert_ok "$?" "set -u 下 log_debug（TMUX_CLAUDE_VOICE_DEBUG 未設定）が abort しない"

echo ""
echo "=== 他ログ関数も set -u 下で動作 ==="
for fn in log_info log_warn log_error; do
    ( set -euo pipefail; source "$LIB"; "$fn" "regression check" ) >/dev/null 2>&1
    assert_ok "$?" "set -u 下 $fn が動作"
done

echo ""
echo "=== 結果: ${fails} 失敗 ==="
[[ "$fails" -eq 0 ]] && exit 0 || exit 1
