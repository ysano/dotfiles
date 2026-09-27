#!/bin/bash
# .claude/statusline-command.sh の利用枠（5h/7d）表示・色ロジックの検証
# 使い方: bash test_statusline.sh   （jq が必要）
set -u
SCRIPT="$(cd "$(dirname "$0")" && pwd)/.claude/statusline-command.sh"
# アカウント表示・キャッシュを実環境から切り離す
CLAUDE_CONFIG_DIR=$(mktemp -d)
export CLAUDE_CONFIG_DIR
trap 'rm -rf "$CLAUDE_CONFIG_DIR"' EXIT

now=$(date +%s)
fail=0

# 色コードを [G]/[Y]/[R] に置換し、その他のエスケープと先頭のモデル名を除去
normalize() {
    sed -e 's/\x1b\[38;5;76m/[G]/g' -e 's/\x1b\[38;5;220m/[Y]/g' -e 's/\x1b\[38;5;196m/[R]/g' \
        -e 's/\x1b\[[0-9;]*m//g' -e 's/^O │ //'
}

# check <名前> <five_hour|seven_day> <used_percentage> <リセットまでの秒数|空> <期待値>
check() {
    local resets="" out
    [ -n "$4" ] && resets=",\"resets_at\":$((now + $4))"
    out=$(echo "{\"model\":{\"display_name\":\"O\"},\"rate_limits\":{\"$2\":{\"used_percentage\":$3$resets}}}" \
        | bash "$SCRIPT" 2>&1 | tail -1 | normalize)
    if [ "$out" = "$5" ]; then
        echo "ok   $1: $out"
    else
        echo "FAIL $1: got '$out' want '$5'"
        fail=1
    fi
}

check "7d ペース内"                   seven_day 50 172800 "[G]48h/7d:50%"
check "7d 残り時間の前半で枯渇"       seven_day 60 432000 "[R]5d/7d:40% ⚠32h"
check "7d リセット直前・ペース内"     seven_day 85 43200  "[G]12h/7d:15%"
check "7d 序盤でも経過 5% 以上は判定" seven_day 30 561600 "[R]7d/7d:70% ⚠28h"
check "7d 序盤は残り% 閾値"           seven_day 60 583200 "[Y]7d/7d:40%"
check "5h リセット直前・ペース内"     five_hour 90 1800   "[G]30m/5h:10%"
check "5h 序盤は残り% 閾値"           five_hour 60 15000  "[Y]5h/5h:40%"
check "5h ペース内"                   five_hour 45 9000   "[G]3h/5h:55%"
check "5h 残り時間の後半で枯渇"       five_hour 55 9000   "[Y]3h/5h:45% ⚠2h"
check "残り 5% 未満はペース内でも赤"  five_hour 96 60     "[R]1m/5h:4%"
check "残り 5% 未満でも枯渇警告"      five_hour 97 1000   "[R]17m/5h:3% ⚠8m"
check "100% 使用"                     five_hour 100 3600  "[R]60m/5h:0%"
check "0% 使用"                       seven_day 0 302400  "[G]4d/7d:100%"
check "resets_at 無しは残り% 閾値"    seven_day 60 ""     "[Y]7d:40%"

# rate_limits 無しでは利用枠を表示しない
out=$(echo '{"model":{"display_name":"O"}}' | bash "$SCRIPT" 2>&1 | tail -1 | normalize)
if [ "$out" = "O" ]; then echo "ok   rate_limits 無し: $out"; else echo "FAIL rate_limits 無し: got '$out'"; fail=1; fi

exit $fail
