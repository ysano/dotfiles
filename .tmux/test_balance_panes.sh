#!/bin/bash
# scripts/balance-panes.sh（pane の大きさを分割ごとに均等にそろえる）と balance.conf の検証
# 使い方: bash test_balance_panes.sh
set -u
DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$DIR/scripts/balance-panes.sh"
work=$(mktemp -d) && [ -d "$work" ] || { echo "FAIL mktemp -d"; exit 1; }
trap 'tmux -L "balance-test-$$" kill-server 2>/dev/null; rm -rf "$work"' EXIT
fail=0

# check <名前> <期待値> <コマンド...>
check() {
    local name="$1" want="$2" out
    shift 2
    out=$("$@" 2>/dev/null)
    if [ "$out" = "$want" ]; then
        echo "ok   $name"
    else
        echo "FAIL $name: got '$out' want '$want'"
        fail=1
    fi
}

# --- 偽の tmux で振り分けを確かめる（JSON でない古い layout は -E、拡大表示中は何もしない） ---
stub="$work/stub"; mkdir -p "$stub"
cat > "$stub/tmux" <<'STUB'
#!/bin/sh
case "$*" in
    *window_zoomed_flag*) echo "${STUB_zoomed:-0}" ;;
    *window_layout*) echo "abcd,284x51,0,0{94x51,0,0,0,94x51,95,0,1}" ;;
    *) echo "CALL: $*" ;;
esac
STUB
chmod +x "$stub/tmux"
check "古い layout は select-layout -E" "CALL: select-layout -t @1 -E" env PATH="$stub:$PATH" sh "$SCRIPT" @1
check "拡大表示中は何もしない"          ""                             env PATH="$stub:$PATH" STUB_zoomed=1 sh "$SCRIPT" @1

if ! command -v jq >/dev/null 2>&1; then
    echo "SKIP jq が無いため layout の検証を省略"
    exit 0
fi

# 「pane 番号:幅x高さ@左,上」を並べて比べる
cells() { jq -r '[.L | .. | objects | select(.t == "p") | "\(.i):\(.w)x\(.h)@\(.x),\(.y)"] | join(" ")'; }
balanced() { printf '%s' "$1" | sh "$SCRIPT" layout | cells; }

# --- layout: 標準入力の window_layout（JSON）を分割ごとに均等にした JSON を出す ---
flat='{"V":2,"L":{"t":"h","w":284,"h":51,"x":0,"y":0,"c":[{"t":"p","w":143,"h":51,"x":0,"y":0,"a":true,"i":0,"I":"%1"},{"t":"p","w":34,"h":51,"x":144,"y":0,"l":1,"i":1,"I":"%2"},{"t":"p","w":105,"h":51,"x":179,"y":0,"l":0,"i":2,"I":"%3"}]}}'
check "横 3 列を均等に"          "0:94x51@0,0 1:94x51@95,0 2:94x51@190,0" balanced "$flat"
rem='{"V":2,"L":{"t":"h","w":285,"h":51,"x":0,"y":0,"c":[{"t":"p","w":10,"h":51,"x":0,"y":0,"i":0,"I":"%1"},{"t":"p","w":10,"h":51,"x":11,"y":0,"i":1,"I":"%2"},{"t":"p","w":263,"h":51,"x":22,"y":0,"i":2,"I":"%3"}]}}'
check "余りは最後の pane に"     "0:94x51@0,0 1:94x51@95,0 2:95x51@190,0" balanced "$rem"
nested='{"V":2,"L":{"t":"h","w":284,"h":51,"x":0,"y":0,"c":[{"t":"p","w":200,"h":51,"x":0,"y":0,"i":0,"I":"%0"},{"t":"v","w":83,"h":51,"x":201,"y":0,"c":[{"t":"p","w":83,"h":10,"x":201,"y":0,"i":1,"I":"%1"},{"t":"p","w":83,"h":40,"x":201,"y":11,"a":true,"i":2,"I":"%2"}]}]}}'
check "入れ子も分割ごとに均等"   "0:141x51@0,0 1:142x25@142,0 2:142x25@142,26" balanced "$nested"
check "pane 以外のキーを保つ"    "true 2 %2 2" sh -c 'printf "%s" "$1" | sh "$2" layout | jq -r "[.L.c[1].c[1].a, .L.c[1].c[1].i, .L.c[1].c[1].I, .V] | map(tostring) | join(\" \")"' _ "$nested" "$SCRIPT"
# 子孫の分割に必要な最小の大きさに足りないなら、はみ出す layout を作らず失敗する（変更しない）
tight='{"V":2,"L":{"t":"h","w":9,"h":10,"x":0,"y":0,"c":[{"t":"p","w":1,"h":10,"x":0,"y":0,"i":0,"I":"%0"},{"t":"v","w":7,"h":10,"x":2,"y":0,"c":[{"t":"h","w":7,"h":4,"x":2,"y":0,"c":[{"t":"p","w":1,"h":4,"x":2,"y":0,"i":1,"I":"%1"},{"t":"p","w":1,"h":4,"x":4,"y":0,"i":2,"I":"%2"},{"t":"p","w":3,"h":4,"x":6,"y":0,"i":3,"I":"%3"}]},{"t":"p","w":7,"h":5,"x":2,"y":5,"i":4,"I":"%4"}]}]}}'
check "最小の大きさに足りなければ変えない" "" sh -c 'printf "%s" "$1" | sh "$2" layout' _ "$tight" "$SCRIPT"
single='{"V":2,"L":{"t":"p","w":284,"h":51,"x":0,"y":0,"a":true,"i":0,"I":"%0"}}'
check "pane 1 つはそのまま"      "0:284x51@0,0" balanced "$single"

# floating pane（tmux 3.8。z を持つ）は分割の大きさに数えず、位置・大きさも変えない
float='{"V":2,"L":{"t":"h","w":284,"h":51,"x":0,"y":0,"c":[{"t":"p","w":200,"h":51,"x":0,"y":0,"a":true,"i":0,"I":"%0"},{"t":"p","w":142,"h":12,"x":4,"y":2,"i":2,"z":0,"I":"%2"},{"t":"p","w":83,"h":51,"x":201,"y":0,"i":1,"I":"%1"}]}}'
check "floating pane は数えず動かさない" "0:141x51@0,0 2:142x12@4,2 1:142x51@142,0" balanced "$float"
# --- grid-layout <最小の幅>: 標準入力の window_layout（JSON）を、最小の幅を保てる列数の碁盤目にする ---
gridded() { printf '%s' "$2" | sh "$SCRIPT" grid-layout "$1" | cells; }
four='{"V":2,"L":{"t":"h","w":284,"h":51,"x":0,"y":0,"c":[{"t":"p","w":70,"h":51,"x":0,"y":0,"i":0,"I":"%1"},{"t":"p","w":70,"h":51,"x":71,"y":0,"i":1,"I":"%2"},{"t":"p","w":70,"h":51,"x":142,"y":0,"a":true,"i":2,"I":"%3"},{"t":"p","w":71,"h":51,"x":213,"y":0,"i":3,"I":"%4"}]}}'
check "grid: 4 枚・幅 90 → 3 列（1 列目が 2 段）" "0:94x25@0,0 1:94x25@0,26 2:94x51@95,0 3:94x51@190,0" gridded 90 "$four"
check "grid: 幅 140 → 2 列"      "0:141x25@0,0 1:141x25@0,26 2:142x25@142,0 3:142x25@142,26" gridded 140 "$four"
check "grid: 入れ子も並べ直す"   "0:94x51@0,0 1:94x51@95,0 2:94x51@190,0" gridded 90 "$nested"
check "grid: 幅が足りなければ 1 列" "0:284x12@0,0 1:284x12@0,13 2:284x12@0,26 3:284x12@0,39" gridded 300 "$four"
check "grid: floating pane は数えず動かさない" "0:141x51@0,0 1:142x51@142,0 2:142x12@4,2" gridded 90 "$float"
check "grid: pane 1 つはそのまま" "0:284x51@0,0" gridded 90 "$single"
check "grid: pane 以外のキーを保つ" "true 2 %3" sh -c 'printf "%s" "$1" | sh "$2" grid-layout 90 | jq -r "[.L.c[1].a, .L.c[1].i, .L.c[1].I] | map(tostring) | join(\" \")"' _ "$four" "$SCRIPT"
# 古い tmux 向けの文字列形式（先頭は tmux と同じ 16 ビットのチェックサム）
check "grid: 古い形式に変換"     "9e70,284x51,0,0{94x51,0,0[94x25,0,0,1,94x25,0,26,2],94x51,95,0,3,94x51,190,0,4}" sh -c 'printf "%s" "$1" | sh "$2" grid-layout 90 | sh "$2" legacy' _ "$four" "$SCRIPT"

# --- 実際の tmux（3.8 以降の JSON 形式）で、分割・終了のたびに均等になること ---
if ! command -v tmux >/dev/null 2>&1; then
    echo "SKIP tmux が無いため実機の検証を省略"
    [ "$fail" -eq 0 ] && exit 0 || exit 1
fi
# ~/.tmux/scripts/... を参照する設定をそのまま読めるよう、仮の HOME から作業ツリーを指す
mkdir -p "$work/home"
ln -s "$DIR" "$work/home/.tmux"
L="balance-test-$$"
t() { HOME="$work/home" tmux -L "$L" -f /dev/null "$@"; }
t new-session -d -x 284 -y 51 'sleep 300'
t source-file "$DIR/balance.conf"
geom() { t list-panes -t "${1:-}" -F '#{pane_width}x#{pane_height}@#{pane_left},#{pane_top}' | tr '\n' ' ' | sed 's/ $//'; }
t split-window -h 'sleep 300'
t split-window -h 'sleep 300'
check "分割すると均等になる"           "94x51@0,0 94x51@95,0 94x51@190,0" geom
# prefix M-8: 最小の幅（@layout_grid_min_width、既定 90）を保てる列数の碁盤目に並べ直す
# （古い tmux は文字列形式で渡すため、この検証は JSON でない tmux でも行う）
t new-window -d -n grid 'sleep 300'
for _ in 1 2 3 4 5; do t split-window -d -t grid 'sleep 300'; done
t run-shell "sh '$SCRIPT' grid grid"
check "grid: 6 枚を 3 列 × 2 段に"  "94x25@0,0 94x25@0,26 94x25@95,0 94x25@95,26 94x25@190,0 94x25@190,26" geom grid
t set -g @layout_grid_min_width 140
t run-shell "sh '$SCRIPT' grid grid"
check "grid: 最小の幅を変えると 2 列" "141x16@0,0 141x16@0,17 141x17@0,34 142x16@142,0 142x16@142,17 142x17@142,34" geom grid
t set -gu @layout_grid_min_width
t kill-window -t grid
check "grid: prefix M-8 に割り当て" "1" sh -c 'HOME="$1" tmux -L "$2" list-keys -T prefix M-8 | grep -c "balance-panes.sh grid"' _ "$work/home" "$L"
# 以降の入れ子の検証は JSON の window_layout（tmux 3.8 以降）の経路だけ。
# それより前の tmux（CI の ubuntu 等）は select-layout -E の経路を上の検証で確かめる
if ! t display -p '#{window_layout}' | grep -q '^{'; then
    echo "SKIP window_layout が JSON 形式でない tmux のため入れ子の検証を省略"
    [ "$fail" -eq 0 ] && exit 0 || exit 1
fi
# tmux 3.8 の select-layout -E は、この形で pane を画面外（x=285）に置く
t split-window -v 'sleep 300'
check "入れ子の分割も画面内で均等"     "94x51@0,0 94x51@95,0 94x25@190,0 94x25@190,26" geom
t kill-pane -t :.0
check "kill-pane の後も均等"           "141x51@0,0 142x25@142,0 142x25@142,26" geom
t split-window -h 'sleep 1'
sleep 2
check "pane が終了した後も均等"        "141x51@0,0 142x25@142,0 142x25@142,26" geom
# 別 window を -d で分割しても、その window がそろう（現在の window は変えない）
t new-window -d -n other 'sleep 300'
t split-window -d -h -t other 'sleep 300'
t split-window -d -h -t other 'sleep 300'
check "-t で指定した window がそろう"  "94x51@0,0 94x51@95,0 94x51@190,0" geom other
check "現在の window は変えない"       "141x51@0,0 142x25@142,0 142x25@142,26" geom
# 拡大表示（resize-pane -Z）中は何もしない（拡大を解かない）
t resize-pane -Z
t run-shell "sh '$SCRIPT'"
check "拡大表示中は拡大を保つ"         "1" t display -p '#{window_zoomed_flag}'
t resize-pane -Z
# 無効にすると分割しても動かない（手で変えた大きさを保つ）
t set -g @pane_auto_balance off
t resize-pane -t :.0 -x 60
t split-window -h -t :.0 'sleep 300'
check "@pane_auto_balance off で無効"   "30x51@0,0 29x51@31,0" sh -c 'HOME="$1" tmux -L "$2" list-panes -F "#{pane_width}x#{pane_height}@#{pane_left},#{pane_top}" | head -2 | tr "\n" " " | sed "s/ $//"' _ "$work/home" "$L"
# 手動の C-x + は無効でも効く
t run-shell "sh '$SCRIPT'"
check "手動の実行は無効でも効く"       "94x51@0,0 94x51@95,0" sh -c 'HOME="$1" tmux -L "$2" list-panes -F "#{pane_width}x#{pane_height}@#{pane_left},#{pane_top}" | head -2 | tr "\n" " " | sed "s/ $//"' _ "$work/home" "$L"

# hook から呼ばれ window が既に無いとき（最後の pane が終了）は何も出さず 0 で終わる
check "存在しない window でも 0 で終わる" "rc=0" t run-shell "sh '$SCRIPT' @9999; echo rc=\$?"

[ "$fail" -eq 0 ] && exit 0 || exit 1
