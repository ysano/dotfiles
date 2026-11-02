#!/bin/sh
# balance-panes.sh - window の pane を分割ごとに均等な大きさにそろえる（Emacs の balance-windows）
#
#   balance-panes.sh [<window>]   指定（省略時は現在）の window をそろえる
#   balance-panes.sh grid [<window>]  最小の幅（@layout_grid_min_width、既定 90）を保てる
#                                 だけ列を増やした碁盤目に並べ直す（prefix M-8）
#   balance-panes.sh main [<window>]  main pane が左半分を占め、残りを最小の幅
#                                 （@layout_main_min_width、既定 80）で碁盤目にする（prefix M-4）
#   balance-panes.sh main-mirrored [<window>]  同じく main pane を右に（prefix M-7）
# テスト用（標準入力 → 標準出力）:
#   balance-panes.sh layout       window_layout（JSON）を均等にして出す
#   balance-panes.sh grid-layout <最小の幅>  window_layout（JSON）を碁盤目にして出す
#   balance-panes.sh main-layout <最小の幅> [mirrored]  window_layout（JSON）を main ＋碁盤目にして出す
#   balance-panes.sh legacy       JSON の layout を古い tmux の文字列形式（チェックサム付き）にする
#
# tmux 3.8 の select-layout -E は、入れ子の分割（左右に分けた中をさらに上下に分ける等）で
# pane を画面の外に置いてしまう。そのため window_layout（3.8 以降は JSON）の大きさと位置だけを
# 組み直して select-layout に渡す。JSON でない古い tmux では -E に任せる。

# 分割（t が h: 左右 / v: 上下）ごとに、子の大きさを境界線 1 桁を除いて均等に割り、余りは
# 最後の子に足す。pane 番号・アクティブ等の他のキーはそのまま残す。
# 割り当てが子孫の分割に必要な最小の大きさ（pane 1 桁＋境界線）に足りなければ、はみ出す
# layout を作らないよう失敗する（呼び出し側は何も変えない）
# shellcheck disable=SC2016  # jq の変数（$x 等）はシェルで展開しない
JQ_DEFS='
    # floating pane（tmux 3.8。z を持つ）は分割に属さないため、大きさの計算に数えず動かさない
    def floating: type == "object" and has("z");
    def tiled: [(.c // [])[] | select(floating | not)];
    def minsz($d):
      if .t == "p" or (tiled | length) == 0 then 1
      elif (.t == "h") == ($d == "w") then ([tiled[] | minsz($d)] | add) + (tiled | length) - 1
      else [tiled[] | minsz($d)] | max end;
    def bal($x; $y; $w; $h):
      .x = $x | .y = $y | .w = $w | .h = $h
      | if .t == "p" or (tiled | length) == 0 then .
        else .t as $t | (tiled | length) as $n
          | (if $t == "h" then "w" else "h" end) as $d
          | (if $t == "h" then $w else $h end) as $size
          | ((($size - ($n - 1)) / $n) | floor) as $each
          | ($size - ($n - 1) - $each * $n) as $rem
          | .c |= (reduce .[] as $ch ({i: 0, out: []};
              if ($ch | floating) then .out += [$ch]
              else .i as $i
                | ($each + (if $i == $n - 1 then $rem else 0 end)) as $s
                | if ($ch | minsz($d)) > $s then error("too small") else . end
                | .out += [$ch | if $t == "h" then bal($x + $i * ($each + 1); $y; $s; $h)
                                 else bal($x; $y + $i * ($each + 1); $w; $s) end]
                | .i += 1
              end) | .out)
        end;
    def leaves: if floating then [] elif .t == "p" or ((.c // []) | length) == 0 then [.] else [.c[] | leaves] | add end;
    def floats: [.. | select(floating)];
    # pane の配列を $k 列に分け、左の列から上から順に詰める。列ごとの段数の差は 1 以内（多い
    # 列を左に寄せる）。1 枚だけの列は pane のまま、2 枚以上は上下の分割にする
    def columns($k):
      . as $p | (length / $k | floor) as $base | (length % $k) as $extra
      | [range(0; $k) as $i
          | ($i * $base + ([$i, $extra] | min)) as $s
          | $p[$s:$s + $base + (if $i < $extra then 1 else 0 end)]
          | if length == 1 then .[0] else {t: "v", c: .} end];
'

layout() {
    jq -c "$JQ_DEFS"'.L |= bal(.x; .y; .w; .h)'
}

# pane を表示順に取り出し、幅 $min 以上を保てる列数（1 以上・pane 数以下）に、左の列から
# 上から順に詰める。列ごとの段数の差は 1 以内（多い列を左に寄せる）。大きさは bal で割る。
# 分割する pane が 1 つ以下なら変えない
grid_layout() {
    jq -c --argjson min "$1" "$JQ_DEFS"'
    .L as $r | ($r | leaves) as $p | ($p | length) as $n | ($r | floats) as $f
    | if $n < 2 then . else
      ((($r.w + 1) / ($min + 1)) | floor | [., 1] | max | [., $n] | min) as $cols
    | ($p | columns($cols)) as $columns
    # floating pane は最上位の分割の後ろにそのまま戻す
    | (if $cols == 1 then $columns[0] | .c += $f else {t: "h", c: ($columns + $f)} end) as $tree
    | .L = ($tree | bal($r.x; $r.y; $r.w; $r.h)) end'
}

# main（tmux の main-vertical と同じく pane 番号が最小の pane）が左半分を占め、残りの pane を
# 右半分の列に grid_layout と同じ順で詰める（$right が true なら左右を入れ替える）。列の数は
# 右半分で幅 $min 以上を保てる数で、1 列に 2 枚以上入るときだけ増やす（3 枚なら 2 段の 1 列）。
# 半分の中をさらに左右に分けるため、分割・終了の後の bal でも main は半分の幅のまま
main_layout() {
    jq -c --argjson min "$1" --argjson right "$2" "$JQ_DEFS"'
    .L as $r | ($r | leaves | sort_by(.i)) as $p | ($p | length) as $n | ($r | floats) as $f
    | if $n < 2 then . else
      # 余りの 1 桁を含まない狭いほうの半分で数える（左右を入れ替えても列の数を変えない）
      (($r.w - 1) / 2 | floor) as $half
    | ((($half + 1) / ($min + 1)) | floor) as $fit
    | ([$fit, (($n - 1) / 2 | floor)] | min | [., 1] | max) as $k
    | ($p[1:] | columns($k) | if length == 1 then .[0] else {t: "h", c: .} end) as $others
    | (if $right then [$others, $p[0]] else [$p[0], $others] end) as $halves
    | .L = ({t: "h", c: ($halves + $f)} | bal($r.x; $r.y; $r.w; $r.h)) end'
}

# 古い tmux の layout 文字列: 「チェックサム,本体」。本体は「幅x高さ,左,上」に続けて
# pane なら「,番号」、左右の分割なら {…}、上下なら […]。チェックサムは tmux の
# layout_checksum と同じ（右に 1 ビット回転して文字コードを足す 16 ビット）
legacy() {
    jq -r 'def leg: "\(.w)x\(.h),\(.x),\(.y)" +
        (if .t == "p" or ((.c // []) | length) == 0 then "," + (.I | ltrimstr("%"))
         elif .t == "h" then "{" + ([.c[] | leg] | join(",")) + "}"
         else "[" + ([.c[] | leg] | join(",")) + "]" end);
        .L | leg' |
    LC_ALL=C awk 'BEGIN { for (i = 1; i < 256; i++) ord[sprintf("%c", i)] = i }
    {
        c = 0
        for (i = 1; i <= length($0); i++)
            c = (int(c / 2) + (c % 2) * 32768 + ord[substr($0, i, 1)]) % 65536
        printf "%04x,%s\n", c, $0
    }'
}

# hook から呼ばれるため、window が既に無い（最後の pane が終わった）等の失敗では何も表示せず
# 0 で終わる（run-shell の非 0 終了は画面にエラーを出す）
balance() {
    win=${1:-}
    # 拡大表示（resize-pane -Z）中は大きさを変えない（拡大が解けるため）
    [ "$(tmux display -p ${win:+-t "$win"} '#{window_zoomed_flag}' 2>/dev/null)" = 1 ] && return 0
    cur=$(tmux display -p ${win:+-t "$win"} '#{window_layout}' 2>/dev/null) || return 0
    case $cur in
        "{"*)
            if ! command -v jq >/dev/null 2>&1; then
                tmux display-message "balance-panes: jq が必要です"
                return 0
            fi
            new=$(printf '%s' "$cur" | layout 2>/dev/null) && [ -n "$new" ] || return 0
            [ "$new" = "$cur" ] && return 0
            tmux select-layout ${win:+-t "$win"} "$new" 2>/dev/null ;;
        *)
            tmux select-layout ${win:+-t "$win"} -E 2>/dev/null ;;
    esac
}

# rearrange <grid|main|main-mirrored> <最小の幅>: 標準入力の layout（JSON）を並べ直す
rearrange() {
    case $1 in
        grid) grid_layout "$2" ;;
        main) main_layout "$2" false ;;
        main-mirrored) main_layout "$2" true ;;
    esac
}

# arrange <grid|main|main-mirrored> [<window>]: window を並べ直す（prefix M-8 / M-4 / M-7）。
# 古い tmux（layout が JSON でない）では、pane の一覧と window の大きさから JSON を組んで
# 同じ計算をし、文字列形式に直して渡す
arrange() {
    mode=$1 win=${2:-}
    [ "$(tmux display -p ${win:+-t "$win"} '#{window_zoomed_flag}' 2>/dev/null)" = 1 ] && return 0
    if ! command -v jq >/dev/null 2>&1; then
        tmux display-message "balance-panes: jq が必要です"
        return 0
    fi
    # 最小の幅は M-8 が @layout_grid_min_width（既定 90）、M-4 / M-7 が @layout_main_min_width
    # （既定 80。341 桁の画面でも、main の残りの半分に 80 桁超の列を 2 つ取れる）
    case $mode in
        grid) opt=@layout_grid_min_width def=90 ;;
        *) opt=@layout_main_min_width def=80 ;;
    esac
    min=$(tmux show -gv "$opt" 2>/dev/null)
    case $min in ''|0*|*[!0-9]*) min=$def ;; esac
    cur=$(tmux display -p ${win:+-t "$win"} '#{window_layout}' 2>/dev/null) || return 0
    case $cur in
        "{"*)
            new=$(printf '%s' "$cur" | rearrange "$mode" "$min" 2>/dev/null) ;;
        *)
            # 古い tmux は layout 文字列の pane 番号を無視し、番号の順にセルへ割り当てる。そのため
            # main-mirrored では、番号が最大の pane が右の main になる（main-vertical-mirrored も 3.5 から）
            # shellcheck disable=SC2046  # 「幅 高さ」を 2 つの引数に分ける
            set -- $(tmux display -p ${win:+-t "$win"} '#{window_width} #{window_height}' 2>/dev/null)
            new=$(tmux list-panes ${win:+-t "$win"} -F '#{pane_id}' 2>/dev/null |
                jq -Rsc --argjson w "${1:-0}" --argjson h "${2:-0}" \
                    '{L: {t: "h", w: $w, h: $h, x: 0, y: 0, c: [split("\n")[] | select(. != "") | {t: "p", I: .}]}}' |
                rearrange "$mode" "$min" | legacy) 2>/dev/null ;;
    esac
    [ -n "$new" ] && [ "$new" != "$cur" ] || return 0
    tmux select-layout ${win:+-t "$win"} "$new" 2>/dev/null
}

case ${1:-} in
    layout) layout ;;
    grid-layout) grid_layout "${2:-90}" ;;
    main-layout) [ "${3:-}" = mirrored ] && right=true || right=false; main_layout "${2:-90}" "$right" ;;
    legacy) legacy ;;
    grid|main|main-mirrored) arrange "$@"; exit 0 ;;
    *) balance "$@"; exit 0 ;;
esac
