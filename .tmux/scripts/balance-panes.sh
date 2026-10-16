#!/bin/sh
# balance-panes.sh - window の pane を分割ごとに均等な大きさにそろえる（Emacs の balance-windows）
#
#   balance-panes.sh [<window>]   指定（省略時は現在）の window をそろえる
#   balance-panes.sh layout       標準入力の window_layout（JSON）を均等にして出す（テスト用）
#
# tmux 3.8 の select-layout -E は、入れ子の分割（左右に分けた中をさらに上下に分ける等）で
# pane を画面の外に置いてしまう。そのため window_layout（3.8 以降は JSON）の大きさと位置だけを
# 組み直して select-layout に渡す。JSON でない古い tmux では -E に任せる。

# 分割（t が h: 左右 / v: 上下）ごとに、子の大きさを境界線 1 桁を除いて均等に割り、余りは
# 最後の子に足す。pane 番号・アクティブ等の他のキーはそのまま残す。
# 割り当てが子孫の分割に必要な最小の大きさ（pane 1 桁＋境界線）に足りなければ、はみ出す
# layout を作らないよう失敗する（呼び出し側は何も変えない）
layout() {
    jq -c '
    def minsz($d):
      if .t == "p" or ((.c // []) | length) == 0 then 1
      elif (.t == "h") == ($d == "w") then ([.c[] | minsz($d)] | add) + (.c | length) - 1
      else [.c[] | minsz($d)] | max end;
    def bal($x; $y; $w; $h):
      .x = $x | .y = $y | .w = $w | .h = $h
      | if .t == "p" or ((.c // []) | length) == 0 then .
        else .t as $t | (.c | length) as $n
          | (if $t == "h" then "w" else "h" end) as $d
          | (if $t == "h" then $w else $h end) as $size
          | ((($size - ($n - 1)) / $n) | floor) as $each
          | ($size - ($n - 1) - $each * $n) as $rem
          | .c |= [to_entries[] | .key as $i | .value
              | ($each + (if $i == $n - 1 then $rem else 0 end)) as $s
              | if minsz($d) > $s then error("too small") else . end
              | if $t == "h" then bal($x + $i * ($each + 1); $y; $s; $h)
                else bal($x; $y + $i * ($each + 1); $w; $s) end]
        end;
    .L |= bal(.x; .y; .w; .h)'
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

case ${1:-} in
    layout) layout ;;
    *) balance "$@"; exit 0 ;;
esac
