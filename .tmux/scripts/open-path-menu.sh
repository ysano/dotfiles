#!/bin/sh
# open-path-menu.sh - tmux 既定の右クリックメニュー（MouseDown3Pane）に「Preview Path」を足す
#
# マウスを使うアプリ（fullscreen の Claude Code 等）の中でもメニューを出す。
# tmux にはメニューへ項目を追加する仕組みが無いため、設定読込のたびに素の tmux から
# 既定の割り当てを取り出し、項目を差し込んで割り当て直す（tmux 更新にも追従する）。
#   install     既定を取り出して差し込み、稼働中のサーバーへ source-file する
#   transform   既定の割り当て 1 行（list-keys 形式）を標準入力から受け、差し込み後を出力
#
# メニュー項目のコマンドはメニュー生成時にフォーマット展開されるため、画面由来の値は
# 埋め込まない。値はメニュー表示前に set -gF で @open_path_* へ退避し、項目は固定の
# コマンドで open-path.sh click を呼ぶ。目印が無い等で差し込めなければ既定のままにする。

ANCHOR='-x M -y M '

transform() {
    IFS= read -r line || return 1
    cmd=$(printf '%s\n' "$line" | sed -n 's/^bind-key  *-T root  *MouseDown3Pane  *//p')
    case $cmd in
        *"$ANCHOR"*) ;;
        *) return 1 ;;
    esac
    script="$(cd "$(dirname "$0")" && pwd)/open-path.sh"
    case $script in *"'"*) return 1 ;; esac
    # 既定は「アプリがマウスを使っていれば右クリックをアプリへ渡す」。fullscreen の
    # Claude Code 等でもメニューを出すため、条件から mouse_any_flag を外す
    # （copy-mode 以外のモード中はアプリへ渡す、という残りの条件は保つ）
    any='#{||:#{mouse_any_flag},'
    case $cmd in
        *"$any"*'}}}"'*)
            # "#{||:#{mouse_any_flag},#{&&:...,#{?...,0,1}}}" → "#{&&:...,#{?...,0,1}}"
            # （末尾の }}} は #{? と #{&& と #{|| の閉じ。#{|| の分を 1 つ外す）
            head=${cmd%%"$any"*}
            rest=${cmd#*"$any"}
            cond=${rest%%'}}}"'*}
            tail=${rest#*'}}}"'}
            cmd="$head$cond}}\"$tail"
            ;;
    esac
    before=${cmd%%"$ANCHOR"*}
    after=${cmd#*"$ANCHOR"}
    item="\"Preview Path\" o { run-shell -b '$script click' } '' "
    save="set -gF @open_path_client '#{client_name}' \; set -gF -t = @open_path_cwd '#{pane_current_path}' \; set -gF @open_path_link '#{mouse_hyperlink}' \; set -gF @open_path_line '#{mouse_line}' \; set -gF @open_path_x '#{mouse_x}' \;"
    printf 'bind-key -n MouseDown3Pane %s %s%s%s%s\n' "$save" "$before" "$ANCHOR" "$item" "$after"
}

install() {
    sock="open-path-menu-$$"
    tmux -L "$sock" -f /dev/null new-session -d 2>/dev/null || return 0
    sockpath=$(tmux -L "$sock" display -p '#{socket_path}' 2>/dev/null)
    line=$(tmux -L "$sock" list-keys -T root 2>/dev/null | grep -E '^bind-key +-T root +MouseDown3Pane ')
    tmux -L "$sock" kill-server 2>/dev/null
    # kill-server 後もソケットファイルが残るため片付ける
    [ -S "$sockpath" ] && rm -f -- "$sockpath"
    conf=$(printf '%s\n' "$line" | transform) || return 0
    tmp=$(mktemp) || return 0
    printf '%s\n' "$conf" > "$tmp"
    tmux source-file "$tmp"
    rm -f "$tmp"
}

case ${1:-} in
    transform) transform ;;
    install) install ;;
    *) echo "usage: open-path-menu.sh {install|transform}" >&2; exit 2 ;;
esac
