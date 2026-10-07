#!/bin/sh
# open-path.sh - クリックしたファイルパスの中身をポップアップでプレビューする
#
# keybindings.conf の Option + クリックから呼ばれる（値は @open_path_* ユーザーオプション経由）:
#   open-path.sh click
# 段階ごとのサブコマンド（テスト・デバッグ用）:
#   extract <行> <桁>        クリック位置（0 起点の表示桁）のパス候補を切り出す
#   parse <候補>             末尾の記号を落とし「パス<TAB>行番号」に分ける
#   resolve <候補> <cwd>     実在するファイルの「実体パス<TAB>行番号」を返す
#   view [<パス> [行番号]]   ポップアップ内でプレビューし、Emacs / 既定アプリで開ける
#
# 画面上の文字列は信用しない入力として扱う: eval・シェルへの埋め込みはせず、
# 引数として渡し、実在確認を通ったパスだけを表示する。

# 表示桁 → 文字の対応は UTF-8 の先頭バイトで推定する（3/4 バイト文字 = 全角 2 桁）。
# mawk / BSD awk / gawk で同じ結果になるよう LC_ALL=C でバイト単位に処理する。
extract() {
    printf '%s\n' "$1" | LC_ALL=C awk -v x="$2" '
    function pathchar(c) { return c ~ /[A-Za-z0-9._~\/@+%:-]/ || c >= "\200" }
    {
        n = length($0); col = 0; k = 0; hit = 0; x += 0
        for (i = 1; i <= n; i++) {
            c = substr($0, i, 1)
            if (c >= "\200" && c < "\300") continue
            start[++k] = i
            w = (c >= "\340") ? 2 : 1
            if (x >= col && x < col + w) hit = k
            col += w
        }
        start[k + 1] = n + 1
        if (!hit || !pathchar(substr($0, start[hit], 1))) exit 1
        l = hit; r = hit
        while (l > 1 && pathchar(substr($0, start[l - 1], 1))) l--
        while (r < k && pathchar(substr($0, start[r + 1], 1))) r++
        print substr($0, start[l], start[r + 1] - start[l])
    }'
}

parse() {
    printf '%s\n' "$1" | awk '{
        t = $0
        while (t ~ /[.:]$/) t = substr(t, 1, length(t) - 1)
        line = ""
        if (match(t, /:[0-9]+(:[0-9]+)?$/)) {
            split(substr(t, RSTART + 1), a, ":"); line = a[1]
            t = substr(t, 1, RSTART - 1)
        }
        if (t == "") exit 1
        printf "%s\t%s\n", t, line
    }'
}

# file://host/path%20x → /path x（host は空・localhost・自ホスト名のみ受け付ける）
uri_path() {
    rest=${1#file://}
    host=${rest%%/*}
    case $host in
        ""|localhost|"$(uname -n)"|"$(uname -n | cut -d. -f1)") ;;
        *) return 1 ;;
    esac
    printf '%s\n' "$rest" | LC_ALL=C awk '{
        p = $0; sub(/^[^\/]*/, "", p); out = ""; hex = "0123456789abcdef"
        while (match(p, /%[0-9A-Fa-f][0-9A-Fa-f]/)) {
            h = tolower(substr(p, RSTART + 1, 2))
            out = out substr(p, 1, RSTART - 1) sprintf("%c", (index(hex, substr(h, 1, 1)) - 1) * 16 + index(hex, substr(h, 2, 1)) - 1)
            p = substr(p, RSTART + 3)
        }
        print out p
    }'
}

# 通常ファイル・ディレクトリなら実体パスを出力する
# （FIFO・デバイス等は bat / less が EOF を待ち続けるため対象外）
canonical() {
    if [ -d "$1" ]; then
        (cd -- "$1" 2>/dev/null && pwd -P)
    elif [ -f "$1" ]; then
        d=$(cd -- "$(dirname -- "$1")" 2>/dev/null && pwd -P) || return 1
        printf '%s/%s\n' "${d%/}" "$(basename -- "$1")"
    else
        return 1
    fi
}

# 候補 1 つを ~ / 絶対 / cwd 相対 / git ルート相対の順に探す
try_candidate() {
    c=$1 cwd=$2
    [ -n "$c" ] || return 1
    # shellcheck disable=SC2088  # 画面上の "~/" を文字列として照合して展開する
    case $c in
        "~") c=$HOME ;;
        "~/"*) c=$HOME/${c#"~/"} ;;
    esac
    case $c in
        /*) canonical "$c"; return ;;
    esac
    canonical "$cwd/$c" && return 0
    [ -n "$git_root" ] || return 1
    canonical "$git_root/$c"
}

# コマンドを <秒>（SIGTERM を無視されたら +1 秒で SIGKILL）で打ち切る
# （timeout コマンドの無い環境でも効くよう sh だけで実装）。
# 見張りの出力は捨てる（残った sleep がパイプを開いたままにして後段を待たせないため）
bounded() {
    secs=$1; shift
    "$@" &
    pid=$!
    # SIGTERM を無視するプロセスにも効くよう、1 秒の猶予の後に SIGKILL へ上げる
    ( sleep "$secs"; kill "$pid" 2>/dev/null; sleep 1; kill -9 "$pid" 2>/dev/null ) >/dev/null 2>&1 &
    watcher=$!
    wait "$pid" 2>/dev/null
    status=$?
    kill "$watcher" 2>/dev/null
    return $status
}

# ファイル名だけの候補を cwd 配下から探し、最も浅いもの（同じ深さなら名前順）を返す。
# git 管理下は ls-files（.gitignore を尊重）、管理外は深さを制限した find で探す。
search_name() {
    name=$1 cwd=$2
    # パス区切り・glob 文字（find -name がパターンとして解釈する）を含む名前は探さない
    case $name in
        ""|.|..|*/*|*\**|*\?*|*\[*|*\\*) return 1 ;;
    esac
    if [ -n "$git_root" ]; then
        # quotePath=true（git 既定）だと日本語名が \NNN にエスケープされ照合できない
        bounded 3 git -C "$cwd" -c core.quotePath=false ls-files --cached --others --exclude-standard 2>/dev/null
    else
        # $HOME 等の大きな木で待たされないよう、隠しディレクトリ・依存・Library を除外し
        # 深さと時間を制限する（隠し通常ファイルは対象に含む）
        (cd -- "$cwd" 2>/dev/null && bounded 3 find . -maxdepth 4 \
            -type d \( -name '.?*' -o -name node_modules -o -name Library \) -prune \
            -o -type f -name "$name" -print 2>/dev/null)
    fi | NAME=$name awk '{
        sub(/^\.\//, ""); n = split($0, p, "/")
        if (p[n] == ENVIRON["NAME"]) printf "%d\t%s\n", n, $0
    }' | sort -t "$(printf '\t')" -k1,1n -k2,2 | cut -f2- | {
        base=$(canonical "$cwd") || exit 1
        # 浅い順に、実在し（ls-files --cached は削除済みも返す）cwd 配下にある最初の候補を選ぶ。
        # 改行入りのディレクトリ名は行分割で ../ 等を作りうるため、実体パスで配下か確かめる
        while IFS= read -r rel; do
            [ -n "$rel" ] || continue
            f=$(canonical "$cwd/$rel") || continue
            case $f in
                "${base%/}"/*) printf '%s\n' "$f"; exit 0 ;;
            esac
        done
        exit 1
    }
}

resolve() {
    token=$1 cwd=$2
    case $token in
        file://*)
            p=$(uri_path "$token") && f=$(canonical "$p") || return 1
            printf '%s\t\n' "$f"; return 0 ;;
    esac
    parsed=$(parse "$token") || return 1
    # git ルートは 1 回だけ求めて候補間で共有する（止まる git で待ちが積み上がらないよう打ち切る）
    git_root=$(bounded 3 git -C "$cwd" rev-parse --show-toplevel 2>/dev/null) || git_root=""
    tab=$(printf '\t')
    path=${parsed%%"$tab"*} line=${parsed#*"$tab"}
    # 候補: そのまま → diff の a/ b/ を外す → 前後の非 ASCII（日本語の地の文）を外す
    trimmed=$(printf '%s\n' "$path" | LC_ALL=C sed 's/[^ -~]*$//; s/^[^ -~]*//')
    for c in "$path" "${path#[ab]/}" "$trimmed" "${trimmed#[ab]/}"; do
        if f=$(try_candidate "$c" "$cwd"); then
            printf '%s\t%s\n' "$f" "$line"; return 0
        fi
    done
    # 直接見つからなければ、ファイル名だけの候補を cwd 配下から探す
    if f=$(search_name "$path" "$cwd"); then
        printf '%s\t%s\n' "$f" "$line"; return 0
    fi
    if [ "$trimmed" != "$path" ] && f=$(search_name "$trimmed" "$cwd"); then
        printf '%s\t%s\n' "$f" "$line"; return 0
    fi
    return 1
}

# 標準入力の <n> 行目に ▶ を付け、他の行は 2 桁ずらして揃える
# （bat の --highlight-line はテーマの強調色次第で背景とほぼ同色になり見えないため）
mark() {
    awk -v n="$1" '{ printf "%s%s\n", (NR == n + 0 ? "\033[1;33m▶\033[0m " : "  "), $0 }'
}

# シェル用に単一引用符でクォートする
shquote() {
    printf "'%s'" "$(printf '%s' "$1" | sed "s/'/'\\\\''/g")"
}

# 割り当て側が set -gF で書いた @open_path_* を読む。画面上の文字列（行・cwd 等）を
# run-shell のコマンド文字列に載せると #{q:} が改行・タブをエスケープせずシェルに
# 解釈されるため、値はユーザーオプション経由でだけ受け渡す
opt() {
    tmux show -gv "@open_path_$1" 2>/dev/null
}

# display-popup -T は tmux 3.3+
popup_title_supported() {
    tmux display -p '#{version}' 2>/dev/null | awk -F. '{
        split($2, m, /[^0-9]/); exit !($1 + 0 > 3 || ($1 + 0 == 3 && m[1] + 0 >= 3))
    }'
}

click() {
    client=$(opt client) link=$(opt link) line=$(opt line) x=$(opt x) cwd=$(opt cwd)
    res=""
    case $link in
        file://*) res=$(resolve "$link" "$cwd") ;;
    esac
    if [ -z "$res" ]; then
        tok=$(extract "$line" "$x") && res=$(resolve "$tok" "$cwd")
    fi
    if [ -z "$res" ]; then
        tmux display-message -c "$client" "open-path: ファイルが見つかりません"
        return 0
    fi
    tab=$(printf '\t')
    path=${res%%"$tab"*} lineno=${res#*"$tab"}
    # display-popup の -d やコマンド文字列はフォーマット展開されうるため、画面由来の
    # 値（パス等）は載せない。view へは展開なしの set -g で渡し、-d も使わない
    tmux set -g @open_path_view_path "$path"
    tmux set -g @open_path_view_line "$lineno"
    popup="sh $(shquote "$0") view"
    if popup_title_supported; then
        # -T はフォーマットなので # を ## にエスケープする（#() の実行を防ぐ）
        title=$(printf ' %s ' "$path" | sed 's/#/##/g')
        tmux display-popup -c "$client" -E -w 85% -h 85% -T "$title" "$popup"
    else
        tmux display-popup -c "$client" -E -w 85% -h 85% "$popup"
    fi
}

open_emacs() {
    command -v emacsclient >/dev/null 2>&1 || return 1
    # 応答しない Emacs サーバーで待たされないよう打ち切る
    bounded 2 emacsclient -a false -e t >/dev/null 2>&1 || return 1
    bounded 5 emacsclient -n ${2:+"+$2"} -- "$1" >/dev/null 2>&1
}

open_default() {
    # resolve の結果は常に絶対パス（/ 始まり）のため -- は付けない（open の版差を避ける）
    if command -v open >/dev/null 2>&1; then open "$1"
    elif command -v xdg-open >/dev/null 2>&1; then xdg-open "$1" >/dev/null 2>&1
    else return 1
    fi
}

# less の終了コード → 動作（open-path.lesskey の quit e / quit o）
action() {
    case $1 in
        101) echo emacs ;;
        111) echo default ;;
        114) echo toggle ;;
        *) echo close ;;
    esac
}

# less --version の 1 行目から --lesskey-src（less 582+）が使えるか判定する
lesskey_ok() {
    printf '%s\n' "$1" | awk '$1 == "less" && $2 + 0 >= 582 { ok = 1 } END { print ok ? "yes" : "no"; exit !ok }'
}

# Markdown の整形に使うツール（glow → mdcat。どちらも無ければ失敗）
md_renderer() {
    if command -v glow >/dev/null 2>&1; then echo glow
    elif command -v mdcat >/dev/null 2>&1; then echo mdcat
    else return 1
    fi
}

is_markdown() {
    case $(printf '%s' "$1" | tr '[:upper:]' '[:lower:]') in
        *.md|*.markdown|*.mdx) echo yes ;;
        *) return 1 ;;
    esac
}

# プレビューする中身を標準出力へ。mode=md は Markdown を整形（パイプでも色を付ける）、
# mode=raw は原文（行番号付きは折り返さず、対象行に ▶ を付ける）
render() {
    if [ "$1" = md ]; then
        width=$(( $(tput cols 2>/dev/null || echo 100) - 2 ))
        case $(md_renderer) in
            glow) glow -s dark -w "$width" "$path" ;;
            mdcat) mdcat --ansi --columns "$width" "$path" ;;
        esac
        return
    fi
    if [ -d "$path" ]; then
        # shellcheck disable=SC2012  # 人が読む一覧表示のため ls を使う
        ls -la -- "$path"
    elif command -v bat >/dev/null 2>&1; then
        bat --paging=never --wrap=never --style=numbers --color=always \
            ${lineno:+--highlight-line "$lineno"} -- "$path"
    else
        awk '{ printf "%6d  %s\n", NR, $0 }' "$path"
    fi | if [ -n "$lineno" ]; then mark "$lineno"; else cat; fi
}

do_action() {
    case $1 in
        emacs) open_emacs "$path" "$lineno" || { printf '\nEmacs に接続できません: %s\n' "$path"; sleep 2; } ;;
        default) open_default "$path" || { printf '\n既定アプリで開けません: %s\n' "$path"; sleep 2; } ;;
    esac
}

# 引数が無ければ click が set -g した @open_path_view_* を読む
view() {
    if [ $# -gt 0 ]; then
        path=$1 lineno=${2:-}
    else
        path=$(opt view_path) lineno=$(opt view_line)
    fi
    case $lineno in *[!0-9]*) lineno="" ;; esac
    [ -f "$path" ] || [ -d "$path" ] || { printf 'open-path: 見つかりません: %s\n' "$path"; sleep 2; return 1; }
    # 利用者の LESS（-F は 1 画面に収まると即終了してポップアップが閉じる、-M は -Ps の
    # プロンプトを無視させる等）に左右されないよう、必要なオプションは引数で渡す
    LESS=""
    export LESS
    # 行番号なしの Markdown は整形して開く（整形すると行の対応が崩れるため行番号付きは原文）
    mode=raw md=""
    if [ -f "$path" ] && is_markdown "$path" >/dev/null && md_renderer >/dev/null; then
        md=yes
        [ -z "$lineno" ] && mode=md
    fi
    keys="$(cd "$(dirname "$0")" && pwd)/open-path.lesskey"
    if [ -f "$keys" ] && lesskey_ok "$(less --version 2>/dev/null | head -n 1)" >/dev/null; then
        # q: 閉じる / e: Emacs / o: 既定アプリ / r: 整形と原文の切替 を less の中で 1 手にする
        prompt="q 閉じる  e Emacs  o 既定アプリ${md:+  r 整形/原文}"
        while :; do
            start=1
            [ "$mode" = raw ] && start=${lineno:-1}
            render "$mode" | less -R -j.3 "+${start}g" --lesskey-src="$keys" "-Ps$prompt"
            act=$(action $?)
            if [ "$act" = toggle ]; then
                # Markdown 以外の r は再描画と同じ扱いで開き直す
                if [ -n "$md" ]; then
                    if [ "$mode" = md ]; then mode=raw; else mode=md; fi
                fi
                continue
            fi
            do_action "$act"
            return 0
        done
    fi
    # 古い less: 閉じた後に 1 キーで選ぶ
    start=1
    [ "$mode" = raw ] && start=${lineno:-1}
    render "$mode" | less -R -j.3 "+${start}g"
    printf '\n[e] Emacs で開く  [o] 既定アプリで開く  [その他] 閉じる: '
    old=$(stty -g 2>/dev/null)
    stty -icanon -echo min 1 2>/dev/null
    key=$(dd bs=1 count=1 2>/dev/null)
    [ -n "$old" ] && stty "$old" 2>/dev/null
    case $key in
        e) do_action emacs ;;
        o) do_action default ;;
    esac
}

cmd=${1:-}
[ $# -gt 0 ] && shift
case $cmd in
    extract) extract "$@" ;;
    parse) parse "$@" ;;
    resolve) resolve "$@" ;;
    click) click "$@" ;;
    view) view "$@" ;;
    mark) mark "$@" ;;
    action) action "$@" ;;
    md_renderer) md_renderer "$@" ;;
    is_markdown) is_markdown "$@" ;;
    lesskey_ok) lesskey_ok "$@" ;;
    *) echo "usage: open-path.sh {click|extract|parse|resolve|view|mark|action|lesskey_ok|md_renderer|is_markdown} ..." >&2; exit 2 ;;
esac
