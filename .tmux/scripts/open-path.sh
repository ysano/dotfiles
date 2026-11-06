#!/bin/sh
# open-path.sh - クリックしたファイルパスの中身をポップアップでプレビューする
# （コミットの SHA は差分、GitHub の PR / Issue の参照は gh で取得した本文・コメント）
#
# keybindings.conf の Option + クリックから呼ばれる（値は @open_path_* ユーザーオプション経由）:
#   open-path.sh click        プレビュー（ポップアップ）
#   open-path.sh click open   既定アプリで開く（右クリックメニューの "Open in Default App"）
#   open-path.sh click link [<通し番号>]  普通のクリック: URL・PR / Issue の参照をブラウザで開く
#                             （それ以外は何もしない。通し番号が今の @open_path_seq と違えば何もしない）
# GitHub 以外の URL は、どの click でもブラウザで開く（プレビューできないため）
# 段階ごとのサブコマンド（テスト・デバッグ用）:
#   extract <行> <桁>        クリック位置（0 起点の表示桁）のパス候補を切り出す
#   parse <候補>             末尾の記号を落とし「パス<TAB>行番号」に分ける
#   resolve <候補> <cwd>     実在するファイルの「実体パス<TAB>行番号」を返す
#   popup_size <パス> <幅> <高さ>  中身に合わせたポップアップの「幅 高さ」
#   render <md|raw> <パス> [行番号]  プレビューする中身を標準出力へ
#   render commit <リポジトリ> <SHA>  コミットの差分を標準出力へ
#   context <名前> <cwd> <行の番号> <折り返し桁>  標準入力の画面の行のディレクトリから探す
#   commit_repo <SHA> <ディレクトリ...>  そのコミットを持つリポジトリ
#   gh_ref <行> <桁> / gh_url <URL>  PR / Issue の参照（#番号・owner/repo#番号・URL）
#   url_at <行> <桁> [<次の行> <端の桁>]  クリック位置の http(s) の URL
#   gh_nwo <remote の URL> / gh_repo <ディレクトリ...>  GitHub の owner/repo
#   gh_kind <owner/repo> <番号>   pr か issue か（gh api で問い合わせる）
#   render gh <owner/repo> <番号> <pr|issue> [body|diff]  PR / Issue の本文・差分を標準出力へ
#   glow_dir <ディレクトリ>  ls -la の代わりに glow の一覧で開くか（yes / 失敗）
#   view [<パス> [行番号]]   ポップアップ内でプレビューし、Emacs / 既定アプリで開ける
#                            （Markdown を含むディレクトリは glow の一覧で開く）
#
# 画面上の文字列は信用しない入力として扱う: eval・シェルへの埋め込みはせず、
# 引数として渡し、実在確認を通ったパスだけを表示する。

# East Asian Width が W/F の符号位置の範囲（Unicode 15.1。私用領域を除く）。次で生成:
#   python3 -c 'import unicodedata as u, itertools as t; print(" ".join("%d-%d" % (g[0][1], g[-1][1]) for k, g in ((k, list(g)) for k, g in t.groupby(((u.east_asian_width(chr(c)) in "WF" and u.category(chr(c)) != "Co", c) for c in range(0x110000)), key=lambda e: e[0])) if k))'
WIDE_RANGES='
    4352-4447 8986-8987 9001-9002 9193-9196 9200-9200 9203-9203 9725-9726 9748-9749 9800-9811 9855-9855
    9875-9875 9889-9889 9898-9899 9917-9918 9924-9925 9934-9934 9940-9940 9962-9962 9970-9971 9973-9973
    9978-9978 9981-9981 9989-9989 9994-9995 10024-10024 10060-10060 10062-10062 10067-10069 10071-10071 10133-10135
    10160-10160 10175-10175 11035-11036 11088-11088 11093-11093 11904-11929 11931-12019 12032-12245 12272-12350 12353-12438
    12441-12543 12549-12591 12593-12686 12688-12771 12783-12830 12832-12871 12880-19903 19968-42124 42128-42182 43360-43388
    44032-55203 63744-64255 65040-65049 65072-65106 65108-65126 65128-65131 65281-65376 65504-65510 94176-94180 94192-94193
    94208-100343 100352-101589 101632-101640 110576-110579 110581-110587 110589-110590 110592-110882 110898-110898 110928-110930 110933-110933
    110948-110951 110960-111355 126980-126980 127183-127183 127374-127374 127377-127386 127488-127490 127504-127547 127552-127560 127568-127569
    127584-127589 127744-127776 127789-127797 127799-127868 127870-127891 127904-127946 127951-127955 127968-127984 127988-127988 127992-128062
    128064-128064 128066-128252 128255-128317 128331-128334 128336-128359 128378-128378 128405-128406 128420-128420 128507-128591 128640-128709
    128716-128716 128720-128722 128725-128727 128732-128735 128747-128748 128756-128764 128992-129003 129008-129008 129292-129338 129340-129349
    129351-129535 129648-129660 129664-129672 129680-129725 129727-129733 129742-129755 129760-129768 129776-129784 131072-196605 196608-262141
'

# 画面の文字列を扱う awk の共通部分（extract と context で使う。OPEN_PATH_WIDE に
# WIDE_RANGES、wrapcol に折り返し桁を渡す）
TEXT_AWK='
    BEGIN {
        for (i = 1; i < 256; i++) ord[sprintf("%c", i)] = i
        nw = split(ENVIRON["OPEN_PATH_WIDE"], rs, /[ \n]+/); m = 0
        for (i = 1; i <= nw; i++) if (split(rs[i], p, "-") == 2) { lo[++m] = p[1] + 0; hi[m] = p[2] + 0 }
    }
    function within(u, lo, hi) { return u >= lo && u <= hi }
    # UTF-8 の 1 文字（バイト列）→ 符号位置
    function cp(c,    n, b) {
        n = length(c); b = ord[substr(c, 1, 1)]
        if (n == 1) return b
        if (n == 2) return (b - 192) * 64 + ord[substr(c, 2, 1)] - 128
        if (n == 3) return (b - 224) * 4096 + (ord[substr(c, 2, 1)] - 128) * 64 + ord[substr(c, 3, 1)] - 128
        return (b - 240) * 262144 + (ord[substr(c, 2, 1)] - 128) * 4096 + (ord[substr(c, 3, 1)] - 128) * 64 + ord[substr(c, 4, 1)] - 128
    }
    # WIDE_RANGES を二分探索する
    function width(u,    a, b, c) {
        if (u < 4352) return 1
        a = 1; b = m
        while (a <= b) {
            c = int((a + b) / 2)
            if (u < lo[c]) b = c - 1
            else if (u > hi[c]) a = c + 1
            else return 2
        }
        return 1
    }
    # 区切り: 一般句読点〜記号・罫線（U+2000–2BFF）、CJK 記号と句読点、中黒、全角の記号
    function pathchar(c,    u) {
        if (c < "\200") return c ~ /[A-Za-z0-9._~\/@+%:-]/
        u = cp(c)
        return !(within(u, 8192, 11263) || within(u, 12288, 12351) || u == 12539 || within(u, 65281, 65295) \
            || within(u, 65306, 65312) || within(u, 65339, 65344) || within(u, 65371, 65381))
    }
    # 文字列 s を 1 文字ずつ a[1..] に分け、文字数を返す
    function chars(s, a,    n, i, k, c) {
        n = length(s); k = 0
        for (i = 1; i <= n; i++) {
            c = substr(s, i, 1)
            if (c >= "\200" && c < "\300") a[k] = a[k] c
            else a[++k] = c
        }
        return k
    }
    # 次の行が字下げされていれば、その空白に続くパス文字の並び
    function headrun(s,    a, k, i, t) {
        k = chars(s, a); i = 1; t = ""
        while (i <= k && a[i] == " ") i++
        if (i == 1) return ""
        while (i <= k && pathchar(a[i])) t = t a[i++]
        return t
    }
    # 前の行が折り返し桁まで埋まっていれば、行末の空白の手前にあるパス文字の並び
    function tailrun(s,    a, k, i, t, w, j2) {
        k = chars(s, a); i = k; t = ""; w = 0
        while (i >= 1 && a[i] == " ") i--
        for (j2 = 1; j2 <= i; j2++) w += width(cp(a[j2]))
        if (w < wrapcol) return ""
        while (i >= 1 && pathchar(a[i])) t = a[i--] t
        return t
    }
'

# 表示桁 → 文字の対応は UTF-8 を復号した符号位置で推定する（CJK・全角形・絵文字 = 2 桁、
# 罫線・記号・私用領域のアイコン等 = 1 桁）。
# 和文の約物（、。「」（）・等）と罫線・記号はパスの区切りとして扱う（表や和文に接したパス）。
# mawk / BSD awk / gawk で同じ結果になるよう LC_ALL=C でバイト単位に処理する。
# <前の行> <次の行> <折り返し桁> を渡すと、改行で折り返されたパスをつないだ候補も出す
extract() {
    printf '%s\n' "$1" | OPEN_PATH_PREV=${3:-} OPEN_PATH_NEXT=${4:-} OPEN_PATH_WIDE=$WIDE_RANGES \
        LC_ALL=C awk -v x="$2" -v wrapcol="${5:-0}" "$TEXT_AWK"'
    {
        k = chars($0, ch); col = 0; hit = 0; x += 0; wrapcol += 0
        for (j = 1; j <= k; j++) {
            w = width(cp(ch[j]))
            if (x >= col && x < col + w) hit = j
            col += w
            end[j] = col
        }
        if (!hit || !pathchar(ch[hit])) exit 1
        l = hit; r = hit
        while (l > 1 && pathchar(ch[l - 1])) l--
        while (r < k && pathchar(ch[r + 1])) r++
        tok = ""
        for (j = l; j <= r; j++) tok = tok ch[j]
        # Claude Code 等は長い行を改行で折り返し、続きを字下げする。クリックした並びが
        # 折り返し桁まで埋まった行の末尾にあれば次の行、字下げした行の先頭にあれば前の行の
        # 続きをつないだ候補を先に出す
        tail = ""; head = ""
        for (j = r + 1; j <= k && ch[j] == " "; j++) ;
        if (j > k && end[r] >= wrapcol) tail = headrun(ENVIRON["OPEN_PATH_NEXT"])
        for (j = l - 1; j >= 1 && ch[j] == " "; j--) ;
        if (j < 1 && l > 1) head = tailrun(ENVIRON["OPEN_PATH_PREV"])
        n = 0
        if (head != "" && tail != "") cand[++n] = head tok tail
        if (tail != "") cand[++n] = tok tail
        if (head != "") cand[++n] = head tok
        cand[++n] = tok
        # 長い順（同じ長さなら上の順）に出す
        for (i = 2; i <= n; i++)
            for (j = i; j > 1 && length(cand[j]) > length(cand[j - 1]); j--) { t = cand[j]; cand[j] = cand[j - 1]; cand[j - 1] = t }
        for (i = 1; i <= n; i++) print cand[i]
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

# cwd・git ルート以外で相対パスの起点になりうるディレクトリを、段 <n> の分だけ 1 行ずつ出す:
# 1 同じリポジトリの worktree / 2 OPEN_PATH_ROOTS（: 区切り）/ 3 ghq 配下の各リポジトリ。
# Claude Code 等は起動時の cwd のまま別リポジトリ・別 worktree 相対のパスを表示するため
tier_roots() {
    case $1 in
        1)
            [ -n "$git_root" ] || return 0
            bounded 3 git -C "$cwd" worktree list --porcelain 2>/dev/null | sed -n 's/^worktree //p' ;;
        2)
            printf '%s\n' "${OPEN_PATH_ROOTS:-}" | tr ':' '\n' ;;
        3)
            ghq_roots=${GHQ_ROOT:-}
            if [ -z "$ghq_roots" ] && command -v ghq >/dev/null 2>&1; then
                ghq_roots=$(bounded 2 ghq root 2>/dev/null) || ghq_roots=""
            fi
            printf '%s\n' "${ghq_roots:-$HOME/ghq}" | tr ':' '\n' | while IFS= read -r r; do
                [ -n "$r" ] || continue
                for d in "$r"/*/*/*; do
                    [ -d "$d" ] && printf '%s\n' "$d"
                done
            done ;;
    esac
}

# 追加の探索に使える候補か: 「名前/名前」の形で、絶対・~・.. を含まない相対パス
# （名前だけ・docs/ のような 1 段は多数のリポジトリで同名が見つかるため cwd 配下の探索に
# 任せ、.. で各ルートの外へは出さない）
roots_candidate() {
    # shellcheck disable=SC2088  # 画面上の "~" を文字列として照合する
    case $1 in
        /*|"~"*|..|../*|*/..|*/../*) return 1 ;;
        ?*/?*) return 0 ;;
    esac
    return 1
}

# 候補（引数）を段の順に各ルートからの相対パスとして探す。段の中で一致が 1 つならそれを返し、
# 複数なら取り違えないよう失敗する（下の段へは進まず、下の段のコマンドも呼ばない）。
# ルート数が多いため、まず組み込みの test で絞ってから実体パスを求める
search_roots() {
    n=0
    for c in "$@"; do
        roots_candidate "$c" && n=$((n + 1))
    done
    # 対象の候補が無ければ、git・ghq を呼ぶ前に返す（止まる git の待ちを積み上げない）
    [ "$n" -gt 0 ] || return 1
    for tier in 1 2 3; do
        res=$(tier_roots "$tier" | {
            hit="" count=0
            while IFS= read -r r; do
                [ -n "$r" ] || continue
                for c in "$@"; do
                    roots_candidate "$c" || continue
                    [ -e "$r/$c" ] || continue
                    f=$(canonical "$r/$c") || continue
                    if [ "$f" != "$hit" ]; then hit=$f count=$((count + 1)); fi
                    break
                done
            done
            [ "$count" -eq 0 ] && exit 0
            [ "$count" -eq 1 ] && { printf 'hit\t%s\n' "$hit"; exit 0; }
            printf 'ambiguous\n'
        })
        case $res in
            hit"$(printf '\t')"*) printf '%s\n' "${res#hit?}"; return 0 ;;
            ambiguous) return 1 ;;
        esac
    done
    return 1
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
    if f=$(search_roots "$path" "${path#[ab]/}" "$trimmed" "${trimmed#[ab]/}"); then
        printf '%s\t%s\n' "$f" "$line"; return 0
    fi
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

# ctx <名前>: クリックした位置の値（client・link・line・x・y・pane・cwd）。普通のクリック
# （click link）は、待つ間に Option + クリック・右クリックが共通の @open_path_* を書き換えても
# 混ざらないよう、別の @open_path_up_* を読む（OPEN_CTX=up_）
ctx() {
    opt "${OPEN_CTX:-}$1"
}

# display-popup -T は tmux 3.3+
popup_title_supported() {
    tmux display -p '#{version}' 2>/dev/null | awk -F. '{
        split($2, m, /[^0-9]/); exit !($1 + 0 > 3 || ($1 + 0 == 3 && m[1] + 0 >= 3))
    }'
}

# popup_size <パス> <端末の幅> <高さ>: 中身に合わせたポップアップの「幅 高さ」を返す。
# パスの代わりに --doc を渡すと、整形した文書（gh の PR / Issue 等）として Markdown と同じにする。
# 幅: Markdown は glow が読みやすい行長（約 100 桁）、ディレクトリは ls -la 程度（glow の一覧で
# 開くものは Markdown と同じ）、それ以外は
# 最長行（タブは bat と同じ 4 桁、全角はバイト数で多めに数える）＋行番号・目印・枠の分。
# 高さ: 行数＋枠とプロンプトの分（Markdown・ディレクトリは整形後の行数が読めないので上限）。
# どれも最小 80x12、最大は端末の 85%。端末の大きさが分からなければ 85% のまま
popup_size() {
    # 幅・高さは個別に検証する（空・先頭 0（8 進として算術エラー）・0 は 85% のまま）
    for v in "$2" "$3"; do
        case $v in ""|0*|*[!0-9]*) echo "85% 85%"; return 0 ;; esac
    done
    max_w=$(( $2 * 85 / 100 )) max_h=$(( $3 * 85 / 100 ))
    if [ -d "$1" ]; then
        w=100 h=$max_h
        glow_dir "$1" >/dev/null && w=108
    elif [ "$1" = --doc ] || is_markdown "$1" >/dev/null; then
        w=108 h=$max_h
    else
        # 巨大なファイル（改行の無いものも）で待たされないよう先頭の 256 KiB・2000 行だけを
        # 見る。読み切れなかったときの高さは上限にする
        # shellcheck disable=SC2046  # awk の「幅 高さ」を 2 つの引数に分ける
        set -- $(head -c 262144 -- "$1" 2>/dev/null | head -n 2000 | LC_ALL=C awk '{
            b += length($0) + 1; gsub(/\t/, "    "); if (length($0) > m) m = length($0)
        } END { print m + 12, (b >= 262144 || NR >= 2000 ? 99999 : NR + 3) }')
        w=${1:-80} h=${2:-12}
    fi
    [ "$w" -ge 80 ] || w=80
    [ "$h" -ge 12 ] || h=12
    [ "$w" -le "$max_w" ] || w=$max_w
    [ "$h" -le "$max_h" ] || h=$max_h
    echo "$w $h"
}

# click [open]: 引数なしはポップアップでプレビュー、open は既定アプリで直接開く
# wrapped <行> <桁> <cwd>: 改行で折り返されたパスを、クリックした pane の前後の行を
# つないで探し直す（画面上の #{mouse_y} 行目の前後。コピーモード中は探さない）
wrapped() {
    pane=${cpane:-} y=${cy:-}
    case $y in ''|*[!0-9]*) return 1 ;; esac
    [ -n "$pane" ] || return 1
    # shellcheck disable=SC2046  # 「モード 高さ 幅」を 3 つに分ける
    set -- "$1" "$2" "$3" $(tmux display -p -t "$pane" '#{pane_in_mode} #{pane_height} #{pane_width}' 2>/dev/null)
    # コピーモード中は capture-pane（通常の画面）と表示中の行が一致しないため見ない
    [ "${4:-1}" = 0 ] || return 1
    case ${5:-}${6:-} in ''|*[!0-9]*) return 1 ;; esac
    # 範囲外の行を指定すると tmux は端の行（= クリックした行自身）を返すため、
    # 画面の 1 行目の前と最下行の後は見ない
    prev="" next=""
    [ "$y" -gt 0 ] && prev=$(tmux capture-pane -p -t "$pane" -S $((y - 1)) -E $((y - 1)) 2>/dev/null)
    [ $((y + 1)) -lt "$5" ] && next=$(tmux capture-pane -p -t "$pane" -S $((y + 1)) -E $((y + 1)) 2>/dev/null)
    # 折り返しとみなすのは、pane の幅の半分以上まで埋まった行の末尾だけ（短い独立した行を
    # つながない）
    extract "$1" "$2" "$prev" "$next" $(($6 / 2)) | while IFS= read -r cand; do
        [ "$cand" = "$tok" ] && continue
        r=$(resolve "$cand" "$3") && [ -n "$r" ] && { printf '%s\n' "$r"; break; }
    done
}

# ctx_tokens <クリックした行の番号（0 起点）>: 標準入力の画面の行から、パスらしい並び
# （/ を含むか ~ で始まる）を「クリックした行からの距離<TAB>出現順<TAB>並び」で出す。
# 改行で折り返されたパス（行が折り返し桁まで埋まり、次の行が字下げされている）はつなぐ
ctx_tokens() {
    OPEN_PATH_WIDE=$WIDE_RANGES LC_ALL=C awk -v clicked="$1" -v wrapcol="${2:-0}" "$TEXT_AWK"'
    function rstrip(s) { sub(/ +$/, "", s); return s }
    function lstrip(s) { sub(/^ +/, "", s); return s }
    function absd(d) { return d < 0 ? -d : d }
    # text のパスらしい並びを距離 d で出す
    function emit(text, d,    a, k, j, t) {
        k = chars(text, a); j = 1
        while (j <= k) {
            if (!pathchar(a[j])) { j++; continue }
            t = ""
            while (j <= k && pathchar(a[j])) t = t a[j++]
            sub(/[.:,]+$/, "", t); sub(/:[0-9]+(:[0-9]+)?$/, "", t)
            if (t ~ /\// || t ~ /^~/) printf "%d\t%d\t%s\n", d, ++seq, t
        }
    }
    { L[NR - 1] = $0 }
    END {
        wrapcol += 0; clicked += 0; seq = 0; i = 0
        while (i < NR) {
            first = i; text = L[i]; d = absd(i - clicked)
            while (i + 1 < NR && tailrun(text) != "" && headrun(L[i + 1]) != "") {
                text = rstrip(text) lstrip(L[++i])
                if (absd(i - clicked) < d) d = absd(i - clicked)
            }
            emit(text, d)
            # 折り返しでない行を誤ってつないだ場合に備え、元の各行の並びも出す
            if (i > first) for (j2 = first; j2 <= i; j2++) emit(L[j2], absd(j2 - clicked))
            i++
        }
    }' | sort -t "$(printf '\t')" -k1,1n -k2,2n | cut -f3-
}

# ctx_dirs <cwd> <クリックした行の番号> <折り返し桁>: 画面の行に出ているパスのディレクトリ
# （パスがファイルなら、その親）を、クリックした行に近い順に重複なく出す。
# パス自体が無くても親が実在すればそれを使う（説明の中で名前だけ変わったファイル等）
ctx_dirs() {
    cwd=$1
    git_root=$(bounded 3 git -C "$cwd" rev-parse --show-toplevel 2>/dev/null) || git_root=""
    seen="
"
    ctx_tokens "$2" "$3" | while IFS= read -r t; do
        f=$(try_candidate "$t" "$cwd") || f=$(try_candidate "$(dirname -- "$t")" "$cwd") || continue
        [ -d "$f" ] || f=$(dirname -- "$f")
        case $seen in *"
$f
"*) continue ;; esac
        seen="$seen$f
"
        printf '%s\n' "$f"
    done
}

# in_dirs <名前>: 名前（相対パス）を、標準入力のディレクトリ（1 行ずつ）の順に探す
in_dirs() {
    case $1 in
        ""|/*|"~"*|..|../*|*/..|*/../*) return 1 ;;
    esac
    while IFS= read -r d; do
        canonical "$d/$1" && return 0
    done
    return 1
}

# context <名前> <cwd> <クリックした行の番号> <折り返し桁>: 名前（相対パス）を、標準入力の
# 画面の行に出ているディレクトリから近い順に探す（「同ディレクトリの verdict.md」等）
context() {
    ctx_dirs "$2" "$3" "$4" | in_dirs "$1"
}

# is_sha <候補>: 7〜40 桁の小文字 16 進か（短い SHA の数 % は数字だけになるため英字は求めない）
is_sha() {
    case $1 in *[!0-9a-f]*|"") return 1 ;; esac
    [ "${#1}" -ge 7 ] && [ "${#1}" -le 40 ]
}

# commit_repo <候補> <ディレクトリ...>: 候補がコミットの SHA なら、順に見たディレクトリの
# リポジトリのうち、そのコミットを持つものを返す（同じリポジトリは 1 回だけ確かめる）
commit_repo() {
    sha=$1; shift
    is_sha "$sha" || return 1
    checked="
"
    for d in "$@"; do
        [ -d "$d" ] || continue
        r=$(bounded 3 git -C "$d" rev-parse --show-toplevel 2>/dev/null) || continue
        case $checked in *"
$r
"*) continue ;; esac
        checked="$checked$r
"
        if bounded 3 git -C "$r" rev-parse -q --verify "$sha^{commit}" >/dev/null 2>&1; then
            canonical "$r"; return
        fi
    done
    return 1
}

# PR / Issue の参照「owner/repo#番号」「#番号」・URL を「owner/repo<TAB>番号」にする awk の関数
# （文末の . は落とす。番号は 1 始まりの 10 桁まで、owner / repo は . だけの名前を除く）
GH_REF_AWK='
    function ghref(s,    i, nwo, n, a) {
        while (s ~ /\.$/) s = substr(s, 1, length(s) - 1)
        if (s !~ /^([A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+)?#[1-9][0-9]*$/) return ""
        i = index(s, "#"); nwo = substr(s, 1, i - 1); n = substr(s, i + 1)
        if (length(n) > 10) return ""
        if (nwo != "") { split(nwo, a, "/"); if (a[1] ~ /^\.+$/ || a[2] ~ /^\.+$/) return "" }
        return nwo "\t" n
    }
    # 語の先頭の GitHub の PR / Issue の URL（前の括弧・和文、後ろの句読点や /files・#… の続きは
    # 無視する）。別の URL の中（?next=https://github.com/…）や番号の続き（/pull/9-x）は対象外
    function ghurl(s,    a) {
        if (!match(s, /https?:\/\/(www\.)?github\.com\/[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+\/(pull|issues)\/[0-9]+/)) return ""
        if (substr(s, 1, RSTART - 1) ~ /[A-Za-z0-9\/:=?&%.@#_-]/) return ""
        if (substr(s, RSTART + RLENGTH) ~ /^([A-Za-z0-9_-]|\.[A-Za-z0-9])/) return ""
        s = substr(s, RSTART, RLENGTH)
        sub(/^https?:\/\/(www\.)?github\.com\//, "", s)
        split(s, a, "/")
        return ghref(a[1] "/" a[2] "#" a[4])
    }
    # 登録した省略形「名前#番号」（OPEN_PATH_GH_ALIAS = 空白区切りの「名前=owner/repo」）。
    # 名前は英数字・_・- だけ（. や / を含むものはパス・owner/repo#番号として扱う）
    function ghalias(s,    i, name, n, a, k, j, t) {
        while (s ~ /\.$/) s = substr(s, 1, length(s) - 1)
        if (s !~ /^[A-Za-z0-9_-]+#[1-9][0-9]*$/) return ""
        i = index(s, "#"); name = substr(s, 1, i - 1); n = substr(s, i + 1)
        k = split(ENVIRON["OPEN_PATH_GH_ALIAS"], a, /[ \t]+/)
        for (j = 1; j <= k; j++) {
            if (index(a[j], name "=") != 1) continue
            t = substr(a[j], length(name) + 2)
            if (t !~ /^[^\/]+\/[^\/]+$/) return ""
            return ghref(t "#" n)
        }
        return ""
    }
'

# gh_ref <行> <桁>: クリック位置の「#番号」「owner/repo#番号」・GitHub の PR / Issue の URL を
# 「owner/repo<TAB>番号」で出す（owner/repo が無ければ空。URL から読んだものは <TAB>url を足す）。
# OPEN_PATH_GH_ALIAS に登録した省略形（bp#番号 等）は owner/repo に置き換え、<TAB>alias を足す。
# 英字に続く #（省略形を除く）・見出しの ##・
# 文字参照の &# は対象外。URL の中のクリックは、どこをクリックしても URL 全体を読む
# （?q=#78 の #78 を単独の参照にしない）
gh_ref() {
    printf '%s\n' "$1" | OPEN_PATH_WIDE=$WIDE_RANGES LC_ALL=C awk -v x="$2" "$TEXT_AWK$GH_REF_AWK"'
    function refchar(c) { return c ~ /^[A-Za-z0-9_.\/#-]$/ }
    {
        k = chars($0, ch); col = 0; hit = 0; x += 0
        for (j = 1; j <= k; j++) {
            w = width(cp(ch[j]))
            if (x >= col && x < col + w) hit = j
            col += w
        }
        if (!hit || ch[hit] == " ") exit 1
        for (wl = hit; wl > 1 && ch[wl - 1] != " "; wl--) ;
        for (wr = hit; wr < k && ch[wr + 1] != " "; wr++) ;
        word = ""
        for (j = wl; j <= wr; j++) word = word ch[j]
        if (index(word, "://")) {
            # 空白なしで並んだ URL もあるため、クリック位置から前へ最も近い http(s):// から読む。
            # 直前が URL に使う文字（?next=https://… の = 等。区切りに使う括弧・カンマ・引用符は
            # 除く）なら別の URL の中なので、さらに前を探す。そうした候補しか無ければ
            # （URL=https://… 等）、最も前の候補から読む
            us = 0; first = 0
            for (j = hit; j >= wl && !us; j--) {
                t = ""
                for (m = j; m <= j + 7 && m <= wr; m++) t = t ch[m]
                if (t !~ /^https?:\/\//) continue
                first = j
                if (j == wl || ch[j - 1] !~ /^[A-Za-z0-9\/:=?&%.@#_+~!$*;-]$/) us = j
            }
            if (!us) us = first
            # クリック位置より前に URL が無ければ（#9、https://… の #9 等）、URL でない参照として読む
            if (us) {
                url = ""
                for (j = us; j <= wr; j++) url = url ch[j]
                if ((out = ghurl(url)) == "") exit 1
                print out "\turl"
                exit 0
            }
        }
        if (!refchar(ch[hit])) exit 1
        l = hit; r = hit
        while (l > 1 && refchar(ch[l - 1])) l--
        while (r < k && refchar(ch[r + 1])) r++
        # HTML の文字参照（&#123;）やパスの続き（@scope/pkg#9・~/a#9 等）は対象外
        if (l > 1 && ch[l - 1] ~ /^[&@~+%]$/) exit 1
        s = ""
        for (j = l; j <= r; j++) s = s ch[j]
        if ((out = ghref(s)) == "") {
            if ((out = ghalias(s)) == "") exit 1
            out = out "\talias"
        }
        print out
    }'
}

# gh_url <URL>: GitHub の PR / Issue の URL（OSC 8 のリンク等）を「owner/repo<TAB>番号」にする
gh_url() {
    printf '%s\n' "$1" | awk "$GH_REF_AWK"'
    NR == 1 {
        if ((out = ghurl($0)) == "") exit 1
        print out
    }'
}

# url_at <行> <桁>: クリック位置の http(s) の URL を出す。
# 前後の括弧・引用符・句読点は含めない（URL の中で対になる丸括弧は残す）。別の URL の中
# （?next=https://…）のクリックは外側の URL として読み、区切りの後に続く別の URL
# （https://a,https://b）の手前で止める
url_at() {
    printf '%s\n' "$1" | OPEN_PATH_WIDE=$WIDE_RANGES LC_ALL=C awk -v x="$2" "$TEXT_AWK"'
    # URL の文字（RFC 3986 の非予約文字・予約文字と %。角括弧と引用符は Markdown や文の
    # 区切りに使われるため除く。端末に出る URL は % で符号化されているものとし、空白なしで
    # 続く和文（https://…を見る）を含めないよう、多バイト文字は区切りにする）
    function urlchar(c) {
        return c < "\200" && c ~ /[A-Za-z0-9._~:\/?#@!$&()*+,;=%-]/
    }
    # a[i] から http:// か https:// に 1 文字以上続く並びが始まるか（a = chars の結果、k = 文字数）
    function scheme(a, k, i,    t, m) {
        t = ""
        for (m = i; m <= i + 8 && m <= k; m++) t = t a[m]
        return t ~ /^https?:\/\/./
    }
    # a[i] から、別の URL の中ではない URL が始まるか（直前が無いか、URL の続きに使う文字で
    # ない。区切りの括弧・カンマ・引用符は URL の続きとみなさない）
    function own(a, k, i) {
        return scheme(a, k, i) && (i == 1 || a[i - 1] !~ /^[A-Za-z0-9\/:=?&%.@#_+~!$*;-]$/)
    }
    # a[from..to] のうち URL が始まる位置: 後ろから見て最初の own。無ければ最も前の http(s)://
    function start(a, k, from, to,    j, first) {
        first = 0
        for (j = to; j >= from; j--) {
            if (!scheme(a, k, j)) continue
            first = j
            if (j == from || own(a, k, j)) return j
        }
        return first
    }
    function count(s, c,    n, i) {
        n = 0
        for (i = 1; i <= length(s); i++) if (substr(s, i, 1) == c) n++
        return n
    }
    {
        k = chars($0, ch); col = 0; hit = 0; x += 0
        for (j = 1; j <= k; j++) {
            w = width(cp(ch[j]))
            if (x >= col && x < col + w) hit = j
            col += w
        }
        if (!hit || !urlchar(ch[hit])) exit 1
        l = hit
        while (l > 1 && urlchar(ch[l - 1])) l--
        if (!(us = start(ch, k, l, hit))) exit 1
        r = hit
        while (r < k && urlchar(ch[r + 1]) && !own(ch, k, r + 1)) r++
        url = ""
        for (j = us; j <= r; j++) url = url ch[j]
        # 末尾の句読点と、対にならない閉じ括弧は URL に含めない
        while (url != "") {
            c = substr(url, length(url), 1)
            if (c ~ /[.,;:!?*]/ || (c == ")" && count(url, "(") < count(url, ")"))) url = substr(url, 1, length(url) - 1)
            else break
        }
        if (url !~ /^https?:\/\/./) exit 1
        print url
    }'
}

# gh_nwo <remote の URL>: github.com の remote（https・scp 形式・ssh://）の owner/repo
gh_nwo() {
    printf '%s\n' "$1" | awk "$GH_REF_AWK"'
    NR == 1 {
        s = $0
        if (!sub(/^(https?:\/\/([^@\/]*@)?|ssh:\/\/([^@\/]*@)?|git:\/\/|[^@\/:]*@)(www\.)?github\.com[:\/]/, "", s)) exit 1
        sub(/\/$/, "", s); sub(/\.git$/, "", s)
        if ((out = ghref(s "#1")) == "") exit 1
        sub(/\t.*/, "", out)
        if (out == "") exit 1
        print out
    }'
}

# gh_repo <ディレクトリ...>: 順に見たディレクトリのリポジトリのうち、github.com の remote を
# 持つ最初のものの owner/repo。remote は gh と同じく gh repo set-default で選んだもの →
# upstream → github → origin → その他の順に選ぶ（fork では #番号は元のリポジトリを指す）
gh_repo() {
    checked="
"
    for d in "$@"; do
        [ -d "$d" ] || continue
        r=$(bounded 3 git -C "$d" rev-parse --show-toplevel 2>/dev/null) || continue
        case $checked in *"
$r
"*) continue ;; esac
        checked="$checked$r
"
        remotes=$(git -C "$r" remote 2>/dev/null) || continue
        base=$(git -C "$r" config --get-regexp '^remote\..*\.gh-resolved$' 2>/dev/null |
            awk '$2 == "base" { sub(/^remote\./, "", $1); sub(/\.gh-resolved$/, "", $1); print $1; exit }')
        for name in $base upstream github origin $remotes; do
            case "
$remotes
" in *"
$name
"*) ;; *) continue ;; esac
            url=$(git -C "$r" remote get-url "$name" 2>/dev/null) || continue
            if nwo=$(gh_nwo "$url"); then printf '%s\n' "$nwo"; return 0; fi
        done
    done
    return 1
}

# valid_ref <owner/repo> <番号>: gh や URL に渡してよい参照か
valid_ref() {
    case $1 in */*) ;; *) return 1 ;; esac
    case $1$2 in *"
"*) return 1 ;; esac
    [ -n "$(printf '%s#%s\n' "$1" "$2" | awk "$GH_REF_AWK"'{ print ghref($0) }')" ]
}

# gh を対話・更新通知・pager なしで呼ぶ環境変数（remote が複数あると対話で既定を尋ねて
# 止まるため、呼び出しには常に -R / repos/<owner>/<repo> を付ける）
gh_env() {
    GH_PROMPT_DISABLED=1 GH_NO_UPDATE_NOTIFIER=1 GH_PAGER=cat
    export GH_PROMPT_DISABLED GH_NO_UPDATE_NOTIFIER GH_PAGER
}

# gh を打ち切るまでの秒数を secs に入れる（OPEN_PATH_GH_TIMEOUT、既定 15）
gh_secs() {
    secs=${OPEN_PATH_GH_TIMEOUT:-15}
    case $secs in ""|0*|*[!0-9]*) secs=15 ;; esac
}

# gh_kind <owner/repo> <番号>: PR なら pr、Issue なら issue（Issue の API は PR にも答え、
# PR には pull_request が付く）。OPEN_PATH_GH_TIMEOUT 秒（既定 15）で打ち切る
gh_kind() {
    valid_ref "$1" "$2" || return 1
    gh_secs
    gh_env
    # bounded には gh を直接渡す（関数を渡すと打ち切られるのはサブシェルだけで、gh が
    # 出力を開いたまま残り $(...) が待ち続ける）
    k=$(bounded "$secs" gh api "repos/$1/issues/$2" --jq 'if .pull_request then "pr" else "issue" end') || return 1
    case $k in pr|issue) echo "$k" ;; *) return 1 ;; esac
}

# local_file <相対パス> <cwd>: cwd → git ルートの順に、実在する通常のファイルの実体パス
local_file() {
    for base in "$2" "$(bounded 3 git -C "$2" rev-parse --show-toplevel 2>/dev/null)"; do
        [ -n "$base" ] && [ -f "$base/$1" ] && { canonical "$base/$1"; return; }
    done
    return 1
}

# open_ref [open]: click が見つけた PR / Issue（$ref = owner/repo<TAB>番号）を開く。owner/repo が
# 無ければ cwd → 前後の行のディレクトリのリポジトリから決める。click では通信しない
open_ref() {
    tab=$(printf '\t')
    nwo=${ref%%"$tab"*} num=${ref#*"$tab"}
    if [ -z "$nwo" ]; then
        nwo=$(gh_repo "$cwd")
        if [ -z "$nwo" ] && near_lines; then
            dirs=$(printf '%s\n' "$near" | ctx_dirs "$cwd" "$OPEN_NEAR_IDX" "$OPEN_NEAR_WRAP")
            # shellcheck disable=SC2086  # ディレクトリは 1 行ずつ渡す（glob は展開しない）
            nwo=$(IFS='
'; set -f; gh_repo $dirs)
        fi
    fi
    if ! valid_ref "$nwo" "$num"; then
        tmux display-message -c "$client" "open-path: PR / Issue の GitHub リポジトリが分かりません"
        return 0
    fi
    if [ "$1" = open ]; then
        # Issue の URL は PR でも GitHub が PR へ転送する（gh を使わず、未認証でも開ける）
        bounded 5 open_default "https://github.com/$nwo/issues/$num" >/dev/null 2>&1 ||
            tmux display-message -c "$client" "open-path: ブラウザで開けません"
        return 0
    fi
    tmux set -g @open_path_view_kind gh
    tmux set -g @open_path_view_repo "$nwo"
    tmux set -g @open_path_view_num "$num"
    popup="sh $(shquote "$0") view"
    # gh の本文は整形した Markdown なので、Markdown のプレビューと同じ大きさにする
    # shellcheck disable=SC2046  # 「幅 高さ」を 2 つの引数に分ける
    set -- $(popup_size --doc $(tmux display -p -c "$client" '#{client_width} #{client_height}' 2>/dev/null))
    if popup_title_supported; then
        title=$(printf ' %s#%s ' "$nwo" "$num" | sed 's/#/##/g')
        tmux display-popup -c "$client" -E -w "$1" -h "$2" -T "$title" "$popup"
    else
        tmux display-popup -c "$client" -E -w "$1" -h "$2" "$popup"
    fi
}

# near_lines: クリックした pane の前後（上 15 行・下 5 行）を出し、OPEN_NEAR_IDX に
# クリックした行の番号、OPEN_NEAR_WRAP に折り返し桁を入れる（コピーモード中は失敗）
near_lines() {
    pane=${cpane:-} y=${cy:-}
    case $y in ''|*[!0-9]*) return 1 ;; esac
    [ -n "$pane" ] || return 1
    # shellcheck disable=SC2046  # 「モード 高さ 幅 履歴」を 4 つに分ける
    set -- $(tmux display -p -t "$pane" '#{pane_in_mode} #{pane_height} #{pane_width} #{history_size}' 2>/dev/null)
    [ "${1:-1}" = 0 ] || return 1
    case ${2:-}${3:-} in ''|*[!0-9]*) return 1 ;; esac
    hist=${4:-0}
    case $hist in *[!0-9]*) hist=0 ;; esac
    # 範囲外を指定すると tmux は端に丸めるため、行の番号がずれないよう先に丸める
    start=$((y - 15)) end=$((y + 5))
    [ "$start" -ge $((0 - hist)) ] || start=$((0 - hist))
    [ "$end" -lt "$2" ] || end=$(($2 - 1))
    OPEN_NEAR_IDX=$((y - start)) OPEN_NEAR_WRAP=$(($3 / 2))
    near=$(tmux capture-pane -p -t "$pane" -S "$start" -E "$end" 2>/dev/null) || return 1
}

# digits <文字列>: 1 文字以上の数字だけか
digits() {
    case $1 in ''|*[!0-9]*) return 1 ;; esac
}

# web_url <URL>: ブラウザに渡してよい URL か（http / https だけ。空白・制御文字は含めない）
web_url() {
    case $1 in http://?*|https://?*) ;; *) return 1 ;; esac
    case $1 in *[[:space:]]*) return 1 ;; esac
    no_cntrl "$1"
}

# open_url <URL>: ブラウザ（既定アプリ）で開く。応答しない場合に run-shell が残らないよう打ち切る
open_url() {
    web_url "$1" || return 1
    bounded 5 open_default "$1" >/dev/null 2>&1 ||
        tmux display-message -c "$client" "open-path: ブラウザで開けません"
}

# url_click <行> <桁>: クリック位置の URL（url_at）。端末の幅で自動的に折り返された行
# （tmux が折り返しとして記録した行）の続きはつなぐ。クリックした画面の行から下の数行を
# capture-pane -J（折り返しをつなぐ）で読み、その最初の行（= クリックした行から始まる論理行）
# を使う。改行で折り返した行（Claude Code 等）は、画面の行だけでは折り返しと確かめられない
# ためつながない（Claude Code が出す URL は OSC 8 のリンクで、折り返しても全体を読める）。
# 続きの行のクリックも前の行とはつながない
url_click() {
    case $1 in *://*) ;; *) return 1 ;; esac
    logical=$1
    pane=${cpane:-} y=${cy:-}
    case $y in ''|*[!0-9]*) y="" ;; esac
    if [ -n "$pane" ] && [ -n "$y" ]; then
        # shellcheck disable=SC2046  # 「モード 高さ」を 2 つに分ける
        set -- "$1" "$2" $(tmux display -p -t "$pane" '#{pane_in_mode} #{pane_height}' 2>/dev/null)
        # コピーモード中は capture-pane（通常の画面）と表示中の行が一致しないため見ない。
        # 範囲外の行は tmux が端の行に丸めるため、最下行より下は読まない
        if [ "${3:-1}" = 0 ] && digits "${4:-}" && [ "$y" -lt "$4" ]; then
            # 待つ間に画面の行が書き換わっていれば（出力が続く pane 等）、つながない
            row=$(tmux capture-pane -p -t "$pane" -S "$y" -E "$y" 2>/dev/null)
            if [ "$(rtrim "$row")" = "$(rtrim "$1")" ]; then
                last=$((y + 3))
                [ "$last" -lt "$4" ] || last=$(($4 - 1))
                joined=$(tmux capture-pane -p -t "$pane" -S "$y" -E "$last" -J 2>/dev/null | head -n 1)
                case $joined in "$row"*) logical=$joined ;; esac
            fi
        fi
    fi
    url_at "$logical" "$2"
}

# rtrim <文字列>: 末尾の空白を除く
rtrim() {
    printf '%s' "${1%"${1##*[! ]}"}"
}

click() {
    mode=${1:-preview}
    # 普通のクリックは、ボタンを離した時の押下の通し番号（@open_path_seq）を受け取る。
    # 待つ間に次の押下（ダブルクリック等）があって番号が進んでいたら、何もしない
    if [ "$mode" = link ] && [ -n "${2:-}" ] && [ "$2" != "$(opt seq)" ]; then
        return 0
    fi
    [ "$mode" = link ] && OPEN_CTX=up_
    # クリックした位置はここで一度に読み、以降（wrapped・near_lines・url_click も）この値だけを使う
    client=$(ctx client) link=$(ctx link) line=$(ctx line) x=$(ctx x) cwd=$(ctx cwd) cpane=$(ctx pane) cy=$(ctx y)
    # 読む間に次のクリックが来て書き換えた値かもしれないため、読んだ後にも通し番号を確かめる
    if [ "$mode" = link ] && [ -n "${2:-}" ] && [ "$2" != "$(opt seq)" ]; then
        return 0
    fi
    # 追加の探索ルート（任意）: set -g @open_path_roots "$HOME/work:$HOME/src"
    roots=$(opt roots)
    if [ -n "$roots" ]; then OPEN_PATH_ROOTS=$roots; export OPEN_PATH_ROOTS; fi
    # PR / Issue の省略形（任意。同名のファイルより優先）:
    # set -g @open_path_gh_alias "bp=owner/repo int=owner/repo2"
    gh_alias=$(opt gh_alias)
    if [ -n "$gh_alias" ]; then OPEN_PATH_GH_ALIAS=$gh_alias; export OPEN_PATH_GH_ALIAS; fi
    res="" ref="" tok="" url=""
    case $link in
        file://*) res=$(resolve "$link" "$cwd") ;;
        http://*|https://*) web_url "$link" && url=$link ;;
    esac
    if [ -z "$res$url" ]; then
        url=$(url_click "$line" "$x") || url=""
    fi
    # URL: PR / Issue の URL はプレビューでは gh の本文を開く。それ以外（普通のクリック・既定アプリ・
    # GitHub 以外の URL）は、クリックした URL をそのまま（#issuecomment 等の続きも）ブラウザで開く
    if [ -n "$url" ]; then
        if [ "$mode" = preview ] && ref=$(gh_url "$url"); then
            open_ref preview
        else
            open_url "$url"
        fi
        return 0
    fi
    # PR / Issue の参照（#番号・owner/repo#番号・URL）はファイルより先に見る。ただし
    # owner/repo#番号（URL・省略形は除く）の全体か owner/repo が cwd → git ルートの配下に実在する
    # ファイルなら、ファイルとして開く（docs/README.md#123 等。ディレクトリは除く: ghq の
    # ルートの ysano/dotfiles#78 等）
    tab=$(printf '\t')
    if [ -z "$res$ref" ] && ref=$(gh_ref "$line" "$x"); then
        case $ref in
            "$tab"*) ;;
            *"${tab}url"|*"${tab}alias") ref=${ref%"$tab"*} ;;
            *)
                if f=$(local_file "${ref%%"$tab"*}#${ref#*"$tab"}" "$cwd"); then
                    res="$f$tab" ref=""
                elif local_file "${ref%%"$tab"*}" "$cwd" >/dev/null; then
                    ref=""
                fi ;;
        esac
    fi
    if [ -n "$ref" ]; then
        # 普通のクリックは、PR / Issue をブラウザで開く
        if [ "$mode" = link ]; then open_ref open; else open_ref "$mode"; fi
        return 0
    fi
    # 普通のクリックは URL・PR / Issue だけを扱う（ファイル・SHA・ただの語では何もしない）
    [ "$mode" = link ] && return 0
    if [ -z "$res" ]; then
        tok=$(extract "$line" "$x") && res=$(resolve "$tok" "$cwd")
    fi
    if [ -z "$res" ] && [ -n "${tok:-}" ]; then
        res=$(wrapped "$line" "$x" "$cwd")
    fi
    # 見つからなければ、前後の行に出ているパスのディレクトリから探す。コミットの SHA なら
    # cwd → 前後の行のディレクトリのリポジトリの順に探し、差分を開く
    # （git の呼び出しが積み上がらないよう、SHA は cwd を先に確かめ、前後の行は 1 回だけ読む）
    repo=""
    if [ -z "$res" ] && [ -n "${tok:-}" ] && parsed=$(parse "$tok"); then
        tab=$(printf '\t')
        name=${parsed%%"$tab"*}
        is_sha "$name" && repo=$(commit_repo "$name" "$cwd")
        if [ -z "$repo" ] && near_lines; then
            dirs=$(printf '%s\n' "$near" | ctx_dirs "$cwd" "$OPEN_NEAR_IDX" "$OPEN_NEAR_WRAP")
            if f=$(printf '%s\n' "$dirs" | in_dirs "$name"); then
                res="$f$tab${parsed#*"$tab"}"
            elif is_sha "$name"; then
                # shellcheck disable=SC2086  # ディレクトリは 1 行ずつ渡す（glob は展開しない。改行を含む名前は対象外）
                repo=$(IFS='
'; set -f; commit_repo "$name" $dirs)
            fi
        fi
    fi
    if [ -n "$repo" ]; then
        [ "$mode" = open ] && { tmux display-message -c "$client" "open-path: コミットは既定アプリで開けません"; return 0; }
        tmux set -g @open_path_view_kind commit
        tmux set -g @open_path_view_path "$repo"
        tmux set -g @open_path_view_sha "$name"
        popup="sh $(shquote "$0") view"
        if popup_title_supported; then
            title=$(printf ' %s  %s ' "$name" "$(basename -- "$repo")" | sed 's/#/##/g')
            tmux display-popup -c "$client" -E -w 85% -h 85% -T "$title" "$popup"
        else
            tmux display-popup -c "$client" -E -w 85% -h 85% "$popup"
        fi
        return 0
    fi
    if [ -z "$res" ]; then
        tmux display-message -c "$client" "open-path: ファイルが見つかりません"
        return 0
    fi
    tab=$(printf '\t')
    path=${res%%"$tab"*} lineno=${res#*"$tab"}
    if [ "$mode" = open ]; then
        # 実在確認を通った絶対パスだけを渡す。応答しない場合に run-shell が残らないよう打ち切る
        bounded 5 open_default "$path" >/dev/null 2>&1 ||
            tmux display-message -c "$client" "open-path: 既定アプリで開けません"
        return 0
    fi
    # display-popup の -d やコマンド文字列はフォーマット展開されうるため、画面由来の
    # 値（パス等）は載せない。view へは展開なしの set -g で渡し、-d も使わない
    tmux set -g @open_path_view_kind ""
    tmux set -g @open_path_view_path "$path"
    tmux set -g @open_path_view_line "$lineno"
    popup="sh $(shquote "$0") view"
    # shellcheck disable=SC2046  # 「幅 高さ」を 2 つの引数に分ける
    set -- $(popup_size "$path" $(tmux display -p -c "$client" '#{client_width} #{client_height}' 2>/dev/null))
    if popup_title_supported; then
        # -T はフォーマットなので # を ## にエスケープする（#() の実行を防ぐ）
        title=$(printf ' %s ' "$path" | sed 's/#/##/g')
        tmux display-popup -c "$client" -E -w "$1" -h "$2" -T "$title" "$popup"
    else
        tmux display-popup -c "$client" -E -w "$1" -h "$2" "$popup"
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

# less の終了コード → 動作（open-path.lesskey の quit e / quit o / quit r / quit c）
action() {
    case $1 in
        101) echo emacs ;;
        111) echo default ;;
        114) echo toggle ;;
        99) echo claude ;;
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

# glow_dir <ディレクトリ>: ls -la の代わりに glow の一覧（glow -t）で開くか。glow が一覧に出す
# Markdown（拡張子は glow と同じ。隠し・node_modules の中は glow も出さない）があるときだけ yes。
# 一覧の起点そのものが隠しディレクトリ（.claude 等）だと glow は 1 件も出さないため対象外にする。
# git 管理下は、glow と同じく追跡しているかに関わらず .gitignore で無視されるものを数えない
# （check-ignore --no-index）。-z の出力は 1 件ごとに「規則のファイル・行番号・規則・パス」の
# 4 つで、どの規則にも合わなければ規則が空、否定（!）の規則で含め直されていれば ! 始まり。
# 名前の改行で 4 つの区切りがずれないよう、改行を ? に替えてから NUL を改行にする。
# 管理外は 1 件見つけたら find を止める（-quit。読むのをやめても find は次の書き込みまで
# 走り続ける）
glow_dir() {
    [ -d "$1" ] && command -v glow >/dev/null 2>&1 || return 1
    base=${1%/}
    case ${base##*/} in .*) return 1 ;; esac
    # glow は ~/Library（macOS だけ）と $GOPATH も起点にすると 1 件も出さない（その下は出す）
    [ -n "${HOME:-}" ] && [ "$base" = "${HOME%/}/Library" ] && [ "$(uname -s 2>/dev/null)" = Darwin ] &&
        return 1
    [ -n "${GOPATH:-}" ] && [ "$base" = "${GOPATH%/}" ] && return 1
    if [ "$(git -C "$1" rev-parse --is-inside-work-tree 2>/dev/null)" = true ]; then
        found=$(md_files "$1" -print0 | git -C "$1" check-ignore --no-index --stdin -z -v -n 2>/dev/null |
            tr '\n\000' '?\n' | awk 'NR % 4 == 3 && (/^$/ || /^!/) { print "yes"; exit }')
    else
        found=$(md_files "$1" -print -quit | head -n 1)
    fi
    [ -n "$found" ] || return 1
    echo yes
}

# md_files <ディレクトリ> <find の出力の指定...>: glow が一覧に出しうる Markdown（拡張子は glow と
# 同じく大文字・小文字を区別しない。隠し・node_modules の中は glow も出さない）を、そのディレクトリ
# からの相対パス（./ 始まり）で出す。大きな木で待たされないよう深さ 4・3 秒で打ち切る
md_files() {
    mdir=$1
    shift
    (cd -- "$mdir" 2>/dev/null && bounded 3 find . -maxdepth 4 \
        -type d \( -name '.?*' -o -name node_modules \) -prune \
        -o -type f ! -name '.*' \( -iname '*.md' -o -iname '*.mdown' -o -iname '*.mkdn' -o -iname '*.mkd' \
            -o -iname '*.markdown' \) "$@" 2>/dev/null)
}

# 端末（ポップアップ）の桁数。render は less へのパイプの中で動くため、tput は stdout から
# 幅を取れず terminfo の既定の 80 を返す。制御端末から読み、読めなければ tput に任せる。
# 呼び出し側は算術展開に使うため、空・0・先頭 0（8 進として失敗する）・数字以外は 100 にする
term_cols() {
    tc=$(stty size 2>/dev/null </dev/tty | awk '{ print $2 }')
    case $tc in ""|0*|*[!0-9]*) tc=$(tput cols 2>/dev/null) ;; esac
    case $tc in ""|0*|*[!0-9]*) tc=100 ;; esac
    echo "$tc"
}

# 標準入力の差分を、delta があれば git の pager 設定（~/.config/git/pager.sh）と同じく
# 整形する（幅 120 以上は左右に並べる）。ポップアップでは less に渡すため delta 自身は page しない
pretty_diff() {
    if command -v delta >/dev/null 2>&1; then
        if [ "$1" -ge 120 ]; then delta --paging=never --width "$1" --side-by-side
        else delta --paging=never --width "$1"; fi
    else
        cat
    fi
}

# プレビューする中身を標準出力へ。mode=md は Markdown を整形（パイプでも色を付ける）、
# mode=raw は原文（行番号付きは折り返さず、対象行に ▶ を付ける）。差分・PR / Issue・Markdown の
# 整形の幅は、ポップアップ（端末）の幅に合わせる
render() {
    cols=$(term_cols)
    if [ "$1" = commit ]; then
        git -C "$path" -c core.quotePath=false show --stat --patch --color=always "$sha" | pretty_diff "$cols"
        return
    fi
    if [ "$1" = gh ]; then
        valid_ref "$nwo" "$num" || return 1
        gh_secs
        gh_env
        # GH_FORCE_TTY: パイプに出しても端末と同じく整形・色付けする（値は折り返し桁）。
        # 止まった gh を待ち続けると less を抜けてもポップアップが閉じないため、gh_kind と同じ秒数で
        # 打ち切る（bounded には gh を直接渡す）
        case $kind$part in
            prdiff) bounded "$secs" env GH_FORCE_TTY="$cols" gh pr diff "$num" -R "$nwo" --color=always | pretty_diff "$cols" ;;
            pr*|issue*) bounded "$secs" env GH_FORCE_TTY="$cols" gh "$kind" view "$num" -R "$nwo" --comments ;;
            *) return 1 ;;
        esac
        return
    fi
    if [ "$1" = md ]; then
        width=$(( cols - 2 ))
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
        # タブは bat と同じ 4 桁に展開する（popup_size の幅の計算とそろえる）
        awk '{ gsub(/\t/, "    "); printf "%6d  %s\n", NR, $0 }' "$path"
    fi | if [ -n "$lineno" ]; then mark "$lineno"; else cat; fi
}

# claude_text <file|gh|commit> ...: Claude Code の入力欄に入れる、文書を指す文字列（後ろに空白）。
#   file <パス> [行番号]: @パス（行番号は (L42) と添える。@ の行指定は文書化されていないため）。
#     空白を含むパスは @ の書き方が文書化されていないため、引用符で囲むだけにする
#   gh <owner/repo> <番号> <pr|issue|空>: URL / commit <リポジトリ> <SHA>: commit <SHA> (<リポジトリ>)
# 改行等の制御文字を含むものは渡さない（入力欄で送信されうるため）
claude_text() {
    no_cntrl "$*" || return 1
    case $1 in
        file)
            case $2 in
                *" "*) printf '"%s" %s' "$2" "${3:+(L$3) }" ;;
                *) printf '@%s %s' "$2" "${3:+(L$3) }" ;;
            esac ;;
        gh)
            if [ "$4" = pr ]; then printf 'https://github.com/%s/pull/%s ' "$2" "$3"
            else printf 'https://github.com/%s/issues/%s ' "$2" "$3"; fi ;;
        commit) printf 'commit %s (%s) ' "$3" "$2" ;;
        *) return 1 ;;
    esac
}

# no_cntrl <文字列>: 制御文字（改行・ESC 等）を含まないか（tr はバイト単位で、UTF-8 の
# 多バイト文字は 0x80 以上のバイトだけなので影響しない）
no_cntrl() {
    [ "$(printf '%s' "$1" | tr -d '\000-\037\177')" = "$1" ]
}

# claude_ancestors: 標準入力の ps の表（pid ppid 実行ファイル）から、実行ファイル名が
# claude / claude.exe のプロセスとその祖先の pid を出す（agents/claude_status.py と同じ判定。
# npm 版は node が前面に出るため、pane のプロセスの子孫を実行ファイル名で見る）
claude_ancestors() {
    awk '{
        # 実行ファイルのパスは空白を含みうるため、pid・ppid より後ろ全体を使う
        pp[$1] = $2; n = $0; sub(/^[ \t]*[0-9]+[ \t]+[0-9]+[ \t]+/, "", n); sub(/.*\//, "", n)
        if (n == "claude" || n == "claude.exe") hit[$1] = 1
    }
    END {
        for (p in hit)
            for (q = p; (q in pp) && !(q in seen); q = pp[q]) { seen[q] = 1; print q }
    }'
}

# claude_pane <pane>: クリックした pane が Claude Code ならそれ、違えば同じ window の
# Claude Code の pane（最初のもの）
claude_pane() {
    # awk -v には改行を渡せない（BSD awk）ため空白区切りにする
    pids=$(ps -axo pid=,ppid=,comm= 2>/dev/null | claude_ancestors | tr '\n' ' ')
    [ -n "$pids" ] || return 1
    tmux list-panes -t "$1" -F '#{pane_id} #{pane_pid}' 2>/dev/null |
        awk -v me="$1" -v pids="$pids" '
            BEGIN { n = split(pids, a, " "); for (i = 1; i <= n; i++) c[a[i]] = 1 }
            ($2 in c) { if ($1 == me) { print $1; found = 1; exit } if (first == "") first = $1 }
            END { if (!found && first != "") print first; exit (!found && first == "") }'
}

# to_claude <文字列>: クリックした pane（Claude でなければ同じ window の Claude Code）の入力欄へ
# 貼り付け（bracketed paste。@ の補完メニューを開かない）、その pane を選ぶ。Enter は送らない
to_claude() {
    if [ -z "$1" ] || ! no_cntrl "$1"; then
        tmux display-message "open-path: Claude Code に渡せない名前です（制御文字を含む等）"
        return 1
    fi
    pane=$(opt pane)
    if [ -z "$pane" ] || ! target=$(claude_pane "$pane") || [ -z "$target" ]; then
        tmux display-message "open-path: この window に Claude Code の pane が見つかりません"
        return 1
    fi
    # コピーモードの画面には bracketed paste が効かず改行が Enter になるため、先に解除する
    case $(tmux display -p -t "$target" '#{pane_in_mode}' 2>/dev/null) in
        1*) tmux send-keys -t "$target" -X cancel ;;
    esac
    tmux set-buffer -b open-path-claude "$1" &&
        tmux paste-buffer -p -d -b open-path-claude -t "$target" &&
        tmux select-pane -t "$target"
}

do_action() {
    case $1 in
        claude) to_claude "$(claude_text file "$path" "$lineno")" ;;
        emacs) open_emacs "$path" "$lineno" || { printf '\nEmacs に接続できません: %s\n' "$path"; sleep 2; } ;;
        default) open_default "$path" || { printf '\n既定アプリで開けません: %s\n' "$path"; sleep 2; } ;;
    esac
}

# 引数が無ければ click が set -g した @open_path_view_* を読む
# コミットの差分を less で見る（q で閉じる、c で Claude Code に渡す）
view_commit() {
    case $sha in ""|*[!0-9a-f]*) printf 'open-path: SHA ではありません: %s\n' "$sha"; sleep 2; return 1 ;; esac
    [ -d "$path" ] || { printf 'open-path: 見つかりません: %s\n' "$path"; sleep 2; return 1; }
    LESS=""
    export LESS
    keys="$(cd "$(dirname "$0")" && pwd)/open-path.lesskey"
    if ! [ -f "$keys" ] || ! lesskey_ok "$(less --version 2>/dev/null | head -n 1)" >/dev/null; then
        render commit | less -R
        return 0
    fi
    while :; do
        render commit | less -R --mouse --wheel-lines=3 --lesskey-src="$keys" "-Psq 閉じる  c Claude"
        case $(action $?) in
            # r（他の表示の切替キー）は描き直し、e / o は閉じる
            toggle) continue ;;
            claude) to_claude "$(claude_text commit "$path" "$sha")" ;;
        esac
        return 0
    done
}

# PR / Issue を less で見る（o でブラウザ、c で Claude Code に渡す、PR は r で本文・コメントと
# 差分を切り替える）
view_gh() {
    if ! valid_ref "$nwo" "$num"; then
        printf 'open-path: PR / Issue の参照ではありません: %s#%s\n' "$nwo" "$num"; sleep 2; return 1
    fi
    url="https://github.com/$nwo/issues/$num"
    LESS=""
    export LESS
    printf '読み込み中: %s#%s\n' "$nwo" "$num"
    err=""
    if ! command -v gh >/dev/null 2>&1; then
        kind="" err="gh（GitHub CLI）がありません"
    elif ! kind=$(gh_kind "$nwo" "$num" 2>/dev/null); then
        # 失敗の理由（未認証・404 等）を見せるため、もう一度だけ stderr を取る
        kind="" err=$(gh_kind "$nwo" "$num" 2>&1 >/dev/null)
    fi
    keys="$(cd "$(dirname "$0")" && pwd)/open-path.lesskey"
    if ! [ -f "$keys" ] || ! lesskey_ok "$(less --version 2>/dev/null | head -n 1)" >/dev/null; then
        if [ -n "$kind" ]; then render gh; else printf '%s\n\n%s\n' "${err:-取得できません}" "$url"; fi | less -R
        return 0
    fi
    part=body
    while :; do
        prompt="q 閉じる  o ブラウザ  c Claude"
        [ "$kind" = pr ] && prompt="$prompt  r 本文/差分"
        if [ -n "$kind" ]; then
            render gh
        else
            printf 'open-path: %s#%s を取得できません\n\n%s\n\n%s\n' "$nwo" "$num" "${err:-}" "$url"
        fi | less -R --mouse --wheel-lines=3 --lesskey-src="$keys" "-Ps$prompt"
        case $(action $?) in
            toggle)
                if [ "$kind" = pr ]; then
                    if [ "$part" = body ]; then part="diff"; else part="body"; fi
                fi
                continue ;;
            default)
                bounded 5 open_default "$url" >/dev/null 2>&1 || { printf '\nブラウザで開けません: %s\n' "$url"; sleep 2; } ;;
            claude) to_claude "$(claude_text gh "$nwo" "$num" "$kind")" ;;
        esac
        return 0
    done
}

view() {
    if [ $# -eq 0 ]; then
        case $(opt view_kind) in
            commit) path=$(opt view_path) sha=$(opt view_sha); view_commit; return ;;
            gh) nwo=$(opt view_repo) num=$(opt view_num); view_gh; return ;;
        esac
    fi
    if [ $# -gt 0 ]; then
        path=$1 lineno=${2:-}
    else
        path=$(opt view_path) lineno=$(opt view_line)
    fi
    case $lineno in *[!0-9]*) lineno="" ;; esac
    [ -f "$path" ] || [ -d "$path" ] || { printf 'open-path: 見つかりません: %s\n' "$path"; sleep 2; return 1; }
    # Markdown を含むディレクトリは ls -la の代わりに glow の一覧で開く（Enter で読む・Esc で
    # 一覧へ・q で閉じる。一覧から開いた文書はファイルの変更に追従する）。幅は Markdown の整形と
    # そろえ、ホイールで動かせるようにする（マウスは隠しオプションのため、廃止されても起動が
    # 止まらない環境変数で渡す）。起動に失敗したら ls -la の一覧に戻る。
    # Markdown ファイルは渡さない: glow 3.0 の TUI はファイルを直接渡すと最初の描画が折り返されない
    if [ -d "$path" ] && glow_dir "$path" >/dev/null &&
        GLOW_MOUSE=true glow -t -s dark -w "$(( $(term_cols) - 2 ))" "$path"; then
        return 0
    fi
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
        prompt="q 閉じる  e Emacs  o 既定アプリ  c Claude${md:+  r 整形/原文}"
        while :; do
            start=1
            [ "$mode" = raw ] && start=${lineno:-1}
            # --mouse: ホイールでスクロールする（tmux のポップアップは中のアプリへマウスを渡す）
            render "$mode" | less -R -j.3 --mouse --wheel-lines=3 "+${start}g" --lesskey-src="$keys" "-Ps$prompt"
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
    printf '\n[e] Emacs で開く  [o] 既定アプリで開く  [c] Claude に渡す  [その他] 閉じる: '
    old=$(stty -g 2>/dev/null)
    stty -icanon -echo min 1 2>/dev/null
    key=$(dd bs=1 count=1 2>/dev/null)
    [ -n "$old" ] && stty "$old" 2>/dev/null
    case $key in
        e) do_action emacs ;;
        o) do_action default ;;
        c) do_action claude ;;
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
    glow_dir) glow_dir "$@" ;;
    lesskey_ok) lesskey_ok "$@" ;;
    popup_size) popup_size "$@" ;;
    render) mode=${1:-raw} path=${2:-} lineno=${3:-} sha=${3:-} nwo=${2:-} num=${3:-} kind=${4:-} part=${5:-body}
            render "$mode" ;;
    context) context "$@" ;;
    commit_repo) commit_repo "$@" ;;
    gh_ref) gh_ref "$@" ;;
    gh_url) gh_url "$@" ;;
    url_at) url_at "$@" ;;
    gh_nwo) gh_nwo "$@" ;;
    gh_repo) gh_repo "$@" ;;
    gh_kind) gh_kind "$@" ;;
    claude_text) claude_text "$@" ;;
    claude_ancestors) claude_ancestors ;;
    to_claude) to_claude "$@" ;;
    *) echo "usage: open-path.sh {click|extract|parse|resolve|context|commit_repo|gh_ref|gh_url|url_at|gh_nwo|gh_repo|gh_kind|claude_text|claude_ancestors|to_claude|view|mark|action|lesskey_ok|md_renderer|is_markdown|glow_dir|popup_size|render} ..." >&2; exit 2 ;;
esac
