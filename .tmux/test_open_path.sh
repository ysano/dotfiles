#!/bin/bash
# scripts/open-path.sh（クリックしたファイルパスのプレビュー）の検証
# 使い方: bash test_open_path.sh
set -u
SCRIPT="$(cd "$(dirname "$0")" && pwd)/scripts/open-path.sh"
work=$(mktemp -d) && [ -d "$work" ] || { echo "FAIL mktemp -d"; exit 1; }
# resolve は実体パスを返す（macOS の /var → /private/var 等）ため期待値も実体パスにそろえる
work=$(cd "$work" && pwd -P)
trap 'rm -rf "$work"' EXIT
fail=0
TAB=$(printf '\t')
# 利用者の git 設定（core.quotePath=false 等）で結果が変わらないよう、全体設定を無効にする
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1

# check <名前> <期待値> <コマンド...>（期待値が空なら非 0 終了も確認する）
check() {
    local name="$1" want="$2" out rc
    shift 2
    out=$("$@" 2>/dev/null); rc=$?
    if [ "$out" = "$want" ] && { [ -n "$want" ] || [ "$rc" -ne 0 ]; }; then
        echo "ok   $name"
    else
        echo "FAIL $name: got '$out' (rc=$rc) want '$want'"
        fail=1
    fi
}

# --- extract <行> <クリック桁(0 起点)>: クリック位置のパス候補を切り出す ---
check "行番号・桁付き"           "src/foo.ts:42:7"   sh "$SCRIPT" extract "  src/foo.ts:42:7 error" 5
check "括弧と句点で止まる"       "README.md"         sh "$SCRIPT" extract "see (README.md)." 7
check "バッククォートで止まる"   ".tmux/base.conf"   sh "$SCRIPT" extract 'edit `.tmux/base.conf` now' 8
check "全角の後ろの桁を補正"     "docs/設計.md"      sh "$SCRIPT" extract "日本語 docs/設計.md を見る" 9
check "全角文字上のクリック"     "docs/設計.md"      sh "$SCRIPT" extract "日本語 docs/設計.md を見る" 12
check "全角括弧で止まる"         ".tmux/balance.conf" sh "$SCRIPT" extract "読み込み（.tmux/balance.conf）を確認" 12
check "読点で止まる"             ".tmux/ci.sh"       sh "$SCRIPT" extract "修正: .tmux/ci.sh、.tmux/test.sh" 8
check "読点の後ろも切り出す"     ".tmux/test.sh"     sh "$SCRIPT" extract "修正: .tmux/ci.sh、.tmux/test.sh" 20
check "かぎ括弧で止まる"         "link.sh"           sh "$SCRIPT" extract "「link.sh」を開く" 4
check "罫線は 1 桁（表の中）"    "scripts/link.sh"   sh "$SCRIPT" extract "│ a │ b │ scripts/link.sh │" 10
check "罫線上は候補なし"         ""                  sh "$SCRIPT" extract "│ a │ b │" 4
check "罫線に接したパス"         "link.sh"           sh "$SCRIPT" extract "│link.sh│" 3
check "中黒で止まる"             "docs/a.md"         sh "$SCRIPT" extract "docs/a.md・3 件" 3
# extract <行> <桁> <前の行> <次の行>: 行末・行頭で折り返されたパスをつないだ候補も出す（長い順）
wrapped=$(printf '%s\n' "/tmp/wt-karin/x/pr.md" "/tmp/wt-ka")
check "次の行へ折り返したパス"   "$wrapped" sh "$SCRIPT" extract "  本文は /tmp/wt-ka" 12 "" "     rin/x/pr.md。採否" 10
check "前の行から折り返したパス" "$(printf '%s\n' "/tmp/wt-karin/x/pr.md" "rin/x/pr.md")" sh "$SCRIPT" extract "     rin/x/pr.md。採否" 7 "  本文は /tmp/wt-ka" "" 10
check "短い行は折り返しとみなさない" "READ"  sh "$SCRIPT" extract "READ" 1 "" " ME.md" 20
check "継続行は字下げが要る"     "/tmp/wt-ka"        sh "$SCRIPT" extract "  本文は /tmp/wt-ka" 12 "" "rin/x/pr.md" 10
check "絵文字は 2 桁"            "a.txt"             sh "$SCRIPT" extract "⌚ a.txt b.txt" 7
check "候補は長い順"             "$(printf '%s\n' path/to/longer/foo.md path/to/longer/foo foo.md foo)" sh "$SCRIPT" extract "          foo" 11 "path/to/longer/" " .md" 10
check "補助平面の全角は 2 桁"    "a.txt"             sh "$SCRIPT" extract "$(printf '\360\233\200\200 a.txt b.txt')" 7
check "補助私用領域は 1 桁"      "a.txt"             sh "$SCRIPT" extract "$(printf '\363\260\200\200 a.txt b.txt')" 2
check "行の途中なら折り返しを見ない" "a/b" sh "$SCRIPT" extract "see a/b here" 5 "x/y" "z/w"
check "diff の b/ 側"          "b/link.sh"         sh "$SCRIPT" extract "diff a/link.sh b/link.sh" 17
check "チルダ"                   "~/dotfiles/link.sh" sh "$SCRIPT" extract "~/dotfiles/link.sh" 3
check "空白上は候補なし"         ""                  sh "$SCRIPT" extract "foo bar" 3
check "行末より右は候補なし"     ""                  sh "$SCRIPT" extract "foo" 10

# --- parse <候補>: 末尾の記号を落とし、パスと行番号に分ける ---
check "パス:行:桁"   "src/foo.ts${TAB}42" sh "$SCRIPT" parse "src/foo.ts:42:7"
check "パス:行"      "src/foo.ts${TAB}42" sh "$SCRIPT" parse "src/foo.ts:42"
check "パス:行:"     "main.go${TAB}12"    sh "$SCRIPT" parse "main.go:12:"
check "末尾の句点"   "README.md${TAB}"    sh "$SCRIPT" parse "README.md."
check "末尾のコロン" "Error${TAB}"        sh "$SCRIPT" parse "Error:"

# --- mark <行番号>: プレビューで対象行に ▶ を付ける（テーマの強調色が薄くても見える） ---
ESC=$(printf '\033')
check "対象行に目印" "  a${TAB}${ESC}[1;33m▶${ESC}[0m b${TAB}  c" sh -c 'printf "a\nb\nc\n" | sh "$1" mark 2 | paste -s -' _ "$SCRIPT"

# --- action <less の終了コード>: lesskey の quit e / quit o（ASCII 101 / 111）を動作に変換 ---
check "less の e → Emacs"        "emacs"   sh "$SCRIPT" action 101
check "less の o → 既定アプリ"   "default" sh "$SCRIPT" action 111
check "less の q → 閉じる"       "close"   sh "$SCRIPT" action 0
check "それ以外 → 閉じる"        "close"   sh "$SCRIPT" action 2

check "less の r → 整形/原文の切替" "toggle" sh "$SCRIPT" action 114
check "less の c → Claude に渡す"   "claude" sh "$SCRIPT" action 99

# --- md_renderer: Markdown の整形に使うツール（glow → mdcat → なし） ---
mdbin="$work/mdbin"; mkdir -p "$mdbin/both" "$mdbin/mdcat" "$mdbin/none"
for t in glow mdcat; do printf '#!/bin/sh\necho %s "$@"\n' "$t" > "$mdbin/both/$t"; chmod +x "$mdbin/both/$t"; done
cp "$mdbin/both/mdcat" "$mdbin/mdcat/mdcat"
for d in both mdcat none; do for c in sh awk sed cat; do ln -sf "$(command -v $c)" "$mdbin/$d/$c"; done; done
check "整形は glow を優先"     "glow"  env PATH="$mdbin/both" sh "$SCRIPT" md_renderer
check "glow が無ければ mdcat"  "mdcat" env PATH="$mdbin/mdcat" sh "$SCRIPT" md_renderer
check "どちらも無ければなし"   ""      env PATH="$mdbin/none" sh "$SCRIPT" md_renderer

# --- render md: 整形の幅はポップアップ（端末）の幅に合わせる ---
# render は less へのパイプの中で動くため、stdout からは端末の幅が取れない。幅 120 の端末を
# 別ソケットの tmux で用意し、偽の glow に渡った -w を見る
if command -v tmux >/dev/null 2>&1; then
    printf '# t\n' > "$work/w.md"
    sock="open-path-test-$$"
    tmux -L "$sock" -f /dev/null new-session -d -x 120 -y 20 \
        "env PATH='$mdbin/both:$PATH' sh '$SCRIPT' render md '$work/w.md' > '$work/w.out' 2>&1; touch '$work/w.done'"
    for _ in $(seq 50); do [ -f "$work/w.done" ] && break; sleep 0.1; done
    tmux -L "$sock" kill-server 2>/dev/null
    check "整形の幅は端末の幅 − 2" "glow -s dark -w 118 $work/w.md" cat "$work/w.out"
else
    echo "skip render md の幅（tmux が無い）"
fi
# 端末の幅が先頭 0 等（算術展開で 8 進として失敗する）なら既定の 100 として扱う
# （stty / tput を偽物にし、実行した端末の幅に左右されないようにする）
badw="$work/badw"; mkdir -p "$badw"
printf '#!/bin/sh\necho "24 08"\n' > "$badw/stty"; printf '#!/bin/sh\necho 08\n' > "$badw/tput"
chmod +x "$badw/stty" "$badw/tput"
printf '# t\n' > "$work/w8.md"
check "端末の幅が 08 なら既定の 100" "glow -s dark -w 98 $work/w8.md" env PATH="$badw:$mdbin/both:$PATH" sh "$SCRIPT" render md "$work/w8.md"

# --- is_markdown <パス> ---
check "README.md は Markdown"   "yes" sh "$SCRIPT" is_markdown "docs/README.md"
check ".markdown は Markdown"   "yes" sh "$SCRIPT" is_markdown "a/b.MARKDOWN"
check ".sh は Markdown でない"  ""    sh "$SCRIPT" is_markdown "a/b.sh"

# --- view は利用者の LESS（-F で即終了・-M でプロンプト無視 等）に左右されない ---
lessbin="$work/lessbin"; mkdir -p "$lessbin"
cat > "$lessbin/less" <<'STUB'
#!/bin/sh
case $1 in --version) echo "less 668 (stub)"; exit 0 ;; esac
cat >/dev/null
{ printf 'LESS=[%s]' "${LESS-unset}"; for a in "$@"; do printf ' [%s]' "$a"; done; echo; } >> "$LESS_STUB_LOG"
exit 0
STUB
chmod +x "$lessbin/less"
echo x > "$work/short.txt"
env PATH="$lessbin:$PATH" LESS=-RFXx3M LESS_STUB_LOG="$work/less.log" sh "$SCRIPT" view "$work/short.txt" 3 >/dev/null 2>&1
lesslog=$(cat "$work/less.log" 2>/dev/null)
case $lesslog in
    "LESS=[]"*"[--mouse]"*"[--lesskey-src="*"[-Psq 閉じる  e Emacs  o 既定アプリ  c Claude]"*) echo "ok   view は LESS を空にしてプロンプトを渡す" ;;
    *) echo "FAIL view は LESS を空にしてプロンプトを渡す: $lesslog"; fail=1 ;;
esac

# --- glow_dir <ディレクトリ>: ls -la の代わりに glow の一覧（TUI）で開くか ---
# glow が一覧に出すもの（Markdown の拡張子で、隠し・node_modules の外）が深さ 4 までにあるときだけ
gd="$work/glowdir"
mkdir -p "$gd/bin" "$gd/nobin" "$gd/md/docs" "$gd/mkd" "$gd/plain" "$gd/inhidden/.git" \
    "$gd/deps/node_modules/x" "$gd/.dot" "$gd/deep/1/2/3/4"
for f in md/docs/a.md mkd/a.markdown inhidden/.git/a.md inhidden/.hidden.md deps/node_modules/x/a.md \
    .dot/a.md deep/1/2/3/4/a.md; do
    printf '# t\n' > "$gd/$f"
done
echo x > "$gd/plain/a.txt"
cat > "$gd/bin/glow" <<'STUB'
#!/bin/sh
{ printf 'GLOW_MOUSE=[%s]' "${GLOW_MOUSE-unset}"; for a in "$@"; do printf ' [%s]' "$a"; done; echo; } >> "$GLOW_STUB_LOG"
exit "${GLOW_STUB_RC:-0}"
STUB
chmod +x "$gd/bin/glow"
for c in sh find head; do ln -sf "$(command -v "$c")" "$gd/nobin/$c"; done
withglow="$gd/bin:$PATH"
check "glow_dir: Markdown を含むディレクトリ"         "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/md"
check "glow_dir: .markdown も Markdown"              "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/mkd"
check "glow_dir: Markdown が無い"                    ""    env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/plain"
check "glow_dir: 隠しファイル・隠しディレクトリの中だけ" "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/inhidden"
check "glow_dir: node_modules の中だけ"              ""    env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/deps"
check "glow_dir: 起点が隠しディレクトリ（glow は 0 件）" "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/.dot"
check "glow_dir: 深さ 4 より下だけ"                  ""    env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/deep"
check "glow_dir: ファイルは対象外"                   ""    env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/md/docs/a.md"
check "glow_dir: glow が無い"                        ""    env PATH="$gd/nobin" sh "$SCRIPT" glow_dir "$gd/md"
# git 管理下は glow と同じく .gitignore で無視された Markdown を数えない（サブディレクトリからも）
gr="$gd/repo"; mkdir -p "$gr/gen" "$gr/docs" "$gr/new" "$gr/tracked"
git -C "$gr" init -q
printf 'gen/\n*.local.md\n' > "$gr/.gitignore"
printf '# t\n' > "$gr/gen/a.md"; echo x > "$gr/gen/b.txt"
printf '# t\n' > "$gr/docs/n.local.md"; echo x > "$gr/docs/t.txt"
printf '# t\n' > "$gr/new/a.md"
printf '# t\n' > "$gr/tracked/a.md"; git -C "$gr" add tracked/a.md
check "glow_dir: 無視されたディレクトリの Markdown だけ" "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/gen"
check "glow_dir: 無視されたファイルの Markdown だけ"   "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/docs"
check "glow_dir: 追跡していない Markdown"            "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/new"
check "glow_dir: 追跡している Markdown"              "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/tracked"
check "glow_dir: git の起点からも深さ 4 まで"        "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr"
# glow は追跡しているかに関わらず .gitignore を当てる（強制で追加した Markdown も一覧に出さない）。
# 削除してまだステージしていない Markdown は無い。引用符を含む名前（git が引用する）も Markdown
mkdir -p "$gr/forced" "$gr/deleted" "$gr/quoted"
printf 'forced/\n' >> "$gr/.gitignore"
printf '# t\n' > "$gr/forced/a.md"; git -C "$gr" add -f forced/a.md
printf '# t\n' > "$gr/deleted/a.md"; echo x > "$gr/deleted/t.txt"; git -C "$gr" add deleted/a.md; rm "$gr/deleted/a.md"
printf '# t\n' > "$gr/quoted/a\"b.md"
check "glow_dir: 強制で追加した無視対象の Markdown だけ" "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/forced"
check "glow_dir: 削除してステージ前の Markdown だけ"   "" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/deleted"
check "glow_dir: 引用符を含む名前の Markdown"         "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/quoted"
# 否定（!）の規則で含め直したものは一覧に出る。改行を含む名前を 2 つに割って誤判定しない。
# 拡張子の大文字・小文字は glow と同じく区別しない
mkdir -p "$gr/neg" "$gr/nl" "$gd/upper"
printf 'neg/*.md\n!neg/README.md\nnl/*.md\n' >> "$gr/.gitignore"
printf '# t\n' > "$gr/neg/README.md"
printf '# t\n' > "$gr/nl/$(printf 'a\nb').md"
printf '# t\n' > "$gd/upper/README.MD"
check "glow_dir: 否定の規則で含め直した Markdown"     "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/neg"
check "glow_dir: 改行を含む名前（無視される）だけ"    ""    env PATH="$withglow" sh "$SCRIPT" glow_dir "$gr/nl"
check "glow_dir: 拡張子が大文字の Markdown"           "yes" env PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/upper"
# glow は ~/Library（macOS だけ）と $GOPATH を起点にすると 1 件も出さない（その下のディレクトリは出す）
mkdir -p "$gd/home/Library/sub" "$gd/gop"
printf '# t\n' > "$gd/home/Library/a.md"; printf '# t\n' > "$gd/home/Library/sub/a.md"; printf '# t\n' > "$gd/gop/a.md"
if [ "$(uname -s)" = Darwin ]; then
    check "glow_dir: ~/Library そのもの（macOS）"       ""    env HOME="$gd/home" PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/home/Library"
else
    check "glow_dir: ~/Library は macOS 以外では普通のディレクトリ" "yes" env HOME="$gd/home" PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/home/Library"
fi
check "glow_dir: HOME が無くても動く"                "yes" env -u HOME PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/md"
check "glow_dir: ~/Library の下のディレクトリ"        "yes" env HOME="$gd/home" PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/home/Library/sub"
check "glow_dir: \$GOPATH そのもの"                   ""    env GOPATH="$gd/gop" PATH="$withglow" sh "$SCRIPT" glow_dir "$gd/gop"

# --- view <ディレクトリ>: Markdown を含めば glow の一覧で開き、less（ls -la）は出さない ---
# vrun <view の引数...>: 偽の glow / less で view を動かし、それぞれに渡った引数を記録する
vrun() {
    rm -f "$gd/glow.log" "$gd/less.log"
    env PATH="$gd/bin:$lessbin:$PATH" GLOW_STUB_LOG="$gd/glow.log" LESS_STUB_LOG="$gd/less.log" \
        sh "$SCRIPT" view "$@" >/dev/null 2>&1 </dev/null
    glowlog=$(cat "$gd/glow.log" 2>/dev/null) lesslog=$(cat "$gd/less.log" 2>/dev/null)
}
vrun "$gd/md"
case $glowlog in
    "GLOW_MOUSE=[true] [-t] [-s] [dark] [-w] ["[0-9]*"] [$gd/md]")
        if [ -z "$lesslog" ]; then echo "ok   view: Markdown を含むディレクトリは glow の一覧で開く"
        else echo "FAIL view: glow の後に less も開いた: $lesslog"; fail=1; fi ;;
    *) echo "FAIL view: Markdown を含むディレクトリは glow の一覧で開く: $glowlog"; fail=1 ;;
esac
vrun "$gd/plain"
case $glowlog/$lesslog in
    "/LESS=[]"*"[-Psq 閉じる  e Emacs  o 既定アプリ  c Claude]"*) echo "ok   view: Markdown の無いディレクトリは今までどおり less" ;;
    *) echo "FAIL view: Markdown の無いディレクトリは今までどおり less: glow=$glowlog less=$lesslog"; fail=1 ;;
esac
GLOW_STUB_RC=1 vrun "$gd/md"
case $glowlog/$lesslog in
    *"[-t]"*"/LESS=[]"*"[-Psq 閉じる"*) echo "ok   view: glow が起動に失敗したら less の一覧に戻る" ;;
    *) echo "FAIL view: glow が起動に失敗したら less の一覧に戻る: glow=$glowlog less=$lesslog"; fail=1 ;;
esac
vrun "$gd/md/docs/a.md"
case $glowlog/$lesslog in
    *"[-t]"*) echo "FAIL view: Markdown ファイルは glow -t で開かない: $glowlog"; fail=1 ;;
    */"LESS=[]"*"r 整形/原文]"*) echo "ok   view: Markdown ファイルは今までどおり less（glow -t は使わない）" ;;
    *) echo "FAIL view: Markdown ファイルは今までどおり less: glow=$glowlog less=$lesslog"; fail=1 ;;
esac

# --- lesskey_ok <less --version の 1 行目>: --lesskey-src（less 582+）が使えるか ---
check "less 668 は対応"   "yes" sh "$SCRIPT" lesskey_ok "less 668 (POSIX regular expressions)"
check "less 590 は対応"   "yes" sh "$SCRIPT" lesskey_ok "less 590 (GNU regular expressions)"
check "less 551 は非対応" "no"  sh "$SCRIPT" lesskey_ok "less 551 (GNU regular expressions)"
check "不明は非対応"      "no"  sh "$SCRIPT" lesskey_ok "BusyBox v1.36"

# --- popup_size <パス> <端末の幅> <高さ>: 中身に合わせたポップアップの「幅 高さ」 ---
sz="$work/size"; mkdir -p "$sz"
echo x > "$sz/short.txt"
awk 'BEGIN { for (i = 0; i < 300; i++) { s = ""; for (j = 0; j < (i == 7 ? 200 : 20); j++) s = s "a"; print s } }' > "$sz/long.txt"
awk 'BEGIN { s = ""; for (j = 0; j < 500; j++) s = s "a"; print s }' > "$sz/wide.txt"
printf '\tx\n' > "$sz/tab.txt"
printf '# t\n' > "$sz/doc.md"
check "短いファイルは最小の 80x12"        "80 12"   sh "$SCRIPT" popup_size "$sz/short.txt" 310 56
check "最長行＋行番号の幅、高さは上限"    "212 47"  sh "$SCRIPT" popup_size "$sz/long.txt" 310 56
check "幅は端末の 85% まで"               "263 12"  sh "$SCRIPT" popup_size "$sz/wide.txt" 310 56
check "Markdown は読みやすい幅に固定"     "108 47"  sh "$SCRIPT" popup_size "$sz/doc.md" 310 56
check "狭い端末では 85% に収める"         "51 17"   sh "$SCRIPT" popup_size "$sz/doc.md" 60 20
check "ディレクトリ（ls -la）は 100 桁"   "100 47"  env PATH="$withglow" sh "$SCRIPT" popup_size "$gd/plain" 310 56
check "glow の一覧で開くディレクトリは Markdown と同じ" "108 47" env PATH="$withglow" sh "$SCRIPT" popup_size "$gd/md" 310 56
check "glow が無ければ Markdown を含んでも 100 桁" "100 47" env PATH="$gd/nobin" sh "$SCRIPT" popup_size "$gd/md" 310 56
check "整形した文書（PR / Issue）は Markdown と同じ" "108 47" sh "$SCRIPT" popup_size --doc 310 56
check "整形した文書も 85% に収める"       "51 17"   sh "$SCRIPT" popup_size --doc 60 20
check "端末の大きさが不明なら 85%"        "85% 85%" sh "$SCRIPT" popup_size "$sz/short.txt" "" ""
check "片方だけ空なら 85%"                "85% 85%" sh "$SCRIPT" popup_size "$sz/short.txt" 310 ""
check "先頭 0 の寸法は 85%"               "85% 85%" sh "$SCRIPT" popup_size "$sz/short.txt" 08 56
check "0 の寸法は 85%"                    "85% 85%" sh "$SCRIPT" popup_size "$sz/short.txt" 0 0
# 改行の無い巨大なファイルも全体を読まず、高さは上限にする
awk 'BEGIN { s = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; for (i = 0; i < 16; i++) s = s s; for (i = 0; i < 4; i++) printf "%s", s }' > "$sz/huge.txt"
check "改行の無い巨大ファイルは上限"      "263 47"  sh "$SCRIPT" popup_size "$sz/huge.txt" 310 56
# bat が無いときの表示もタブを 4 桁に展開する（大きさの計算とそろえる）
nobat="$work/nobat"; mkdir -p "$nobat"
for c in sh awk sed sort head cut cat tr dirname basename tput; do
    p=$(command -v "$c") && [ "${p#/}" != "$p" ] && ln -sf "$p" "$nobat/$c"
done
check "bat 無しでもタブは 4 桁"           "     1      x" env PATH="$nobat" sh "$SCRIPT" render raw "$sz/tab.txt"

# --- resolve <候補> <pane の cwd>: 実在するファイルの絶対パスと行番号 ---
repo="$work/repo"
mkdir -p "$repo/sub" "$repo/docs" "$work/home/dotfiles"
git -C "$repo" init -q
echo x > "$repo/link.sh"
echo x > "$repo/docs/設計.md"
echo x > "$work/home/dotfiles/link.sh"
echo x > "$work/a b.txt"

check "cwd 相対"                 "$repo/link.sh${TAB}3"    sh "$SCRIPT" resolve "link.sh:3" "$repo"
check "git ルート相対"           "$repo/docs/設計.md${TAB}" sh "$SCRIPT" resolve "docs/設計.md" "$repo/sub"
check "diff の b/ を外す"        "$repo/link.sh${TAB}"     sh "$SCRIPT" resolve "b/link.sh" "$repo"
check "後続の全角を外す"         "$repo/link.sh${TAB}"     sh "$SCRIPT" resolve "link.shを参照" "$repo"
check "チルダ展開"               "$work/home/dotfiles/link.sh${TAB}" env HOME="$work/home" sh "$SCRIPT" resolve "~/dotfiles/link.sh" "$repo"
check "絶対パス"                 "$repo/link.sh${TAB}9"    sh "$SCRIPT" resolve "$repo/link.sh:9" "/"
check "ディレクトリ"             "$repo/docs${TAB}"        sh "$SCRIPT" resolve "docs/" "$repo"
check "file:// URI（%20）"       "$work/a b.txt${TAB}"     sh "$SCRIPT" resolve "file://$work/a%20b.txt" "/"
check "file:// localhost"        "$work/a b.txt${TAB}"     sh "$SCRIPT" resolve "file://localhost$work/a%20b.txt" "/"
check "file:// 自ホスト名"       "$work/a b.txt${TAB}"     sh "$SCRIPT" resolve "file://$(uname -n)$work/a%20b.txt" "/"
check "file:// 他ホストは拒否"   ""                        sh "$SCRIPT" resolve "file://remote.invalid$work/a%20b.txt" "/"
check "存在しなければ失敗"       ""                        sh "$SCRIPT" resolve "nope.txt" "$repo"

# --- cwd・git ルートに無ければ、同じリポジトリの worktree → ghq 配下 → OPEN_PATH_ROOTS を探す ---
# （Claude Code の cwd は起動時のまま固定のため、別リポジトリ・別 worktree 相対のパスが表示される）
git -C "$repo" -c user.name=t -c user.email=t@t commit -q --allow-empty -m init
git -C "$repo" worktree add -q -b wt "$work/repo-wt"
mkdir -p "$work/repo-wt/new" "$work/ghq/github.com/x/other/scripts" "$work/extra/tools"
echo x > "$work/repo-wt/new/only-in-wt.sh"
echo x > "$work/ghq/github.com/x/other/scripts/foo.sh"
echo x > "$work/ghq/github.com/x/other/rootname.md"
echo x > "$work/extra/tools/bar.sh"
mkdir -p "$work/ghq/github.com/x/other/sub"; echo x > "$work/ghq/github.com/x/other/sub/link.sh"
check "別 worktree 相対"                 "$work/repo-wt/new/only-in-wt.sh${TAB}4" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "new/only-in-wt.sh:4" "$repo"
check "ghq 配下の別リポジトリ相対"       "$work/ghq/github.com/x/other/scripts/foo.sh${TAB}" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "scripts/foo.sh" "$repo"
check "OPEN_PATH_ROOTS（: 区切り）"      "$work/extra/tools/bar.sh${TAB}" env GHQ_ROOT="$work/ghq" OPEN_PATH_ROOTS="/nonexistent:$work/extra" sh "$SCRIPT" resolve "tools/bar.sh" "$repo"
check "cwd に有れば cwd を優先"          "$repo/link.sh${TAB}" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "link.sh" "$repo"
check "名前だけは他リポジトリを探さない" "" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "rootname.md" "$repo"
check "git 管理外の cwd でも ghq を探す" "$work/ghq/github.com/x/other/scripts/foo.sh${TAB}" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "scripts/foo.sh" "$work/plain"
# 追加ルートからは ../ で外へ出ない（cwd 相対の ../ は従来どおり許す）
mkdir -p "$work/outside"; echo x > "$work/outside/leak.txt"
check "追加ルートからの ../ は拒否"      "" env GHQ_ROOT="$work/ghq" OPEN_PATH_ROOTS="$work/extra" sh "$SCRIPT" resolve "../outside/leak.txt" "$work/plain"
# 同じ段に複数あれば取り違えないよう失敗し、明示したルートは ghq より優先する
mkdir -p "$work/ghq/github.com/y/dup/scripts"; echo x > "$work/ghq/github.com/y/dup/scripts/foo.sh"
check "ghq で複数一致なら開かない"       "" env GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "scripts/foo.sh" "$repo"
mkdir -p "$work/extra/scripts"; echo x > "$work/extra/scripts/foo.sh"
check "OPEN_PATH_ROOTS は ghq より優先"  "$work/extra/scripts/foo.sh${TAB}" env GHQ_ROOT="$work/ghq" OPEN_PATH_ROOTS="$work/extra" sh "$SCRIPT" resolve "scripts/foo.sh" "$repo"
# 上の段で決まれば下の段（ghq root）を呼ばない
slowghq="$work/slowghq"; mkdir -p "$slowghq"
printf '#!/bin/sh\nsleep 5\necho "%s"\n' "$work/ghq" > "$slowghq/ghq"; chmod +x "$slowghq/ghq"
t0=$(date +%s)
check "上の段で決まれば ghq を待たない"  "$work/extra/scripts/foo.sh${TAB}" env -u GHQ_ROOT PATH="$slowghq:$PATH" OPEN_PATH_ROOTS="$work/extra" sh "$SCRIPT" resolve "scripts/foo.sh" "$repo"
elapsed=$(( $(date +%s) - t0 ))
if [ "$elapsed" -le 1 ]; then echo "ok   ghq を呼ばずに返す（${elapsed}s）"; else echo "FAIL ghq を呼ばずに返す（${elapsed}s）"; fail=1; fi

# --- ファイル名だけの候補は cwd 配下を探す ---
mkdir -p "$repo/sub/deep" "$repo/a/b" "$repo/ignored" "$repo/other" "$work/plain/x/y"
echo x > "$repo/sub/deep/only.txt"
echo x > "$repo/a/dup.txt"; echo x > "$repo/a/b/dup.txt"
echo "ignored/" > "$repo/.gitignore"; echo x > "$repo/ignored/secret.txt"
echo x > "$work/plain/x/y/found.md"
check "名前だけ: git 配下を探索"         "$repo/sub/deep/only.txt${TAB}"  sh "$SCRIPT" resolve "only.txt" "$repo"
check "名前だけ: 行番号を保つ"           "$repo/sub/deep/only.txt${TAB}5" sh "$SCRIPT" resolve "only.txt:5" "$repo"
check "名前だけ: 複数なら浅い方"         "$repo/a/dup.txt${TAB}"          sh "$SCRIPT" resolve "dup.txt" "$repo"
check "名前だけ: 日本語名（git の quotePath）" "$repo/docs/設計.md${TAB}" sh "$SCRIPT" resolve "設計.md" "$repo"
check "名前だけ: git 管理外も探索"       "$work/plain/x/y/found.md${TAB}" sh "$SCRIPT" resolve "found.md" "$work/plain"
check "名前だけ: .gitignore 対象は除外"  ""                               sh "$SCRIPT" resolve "secret.txt" "$repo"
check "名前だけ: cwd 配下に限る"         ""                               sh "$SCRIPT" resolve "only.txt" "$repo/other"
check "名前だけ: glob 文字は探さない"    ""                               sh "$SCRIPT" resolve "*.txt" "$repo"

# 削除済みでもインデックスに残る浅い候補より、実在する深い候補を選ぶ
mkdir -p "$repo/del/a" "$repo/del/b/deep"
echo x > "$repo/del/a/gone.txt"; git -C "$repo" add del/a/gone.txt; rm "$repo/del/a/gone.txt"
echo y > "$repo/del/b/deep/gone.txt"
check "名前だけ: 削除済みの候補を飛ばす" "$repo/del/b/deep/gone.txt${TAB}" sh "$SCRIPT" resolve "gone.txt" "$repo/del"

# 通常ファイル・ディレクトリ以外（FIFO・デバイス）は開かない
mkfifo "$work/fifo"
check "FIFO は対象外"     "" sh "$SCRIPT" resolve "$work/fifo" "/"
check "デバイスは対象外"  "" sh "$SCRIPT" resolve "/dev/null" "/"

# 改行入りのディレクトリ名で cwd の外（../hosts 等）を指させない
esc="$work/esc"; mkdir -p "$esc/in/$(printf 'evil\n..')"
echo x > "$esc/in/$(printf 'evil\n..')/hosts"; echo x > "$esc/hosts"
check "名前だけ: 探索結果は cwd 配下に限る" "" sh "$SCRIPT" resolve "hosts" "$esc/in"

# timeout コマンドが無くても、止まる find を 3 秒程度で打ち切る
slow="$work/slow"; mkdir -p "$slow"
# SIGTERM を無視する find でも打ち切れること（SIGKILL へ段階的に上げる）
printf '#!/bin/sh\ntrap "" TERM\nexec sleep 10\n' > "$slow/find"; chmod +x "$slow/find"
# timeout / gtimeout の無い PATH（標準の macOS 相当）にする。Linux の /usr/bin/timeout は除外
nt="$work/notimeout"; mkdir -p "$nt"
for c in sh awk sed sort head cut git date sleep kill cat dirname basename; do
    p=$(command -v "$c") && [ "${p#/}" != "$p" ] && ln -sf "$p" "$nt/$c"
done
t0=$(date +%s)
env PATH="$slow:$nt" sh "$SCRIPT" resolve "zz-missing.txt" "$work/plain" >/dev/null 2>&1
elapsed=$(( $(date +%s) - t0 ))
if [ "$elapsed" -le 5 ]; then echo "ok   find を時間で打ち切る（${elapsed}s）"; else echo "FAIL find を時間で打ち切る（${elapsed}s）"; fail=1; fi

# git が止まっても（NFS / FUSE 上の worktree 等）時間で打ち切る
hang="$work/hang"; mkdir -p "$hang"
printf '#!/bin/sh\nexec sleep 10\n' > "$hang/git"; chmod +x "$hang/git"
t0=$(date +%s)
env PATH="$hang:$PATH" sh "$SCRIPT" resolve "zz-missing.txt" "$repo" >/dev/null 2>&1
elapsed=$(( $(date +%s) - t0 ))
if [ "$elapsed" -le 9 ]; then echo "ok   git を時間で打ち切る（${elapsed}s）"; else echo "FAIL git を時間で打ち切る（${elapsed}s）"; fail=1; fi
# rev-parse は通り、その後の git（worktree list・ls-files）が止まっても待ちを積み上げない
hang2="$work/hang2"; mkdir -p "$hang2"
realgit=$(command -v git)
printf '#!/bin/sh\ncase " $* " in *" rev-parse "*) exec "%s" "$@" ;; esac\ntrap "" TERM\nexec sleep 10\n' "$realgit" > "$hang2/git"; chmod +x "$hang2/git"
for tok in "zz-missing.txt" "zz/missing.txt"; do
    t0=$(date +%s)
    env PATH="$hang2:$PATH" GHQ_ROOT="$work/ghq" sh "$SCRIPT" resolve "$tok" "$repo" >/dev/null 2>&1
    elapsed=$(( $(date +%s) - t0 ))
    if [ "$elapsed" -le 6 ]; then echo "ok   止まる git の待ちを積み上げない: $tok（${elapsed}s）"; else echo "FAIL 止まる git の待ちを積み上げない: $tok（${elapsed}s）"; fail=1; fi
done

# 画面上の文字列はシェルに展開しない
sh "$SCRIPT" resolve "\$(touch $work/pwned)" "$repo" >/dev/null 2>&1
sh "$SCRIPT" extract "\$(touch $work/pwned2)" 3 >/dev/null 2>&1
if [ ! -e "$work/pwned" ] && [ ! -e "$work/pwned2" ]; then echo "ok   コマンド置換を実行しない"; else echo "FAIL コマンド置換を実行しない"; fail=1; fi

# --- context <名前> <cwd> <クリックした行の番号> <折り返し桁>: 標準入力の画面の行に出ている
#     パスのディレクトリから、名前（相対パス）を近い行から順に探す ---
ctx="$work/ctx"; mkdir -p "$ctx/a/b" "$ctx/c"
for f in a/b/pr.md a/b/verdict.md c/verdict.md; do echo x > "$ctx/$f"; done
near() { printf '%s\n' "${@:5}" | sh "$SCRIPT" context "$1" "$2" "$3" "$4"; }
check "context: 前の行のパスと同じディレクトリ" "$ctx/a/b/verdict.md" near verdict.md / 1 10 "本文は $ctx/a/b/pr.md。採否記録は同ディレクトリ" "     verdict.md。"
check "context: 近い行を優先（前）"   "$ctx/a/b/verdict.md" near verdict.md / 3 10 "$ctx/c/" "" "see $ctx/a/b/pr.md" "verdict.md"
check "context: 近い行を優先（後）"   "$ctx/c/verdict.md"   near verdict.md / 0 10 "verdict.md" "$ctx/c/" "" "see $ctx/a/b/pr.md"
check "context: 同じ行のパス"         "$ctx/c/verdict.md"   near verdict.md / 1 10 "see $ctx/a/b/pr.md" "$ctx/c/ と verdict.md"
check "context: 折り返されたパス"     "$ctx/a/b/verdict.md" near verdict.md / 2 10 "本文は $ctx/a" "   /b/pr.md。" "verdict.md"
check "context: cwd 相対のパス"       "$ctx/a/b/verdict.md" near verdict.md "$ctx" 1 10 "edit a/b/pr.md" "verdict.md"
check "context: 名前が相対パス"       "$ctx/a/b/verdict.md" near b/verdict.md / 1 10 "$ctx/a/pr.md" "b/verdict.md"
# 誤ってつないだ並びが実在しなくても、元の各行のパスも使う
check "context: 誤連結しても元の行を使う" "$ctx/c/verdict.md" near verdict.md / 2 5 "$ctx/a/b/pr.md" "   $ctx/c/x" "verdict.md"
check "context: 見つからなければ失敗" ""                    near none.md / 1 10 "$ctx/a/b/pr.md" "none.md"
check "context: 絶対パスは探さない"   ""                    near /verdict.md / 1 10 "$ctx/a/b/pr.md" "/verdict.md"

# --- commit_repo <候補> <ディレクトリ...>: コミットの SHA なら、それを持つリポジトリを返す ---
crepo="$work/crepo"; mkdir -p "$crepo/sub"
git -C "$crepo" init -q && echo hi > "$crepo/f.txt" && git -C "$crepo" add f.txt &&
    GIT_AUTHOR_DATE=2026-01-01T00:00:00Z GIT_COMMITTER_DATE=2026-01-01T00:00:00Z \
        git -C "$crepo" -c user.name=t -c user.email=t@example.com commit -qm "init commit"
sha=$(git -C "$crepo" rev-parse HEAD); short=$(printf '%s' "$sha" | cut -c1-7)
check "commit: 短い SHA"             "$crepo"  sh "$SCRIPT" commit_repo "$short" "$crepo"
check "commit: 完全な SHA"           "$crepo"  sh "$SCRIPT" commit_repo "$sha" "$crepo/sub"
check "commit: 順に探す"             "$crepo"  sh "$SCRIPT" commit_repo "$short" "$work/none" "$ctx" "$crepo"
check "commit: 無いコミット"         ""        sh "$SCRIPT" commit_repo deadbeefcafe "$crepo"
check "commit: tree は対象外"        ""        sh "$SCRIPT" commit_repo "$(git -C "$crepo" rev-parse 'HEAD^{tree}' | cut -c1-12)" "$crepo"
check "commit: 16 進でない"          ""        sh "$SCRIPT" commit_repo README "$crepo"
check "commit: 数字だけで該当なし"   ""        sh "$SCRIPT" commit_repo 1234567 "$crepo"
check "commit: 6 桁以下は対象外"     ""        sh "$SCRIPT" commit_repo "$(printf '%s' "$sha" | cut -c1-6)" "$crepo"
check "commit: 差分を表示"           "1"       sh -c 'sh "$1" render commit "$2" "$3" | grep -c "init commit"' _ "$SCRIPT" "$crepo" "$short"

# --- gh_ref <行> <桁>: クリック位置の PR / Issue の参照（#N・owner/repo#N）を「owner/repo<TAB>番号」で ---
check "gh: #番号"                    "${TAB}78"                sh "$SCRIPT" gh_ref "PR #78 を見る" 4
check "gh: # の上"                   "${TAB}78"                sh "$SCRIPT" gh_ref "PR #78 を見る" 3
check "gh: 全角の後ろの桁を補正"     "${TAB}78"                sh "$SCRIPT" gh_ref "日本語 #78 です" 8
check "gh: 全角括弧で止まる"         "${TAB}78"                sh "$SCRIPT" gh_ref "（#78）" 3
check "gh: owner/repo#番号"          "ysano/dotfiles${TAB}78"  sh "$SCRIPT" gh_ref "see ysano/dotfiles#78." 6
check "gh: 文末の句点"               "${TAB}78"                sh "$SCRIPT" gh_ref "see #78." 5
check "gh: 見出しは対象外"           ""                        sh "$SCRIPT" gh_ref "## Heading" 0
check "gh: 数字でない"               ""                        sh "$SCRIPT" gh_ref "see #abc" 5
check "gh: 0 始まりは対象外"         ""                        sh "$SCRIPT" gh_ref "see #012" 5
check "gh: 英字に続く # は対象外"    ""                        sh "$SCRIPT" gh_ref "PR#78" 3
check "gh: 階層の深いパスは対象外"   ""                        sh "$SCRIPT" gh_ref "a/b/c#78" 6
check "gh: 文字参照は対象外"         ""                        sh "$SCRIPT" gh_ref "&#123;" 2
# URL の中のクリックは、# 以降を含め URL 全体を読む（?foo=#78 の #78 を単独の参照にしない）。
# URL から読んだものは 3 つ目に url を付ける（ローカルのファイルを優先する判定から外すため）
check "gh: URL の中は URL 全体"      "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "https://github.com/acme/widget/pull/9?foo=#78" 42
check "gh: URL の記号の上"           "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "https://github.com/acme/widget/pull/9?foo=#78" 37
check "gh: 文中の URL"               "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "see https://github.com/acme/widget/pull/9 ok" 10
check "gh: 括弧の中の URL"           "o/r${TAB}3${TAB}url"              sh "$SCRIPT" gh_ref "(https://github.com/o/r/issues/3)" 5
check "gh: GitHub 以外の URL"        ""                        sh "$SCRIPT" gh_ref "https://example.com/#78" 21
check "gh: 別ホストの URL のクエリ"   ""                        sh "$SCRIPT" gh_ref "https://example.com/?next=https://github.com/o/r/pull/9" 10
two="(https://github.com/o/r/pull/1),(https://github.com/o/r/pull/2)"
check "gh: 並んだ URL の前"          "o/r${TAB}1${TAB}url"     sh "$SCRIPT" gh_ref "$two" 5
check "gh: 並んだ URL の後ろ"        "o/r${TAB}2${TAB}url"     sh "$SCRIPT" gh_ref "$two" 54
# 別の URL のクエリの中の URL をクリックしても、外側の URL を読む
inner="https://github.com/acme/widget/pull/9?next=https://github.com/other/project/pull/12"
check "gh: クエリの中の URL の上"    "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "$inner" 65
check "gh: クエリの中の URL の先頭"  "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "$inner" 44
check "gh: + に続くクエリの中の URL" "acme/widget${TAB}9${TAB}url"      sh "$SCRIPT" gh_ref "https://github.com/acme/widget/pull/9?next=+https://github.com/other/project/pull/12" 70
# = 等に続く URL（シェルの URL=https://… 等）も URL 全体を読む（# 以降を単独の参照にしない）
check "gh: = に続く URL"             "o/r${TAB}1${TAB}url"     sh "$SCRIPT" gh_ref "url=https://github.com/o/r/pull/1?q=#9" 10
check "gh: = に続く URL の # の上"   "o/r${TAB}1${TAB}url"     sh "$SCRIPT" gh_ref "url=https://github.com/o/r/pull/1?q=#9" 36
check "gh: = に続く別ホストの URL"   ""                        sh "$SCRIPT" gh_ref "url=https://example.com/?q=other/project#12" 27
# 同じ語の後ろに URL があっても、その前の #番号は単独の参照として読む
check "gh: URL の前の #番号"         "${TAB}9"                 sh "$SCRIPT" gh_ref "#9、https://github.com/o/r/pull/2" 1
check "gh: URL の前の #番号の後ろの URL" "o/r${TAB}2${TAB}url"  sh "$SCRIPT" gh_ref "#9、https://github.com/o/r/pull/2" 10
check "gh: パスの続きは対象外"       ""                        sh "$SCRIPT" gh_ref "@scope/pkg#9" 3
check "gh: ~ に続くものは対象外"     ""                        sh "$SCRIPT" gh_ref "~/a#9" 2
check "gh: .. は対象外"              ""                        sh "$SCRIPT" gh_ref "o/..#7" 4
check "gh: 参照の外"                 ""                        sh "$SCRIPT" gh_ref "PR #78 を見る" 0
# 登録した省略形「名前#番号」（OPEN_PATH_GH_ALIAS = 空白区切りの 名前=owner/repo）
alias_tbl=" bp=acme/blueprint  int=acme/internal "
check "gh: 省略形"                   "acme/blueprint${TAB}123${TAB}alias" env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "see bp#123 now" 5
check "gh: 2 つ目の省略形と句点"     "acme/internal${TAB}4${TAB}alias" env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "see int#4." 6
check "gh: 未登録の名前は対象外"     ""                        env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "see PR#78" 6
check "gh: 名前の前方一致は対象外"   ""                        env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "see b#78" 5
check "gh: 表が無ければ対象外"       ""                        env -u OPEN_PATH_GH_ALIAS sh "$SCRIPT" gh_ref "see bp#123 now" 5
check "gh: 省略形の先が空"           ""                        env OPEN_PATH_GH_ALIAS="bp=" sh "$SCRIPT" gh_ref "see bp#1" 5
check "gh: 省略形の先が owner だけ"  ""                        env OPEN_PATH_GH_ALIAS="bp=acme" sh "$SCRIPT" gh_ref "see bp#1" 5
check "gh: 省略形の先が .."          ""                        env OPEN_PATH_GH_ALIAS="bp=acme/.." sh "$SCRIPT" gh_ref "see bp#1" 5
check "gh: 省略形の先が深い"         ""                        env OPEN_PATH_GH_ALIAS="bp=a/b/c" sh "$SCRIPT" gh_ref "see bp#1" 5
check "gh: 省略形の名前に . は不可"  ""                        env OPEN_PATH_GH_ALIAS="b.p=acme/blueprint" sh "$SCRIPT" gh_ref "see b.p#1" 5
check "gh: 省略形も文字参照は対象外" ""                        env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "&bp#1" 2
check "gh: owner/repo#番号は表より先" "bp/x${TAB}1"            env OPEN_PATH_GH_ALIAS="$alias_tbl" sh "$SCRIPT" gh_ref "see bp/x#1" 5
# --- gh_url <URL>: GitHub の PR / Issue の URL ---
check "gh: PR の URL"                "ysano/dotfiles${TAB}78"  sh "$SCRIPT" gh_url "https://github.com/ysano/dotfiles/pull/78"
check "gh: Issue の URL"             "ysano/dotfiles${TAB}73"  sh "$SCRIPT" gh_url "https://github.com/ysano/dotfiles/issues/73"
check "gh: 後ろに続く URL"           "o/r${TAB}9"              sh "$SCRIPT" gh_url "https://github.com/o/r/pull/9/files#diff-1"
check "gh: 前後の記号"               "o/r${TAB}9"              sh "$SCRIPT" gh_url "<https://github.com/o/r/pull/9>。"
check "gh: 番号に続く英字は対象外"   ""                        sh "$SCRIPT" gh_url "https://github.com/o/r/pull/9abc"
check "gh: 番号に続く - は対象外"     ""                        sh "$SCRIPT" gh_url "https://github.com/o/r/pull/9-abc"
check "gh: 別の URL の中は対象外"    ""                        sh "$SCRIPT" gh_url "https://example.com/?next=https://github.com/o/r/pull/9"
check "gh: 和文に接した URL"         "o/r${TAB}9"              sh "$SCRIPT" gh_url "詳細はhttps://github.com/o/r/pull/9を参照"
check "gh: PR / Issue 以外の URL"    ""                        sh "$SCRIPT" gh_url "https://github.com/o/r/blob/master/x"
check "gh: GitHub 以外"              ""                        sh "$SCRIPT" gh_url "https://example.com/o/r/pull/1"
# --- gh_nwo <remote の URL>: owner/repo ---
check "gh: https の remote"          "o/r"                     sh "$SCRIPT" gh_nwo "https://github.com/o/r"
check "gh: .git 付き"                "o/r.x"                   sh "$SCRIPT" gh_nwo "https://github.com/o/r.x.git"
check "gh: scp 形式"                 "o/r"                     sh "$SCRIPT" gh_nwo "git@github.com:o/r.git"
check "gh: ssh:// 形式"              "o/r"                     sh "$SCRIPT" gh_nwo "ssh://git@github.com/o/r.git"
check "gh: GitHub 以外の remote"     ""                        sh "$SCRIPT" gh_nwo "https://gitlab.com/o/r"
# --- gh_repo <ディレクトリ...>: GitHub の remote を持つ最初のリポジトリの owner/repo ---
grepo="$work/grepo"; mkdir -p "$grepo/sub"; echo x > "$grepo/README.md"
git -C "$grepo" init -q && git -C "$grepo" remote add origin https://github.com/acme/widget.git
frepo="$work/frepo"; mkdir -p "$frepo"
git -C "$frepo" init -q && git -C "$frepo" remote add origin git@github.com:me/widget.git &&
    git -C "$frepo" remote add upstream https://github.com/acme/widget
lrepo="$work/lrepo"; mkdir -p "$lrepo"
git -C "$lrepo" init -q && git -C "$lrepo" remote add origin https://gitlab.com/o/r
check "gh: origin"                   "acme/widget"             sh "$SCRIPT" gh_repo "$grepo/sub"
check "gh: fork は upstream"         "acme/widget"             sh "$SCRIPT" gh_repo "$frepo"
check "gh: GitHub 以外は飛ばす"      "acme/widget"             sh "$SCRIPT" gh_repo "$lrepo" "$work/none" "$grepo"
check "gh: 該当なし"                 ""                        sh "$SCRIPT" gh_repo "$lrepo" "$work"
git -C "$frepo" config remote.origin.gh-resolved base
check "gh: gh repo set-default を優先" "me/widget"             sh "$SCRIPT" gh_repo "$frepo"
# --- gh_kind / render gh: gh は偽物（CI の gh は未認証のため本物を呼ばない） ---
ghstub="$work/ghstub"; mkdir -p "$ghstub"
cat > "$ghstub/gh" <<'STUB'
#!/bin/sh
for a in "$@"; do printf '[%s]' "$a"; done >> "$STUB_LOG"; echo " <- gh${GH_FORCE_TTY:+ tty}" >> "$STUB_LOG"
[ -z "${STUB_gh_sleep:-}" ] || exec sleep "$STUB_gh_sleep"
case $1 in
    api) [ -z "${STUB_gh_fail:-}" ] || { echo "HTTP 404: Not Found" >&2; exit 1; }; echo "${STUB_gh_kind:-pr}" ;;
    *) echo "body" ;;
esac
STUB
chmod +x "$ghstub/gh"
ghrun() { : > "$work/log"; env PATH="$ghstub:$PATH" STUB_LOG="$work/log" "$@" >/dev/null 2>&1; cat "$work/log"; }
check "gh: PR か"                    "pr"                      env PATH="$ghstub:$PATH" STUB_LOG=/dev/null sh "$SCRIPT" gh_kind acme/widget 78
check "gh: Issue か"                 "issue"                   env PATH="$ghstub:$PATH" STUB_LOG=/dev/null STUB_gh_kind=issue sh "$SCRIPT" gh_kind acme/widget 3
check "gh: 取得できなければ失敗"     ""                        env PATH="$ghstub:$PATH" STUB_LOG=/dev/null STUB_gh_fail=1 sh "$SCRIPT" gh_kind acme/widget 3
# 応答しない gh は打ち切る（gh 自身を終了させ、出力の読み込みで待ち続けない）
t0=$(date +%s)
check "gh: 応答しなければ失敗"       ""                        env PATH="$ghstub:$PATH" STUB_LOG=/dev/null STUB_gh_sleep=8 OPEN_PATH_GH_TIMEOUT=1 sh "$SCRIPT" gh_kind acme/widget 3
if [ $(( $(date +%s) - t0 )) -le 4 ]; then echo "ok   gh: 応答しなければ打ち切る"; else echo "FAIL gh: 応答しなければ打ち切る: $(( $(date +%s) - t0 )) 秒"; fail=1; fi
check "gh: 想定外の応答は失敗"       ""                        env PATH="$ghstub:$PATH" STUB_LOG=/dev/null STUB_gh_kind=x sh "$SCRIPT" gh_kind acme/widget 3
log=$(ghrun sh "$SCRIPT" gh_kind "o/.." 3)
case $log in "") echo "ok   gh: 不正な owner/repo では gh を呼ばない" ;; *) echo "FAIL gh: 不正な owner/repo では gh を呼ばない: $log"; fail=1 ;; esac
check "gh: PR の本文"   "[pr][view][78][-R][acme/widget][--comments] <- gh tty"     ghrun sh "$SCRIPT" render gh acme/widget 78 pr body
check "gh: PR の差分"   "[pr][diff][78][-R][acme/widget][--color=always] <- gh tty" ghrun sh "$SCRIPT" render gh acme/widget 78 pr diff
check "gh: Issue の本文" "[issue][view][3][-R][acme/widget][--comments] <- gh tty"  ghrun sh "$SCRIPT" render gh acme/widget 3 issue body
check "gh: Issue に差分は無い" "[issue][view][3][-R][acme/widget][--comments] <- gh tty" ghrun sh "$SCRIPT" render gh acme/widget 3 issue diff
# 本文・差分の取得も打ち切る（止まった gh を待ち続けると、less を抜けてもポップアップが閉じない）
for part in body diff; do
    t0=$(date +%s)
    env PATH="$ghstub:$PATH" STUB_LOG=/dev/null STUB_gh_sleep=8 OPEN_PATH_GH_TIMEOUT=1 sh "$SCRIPT" render gh acme/widget 78 pr "$part" >/dev/null 2>&1
    if [ $(( $(date +%s) - t0 )) -le 4 ]; then echo "ok   gh: 応答しない $part の取得を打ち切る"; else echo "FAIL gh: 応答しない $part の取得を打ち切る: $(( $(date +%s) - t0 )) 秒"; fail=1; fi
done
# 本文の折り返し（GH_FORCE_TTY）もポップアップ（端末）の幅に合わせる。render md と同じく、幅 120 の
# 端末を別ソケットの tmux で用意し、出力を端末から外して偽の gh に渡った値を見る
if command -v tmux >/dev/null 2>&1; then
    ghw="$work/ghw"; mkdir -p "$ghw"
    printf '#!/bin/sh\necho "$GH_FORCE_TTY" > "%s"\n' "$work/ghw.out" > "$ghw/gh"; chmod +x "$ghw/gh"
    sock="open-path-test-gh-$$"
    tmux -L "$sock" -f /dev/null new-session -d -x 120 -y 20 \
        "env PATH='$ghw:$PATH' sh '$SCRIPT' render gh acme/widget 78 pr body > /dev/null 2>&1; touch '$work/ghw.done'"
    for _ in $(seq 50); do [ -f "$work/ghw.done" ] && break; sleep 0.1; done
    tmux -L "$sock" kill-server 2>/dev/null
    check "gh: 本文の折り返しは端末の幅" "120" cat "$work/ghw.out"
else
    echo "skip gh の本文の幅（tmux が無い）"
fi

# --- click: tmux のユーザーオプション経由で値を受け取りポップアップを開く（tmux は偽物） ---
stub="$work/stub"; mkdir -p "$stub"
cat > "$stub/tmux" <<'STUB'
#!/bin/sh
case $1 in
    show) eval "printf '%s\n' \"\$STUB_$(printf '%s' "$3" | sed 's/^@open_path_//')\"" ;;
    display) case $* in *pane_in_mode*) printf '%s\n' "${STUB_pstate:-0 24 40}" ;; *client_width*) printf '%s\n' "${STUB_size:-310 56}" ;; *) printf '%s\n' "$STUB_version" ;; esac ;;
    display-popup|display-message|set|set-buffer|paste-buffer|select-pane|send-keys) for a in "$@"; do printf '[%s]' "$a"; done >> "$STUB_LOG"; echo >> "$STUB_LOG" ;;
    list-panes) printf '%s\n' "${STUB_panes:-}" ;;
    # capture-pane -p -t <pane> -S <行> -E <行> → STUB_row_<行> を順に（pane が違えば空）
    capture-pane) [ "$4" = "${STUB_pane:-}" ] || exit 0
                  i=$6; while [ "$i" -le "$8" ]; do eval "printf '%s\n' \"\${STUB_row_$i:-}\""; i=$((i + 1)); done ;;
esac
STUB
chmod +x "$stub/tmux"
# click_run <tmux バージョン> <行> <桁> <cwd> → 偽 tmux への呼び出しログ
click_run() {
    : > "$work/log"
    env PATH="$stub:$PATH" STUB_LOG="$work/log" STUB_version="$1" STUB_client=c1 STUB_link="${CLICK_LINK:-}" \
        STUB_line="$2" STUB_x="$3" STUB_cwd="$4" sh "$SCRIPT" click >/dev/null 2>&1
    cat "$work/log"
}
log=$(click_run 3.7c "see link.sh:3 here" 5 "$repo")
case $log in *"[set][-g][@open_path_view_path][$repo/link.sh]"*"[set][-g][@open_path_view_line][3]"*"[display-popup]"*"[-T]"*"view]"*) echo "ok   click でポップアップ（3.3+ はタイトル付き）";; *) echo "FAIL click でポップアップ: $log"; fail=1;; esac
log=$(click_run 3.2a "see link.sh:3 here" 5 "$repo")
# ポップアップの大きさは中身に合わせる（link.sh は 1 行 → 最小の 80x12）
case $log in *"[display-popup][-c][c1][-E][-w][80][-h][12]"*) echo "ok   click は中身に合わせた大きさで開く";; *) echo "FAIL click は中身に合わせた大きさで開く: $log"; fail=1;; esac
case $log in *"[display-popup]"*"[-T]"*) echo "FAIL tmux 3.2 では -T を付けない: $log"; fail=1;; *"[display-popup]"*) echo "ok   tmux 3.2 では -T を付けない";; *) echo "FAIL tmux 3.2 でポップアップ: $log"; fail=1;; esac
# 行末で折り返されたパスは、クリックした行の前後（pane の画面上の行）をつないで探す
log=$(STUB_pane=%7 STUB_y=4 STUB_row_5="   nk.sh。次" click_run 3.7c "本文は $repo/li" 12 /)
case $log in *"[@open_path_view_path][$repo/link.sh]"*) echo "ok   click: 次の行へ折り返したパス";; *) echo "FAIL click: 次の行へ折り返したパス: $log"; fail=1;; esac
log=$(STUB_pane=%7 STUB_y=4 STUB_row_3="本文は $repo/li" click_run 3.7c "   nk.sh。次" 4 /)
case $log in *"[@open_path_view_path][$repo/link.sh]"*) echo "ok   click: 前の行から折り返したパス";; *) echo "FAIL click: 前の行から折り返したパス: $log"; fail=1;; esac
log=$(STUB_pane=%7 STUB_y=4 STUB_pstate="1 24 40" STUB_row_5="   nk.sh。次" click_run 3.7c "本文は $repo/li" 12 /)
case $log in *"[display-message]"*) echo "ok   click: コピーモード中は前後の行をつながない";; *) echo "FAIL click: コピーモード中は前後の行をつながない: $log"; fail=1;; esac
log=$(STUB_pane=%7 STUB_y=23 STUB_row_24="   nk.sh。次" click_run 3.7c "本文は $repo/li" 12 /)
case $log in *"[display-message]"*) echo "ok   click: 最下行の次の行は見ない";; *) echo "FAIL click: 最下行の次の行は見ない: $log"; fail=1;; esac
# コミットの SHA はリポジトリの差分を開く
log=$(click_run 3.7c "see $short here" 6 "$crepo")
case $log in *"[@open_path_view_kind][commit]"*"[@open_path_view_path][$crepo]"*"[@open_path_view_sha][$short]"*"[display-popup]"*) echo "ok   click: SHA は差分を開く";; *) echo "FAIL click: SHA は差分を開く: $log"; fail=1;; esac
log=$(click_run 3.7c "see link.sh:3 here" 5 "$repo")
case $log in *"[@open_path_view_kind][]"*) echo "ok   click: ファイルは種類を空にする";; *) echo "FAIL click: ファイルは種類を空にする: $log"; fail=1;; esac
# PR / Issue の参照は GitHub の内容を開く（click では通信しない）
log=$(click_run 3.7c "PR #78 を見る" 4 "$grepo/sub")
case $log in *"[@open_path_view_kind][gh]"*"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][78]"*"[display-popup]"*"[-T][ acme/widget##78 ]"*) echo "ok   click: #番号は cwd のリポジトリの PR / Issue を開く";; *) echo "FAIL click: #番号は cwd のリポジトリの PR / Issue を開く: $log"; fail=1;; esac
# PR / Issue のポップアップは Markdown のプレビューと同じ大きさ（幅 108、高さは端末の 85%）
case $log in *"[display-popup][-c][c1][-E][-w][108][-h][47]"*) echo "ok   click: PR / Issue は Markdown と同じ大きさで開く";; *) echo "FAIL click: PR / Issue は Markdown と同じ大きさで開く: $log"; fail=1;; esac
log=$(click_run 3.7c "see o/r#5 now" 6 /)
case $log in *"[@open_path_view_kind][gh]"*"[@open_path_view_repo][o/r]"*"[@open_path_view_num][5]"*"[display-popup]"*) echo "ok   click: owner/repo#番号";; *) echo "FAIL click: owner/repo#番号: $log"; fail=1;; esac
# 省略形は @open_path_gh_alias の表で owner/repo にする（同名のファイルより優先）
echo x > "$grepo/bp#12" && mkdir -p "$grepo/acme" && echo x > "$grepo/acme/blueprint" && echo x > "$grepo/acme/blueprint#12"
log=$(STUB_gh_alias="bp=acme/blueprint" click_run 3.7c "see bp#12 now" 5 "$grepo")
case $log in *"[@open_path_view_kind][gh]"*"[@open_path_view_repo][acme/blueprint]"*"[@open_path_view_num][12]"*"[display-popup]"*"[-T][ acme/blueprint##12 ]"*) echo "ok   click: 登録した省略形";; *) echo "FAIL click: 登録した省略形: $log"; fail=1;; esac
log=$(click_run 3.7c "see bp#12 now" 5 "$grepo")
case $log in *"[@open_path_view_kind][gh]"*) echo "FAIL click: 表が無ければ省略形は GitHub で開かない: $log"; fail=1;; *) echo "ok   click: 表が無ければ省略形は GitHub で開かない";; esac
rm -rf "$grepo/bp#12" "$grepo/acme"
log=$(click_run 3.7c "see https://github.com/acme/widget/pull/9 ok" 10 /)
case $log in *"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][9]"*"[display-popup]"*) echo "ok   click: PR の URL";; *) echo "FAIL click: PR の URL: $log"; fail=1;; esac
log=$(CLICK_LINK="https://github.com/acme/widget/issues/3" click_run 3.7c "see the issue" 5 /)
case $log in *"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][3]"*"[display-popup]"*) echo "ok   click: ハイパーリンクの URL";; *) echo "FAIL click: ハイパーリンクの URL: $log"; fail=1;; esac
log=$(STUB_pane=%7 STUB_y=2 STUB_row_0="本文は $grepo/README.md。" STUB_row_1="" STUB_row_2="PR #78 を見る" click_run 3.7c "PR #78 を見る" 4 /)
case $log in *"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][78]"*) echo "ok   click: #番号は前後の行のリポジトリも探す";; *) echo "FAIL click: #番号は前後の行のリポジトリも探す: $log"; fail=1;; esac
mkdir -p "$grepo/docs" && echo x > "$grepo/docs/change#123"
log=$(click_run 3.7c "see docs/change#123" 6 "$grepo")
case $log in *"[@open_path_view_path][$grepo/docs/change#123]"*) echo "ok   click: # を含む実在のファイルはファイルとして開く";; *) echo "FAIL click: # を含む実在のファイルはファイルとして開く: $log"; fail=1;; esac
echo x > "$grepo/docs/README.md"
log=$(click_run 3.7c "see docs/README.md#123" 9 "$grepo")
case $log in *"[@open_path_view_path][$grepo/docs/README.md]"*) echo "ok   click: # の前が実在のファイルならファイルとして開く";; *) echo "FAIL click: # の前が実在のファイルならファイルとして開く: $log"; fail=1;; esac
log=$(click_run 3.7c "see docs/README.md#123" 9 "$grepo/sub")
case $log in *"[@open_path_view_path][$grepo/docs/README.md]"*) echo "ok   click: git ルートからの相対パスもファイルとして開く";; *) echo "FAIL click: git ルートからの相対パスもファイルとして開く: $log"; fail=1;; esac
log=$(click_run 3.7c "see docs/change#123" 6 "$grepo/sub")
case $log in *"[@open_path_view_path][$grepo/docs/change#123]"*) echo "ok   click: git ルートからの # を含むファイル";; *) echo "FAIL click: git ルートからの # を含むファイル: $log"; fail=1;; esac
mkdir -p "$grepo/acme" && echo x > "$grepo/acme/widget" && echo x > "$grepo/acme/widget#9"
log=$(click_run 3.7c "see https://github.com/acme/widget/pull/9 ok" 10 "$grepo")
case $log in *"[@open_path_view_kind][gh]"*"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][9]"*"[display-popup]"*) echo "ok   click: URL は同名のファイルがあっても PR を開く";; *) echo "FAIL click: URL は同名のファイルがあっても PR を開く: $log"; fail=1;; esac
rm -rf "$grepo/acme"
log=$(click_run 3.7c "see https://github.com/acme/widget/pull/9?foo=#78" 47 "$grepo")
case $log in *"[@open_path_view_repo][acme/widget]"*"[@open_path_view_num][9]"*"[display-popup]"*) echo "ok   click: URL の # の後ろも URL として開く";; *) echo "FAIL click: URL の # の後ろも URL として開く: $log"; fail=1;; esac
log=$(click_run 3.7c "PR #78 を見る" 4 "$lrepo")
case $log in *"[display-popup]"*) echo "FAIL click: リポジトリが分からなければ開かない: $log"; fail=1;; *"[display-message]"*) echo "ok   click: リポジトリが分からなければメッセージ";; *) echo "FAIL click: リポジトリが分からなければメッセージ: $log"; fail=1;; esac
# ファイル名だけなら、前後の行に出ているパスのディレクトリから探す
log=$(STUB_pane=%7 STUB_y=2 STUB_row_0="本文は $ctx/a/b/pr.md。" STUB_row_1="" STUB_row_2="verdict.md を見る" click_run 3.7c "verdict.md を見る" 2 /)
case $log in *"[@open_path_view_path][$ctx/a/b/verdict.md]"*) echo "ok   click: 前後の行のディレクトリから探す";; *) echo "FAIL click: 前後の行のディレクトリから探す: $log"; fail=1;; esac
log=$(click_run 3.7c "foo bar" 3 "$repo")
case $log in *"[display-message]"*) echo "ok   見つからなければメッセージ";; *) echo "FAIL 見つからなければメッセージ: $log"; fail=1;; esac
# click open: ポップアップを出さず、既定アプリ（open / xdg-open）で開く
for t in open xdg-open; do printf '#!/bin/sh\nfor a in "$@"; do printf "[%%s]" "$a"; done >> "$STUB_LOG"; echo " <- %s" >> "$STUB_LOG"\n' "$t" > "$stub/$t"; chmod +x "$stub/$t"; done
click_open() {
    : > "$work/log"
    env PATH="$stub:$PATH" STUB_LOG="$work/log" STUB_version=3.7c STUB_client=c1 STUB_link="" \
        STUB_line="$1" STUB_x="$2" STUB_cwd="$3" sh "$SCRIPT" click open >/dev/null 2>&1
    cat "$work/log"
}
log=$(click_open "see link.sh:3 here" 5 "$repo")
case $log in
    *"[display-popup]"*) echo "FAIL click open はポップアップを出さない: $log"; fail=1 ;;
    *"[$repo/link.sh] <- open"*) echo "ok   click open は既定アプリで開く" ;;
    *) echo "FAIL click open は既定アプリで開く: $log"; fail=1 ;;
esac
log=$(click_open "PR #78 を見る" 4 "$grepo")
case $log in *"[https://github.com/acme/widget/issues/78] <- open"*) echo "ok   click open: #番号はブラウザで開く" ;; *) echo "FAIL click open: #番号はブラウザで開く: $log"; fail=1 ;; esac
log=$(click_open "see docs/ here" 6 "$repo")
case $log in *"[$repo/docs] <- open"*) echo "ok   click open はディレクトリも開く" ;; *) echo "FAIL click open はディレクトリも開く: $log"; fail=1 ;; esac
log=$(click_open "foo bar" 3 "$repo")
case $log in *"<- open"*) echo "FAIL click open: 見つからなければ開かない: $log"; fail=1 ;; *"[display-message]"*) echo "ok   click open: 見つからなければメッセージ" ;; *) echo "FAIL click open: 見つからなければメッセージ: $log"; fail=1 ;; esac

# --- click link（普通のクリック）: URL・PR / Issue の参照だけをブラウザで開き、それ以外は何もしない ---
click_link() {
    : > "$work/log"
    env PATH="$stub:$PATH" STUB_LOG="$work/log" STUB_version=3.7c STUB_client=c1 STUB_link="${CLICK_LINK:-}" \
        STUB_line="$1" STUB_x="$2" STUB_cwd="$3" sh "$SCRIPT" click link >/dev/null 2>&1
    cat "$work/log"
}
# opened <名前> <URL> <ログ>: その URL だけをブラウザで開き、ポップアップもメッセージも出さない
opened() {
    if [ "$3" = "[$2] <- open" ]; then echo "ok   $1"; else echo "FAIL $1: $3"; fail=1; fi
}
# silent <名前> <ログ>: 何もしない（開かない・ポップアップもメッセージも出さない）
silent() {
    if [ -z "$2" ]; then echo "ok   $1"; else echo "FAIL $1: $2"; fail=1; fi
}
opened "link: URL"                         "https://example.com/a?b=1&c=2" "$(click_link "see https://example.com/a?b=1&c=2 ok" 10 /)"
opened "link: PR の URL はそのまま（# の続きも）" "https://github.com/acme/widget/pull/9#issuecomment-1" \
    "$(click_link "see https://github.com/acme/widget/pull/9#issuecomment-1 ok" 10 "$grepo")"
opened "link: ハイパーリンク（OSC 8）の URL" "https://example.com/docs" "$(CLICK_LINK="https://example.com/docs" click_link "the docs here" 5 /)"
opened "link: #番号は cwd のリポジトリ"     "https://github.com/acme/widget/issues/78" "$(click_link "PR #78 を見る" 4 "$grepo/sub")"
opened "link: owner/repo#番号"              "https://github.com/o/r/issues/5" "$(click_link "see o/r#5 now" 6 /)"
opened "link: 登録した省略形"               "https://github.com/acme/blueprint/issues/12" "$(STUB_gh_alias="bp=acme/blueprint" click_link "see bp#12 now" 5 "$grepo")"
opened "link: 別の URL の中の URL は外側を開く" "https://login.example.com/?next=https://github.com/acme/widget/pull/9" \
    "$(click_link "go https://login.example.com/?next=https://github.com/acme/widget/pull/9 now" 40 /)"
# 文の中の URL: 前後の括弧・引用符・句読点は URL に含めない（URL の中で対になる括弧は残す）
opened "link: 和文の括弧に挟まれた URL"     "https://example.com/a" "$(click_link "（https://example.com/a）を見る" 5 /)"
opened "link: 文末の句点"                   "https://example.com/a" "$(click_link "see https://example.com/a." 6 /)"
opened "link: URL の中で対になる括弧"       "https://en.wikipedia.org/wiki/Foo_(bar)" "$(click_link "(see https://en.wikipedia.org/wiki/Foo_(bar))" 10 /)"
opened "link: Markdown のリンク"            "https://example.com/a" "$(click_link "[docs](https://example.com/a)" 10 /)"
opened "link: 山括弧"                       "https://example.com/a" "$(click_link "<https://example.com/a>" 5 /)"
opened "link: 引用符"                       "https://example.com/a" "$(click_link "href=\"https://example.com/a\" x" 10 /)"
silent "link: URL の後ろの和文は開かない"   "$(click_link "https://example.com/a）を見る" 23 /)"
opened "link: 空白なしで続く和文は含めない" "https://example.com/a" "$(click_link "https://example.com/aを見る" 5 /)"
# Claude Code が改行で折り返し、続きを字下げした URL は前後の行をつなぐ（pane の幅 40 → 20 桁以上の行）
opened "link: 次の行へ折り返した URL"       "https://example.com/very/long/path?q=1" \
    "$(STUB_pane=%7 STUB_y=4 STUB_row_5="    th?q=1 end" click_link "see https://example.com/very/long/pa" 10 /)"
opened "link: 前の行から折り返した URL"     "https://example.com/very/long/path?q=1" \
    "$(STUB_pane=%7 STUB_y=5 STUB_row_4="see https://example.com/very/long/pa" click_link "    th?q=1 end" 5 /)"
opened "link: 字下げの無い次の行はつながない" "https://example.com/very/long/pa" \
    "$(STUB_pane=%7 STUB_y=4 STUB_row_5="th?q=1 end" click_link "see https://example.com/very/long/pa" 10 /)"
opened "link: 短い行は次の行とつながない"   "https://e.com/a" \
    "$(STUB_pane=%7 STUB_y=4 STUB_row_5="    th?q=1 end" click_link "x https://e.com/a" 5 /)"
opened "link: コピーモード中は前後の行をつながない" "https://example.com/very/long/pa" \
    "$(STUB_pane=%7 STUB_y=4 STUB_pstate="1 24 40" STUB_row_5="    th?q=1 end" click_link "see https://example.com/very/long/pa" 10 /)"
# URL・参照でなければ何もしない（ファイル・SHA・ただの語。メッセージも出さない）
silent "link: ファイルは開かない"           "$(click_link "see link.sh:3 here" 5 "$repo")"
silent "link: SHA は開かない"               "$(click_link "see $short here" 6 "$crepo")"
silent "link: ただの語"                     "$(click_link "foo bar" 3 "$repo")"
silent "link: file:// のハイパーリンクは開かない" "$(CLICK_LINK="file://$repo/link.sh" click_link "link.sh" 2 "$repo")"
# http(s) 以外・空白や制御文字を含むリンクはブラウザに渡さない
silent "link: http(s) 以外のスキーム"       "$(CLICK_LINK="javascript:alert(1)" click_link "click me" 2 /)"
silent "link: 空白を含むリンク"             "$(CLICK_LINK="https://example.com/a b" click_link "click me" 2 /)"
silent "link: 制御文字を含むリンク"         "$(CLICK_LINK="$(printf 'https://example.com/\033x')" click_link "click me" 2 /)"
log=$(click_link "PR #78 を見る" 4 "$lrepo")
case $log in *"<- open"*) echo "FAIL link: リポジトリが分からなければ開かない: $log"; fail=1 ;; *"[display-message]"*) echo "ok   link: リポジトリが分からなければメッセージ" ;; *) echo "FAIL link: リポジトリが分からなければメッセージ: $log"; fail=1 ;; esac
# 右クリックの「Open in Default App」「Preview Path」も、GitHub 以外の URL はブラウザで開く
opened "click open: URL"                    "https://example.com/a" "$(click_open "see https://example.com/a ok" 6 /)"
opened "click open: PR の URL はそのまま"   "https://github.com/acme/widget/pull/9#discussion_r1" \
    "$(click_open "see https://github.com/acme/widget/pull/9#discussion_r1 ok" 6 /)"
opened "click: GitHub 以外の URL はブラウザ" "https://example.com/a" "$(click_run 3.7c "see https://example.com/a ok" 6 /)"
opened "click: GitHub 以外のハイパーリンクはブラウザ" "https://example.com/docs" "$(CLICK_LINK="https://example.com/docs" click_run 3.7c "the docs here" 5 /)"

# フォーマット展開される display-popup の -d / コマンド文字列に画面由来の値を載せない
fmtdir="$work/#(touch pwned5)"; mkdir -p "$fmtdir"; echo x > "$fmtdir/a.txt"
log=$(click_run 3.7c "a.txt" 0 "$fmtdir")
popup_line=$(printf '%s\n' "$log" | grep '^\[display-popup\]')
want_cmd="[sh '$SCRIPT' view]"
case $popup_line in
    *"[-d]"*) echo "FAIL popup に -d を渡さない: $popup_line"; fail=1 ;;
    *"##(touch pwned5)/a.txt ]$want_cmd") echo "ok   popup に画面由来の値を載せない（タイトルは # をエスケープ）" ;;
    *) echo "FAIL popup に画面由来の値を載せない: $popup_line"; fail=1 ;;
esac
evil="$work/$(printf 'cwd\n\ttouch\tpwned3')"; mkdir -p "$evil"
(cd "$work" && click_run 3.7c "$(printf 'x\n touch pwned4')" 0 "$evil" >/dev/null)
if [ ! -e "$work/pwned3" ] && [ ! -e "$work/pwned4" ] && [ ! -e "$evil/pwned3" ]; then echo "ok   改行入りの cwd / 行でもコマンドを実行しない"; else echo "FAIL 改行入りの cwd / 行でもコマンドを実行しない"; fail=1; fi

# --- Claude Code に渡す（ポップアップの c）: 文書を指す文字列を Claude の pane の入力欄へ ---
check "claude: ファイル"             "@/p/a.md "                       sh "$SCRIPT" claude_text file /p/a.md
check "claude: 行番号付き"           "@/p/a.md (L42) "                 sh "$SCRIPT" claude_text file /p/a.md 42
check "claude: 空白を含むパス"       '"/p/a b.md" '                    sh "$SCRIPT" claude_text file "/p/a b.md"
check "claude: 空白を含むパスの行番号" '"/p/a b.md" (L42) '              sh "$SCRIPT" claude_text file "/p/a b.md" 42
# 改行等の制御文字があると Claude の入力欄で送信されうるため渡さない
check "claude: 改行を含むパスは渡さない" ""                            sh "$SCRIPT" claude_text file "$(printf '/p/a\nb.md')"
check "claude: 制御文字を含むリポジトリ" ""                            sh "$SCRIPT" claude_text commit "$(printf '/r\033x')" abc1234
check "claude: PR"                   "https://github.com/o/r/pull/78 " sh "$SCRIPT" claude_text gh o/r 78 pr
check "claude: Issue"                "https://github.com/o/r/issues/3 " sh "$SCRIPT" claude_text gh o/r 3 issue
check "claude: 種類不明の参照"       "https://github.com/o/r/issues/3 " sh "$SCRIPT" claude_text gh o/r 3 ""
check "claude: コミット"             "commit abc1234 (/repo) "         sh "$SCRIPT" claude_text commit /repo abc1234
# claude_ancestors: ps の表（pid ppid 実行ファイル）から、claude / claude.exe とその祖先の pid
pstab=$(printf '%s\n' "1 0 launchd" "100 1 -zsh" "101 100 node" "102 101 /opt/bin/claude" "200 1 zsh" "201 200 vim" "300 1 zsh" "301 300 claude.exe" "400 1 zsh" "401 400 claude-helper" "  500   1 /bin/zsh" "501 500 /Users/Jane Doe/.local/bin/claude")
check "claude: 祖先の pid"           "$(printf '%s\n' 1 100 101 102 300 301 500 501)" sh -c 'printf "%s\n" "$2" | sh "$1" claude_ancestors | sort -n' _ "$SCRIPT" "$pstab"
printf '#!/bin/sh\nprintf "%%s\\n" "$STUB_ps"\n' > "$stub/ps"; chmod +x "$stub/ps"
# to_claude <文字列>: クリックした pane（Claude でなければ同じ window の Claude の pane）へ貼り付ける
to_claude_run() {
    : > "$work/log"
    env PATH="$stub:$PATH" STUB_LOG="$work/log" STUB_ps="$pstab" STUB_pane="$1" STUB_panes="$2" sh "$SCRIPT" to_claude "$3" >/dev/null 2>&1
    cat "$work/log"
}
log=$(to_claude_run %1 "$(printf '%s\n' "%1 100" "%2 200")" "@/p/a.md ")
case $log in *"[set-buffer][-b][open-path-claude][@/p/a.md ]"*"[paste-buffer][-p][-d][-b][open-path-claude][-t][%1]"*"[select-pane][-t][%1]"*) echo "ok   claude: クリックした pane へ貼り付ける" ;; *) echo "FAIL claude: クリックした pane へ貼り付ける: $log"; fail=1 ;; esac
log=$(to_claude_run %2 "$(printf '%s\n' "%1 200" "%2 400" "%3 300")" "@/p/a.md ")
case $log in *"[paste-buffer][-p][-d][-b][open-path-claude][-t][%3]"*"[select-pane][-t][%3]"*) echo "ok   claude: 同じ window の Claude の pane へ貼り付ける" ;; *) echo "FAIL claude: 同じ window の Claude の pane へ貼り付ける: $log"; fail=1 ;; esac
log=$(STUB_pstate="1 24 40" to_claude_run %1 "$(printf '%s\n' "%1 100")" "@/p/a.md ")
case $log in *"[send-keys][-t][%1][-X][cancel]"*"[paste-buffer]"*) echo "ok   claude: コピーモードを解除してから貼り付ける" ;; *) echo "FAIL claude: コピーモードを解除してから貼り付ける: $log"; fail=1 ;; esac
log=$(to_claude_run %1 "$(printf '%s\n' "%1 100")" "$(printf '@/p/a\nb.md ')")
case $log in *"[paste-buffer]"*) echo "FAIL claude: 制御文字を含む文字列は貼り付けない: $log"; fail=1 ;; *) echo "ok   claude: 制御文字を含む文字列は貼り付けない" ;; esac
log=$(to_claude_run %1 "$(printf '%s\n' "%1 200" "%2 400")" "@/p/a.md ")
case $log in *"[paste-buffer]"*) echo "FAIL claude: Claude の pane が無ければ貼り付けない: $log"; fail=1 ;; *"[display-message]"*) echo "ok   claude: Claude の pane が無ければメッセージ" ;; *) echo "FAIL claude: Claude の pane が無ければメッセージ: $log"; fail=1 ;; esac

# --- Option + クリックはマウスを使うアプリ（fullscreen の Claude Code 等）の中でも tmux が処理する ---
optblock=$(sed -n '/^bind -n M-MouseDown1Pane/,/^}/p' "$(dirname "$SCRIPT")/../keybindings.conf")
case $optblock in
    *mouse_any_flag*|*"send-keys -M"*) echo "FAIL Option + クリックをアプリへ渡さない: $optblock"; fail=1 ;;
    *"open-path.sh click"*) echo "ok   Option + クリックをアプリへ渡さない" ;;
    *) echo "FAIL Option + クリックの割り当てが無い"; fail=1 ;;
esac
# 普通のクリックはボタンを離したとき（ドラッグの範囲選択は MouseDragEnd で別）。マウスを使う
# アプリ・コピーモードへはそのまま渡し、行に URL か #数字 がある時だけ open-path を起動する
upblock=$(sed -n '/^bind -n MouseUp1Pane/,/^}/p' "$(dirname "$SCRIPT")/../keybindings.conf")
case $upblock in
    *"#{||:#{mouse_any_flag},#{pane_in_mode}}"*"send-keys -M"*"#{m/r:"*"#{mouse_line}"*"open-path.sh click link"*) echo "ok   普通のクリックは URL・#番号の行だけ open-path へ" ;;
    *) echo "FAIL 普通のクリックの割り当て: $upblock"; fail=1 ;;
esac

# --- open-path-menu.sh: 既定の右クリックメニューへ「Preview Path」を差し込む ---
MENU="$(dirname "$SCRIPT")/open-path-menu.sh"
sample='bind-key  -T root MouseDown3Pane          if-shell -F -t = "#{mouse_any_flag}" { select-pane -t = ; send-keys -M } { display-menu -T "#[align=centre]#{pane_index}" -t = -x M -y M "#{?mouse_word,Copy #[underscore]#{=/9/...:mouse_word},}" c { copy-mode -q ; set-buffer "#{q:mouse_word}" } Kill X { kill-pane } }'
out=$(printf '%s\n' "$sample" | sh "$MENU" transform)
case $out in
    "bind-key -n MouseDown3Pane set -gF @open_path_client "*) echo "ok   menu: 先頭で値を退避する" ;;
    *) echo "FAIL menu: 先頭で値を退避する: $out"; fail=1 ;;
esac
case $out in
    *"-x M -y M \"Preview Path\" o { run-shell -b '$SCRIPT click' } \"Open in Default App\" a { run-shell -b '$SCRIPT click open' } '' \"#{?mouse_word,Copy"*"Kill X { kill-pane } }") echo "ok   menu: 項目を差し込み既定の項目を残す" ;;
    *) echo "FAIL menu: 項目を差し込み既定の項目を残す: $out"; fail=1 ;;
esac
case $out in
    *"set -gF -t = @open_path_cwd '#{pane_current_path}'"*"set -gF @open_path_line '#{mouse_line}'"*) echo "ok   menu: クリックした pane の cwd と行を退避" ;;
    *) echo "FAIL menu: クリックした pane の cwd と行を退避: $out"; fail=1 ;;
esac
# マウスを使うアプリ（fullscreen の Claude Code 等）の中でも tmux のメニューを出す
real='bind-key  -T root MouseDown3Pane          if-shell -F -t = "#{||:#{mouse_any_flag},#{&&:#{pane_in_mode},#{?#{m/r:(copy|view)-mode,#{pane_mode}},0,1}}}" { select-pane -t = ; send-keys -M } { display-menu -T x -t = -x M -y M Kill X { kill-pane } }'
out=$(printf '%s\n' "$real" | sh "$MENU" transform)
case $out in
    *mouse_any_flag*) echo "FAIL menu: アプリがマウスを使っていてもメニューを出す: $out"; fail=1 ;;
    *'if-shell -F -t = "#{&&:#{pane_in_mode},#{?#{m/r:(copy|view)-mode,#{pane_mode}},0,1}}"'*"Preview Path"*) echo "ok   menu: アプリがマウスを使っていてもメニューを出す" ;;
    *) echo "FAIL menu: アプリがマウスを使っていてもメニューを出す: $out"; fail=1 ;;
esac
check "menu: 目印が無ければ何もしない" "" sh -c 'printf "%s\n" "bind-key -T root MouseDown3Pane display-menu -T x" | sh "$1" transform' _ "$MENU"

exit $fail
