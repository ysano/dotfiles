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
check "diff の b/ 側"            "b/link.sh"         sh "$SCRIPT" extract "diff a/link.sh b/link.sh" 17
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

# 画面上の文字列はシェルに展開しない
sh "$SCRIPT" resolve "\$(touch $work/pwned)" "$repo" >/dev/null 2>&1
sh "$SCRIPT" extract "\$(touch $work/pwned2)" 3 >/dev/null 2>&1
if [ ! -e "$work/pwned" ] && [ ! -e "$work/pwned2" ]; then echo "ok   コマンド置換を実行しない"; else echo "FAIL コマンド置換を実行しない"; fail=1; fi

# --- click: tmux のユーザーオプション経由で値を受け取りポップアップを開く（tmux は偽物） ---
stub="$work/stub"; mkdir -p "$stub"
cat > "$stub/tmux" <<'STUB'
#!/bin/sh
case $1 in
    show) eval "printf '%s\n' \"\$STUB_$(printf '%s' "$3" | sed 's/^@open_path_//')\"" ;;
    display) printf '%s\n' "$STUB_version" ;;
    display-popup|display-message|set) for a in "$@"; do printf '[%s]' "$a"; done >> "$STUB_LOG"; echo >> "$STUB_LOG" ;;
esac
STUB
chmod +x "$stub/tmux"
# click_run <tmux バージョン> <行> <桁> <cwd> → 偽 tmux への呼び出しログ
click_run() {
    : > "$work/log"
    env PATH="$stub:$PATH" STUB_LOG="$work/log" STUB_version="$1" STUB_client=c1 STUB_link="" \
        STUB_line="$2" STUB_x="$3" STUB_cwd="$4" sh "$SCRIPT" click >/dev/null 2>&1
    cat "$work/log"
}
log=$(click_run 3.7c "see link.sh:3 here" 5 "$repo")
case $log in *"[set][-g][@open_path_view_path][$repo/link.sh]"*"[set][-g][@open_path_view_line][3]"*"[display-popup]"*"[-T]"*"view]"*) echo "ok   click でポップアップ（3.3+ はタイトル付き）";; *) echo "FAIL click でポップアップ: $log"; fail=1;; esac
log=$(click_run 3.2a "see link.sh:3 here" 5 "$repo")
case $log in *"[display-popup]"*"[-T]"*) echo "FAIL tmux 3.2 では -T を付けない: $log"; fail=1;; *"[display-popup]"*) echo "ok   tmux 3.2 では -T を付けない";; *) echo "FAIL tmux 3.2 でポップアップ: $log"; fail=1;; esac
log=$(click_run 3.7c "foo bar" 3 "$repo")
case $log in *"[display-message]"*) echo "ok   見つからなければメッセージ";; *) echo "FAIL 見つからなければメッセージ: $log"; fail=1;; esac
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

exit $fail
