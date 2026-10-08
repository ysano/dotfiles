#!/bin/bash
# .claude/statusline-command.sh の利用枠（5h/7d）表示・色ロジックの検証
# 使い方: bash test_statusline.sh   （jq が必要）
set -u
SCRIPT="$(cd "$(dirname "$0")" && pwd)/.claude/statusline-command.sh"
# プロファイル表示・キャッシュを実環境から切り離す（プロファイル名は "test" に固定する）
work=$(mktemp -d) && [ -d "$work" ] || { echo "FAIL mktemp -d"; exit 1; }
trap 'rm -rf "$work"' EXIT
CLAUDE_CONFIG_DIR="$work/.claude-test"
mkdir -p "$CLAUDE_CONFIG_DIR"
export CLAUDE_CONFIG_DIR

now=$(date +%s)
fail=0

# 色コードを [G]/[Y]/[R] に置換し、その他のエスケープと先頭のモデル名を除去
normalize() {
    sed -e 's/\x1b\[38;5;76m/[G]/g' -e 's/\x1b\[38;5;220m/[Y]/g' -e 's/\x1b\[38;5;196m/[R]/g' \
        -e 's/\x1b\[[0-9;]*m//g' -e 's/^O │ test │ //' -e 's/^O │ test$/O/'
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

# 使用中のプロファイル名（CLAUDE_CONFIG_DIR）を表示する。同じドメインの別アカウント
# （例: 会社の別 Team）でも、どの利用枠を使っているか区別できるようにする
# profile <名前> <CLAUDE_CONFIG_DIR|unset> <期待するプロファイル表示>
profile() {
    local out
    if [ "$2" = unset ]; then
        out=$(echo '{"model":{"display_name":"O"}}' | env -u CLAUDE_CONFIG_DIR HOME="$work/home" bash "$SCRIPT" 2>&1 | tail -1 | sed 's/\x1b\[[0-9;]*m//g')
    else
        out=$(echo '{"model":{"display_name":"O"}}' | env HOME="$work/home" CLAUDE_CONFIG_DIR="$2" bash "$SCRIPT" 2>&1 | tail -1 | sed 's/\x1b\[[0-9;]*m//g')
    fi
    if [ "$out" = "O │ $3" ]; then echo "ok   profile $1: $out"; else echo "FAIL profile $1: got '$out' want 'O │ $3'"; fail=1; fi
}
mkdir -p "$work/home/.claude" "$work/home/.claude-karin" "$work/home/other dir"
profile "未指定は default"         unset                         default
profile ".claude は default"       "$work/home/.claude"          default
profile ".claude/ 末尾 / も default" "$work/home/.claude/"       default
profile ".claude-karin は karin"   "$work/home/.claude-karin"    karin
profile ".claude-karin/ 末尾 /"    "$work/home/.claude-karin/"   karin
profile "その他は名前そのまま"     "$work/home/other dir"        "other dir"
profile "末尾 / が複数でも karin"  "$work/home/.claude-karin//"  karin
profile "末尾 / が複数でも default" "$work/home/.claude//"      default
profile "HOME の外の .claude-x も x" "$work/opt/.claude-x"      x
profile "HOME の外の .claude は .claude" "$work/opt/.claude"    .claude
# 既定の判定は表記の違い（HOME の末尾 /、symlink 経由）に左右されない
mkdir -p "$work/real/.claude"; ln -s "$work/real" "$work/alias"
out=$(echo '{"model":{"display_name":"O"}}' | env HOME="$work/home/" CLAUDE_CONFIG_DIR="$work/home/.claude" bash "$SCRIPT" 2>&1 | tail -1 | sed 's/\x1b\[[0-9;]*m//g')
if [ "$out" = "O │ default" ]; then echo "ok   profile HOME 末尾 / でも default"; else echo "FAIL profile HOME 末尾 / でも default: got '$out'"; fail=1; fi
out=$(echo '{"model":{"display_name":"O"}}' | env HOME="$work/alias" CLAUDE_CONFIG_DIR="$work/real/.claude" bash "$SCRIPT" 2>&1 | tail -1 | sed 's/\x1b\[[0-9;]*m//g')
if [ "$out" = "O │ default" ]; then echo "ok   profile symlink 経由の HOME でも default"; else echo "FAIL profile symlink 経由の HOME でも default: got '$out'"; fail=1; fi
profile ".claude/. も default"       "$work/home/.claude/."          default
profile ".claude-karin/. も karin"   "$work/home/.claude-karin/."    karin
ln -s "$work/home/.claude" "$work/home-alias-claude"
profile "symlink 別名でも default"   "$work/home-alias-claude"       default
mkdir -p "$work/home/.claude-karin/sub"
profile ".claude-karin/sub/.. も karin" "$work/home/.claude-karin/sub/.." karin
profile "未作成でも .claude-x は x"  "$work/nowhere/.claude-x"       x
profile "ルートは /"                "/"                            /
profile "ルート // も /"            "//"                           /

# 外から来る文字列（プロファイル名・モデル名・ディレクトリ名）は表示制御として解釈しない
# raw <名前> <CLAUDE_CONFIG_DIR> <入力 JSON> <期待する 2 行目（色を除去後）>
raw() {
    local out lines
    out=$(printf '%s' "$3" | env HOME="$work/home" CLAUDE_CONFIG_DIR="$2" bash "$SCRIPT" 2>&1)
    lines=$(printf '%s\n' "$out" | wc -l | tr -d ' ')
    out=$(printf '%s' "$out" | tail -1 | sed 's/\x1b\[[0-9;]*m//g')
    if [ "$out" = "$4" ] && [ "$lines" -le 2 ] && ! printf '%s' "$out" | LC_ALL=C grep -q "$(printf '\033')"; then
        echo "ok   raw $1: $out"
    else
        echo "FAIL raw $1: got '$out' (lines=$lines) want '$4'"; fail=1
    fi
}
raw "名前の \\n は改行しない"     "$work/home/.claude-team\\nadmin" '{"model":{"display_name":"O"}}' 'O │ team?nadmin'
raw "名前の \\c で途切れない"     "$work/home/.claude-a\\cb" '{"model":{"display_name":"O"},"context_window":{"remaining_percentage":75}}' 'O │ a?cb │ ◉75%'
raw "モデル名の ESC 列を出さない" "$work/home/.claude-k" '{"model":{"display_name":"O\\033[2J"}}' 'O?033[2J │ k'
raw "モデル名の実制御文字を出さない" "$work/home/.claude-k" "{\"model\":{\"display_name\":\"O\\u001b[2J\"}}" 'O?[2J │ k'

# メールドメインはもう表示しない（同じドメインの別アカウントを区別できないため）
echo '{"oauthAccount":{"emailAddress":"me@example.com"}}' > "$work/home/.claude-karin/.claude.json"
profile "ドメインは出さない"       "$work/home/.claude-karin"    karin

exit $fail
