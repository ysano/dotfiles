#!/bin/bash
# link.sh の Claude Code 追加プロファイル（~/.claude-*、CLAUDE_CONFIG_DIR 用）向け共有配備の検証
# 使い方: bash test_link.sh   （zsh が必要。HOME と XDG_CONFIG_HOME を fake に隔離して実行する）
set -u
REPO="$(cd "$(dirname "$0")" && pwd)"
# fake HOME が作れなければ実 $HOME や / を操作しないよう即中断する
fake_home=$(mktemp -d) && [ -d "$fake_home" ] || { echo "FAIL mktemp -d"; exit 1; }
trap 'rm -rf "$fake_home"' EXIT
fail=0

# link.sh は HOME に加えて XDG_CONFIG_HOME を参照するため、両方を fake に向ける
run_link() { HOME="$fake_home" XDG_CONFIG_HOME="$fake_home/.config" zsh "$REPO/link.sh" >/dev/null 2>&1; }

# 呼び出し元の実 XDG_CONFIG_HOME を書き換えていないことを最後に検査する
real_xdg="${XDG_CONFIG_HOME:-}"
real_xdg_before=""
[ -n "$real_xdg" ] && [ -d "$real_xdg" ] && real_xdg_before=$(ls -la "$real_xdg")

ok()   { echo "ok   $1"; }
ng()   { echo "FAIL $1"; fail=1; }

# assert_link <名前> <リンク> <期待するリンク先>
assert_link() {
    if [ -L "$2" ] && [ "$(readlink "$2")" = "$3" ]; then ok "$1"; else ng "$1: $2 -> $(readlink "$2" 2>/dev/null || echo '(not a symlink)')"; fi
}

# 既定プロファイル（~/.claude）に共有元となる資産を用意する
ln -s "$REPO" "$fake_home/dotfiles"
mkdir -p "$fake_home/.claude/prompts" "$fake_home/.claude/skills/foo" "$fake_home/.claude/skills/synced/acct-main"
echo "global rules" > "$fake_home/.claude/CLAUDE.md"
echo "p" > "$fake_home/.claude/prompts/x.md"
echo "s" > "$fake_home/.claude/skills/foo/SKILL.md"

# 追加プロファイル: .claude.json を持つ config dir。既存の実ファイルとアカウント固有の synced を持つ
mkdir -p "$fake_home/.claude-alt/skills/synced/acct-alt"
echo '{}' > "$fake_home/.claude-alt/.claude.json"
echo "stale" > "$fake_home/.claude-alt/CLAUDE.md"
# .claude.json を持たない ~/.claude-* は config dir ではないので触らない
mkdir -p "$fake_home/.claude-misc"

if run_link; then ok "link.sh が正常終了"; else ng "link.sh が正常終了"; fi

p="$fake_home/.claude-alt"
assert_link "CLAUDE.md を共有"     "$p/CLAUDE.md"     "$fake_home/.claude/CLAUDE.md"
assert_link "prompts を共有"       "$p/prompts"       "$fake_home/.claude/prompts"
assert_link "skill を個別に共有"   "$p/skills/foo"    "$fake_home/.claude/skills/foo"

if [ -f "$p/CLAUDE.md.orig" ] && [ "$(cat "$p/CLAUDE.md.orig")" = "stale" ]; then ok "既存の実ファイルは .orig に退避"; else ng "既存の実ファイルは .orig に退避"; fi
if [ -d "$p/skills/synced/acct-alt" ] && [ ! -L "$p/skills/synced" ] && [ ! -e "$p/skills/synced/acct-main" ]; then
    ok "アカウント固有の skills/synced は共有しない"
else
    ng "アカウント固有の skills/synced は共有しない"
fi

if [ -z "$(ls -A "$fake_home/.claude-misc")" ]; then ok ".claude.json の無い .claude-* は対象外"; else ng ".claude.json の無い .claude-* は対象外"; fi

# 再実行しても symlink が張り直されるだけで壊れない（冪等）
run_link
assert_link "再実行後も CLAUDE.md を共有" "$p/CLAUDE.md" "$fake_home/.claude/CLAUDE.md"
assert_link "再実行後も prompts を共有"   "$p/prompts"   "$fake_home/.claude/prompts"
if [ ! -e "$fake_home/.claude/prompts/prompts" ]; then ok "再実行でリンク先の中にリンクを作らない"; else ng "再実行でリンク先の中にリンクを作らない"; fi

# プロファイルが無ければ何も作らない
rm -rf "$p"
run_link
if [ ! -e "$p" ]; then ok "未作成のプロファイルは作らない"; else ng "未作成のプロファイルは作らない"; fi

# プロファイルの skills/ が共有元 skills/ への symlink なら、個別リンクで共有元を壊さない
mkdir -p "$p"
echo '{}' > "$p/.claude.json"
ln -s "$fake_home/.claude/skills" "$p/skills"
run_link
if [ -d "$fake_home/.claude/skills/foo" ] && [ ! -L "$fake_home/.claude/skills/foo" ] && [ ! -e "$fake_home/.claude/skills/foo.orig" ]; then
    ok "skills/ が共有元を指すプロファイルでも共有元の skill を壊さない"
else
    ng "skills/ が共有元を指すプロファイルでも共有元の skill を壊さない"
fi
rm -rf "$p"

# プロファイルが ~/.claude 自体への symlink なら共有元を壊さない（自己参照リンクを作らない）
# .claude.json の有無でなく実体比較で除外されることを確かめるため、共有元にも .claude.json を置く
echo '{}' > "$fake_home/.claude/.claude.json"
ln -s .claude "$p"
run_link
if [ ! -L "$fake_home/.claude/CLAUDE.md" ] && [ "$(cat "$fake_home/.claude/CLAUDE.md" 2>/dev/null)" = "global rules" ] \
    && [ ! -L "$fake_home/.claude/prompts" ] && [ ! -L "$fake_home/.claude/skills/foo" ]; then
    ok ".claude を指すプロファイルはスキップ"
else
    ng ".claude を指すプロファイルはスキップ"
fi

if [ -n "$real_xdg_before" ]; then
    if [ "$(ls -la "$real_xdg")" = "$real_xdg_before" ]; then ok "呼び出し元の XDG_CONFIG_HOME を変更しない"; else ng "呼び出し元の XDG_CONFIG_HOME を変更しない"; fi
fi

exit $fail
