#!/bin/bash
# .zsh/ssh_agent.zsh の検証（固定ソケットで agent を 1 つだけ動かす）
# 使い方: bash test_ssh_agent.sh   （zsh と ssh-agent が必要。HOME を fake に隔離して実行する）
set -u
REPO="$(cd "$(dirname "$0")" && pwd)"
fake_home=$(mktemp -d) && [ -d "$fake_home" ] || { echo "FAIL mktemp -d"; exit 1; }
# このテストが起動した agent だけを止める（-a のパスで特定する）
cleanup() { pkill -f "ssh-agent -a $fake_home/" 2>/dev/null; rm -rf "$fake_home"; }
trap cleanup EXIT
fail=0
ok() { echo "ok   $1"; }
ng() { echo "FAIL $1"; fail=1; }

SOCK="$fake_home/.ssh/agent.sock"
# run_zsh <SSH_AUTH_SOCK の初期値（空なら unset）> <source 後に実行する zsh コード>
run_zsh() {
  if [ -n "$1" ]; then
    env -i HOME="$fake_home" PATH="$PATH" SSH_AUTH_SOCK="$1" zsh -f -c "source '$REPO/.zsh/ssh_agent.zsh'; $2"
  else
    env -i HOME="$fake_home" PATH="$PATH" zsh -f -c "source '$REPO/.zsh/ssh_agent.zsh'; $2"
  fi
}
agent_count() { pgrep -fc "ssh-agent -a $SOCK" 2>/dev/null || true; }

# 1. agent が無い状態から: 固定ソケットで起動し、SSH_AUTH_SOCK がそこを指す
out=$(run_zsh "" 'print -r -- "$SSH_AUTH_SOCK"; ssh-add -l >/dev/null 2>&1; print rc=$?')
[ "$(echo "$out" | head -1)" = "$SOCK" ] && ok "SSH_AUTH_SOCK を固定ソケットに設定" || ng "SSH_AUTH_SOCK: $out"
echo "$out" | grep -qx 'rc=1' && ok "agent が応答する（鍵なし rc=1）" || ng "agent 応答: $out"
[ "$(stat -c %a "$fake_home/.ssh")" = "700" ] && ok ".ssh を 700 で作成" || ng ".ssh の権限: $(stat -c %a "$fake_home/.ssh")"

# 2. 2 回目: 新しい agent を起動しない（idempotent）
run_zsh "" 'true'
[ "$(agent_count)" = "1" ] && ok "2 回目は agent を増やさない" || ng "agent 数: $(agent_count)"

# 3. agent が死んでソケットだけ残った: 作り直す
pkill -f "ssh-agent -a $SOCK"; sleep 0.2
[ -S "$SOCK" ] || : > "$SOCK"   # ソケットが消えていても「残骸」を置いて再現する
out=$(run_zsh "" 'ssh-add -l >/dev/null 2>&1; print rc=$?')
echo "$out" | grep -qx 'rc=1' && ok "死んだソケットを作り直す" || ng "再起動: $out"

# 4. 既に生きている別の agent（SSH 転送など）があればそちらを優先し、固定ソケットも用意する
OTHER="$fake_home/other.sock"
ssh-agent -a "$OTHER" >/dev/null
out=$(run_zsh "$OTHER" 'print -r -- "$SSH_AUTH_SOCK"')
[ "$out" = "$OTHER" ] && ok "生きている既存 agent を優先" || ng "既存 agent 優先: $out"
SSH_AUTH_SOCK="$SOCK" ssh-add -l >/dev/null 2>&1; [ $? -eq 1 ] && ok "固定ソケットも動いている" || ng "固定ソケット不在"

# 5. Linux 以外では何もしない（仕事 mac を壊さない）
rm -rf "$fake_home/.ssh"
out=$(env -i HOME="$fake_home" PATH="$PATH" zsh -f -c "OSTYPE=darwin23; source '$REPO/.zsh/ssh_agent.zsh'; print -r -- \"\${SSH_AUTH_SOCK:-unset}\"")
[ "$out" = "unset" ] && [ ! -e "$fake_home/.ssh" ] && ok "darwin では何もしない" || ng "darwin: $out"

# 6. 出力を出さない（p10k instant prompt を乱さない）
pkill -f "ssh-agent -a $SOCK"; sleep 0.2
out=$(run_zsh "" 'true' 2>&1)
[ -z "$out" ] && ok "標準出力・標準エラーに何も出さない" || ng "出力あり: $out"

exit $fail
