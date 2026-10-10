# ssh-agent を固定ソケット ~/.ssh/agent.sock で 1 つだけ動かす（Linux/WSL 向け）。
# 鍵は使う人が `ssh-add -t <有効期間>` で載せる。どのシェルからも同じ agent が見えるので、
# 別プロセス（tmux の別ペイン、エディタ、Claude Code のシェル）からも鍵が使える。
# macOS は launchd の agent があるので触らない。
() {
  [[ "$OSTYPE" == linux* ]] || return 0
  local sock="$HOME/.ssh/agent.sock"
  SSH_AUTH_SOCK="$sock" ssh-add -l >/dev/null 2>&1
  if (( $? == 2 )); then   # 2 = agent に繋がらない（未起動か、死んだソケットの残骸）
    [[ -d "$HOME/.ssh" ]] || mkdir -m 700 "$HOME/.ssh"
    rm -f "$sock"
    ssh-agent -a "$sock" >/dev/null 2>&1 || return 0
  fi
  # SSH 転送などで既に生きている agent があれば、そちらを優先する
  if [[ -n "${SSH_AUTH_SOCK:-}" && "$SSH_AUTH_SOCK" != "$sock" ]]; then
    ssh-add -l >/dev/null 2>&1
    (( $? != 2 )) && return 0
  fi
  export SSH_AUTH_SOCK="$sock"
}
