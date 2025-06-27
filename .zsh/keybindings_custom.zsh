# ~/.zsh/keybindings_custom.zsh
# Custom keybindings to supplement Oh My Zsh key-bindings.zsh
# This file contains unique functionality not provided by OMZ

# 予測機能のトグル
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey '^Xp'  predict-on   # Ctrl+X, p
bindkey '^X^P' predict-off  # Ctrl+X, Ctrl+P

# pecoを使用したソース選択（ghq.zshが存在する場合）
if [[ -f ~/.zsh/ghq.zsh ]]; then
    source ~/.zsh/ghq.zsh
    zle -N peco-src
    bindkey '^]' peco-src  # Ctrl+]
fi

# ヘルプ機能
if [[ "$OSTYPE" != "msys" ]] && command -v help >/dev/null 2>&1; then
  custom-run-help() { help "$BUFFER" }
  zle -N custom-run-help
  bindkey '\ek' custom-run-help  # Alt+K
fi

# カスタムヘルプ表示
show-help() {
  echo ""
  echo "Custom Key Bindings:"
  echo "Ctrl+X, p: Enable predict mode"
  echo "Ctrl+X, Ctrl+P: Disable predict mode"
  if [[ -f ~/.zsh/ghq.zsh ]]; then
    echo "Ctrl+]: pecoを使用したソース選択"
  fi
  echo "Alt+K: Help for current command"
  echo ""
}
zle -N show-help
bindkey '^Xh' show-help  # Ctrl+X, h