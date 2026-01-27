# ~/.zsh/keybindings_custom.zsh
# Custom keybindings for Zsh
# Contains both base keybindings and custom extensions

# ================================
# Base Keybindings
# ================================

case $TERMCAP in
    emacs*)
    ;;

    *)
        # Emacsスタイルのキーバインディングをデフォルトとして使用
        bindkey -e

        # カスタムキーバインディング
        bindkey '^[[1;5C' forward-word   # Ctrl+右矢印で単語単位で前に移動
        bindkey '^[[1;5D' backward-word  # Ctrl+左矢印で単語単位で後ろに移動
        bindkey '^[[3~'   delete-char    # Deleteキー
        bindkey '^H'      backward-delete-char  # Backspaceキー

        # ヒストリ検索
        bindkey '^R' history-incremental-search-backward  # Ctrl+R
        bindkey '^S' history-incremental-search-forward   # Ctrl+S
    ;;
esac

# 単語の境界を定義（Ctrl+wなどで使用）
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ================================
# 予測機能
# ================================

autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey '^Xp'  predict-on   # Ctrl+X, p
bindkey '^X^P' predict-off  # Ctrl+X, Ctrl+P

# ================================
# peco/ghq連携
# ================================

if [[ -f ~/.zsh/ghq.zsh ]]; then
    source ~/.zsh/ghq.zsh
    zle -N peco-src
    bindkey '^]' peco-src  # Ctrl+]
fi

# ================================
# ヘルプ機能
# ================================

if [[ "$OSTYPE" != "msys" ]] && command -v help >/dev/null 2>&1; then
    custom-run-help() { help "$BUFFER" }
    zle -N custom-run-help
    bindkey '\ek' custom-run-help  # Alt+K
fi

# カスタムヘルプ表示
show-keybindings() {
    echo ""
    echo "Keybindings:"
    echo "  Ctrl+R: 履歴の逆方向検索"
    echo "  Ctrl+S: 履歴の順方向検索"
    echo "  Ctrl+Right/Left: 単語単位で移動"
    echo "  Ctrl+X, p: 予測機能オン"
    echo "  Ctrl+X, Ctrl+P: 予測機能オフ"
    if [[ -f ~/.zsh/ghq.zsh ]]; then
        echo "  Ctrl+]: pecoを使用したソース選択"
    fi
    echo "  Alt+K: 現在のコマンドのヘルプ"
    echo "  Ctrl+X, h: このヘルプを表示"
    echo ""
}
zle -N show-keybindings
bindkey '^Xh' show-keybindings  # Ctrl+X, h
