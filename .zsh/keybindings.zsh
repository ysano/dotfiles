# ~/.zsh/keybindings.zsh
# Contains:
# - All keybinding settings
# - Terminal-specific settings

case $TERMCAP in
    emacs*)
    ;;

    *)
        # Emacsスタイルのキーバインディングをデフォルトとして使用
        bindkey -e

        # Ctrl+sとCtrl+qのフロー制御を無効化
        # stty -ixon

        # メタキーの設定（主にxtermで使用）
        # stty pass8

        # カスタムキーバインディング
        bindkey '^[[1;5C' forward-word   # Ctrl+右矢印で単語単位で前に移動
        bindkey '^[[1;5D' backward-word  # Ctrl+左矢印で単語単位で後ろに移動
        bindkey '^[[3~'   delete-char    # Deleteキー
        bindkey '^H'      backward-delete-char  # Backspaceキー

        # ヒストリ検索
        bindkey '^R' history-incremental-search-backward  # Ctrl+R
        bindkey '^S' history-incremental-search-forward   # Ctrl+S

        # 予測機能のトグル
        autoload -U predict-on
        zle -N predict-on
        zle -N predict-off
        bindkey '^Xp'  predict-on   # Ctrl+X, p
        bindkey '^X^P' predict-off  # Ctrl+X, Ctrl+P

    ;;
esac

# 単語の境界を定義（Ctrl+wなどで使用）
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# pecoを使用したソース選択（ghq.zshが存在する場合）
if [ -f $ZUSERDIR/ghq.zsh ]; then
    source $ZUSERDIR/ghq.zsh
    zle -N peco-src
    bindkey '^]' peco-src  # Ctrl+]
fi

# キーバインディングのヘルプ関数
show-keybindings() {
  echo "Custom Keybindings:"
  echo "Ctrl+R: 履歴の逆方向検索"
  echo "Ctrl+S: 履歴の順方向検索"
  echo "Ctrl+X, p: 予測機能オン"
  echo "Ctrl+X, Ctrl+P: 予測機能オフ"
  echo "Ctrl+]: pecoを使用したソース選択"
  echo "Alt+Up: 親ディレクトリに移動"
  # 他のキーバインディングの説明を追加
}
zle -N show-keybindings
bindkey '^[k' show-keybindings  # Alt+K
