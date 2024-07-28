# ~/.zsh/completion.zsh
# Contains:
# - All completion related settings and styles

# ウィンドウから溢れるときは尋ねる。
LISTMAX=0

# compact list
setopt listpacked


# 基本的な補完システムの初期化
# autoload -Uz compinit
# if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
#     compinit
# else
#     compinit -C
# fi

# Zinitを使用した補完プラグインの読み込み
# zinit wait lucid for \
#     atinit"zicompinit; zicdreplay" \
#         zdharma-continuum/fast-syntax-highlighting \
#     atload"!_zsh_autosuggest_start" \
#         zsh-users/zsh-autosuggestions \
#     blockf \
#         zsh-users/zsh-completions

# 補完スタイルの設定
zstyle ':completion:*' menu select interactive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# 補完オプションの設定
setopt complete_in_word
setopt always_to_end
setopt auto_menu
setopt complete_aliases

# キャッシュの設定
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

# 補完関数の読み込み
# zinit wait lucid for \
#     as"completion" \
#     atpull'zinit creinstall -q .' \
#     atload"zicompinit; zicdreplay" \
#         zsh-users/zsh-completions

# 特定のコマンドの補完
# zinit wait lucid as"completion" for \
#     OMZP::docker/_docker \
#     OMZP::docker-compose/_docker-compose

# AWS CLI補完（存在する場合）
# if [ -f /usr/local/bin/aws_completer ]; then
#     zinit ice wait lucid as"completion"
#     zinit snippet https://github.com/aws/aws-cli/blob/v2/bin/aws_zsh_completer.sh
# fi

# その他の補完設定
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# 補完の種類の表示方法
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'

# プロセスの補完
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
