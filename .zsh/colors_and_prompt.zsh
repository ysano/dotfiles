# ~/.zsh/colors_and_prompt.zsh

# Load colors module
autoload -U colors && colors

# load default colored ls setting
if [ -f $ZUSERDIR/lscolors ]; then
  source $ZUSERDIR/lscolors
fi

# 256dark
if [ -f ~/.zsh/dircolors.256dark ]; then
    echo "Loading dircolors.256dark"
    if type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.zsh/dircolors.256dark)
    elif type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.zsh/dircolors.256dark)
    fi
fi

# Use colors in completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Terminal-specific color settings
case "$TERM" in
    xterm-color|xterm-256color)
        export CLICOLOR=1
        export LSCOLORS=ExFxCxDxBxegedabagacad
        color_prompt=yes
        ;;
esac

# Function to set title of terminal window
function set-title() {
    echo -ne "\033]0;${1}\007"
}

# Set title automatically
precmd() {
    set-title "${PWD##*/}"
}

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Color grep output
export GREP_COLOR='1;32'
