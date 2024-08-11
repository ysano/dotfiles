# Improved ghq.zsh for Emacs keybindings

peco-src () {
    local selected_dir=$(ghq list | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd $(ghq list --full-path --exact $selected_dir)"
        zle accept-line
    else
        zle reset-prompt
    fi
}

ghq-cd () {
    if [ -n "$1" ]; then
        local target_dir="$(ghq list --full-path --exact "$1")"
        if [ -z "$target_dir" ]; then
            echo "No directories found for '$1'"
            return 1
        fi
        cd "$target_dir"
    else
        local selected_dir=$(ghq list | peco --query "$LBUFFER")
        if [ -n "$selected_dir" ]; then
            cd "$(ghq list --full-path --exact $selected_dir)"
        else
            echo "No directory selected"
            return 1
        fi
    fi
}

# Add completion for ghq
if (( $+commands[ghq] )); then
    compdef _ghq ghq
fi

zle -N peco-src
bindkey '\e]' peco-src  # Alt+] (ESC followed by ])
# Alternatively, you can keep the original binding:
# bindkey '^]' peco-src

# Alias for quick access
alias repo='ghq-cd'

# Function to update all ghq repositories
ghq-update-all() {
    ghq list | ghq get --update --parallel
}

# Function to clone and cd into a repository
ghq-get-cd() {
    local repo=$1
    if [ -z "$repo" ]; then
        echo "Usage: ghq-get-cd <repository>"
        return 1
    fi
    ghq get $repo && cd $(ghq list --full-path --exact $repo)
}

# Alias for ghq-get-cd
alias gcd='ghq-get-cd'
