peco-src () {
    local selected_dir=$(ghq list | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd $(ghq list --full-path --exact $selected_dir)"
        zle accept-line
    fi
    zle clear-screen
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
        echo "Usage: ghq-cd <repo>"
        return 1
    fi
}

# peco-src () {
#     local repo=$(ghq list | peco --query "$LBUFFER")
#     if [ -n "$repo" ]; then
#         repo=$(ghq list --full-path --exact $repo)
#         BUFFER="cd ${repo}"
#         zle accept-line
#     fi
#     zle clear-screen
# }

# ghq-cd () {
#     if [ -n "$1" ]; then
#         dir="$(ghq list --full-path --exact "$1")"
#         if [ -z "$dir" ]; then
#             echo "no directories found for '$1'"
#             return 1
#         fi
#         cd "$dir"
#         return
#     fi
#     echo 'usage: ghq-cd $repo'
#     return 1
# }

zle -N peco-src
bindkey '^]' peco-src

