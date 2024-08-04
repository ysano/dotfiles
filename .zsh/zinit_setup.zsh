# Zinit installation check and setup
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"

# Load a few important annexes, without Turbo
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

# Load core plugins
zinit wait lucid for \
    atinit"zicompinit; zicdreplay" \
        zdharma-continuum/fast-syntax-highlighting \
    atload"!_zsh_autosuggest_start" \
        zsh-users/zsh-autosuggestions \
    blockf \
        zsh-users/zsh-completions

# Load Oh-My-Zsh libraries and plugins
zinit snippet OMZL::history.zsh
zinit snippet OMZL::key-bindings.zsh
zinit snippet OMZL::completion.zsh
zinit snippet OMZP::git/git.plugin.zsh

zinit wait lucid for \
    OMZP::docker/docker.plugin.zsh \
    OMZP::composer/composer.plugin.zsh

# Load custom files
zinit load $HOME/.zsh/aliases.zsh
zinit load $HOME/.zsh/functions.zsh

# Load development tools
zinit ice wait lucid
zinit snippet OMZP::asdf/asdf.plugin.zsh

zinit ice wait lucid as"program" pick"bin/rbenv" \
    atload'eval "$(rbenv init - --no-rehash zsh)"'
zinit light rbenv/rbenv

# Load Google Cloud SDK
if [[ -f /opt/homebrew/share/google-cloud-sdk/path.zsh.inc ]]; then
    zinit ice wait lucid
    zinit snippet /opt/homebrew/share/google-cloud-sdk/path.zsh.inc
    zinit snippet /opt/homebrew/share/google-cloud-sdk/completion.zsh.inc
fi

# GitHub CLI completion
if [[ -f ~/.zsh/completions/_gh ]]; then
    zinit ice as"completion" wait lucid
    zinit snippet ~/.zsh/completions/_gh
fi

# ngrok completion
if [[ -f ~/.zsh/completions/_ngrok ]]; then
    zinit ice as"completion" wait lucid
    zinit snippet ~/.zsh/completions/_ngrok
fi

# AWS CLI completion
zinit ice as"completion"
zinit snippet https://github.com/aws/aws-cli/blob/v2/bin/aws_zsh_completer.sh

# Load Homebrew
if [[ "$OSTYPE" == "darwin"* && -x /opt/homebrew/bin/brew ]]; then
    homebrew_init() {
        eval "$(/opt/homebrew/bin/brew shellenv)"
    }
    zinit ice wait"2" lucid
    zinit light-mode for \
          trigger-load'!brew' \
          atload'homebrew_init' \
          zdharma-continuum/null
fi

# Load Powerlevel10k theme
zinit ice depth=1
zinit light romkatv/powerlevel10k
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
