# -*- mode:shell-script -*-

# Enable Powerlevel10k instant prompt
# Suppress console output warning in WSL
if [[ $(uname -a) =~ 'microsoft' ]]; then
  typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
fi

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# 1. Core Settings
source "$HOME/.zsh/core_settings.zsh"

# 2. Plugin Management (Zinit) - includes OMZ libraries
source "$HOME/.zsh/zinit_setup.zsh"

# 3. Colors and Prompt
source "$HOME/.zsh/colors_and_prompt.zsh"

# 4. Custom Keybindings (supplements OMZ key-bindings)
source "$HOME/.zsh/keybindings_custom.zsh"

# 5. Functions and Aliases
source "$HOME/.zsh/functions.zsh"
source "$HOME/.zsh/aliases.zsh"

# 6. Local Settings (if exists)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

[ -f ~/.zprofile ] && source ~/.zprofile

########################################
# Some environment variables
########################################



# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fzf, run `$(brew --prefix)/opt/fzf/install` to enable key bindings
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


. "$HOME/.local/bin/env"
