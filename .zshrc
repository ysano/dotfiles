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

# 2. Plugin Management (Zinit)
source "$HOME/.zsh/zinit_setup.zsh"

# 3. OS-specific Settings
#source "$HOME/.zsh/os_specific.zsh"

# 4. Environment Variables
#source "$HOME/.zsh/env_variables.zsh"

# 5. Keybindings
source "$HOME/.zsh/keybindings.zsh"

# 6. Colors and Prompt
source "$HOME/.zsh/colors_and_prompt.zsh"

# 7. Completion
source "$HOME/.zsh/completion.zsh"

# 8. Functions and Aliases
source "$HOME/.zsh/functions.zsh"
source "$HOME/.zsh/aliases.zsh"

# 9. Local Settings (if exists)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# 10. Path Modifications
#source "$HOME/.zsh/path_modifications.zsh"

[ -f ~/.zprofile ] && source ~/.zprofile

########################################
# Some environment variables
########################################



# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fzf, run `$(brew --prefix)/opt/fzf/install` to enable key bindings
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


. "$HOME/.local/bin/env"
