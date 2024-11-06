# -*- mode:shell-script -*-

# Enable Powerlevel10k instant prompt
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

# local setting
[ -f ~/.zshrc.local ] && source ~/.zshrc.local


### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/yoshiaki_sano/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fzf, run `$(brew --prefix)/opt/fzf/install` to enable key bindings
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

