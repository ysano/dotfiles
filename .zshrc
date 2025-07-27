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

# Git Worktree custom commands (after aliases to override conflicts)
source "$HOME/.zsh/git-worktree.zsh"

# Final override for gwt command (ensure our custom function takes precedence)
unalias gwt gwta gwtls gwtmv gwtrm 2>/dev/null || true

# 6. Local Settings (if exists)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# .zprofile is automatically sourced by login shells

########################################
# Some environment variables
########################################

# ICU4C for pkg-config (Homebrew only)
if command -v brew >/dev/null 2>&1; then
    export PKG_CONFIG_PATH="$(brew --prefix)/opt/icu4c/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fzf, run `$(brew --prefix)/opt/fzf/install` to enable key bindings
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Local environment file (if needed)
# [[ -f "$HOME/.local/bin/env" ]] && source "$HOME/.local/bin/env"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/yoshiaki_sano/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
