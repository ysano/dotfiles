# -*- mode:shell-script -*-

# Enable Powerlevel10k instant prompt
# Suppress console output warning in WSL
if [[ $(uname -a) =~ 'microsoft' ]]; then
  typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
fi

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Locale fallback for non-login interactive shells (e.g. tmux panes).
# .zprofile (login専用) が setup_locale を呼ぶが、tmux の `default-command zsh` は
# 非loginのため .zprofile を読まず、ペインは LANG=C を継承してマルチバイトが壊れる
# （tmux 内で Nerd Font 化け）。LANG が UTF-8 でない時のみ自己修復する。
if [[ "${LANG}" != *[Uu][Tt][Ff]* ]]; then
  [[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"
  (( $+functions[setup_locale] )) && setup_locale
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

# 6. Local Settings (if exists)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# .zprofile is automatically sourced by login shells

########################################
# Some environment variables
########################################

# Claude Code telemetry (enables token/cost data for AI-DLC Economics metrics)
export CLAUDE_CODE_ENABLE_TELEMETRY=1

# Claude Code: attribution ヘッダを無効化してプロンプトキャッシュヒット率を改善
# メッセージ内容から生成されるハッシュがセッション跨ぎのキャッシュ再利用を阻害するため
export CLAUDE_CODE_ATTRIBUTION_HEADER=false

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

### Rancher Desktop (portable path)
[[ -d "$HOME/.rd/bin" ]] && export PATH="$HOME/.rd/bin:$PATH"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/yoshiaki_sano/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
eval "$(~/.local/bin/mise activate zsh)"
