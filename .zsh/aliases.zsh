# ~/.zsh/aliases.zsh
# Refactored for better organization and maintainability

# Load utilities for OS detection
[[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"

# ================================
# Core Command Safety
# ================================

# Prevent accidental operations
alias mv='nocorrect mv'
alias cp='nocorrect cp' 
alias mkdir='nocorrect mkdir'
alias rm='nocorrect rm -i'

# ================================
# Enhanced File Operations
# ================================

# Basic listing
alias ll='ls -l'
alias la='ls -a'
alias lsd='ls -ld *(-/DN)'  # List only directories and symbolic links to directories
alias lsa='ls -ld .*'       # List only hidden files

# Enhanced listings with modern tools
if has_command exa; then
    alias ls='exa --color=auto'
    alias ll='exa -l --color=auto'
    alias la='exa -la --color=auto'
    alias lt='exa --tree --color=auto'
elif has_command lsd; then
    alias ls='lsd --color=auto'
    alias ll='lsd -l --color=auto'
    alias la='lsd -la --color=auto'
    alias lt='lsd --tree --color=auto'
fi

# ================================
# Navigation Enhancement
# ================================

alias j='jobs'
alias pu='pushd'
alias po='popd'
alias d='dirs -v'

# Modern navigation with tools
if has_command bat; then
    alias cat='bat --style=auto'
fi

if has_command fd; then
    alias find='fd'
fi

# ================================
# Text Processing
# ================================

# Enhanced grep (with fallbacks)
if has_command rg; then
    alias grep='rg'
    alias egrep='rg'
elif has_command ag; then
    alias grep='ag'
    alias egrep='ag'
else
    alias grep='egrep'
fi

# Global aliases for piping
alias -g M='| more'
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g S='| sort'
alias -g U='| uniq'
alias -g C='| wc -l'

# ================================
# Development Tools
# ================================

# Editor aliases
setup_editor_aliases() {
    if [[ "$TERM_PROGRAM" = "vscode" ]]; then
        alias e='code'
        alias ec='code'
    elif has_command emacsclient; then
        alias e='emacsclient -n'
        alias ec='emacsclient -c'
        alias ekill='emacsclient -e "(kill-emacs)"'
    elif has_command emacs; then
        alias e='emacs'
        if is_macos; then
            alias ee='open -a /Applications/Emacs.app'
        fi
    elif has_command vim; then
        alias e='vim'
    fi
}

# Git aliases (supplement Oh My Zsh git plugin)
setup_git_aliases() {
    if has_command git; then
        # Quick status and log
        alias gs='git status --short'
        alias gl='git log --oneline -10'
        alias gd='git diff'
        alias gdc='git diff --cached'
        
        # Branch management
        alias gb='git branch'
        alias gco='git checkout'
        alias gcb='git checkout -b'
        
        # Stash operations
        alias gst='git stash'
        alias gsp='git stash pop'
        alias gsl='git stash list'
    fi
}

# Development utilities
alias mketags="jexctags --langmap=html:+.rhtml,ruby:+.yml,php:+.thtml -e -R --append=no --exclude=.svn"
alias irb='irb -r irb/completion'

# ================================
# Network Tools
# ================================

# Enhanced wget operations
alias wget-domain='wget -r -L -l 10'
alias wget-subdir='wget -r -np'

# Modern user agent for wget
readonly MODERN_USER_AGENT='Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'

alias wget-site-save="wget -m -np -p -k -K -E -nv --random-wait -U '$MODERN_USER_AGENT'"
alias wget-page-save="wget -m -np -p -k -K -E -nv --random-wait -U '$MODERN_USER_AGENT' -H"

# Network utilities
if has_command curl; then
    alias myip='curl -s https://ipinfo.io/ip'
    alias weather='curl -s "wttr.in/?format=3"'
fi

# ================================
# System Utilities
# ================================

# Enhanced history
alias h='history'
alias hgrep='history | grep'

# Date and time
alias dt='date "+%Y-%m-%d %H:%M:%S"'
alias timestamp='date +%s'
alias iso8601='date -u +"%Y-%m-%dT%H:%M:%SZ"'

# Database tools
if has_command mysql; then
    alias mysql="mysql --pager='less -S -n -i -F -X'"
fi

# ================================
# Color and Terminal Setup
# ================================

setup_colors() {
    # Only set up colors for capable terminals
    case "$TERM" in
        xterm*|rxvt*|urxvt*|linux*|vt*|screen*|tmux*)
            # Enable colors for common commands
            if [[ "$TERM" != "dumb" ]]; then
                # Only override if not using modern alternatives
                if ! has_command rg && ! has_command ag; then
                    alias grep='grep --color=auto'
                    alias fgrep='fgrep --color=auto'
                    alias egrep='egrep --color=auto'
                fi
                
                # ls colors (if not using exa/lsd)
                if ! has_command exa && ! has_command lsd; then
                    alias ls='ls --color=auto'
                fi
            fi
            ;;
        dumb)
            # Ensure no color output for dumb terminals
            unalias grep fgrep egrep ls 2>/dev/null
            ;;
    esac
}

# True color support detection
setup_truecolor() {
    if has_command toe && has_command emacs; then
        if toe 2>/dev/null | grep -q "xterm-direct"; then
            alias emacs='COLORTERM=truecolor emacs'
        fi
    fi
}

# ================================
# Performance Monitoring
# ================================

# System monitoring aliases
if has_command htop; then
    alias top='htop'
fi

if has_command duf; then
    alias df='duf'
fi

if has_command procs; then
    alias ps='procs'
fi

# ================================
# Initialization
# ================================

# Execute setup functions
setup_editor_aliases
setup_git_aliases
setup_colors
setup_truecolor

# ================================
# OS-Specific Aliases
# ================================

# Load OS-specific aliases based on detected OS
load_os_aliases() {
    local os_alias_file=""
    
    if is_macos; then
        os_alias_file="aliases_darwin.zsh"
    elif is_linux; then
        os_alias_file="aliases_linux.zsh"
    else
        case "$OSTYPE" in
            freebsd*) os_alias_file="aliases_freebsd.zsh" ;;
            msys*)    os_alias_file="aliases_msys.zsh" ;;
            cygwin*)  os_alias_file="aliases_cygwin.zsh" ;;
        esac
    fi
    
    if [[ -n "$os_alias_file" ]] && [[ -f "${ZDOTDIR:-$HOME}/.zsh/$os_alias_file" ]]; then
        source "${ZDOTDIR:-$HOME}/.zsh/$os_alias_file"
    fi
}

load_os_aliases

# ================================
# Validation
# ================================

# Validate aliases in debug mode
if [[ -n "${ZSH_DEBUG:-}" ]]; then
    echo "Aliases loaded successfully"
    echo "Available modern tools:"
    for tool in exa lsd bat fd rg ag htop duf procs; do
        if has_command "$tool"; then
            echo "  ✓ $tool"
        fi
    done
fi