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

# ================================
# Basic File Listing Aliases
# ================================

# Core aliases - simple and reliable
alias ll='ls -l'
alias la='ls -a'
alias lsdir='ls -ld *(-/DN)'  # List only directories and symbolic links to directories
alias lsa='ls -ld .*'         # List only hidden files

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

# Modern find replacement disabled
# if has_command fd; then
#     alias find='fd'
# fi

# ================================
# Text Processing
# ================================

# Enhanced grep (with fallbacks) - disabled modern replacements
# if has_command rg; then
#     alias grep='rg'
#     alias egrep='rg'
# elif has_command ag; then
#     alias grep='ag'
#     alias egrep='ag'
# else
#     alias grep='egrep'
# fi

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

# ================================
# Network Tools
# ================================

# Enhanced wget operations
alias wget-domain='wget -r -L -l 10'
alias wget-subdir='wget -r -np'

# Modern user agent for wget
[[ -z "$MODERN_USER_AGENT" ]] && readonly MODERN_USER_AGENT='Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'

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

# .orig cleanup (created by link.sh)
alias clean-orig='find ~ -maxdepth 1 -name "*.orig" -print -exec rm -rf {} \;'
alias clean-orig-dry='find ~ -maxdepth 1 -name "*.orig" -print'

# ================================
# Color and Terminal Setup
# ================================

setup_colors() {
    # Only set up colors for capable terminals
    case "$TERM" in
        xterm*|rxvt*|urxvt*|linux*|vt*|screen*|tmux*)
            # Enable colors for common commands
            if [[ "$TERM" != "dumb" ]]; then
                # Grep colors (only if modern alternatives not available)
                if ! has_command rg && ! has_command ag; then
                    alias grep='grep --color=auto'
                    alias fgrep='fgrep --color=auto'
                    alias egrep='egrep --color=auto'
                fi
                
                # ls colors - use environment variables instead of aliases
                if is_macos; then
                    export CLICOLOR=1
                    export LSCOLORS="ExGxFxdaCxDaDahbadacec"
                else
                    export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32'
                    # Only set ls alias if eza is not available
                    if ! has_command eza; then
                        alias ls='ls --color=auto'
                    fi
                fi
            fi
            ;;
        dumb)
            # Ensure no color output for dumb terminals
            unalias grep fgrep egrep ls 2>/dev/null
            unset CLICOLOR LSCOLORS LS_COLORS
            ;;
    esac
}

# True color support detection
setup_truecolor() {
    has_command emacs || return
    # COLORTERM が既に設定済みなら不要
    [[ -n "$COLORTERM" ]] && return
    # xterm-direct terminfo、または 256color ターミナルでフォールバック
    if (has_command toe && toe 2>/dev/null | grep -q "xterm-direct") \
       || [[ "$TERM" == *-256color ]]; then
        export COLORTERM=truecolor
    fi
}

# ================================
# Modern CLI Tools
# ================================

# eza (ls replacement with icons)
if has_command eza; then
    alias ls='eza --icons --group-directories-first'
    alias ll='eza -l --icons --group-directories-first --git'
    alias la='eza -la --icons --group-directories-first --git'
    alias lt='eza --tree --level=2 --icons'
    alias lta='eza --tree --level=3 --icons -a'
fi

# delta (git pager with syntax highlighting)
if has_command delta; then
    export GIT_PAGER='delta'
    export DELTA_FEATURES='+side-by-side'
fi

# tldr (man alternative with practical examples)
if has_command tldr; then
    alias help='tldr'
fi

# dust (du replacement with visual output)
if has_command dust; then
    alias du='dust'
fi

# ================================
# Performance Monitoring
# ================================

# System monitoring aliases
if has_command btm; then
    alias top='btm'
elif has_command htop; then
    alias top='htop'
fi

if has_command duf; then
    alias df='duf'
fi

if has_command procs; then
    alias ps='procs'
fi

# ================================
# Document Conversion
# ================================

# Markdown to PDF (pandoc + lualatex + mermaid-filter + emoji)
if has_command pandoc; then
    md2pdf() {
        if [[ $# -eq 0 ]]; then
            echo "Usage: md2pdf <file.md>" >&2
            return 1
        fi
        if [[ ! -f "$1" ]]; then
            echo "md2pdf: $1: No such file" >&2
            return 1
        fi
        if ! has_command mermaid-filter; then
            echo "md2pdf: mermaid-filter is required but not found" >&2
            return 1
        fi
        local output="${1:r}.pdf"
        pandoc "$1" -f gfm -o "$output" --pdf-engine=lualatex -F mermaid-filter \
            -V CJKmainfont="Hiragino Mincho ProN" \
            -V CJKmonofont="Cica" && \
            echo "$output"
    }
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
# Validation and Testing
# ================================

# Test core aliases function
test_core_aliases() {
    echo "Testing core aliases..."
    
    # Test ll alias
    if alias ll >/dev/null 2>&1; then
        echo "  ✓ ll alias defined: $(alias ll)"
        if ll --help >/dev/null 2>&1 || [[ $? -eq 1 ]]; then
            echo "  ✓ ll command works"
        else
            echo "  ✗ ll command failed"
        fi
    else
        echo "  ✗ ll alias not defined"
    fi
    
    # Test la alias
    if alias la >/dev/null 2>&1; then
        echo "  ✓ la alias defined: $(alias la)"
    else
        echo "  ✗ la alias not defined"
    fi
    
    # Check color settings
    if is_macos; then
        [[ -n "$CLICOLOR" ]] && echo "  ✓ CLICOLOR set" || echo "  ✗ CLICOLOR not set"
        [[ -n "$LSCOLORS" ]] && echo "  ✓ LSCOLORS set" || echo "  ✗ LSCOLORS not set"
    else
        [[ -n "$LS_COLORS" ]] && echo "  ✓ LS_COLORS set" || echo "  ✗ LS_COLORS not set"
    fi
}

# Validate aliases in debug mode
if [[ -n "${ZSH_DEBUG:-}" ]]; then
    echo "Aliases loaded successfully"
    echo "Available modern tools:"
    for tool in bat fd rg ag htop duf procs; do
        if has_command "$tool"; then
            echo "  ✓ $tool"
        fi
    done
    echo ""
    test_core_aliases
fi