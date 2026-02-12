# ~/.zprofile
# Core environment setup - executed once per login session
# Refactored for better maintainability and performance

# Load utility functions first
[[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"

# ================================
# XDG Base Directory Specification
# ================================

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# ================================
# Core Environment Variables
# ================================

# Locale setup with fallbacks
setup_locale

# Core environment
export PAGER='less'
export LESS='-RFXx3M'

# Development environment
export CVSROOT="$HOME/cvs"
export SVNROOT="$HOME/svn" 
export GZIP='-v9N'
export ACK_COLOR_MATCH='underline white'
export GTAGSLABEL=pygments

# Security
export GPG_TTY=$(tty)

# ================================
# OS-Specific Configuration
# ================================

setup_macos() {
    # Homebrew setup - immediately set PATH to avoid Zinit lazy loading issues
    local brew_paths=(
        "/opt/homebrew/bin/brew"
        "/usr/local/bin/brew"
    )
    
    local homebrew_found=false
    for brew_path in $brew_paths; do
        if [[ -x "$brew_path" ]]; then
            # Execute shellenv immediately to set environment variables
            eval "$($brew_path shellenv)"
            homebrew_found=true
            
            # Also add the bin directory to PATH manually to ensure it's available
            local brew_prefix="$(dirname "$(dirname "$brew_path")")"
            safe_path_prepend "$brew_prefix/bin"
            safe_path_prepend "$brew_prefix/sbin"
            
            # Set Homebrew environment variables
            export HOMEBREW_PREFIX="$brew_prefix"
            export HOMEBREW_CELLAR="$brew_prefix/Cellar"
            export HOMEBREW_REPOSITORY="$brew_prefix"
            break
        fi
    done
    
    if [[ "$homebrew_found" == "false" ]]; then
        echo "Warning: Homebrew not found in standard locations" >&2
    elif [[ -n "${ZSH_DEBUG:-}" ]]; then
        echo "✓ Homebrew setup completed in .zprofile" >&2
    fi

    # GNU tools preference
    safe_path_prepend "/opt/homebrew/opt/gnu-tar/libexec/gnubin"

    # Java setup for macOS
    if [[ -d "/opt/homebrew/opt/openjdk" ]]; then
        export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include $CPPFLAGS"
        export JAVA_HOME="/opt/homebrew/opt/openjdk"
    elif [[ -d "/usr/local/opt/openjdk" ]]; then
        export CPPFLAGS="-I/usr/local/opt/openjdk/include $CPPFLAGS"
        export JAVA_HOME="/usr/local/opt/openjdk"
    fi
}

setup_linux() {
    # WSL-specific configuration
    if is_wsl; then
        setup_wsl
    fi
    # Regular Linux settings would go here
}

setup_wsl() {
    # X11 configuration for WSL
    local display_hosts=(
        "$(hostname).mshome.net:0.0"
        "localhost:0.0"
        ":0.0"
    )
    
    for display in $display_hosts; do
        export DISPLAY="$display"
        if timeout 1 xset q >/dev/null 2>&1; then
            break
        fi
    done
    
    # WSL-specific environment
    export LIBGL_ALWAYS_INDIRECT=1
    [[ -f "$HOME/.xinitrc" ]] && source "$HOME/.xinitrc"

    # Mozc setup for WSL
    if [[ -f "/mnt/c/opt/mozc/mozc_emacs_helper.sh" ]]; then
        safe_path_append "/mnt/c/opt/mozc"
    fi
}

setup_freebsd() {
    # FreeBSD-specific settings
    :  # placeholder
}

setup_cygwin() {
    # Cygwin-specific settings  
    :  # placeholder
}

# Store OS-specific setup function for later execution
case "$OSTYPE" in
    darwin*)  OS_SETUP_FUNC="setup_macos"  ;;
    linux*)   OS_SETUP_FUNC="setup_linux" ;;
    freebsd*) OS_SETUP_FUNC="setup_freebsd" ;;
    cygwin*)  OS_SETUP_FUNC="setup_cygwin" ;;
esac

# ================================
# System Limits (Unix-like only)
# ================================

if [[ "$OSTYPE" != "cygwin"* ]]; then
    # Set resource limits safely
    command -v unlimit >/dev/null 2>&1 && unlimit
    command -v limit >/dev/null 2>&1 && {
        limit stack 8192 2>/dev/null
        limit core 0 2>/dev/null
    }
fi

# Set secure umask
umask 022

# ================================
# PATH Configuration
# ================================

# Initialize PATH with safe defaults
typeset -U path
path=(
    "$HOME/bin"
    "$HOME/.local/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin"
)

# Append system paths from /etc/paths.d (TeX, etc.)
if [[ -d /etc/paths.d ]]; then
    for _pfile in /etc/paths.d/*(N); do
        while IFS= read -r _p; do
            [[ -d "$_p" ]] && safe_path_append "$_p"
        done < "$_pfile"
    done
    unset _pfile _p
fi

# ================================
# Development Environment Setup
# ================================

setup_go() {
    if has_command go; then
        export GOPATH="${GOPATH:-$HOME/go}"
        safe_path_append "$GOPATH/bin"
    fi
}

setup_rust() {
    safe_path_append "$HOME/.cargo/bin"
}

setup_node() {
    if has_command npm; then
        local npm_global_bin
        npm_global_bin="$(npm prefix -g 2>/dev/null)/bin"
        safe_path_append "$npm_global_bin"
    fi
}

setup_ruby() {
    # ASDF Ruby support
    if [[ -d "${ASDF_DATA_DIR:-$HOME/.asdf}/shims" ]]; then
        safe_path_prepend "${ASDF_DATA_DIR:-$HOME/.asdf}/shims"
    fi
}

setup_android() {
    if [[ -d "$HOME/Android/Sdk" ]]; then
        export ANDROID_HOME="$HOME/Android/Sdk"
        safe_path_append "$ANDROID_HOME/cmdline-tools/latest/bin"
        safe_path_append "$ANDROID_HOME/platform-tools"
    fi
}

setup_flutter() {
    # Flutter setup - make configurable
    local flutter_path="${FLUTTER_HOME:-$HOME/ghq/github.com/flutter/flutter/bin}"
    safe_path_append "$flutter_path"
}

# ================================
# Modern CLI Tools Environment
# ================================

setup_modern_cli() {
    # bat
    export BAT_THEME="Dracula"

    # ripgrep
    export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/config"

    # fzf
    if has_command fzf; then
        export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
        export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
        export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
    fi
}

# ================================
# Legacy and Optional Tools
# ================================

setup_legacy_tools() {
    # Legacy mail system
    safe_path_append "/var/qmail/bin"
    
    # Ruby development
    safe_path_append "$HOME/rd/.bin"
    
    # Rancher Desktop
    safe_path_append "$HOME/.rd/bin"
}

# ================================
# Execute Setup Functions
# ================================

# Core development environments
setup_go
setup_rust
setup_node
setup_ruby
setup_android
setup_flutter

# Modern CLI tools
setup_modern_cli

# Optional/legacy tools
setup_legacy_tools

# OS-specific setup (execute after all other PATH modifications)
# This ensures OS-specific paths (like Homebrew) take precedence
if [[ -n "${OS_SETUP_FUNC:-}" ]]; then
    $OS_SETUP_FUNC
fi

# Editor setup (after OS-specific config)
setup_editor

# Final PATH cleanup and export
cleanup_path

# Force PATH export to ensure changes are applied
export PATH

# ================================
# Validation
# ================================

# Validate configuration if in debug mode
if [[ -n "${ZSH_DEBUG:-}" ]]; then
    validate_zsh_config
fi