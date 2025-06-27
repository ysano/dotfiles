# ~/.zprofile
# Core environment setup - executed once per login session
# Refactored for better maintainability and performance

# Load utility functions first
[[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"

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
    # Homebrew setup
    local brew_paths=(
        "/opt/homebrew/bin/brew"
        "/usr/local/bin/brew"
    )
    
    for brew_path in $brew_paths; do
        if [[ -x "$brew_path" ]]; then
            eval "$($brew_path shellenv)"
            break
        fi
    done

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

# Apply OS-specific setup
case "$OSTYPE" in
    darwin*)  setup_macos  ;;
    linux*)   setup_linux ;;
    freebsd*) setup_freebsd ;;
    cygwin*)  setup_cygwin ;;
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

# Optional/legacy tools
setup_legacy_tools

# Editor setup (after OS-specific config)
setup_editor

# Final PATH cleanup
cleanup_path

# ================================
# Validation
# ================================

# Validate configuration if in debug mode
if [[ -n "${ZSH_DEBUG:-}" ]]; then
    validate_zsh_config
fi