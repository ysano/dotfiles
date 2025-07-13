# ~/.zsh/utils.zsh
# Utility functions for Zsh configuration
# Contains: OS detection, path management, performance helpers

# ================================
# OS Detection & Environment
# ================================

# Cache OS detection result for performance
typeset -g ZSH_OS_TYPE=""
typeset -g ZSH_IS_WSL=""
typeset -g ZSH_IS_MACOS=""
typeset -g ZSH_IS_LINUX=""

# Detect operating system once and cache result
detect_os() {
    if [[ -n "$ZSH_OS_TYPE" ]]; then
        return 0  # Already detected
    fi

    ZSH_OS_TYPE="$OSTYPE"
    
    case "$OSTYPE" in
        darwin*)
            ZSH_IS_MACOS=1
            ;;
        linux*)
            ZSH_IS_LINUX=1
            # Check for WSL
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                ZSH_IS_WSL=1
            fi
            ;;
    esac
}

# Helper functions for OS detection
is_macos() { [[ -n "$ZSH_IS_MACOS" ]] }
is_linux() { [[ -n "$ZSH_IS_LINUX" ]] }
is_wsl() { [[ -n "$ZSH_IS_WSL" ]] }

# ================================
# Path Management
# ================================

# Safely add directory to PATH if it exists
safe_path_prepend() {
    local dir="$1"
    [[ -d "$dir" ]] && path=("$dir" $path)
}

safe_path_append() {
    local dir="$1"
    [[ -d "$dir" ]] && path+=("$dir")
}

# Remove duplicates from PATH
cleanup_path() {
    typeset -U path
    export PATH
}

# ================================
# Command Existence Checks
# ================================

# Cache command existence checks for performance
typeset -A ZSH_CMD_CACHE

has_command() {
    local cmd="$1"
    
    # Check cache first
    if [[ -n "${ZSH_CMD_CACHE[$cmd]:-}" ]]; then
        return ${ZSH_CMD_CACHE[$cmd]}
    fi
    
    # Check for actual executable (not aliases/functions)
    if type -p "$cmd" >/dev/null 2>&1; then
        ZSH_CMD_CACHE[$cmd]=0
        return 0
    else
        ZSH_CMD_CACHE[$cmd]=1
        return 1
    fi
}

# ================================
# Environment Setup
# ================================

# Set up locale with fallbacks
setup_locale() {
    # Try Japanese UTF-8 (both formats), fall back to English UTF-8, then C
    local locales=("ja_JP.UTF-8" "ja_JP.utf8" "en_US.UTF-8" "en_US.utf8" "C.UTF-8" "C")
    
    for locale in $locales; do
        if locale -a 2>/dev/null | grep -q "^${locale}$"; then
            export LANG="$locale"
            export LC_CTYPE="$locale"
            # For better compatibility, also set LC_ALL for Japanese locales
            if [[ "$locale" == ja_JP.* ]]; then
                export LC_ALL="$locale"
            fi
            [[ -n "${ZSH_DEBUG:-}" ]] && echo "✓ Locale set to: $locale" >&2
            return 0
        fi
    done
    [[ -n "${ZSH_DEBUG:-}" ]] && echo "✗ No suitable locale found, using default" >&2
}

# Set up editor with fallbacks
setup_editor() {
    # Don't override if already set via terminal-specific logic
    [[ -n "$EDITOR" ]] && return 0
    
    local editors=("emacsclient" "emacs" "vim" "vi" "nano")
    
    for editor in $editors; do
        if has_command "$editor"; then
            export EDITOR="$editor"
            export VISUAL="$editor"
            return 0
        fi
    done
}

# ================================
# Performance Helpers
# ================================

# Benchmark a command execution
benchmark_cmd() {
    local cmd="$1"
    local start_time=$(date +%s.%N)
    eval "$cmd"
    local end_time=$(date +%s.%N)
    local duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "N/A")
    echo "Command '$cmd' took ${duration}s"
}

# Initialize utils
detect_os

# Validate critical paths
validate_zsh_config() {
    local config_files=(
        "$HOME/.zshrc"
        "$HOME/.zprofile" 
        "$HOME/.zsh/core_settings.zsh"
        "$HOME/.zsh/zinit_setup.zsh"
    )
    
    for file in $config_files; do
        if [[ ! -r "$file" ]]; then
            echo "Warning: Cannot read $file" >&2
            return 1
        fi
    done
    return 0
}