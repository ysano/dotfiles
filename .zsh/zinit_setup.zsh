# ~/.zsh/zinit_setup.zsh
# Optimized Zinit setup with better performance and error handling

# Load utilities for command checking
[[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"

# ================================
# Zinit Installation and Setup
# ================================

# Zinit directories
readonly ZINIT_HOME="${ZINIT_HOME:-$HOME/.zinit}"
readonly ZINIT_BIN_DIR="$ZINIT_HOME/bin"

# Create required directories
setup_zinit_dirs() {
    local dirs=(
        "$HOME/.cache/zinit/completions"
        "$ZINIT_HOME"
        "$ZINIT_BIN_DIR"
    )
    
    for dir in $dirs; do
        [[ ! -d "$dir" ]] && mkdir -p "$dir"
    done
}

# Install Zinit if not present
install_zinit() {
    if [[ ! -f "$ZINIT_BIN_DIR/zinit.zsh" ]]; then
        echo "Installing Zinit plugin manager..."
        
        if ! has_command git; then
            echo "Error: git is required for Zinit installation" >&2
            return 1
        fi
        
        # Secure installation with error handling
        if command git clone --depth=1 https://github.com/zdharma-continuum/zinit "$ZINIT_BIN_DIR" 2>/dev/null; then
            echo "✓ Zinit installed successfully"
        else
            echo "✗ Zinit installation failed" >&2
            return 1
        fi
    fi
}

# Initialize Zinit
init_zinit() {
    setup_zinit_dirs
    install_zinit || return 1
    
    # Source Zinit
    if [[ -f "$ZINIT_BIN_DIR/zinit.zsh" ]]; then
        source "$ZINIT_BIN_DIR/zinit.zsh"
    else
        echo "Error: Zinit not found at $ZINIT_BIN_DIR/zinit.zsh" >&2
        return 1
    fi
}

# ================================
# Core Zinit Configuration
# ================================

setup_zinit_annexes() {
    # Load essential annexes
    zinit light-mode for \
        zdharma-continuum/zinit-annex-as-monitor \
        zdharma-continuum/zinit-annex-bin-gem-node \
        zdharma-continuum/zinit-annex-patch-dl \
        zdharma-continuum/zinit-annex-rust
}

setup_core_plugins() {
    # Fast syntax highlighting with turbo mode
    zinit wait lucid for \
        atinit"zicompinit; zicdreplay" \
            zdharma-continuum/fast-syntax-highlighting \
        atload"!_zsh_autosuggest_start" \
            zsh-users/zsh-autosuggestions \
        blockf \
            zsh-users/zsh-completions
}

setup_omz_libraries() {
    # Essential Oh My Zsh libraries
    zinit snippet OMZL::history.zsh
    zinit snippet OMZL::key-bindings.zsh  
    zinit snippet OMZL::completion.zsh
    
    # Git plugin (always useful)
    zinit snippet OMZP::git/git.plugin.zsh
}

# ================================
# Development Tools
# ================================

setup_dev_tools() {
    # Composer (PHP)
    if has_command composer; then
        zinit wait lucid for OMZP::composer/composer.plugin.zsh
    fi
    
    # ASDF version manager
    if [[ -d "${ASDF_DATA_DIR:-$HOME/.asdf}" ]]; then
        zinit ice wait lucid
        zinit snippet OMZP::asdf/asdf.plugin.zsh
    fi
    
    # rbenv (not on Windows)
    if [[ "$OSTYPE" != "msys" ]] && has_command rbenv; then
        zinit ice wait lucid as"program" pick"bin/rbenv" \
            atload'eval "$(rbenv init - --no-rehash zsh)"'
        zinit light rbenv/rbenv
    fi
}

# ================================
# Cloud Platforms
# ================================

setup_cloud_tools() {
    # Google Cloud SDK
    if has_command gcloud; then
        zinit snippet OMZP::gcloud
    fi
    
    # Azure CLI  
    if has_command az; then
        zinit ice as"completion" wait lucid
        zinit snippet https://github.com/Azure/azure-cli/blob/dev/az.completion
        zinit snippet OMZP::azure
    fi
    
    # AWS CLI
    if has_command aws; then
        zinit ice as"completion" wait lucid  
        zinit snippet https://github.com/aws/aws-cli/blob/v2/bin/aws_zsh_completer.sh
        zinit snippet OMZP::aws
    fi
    
    # Terraform
    if has_command terraform; then
        zinit ice as"completion" wait lucid
        zinit snippet OMZP::terraform/_terraform
        zinit snippet OMZP::terraform/terraform.plugin.zsh
    fi
}

# ================================
# Container Tools
# ================================

setup_container_tools() {
    # Docker completion (only if Docker is running)
    if has_command docker && docker version >/dev/null 2>&1; then
        zinit ice as"completion" wait lucid
        zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker
    fi
    
    # Kubernetes tools
    if has_command kubectl; then
        zinit ice as"completion" wait lucid
        zinit snippet OMZP::kubectl/kubectl.plugin.zsh
    fi
}

# ================================
# Local Completions
# ================================

setup_local_completions() {
    local completion_dir="$HOME/.zsh/completions"
    
    # Only load if directory exists and has files
    if [[ -d "$completion_dir" ]] && [[ -n "$(ls -A "$completion_dir" 2>/dev/null)" ]]; then
        # GitHub CLI
        if [[ -f "$completion_dir/_gh" ]]; then
            zinit ice as"completion" wait lucid
            zinit snippet "$completion_dir/_gh"
        fi
        
        # ngrok
        if [[ -f "$completion_dir/_ngrok" ]]; then
            zinit ice as"completion" wait lucid  
            zinit snippet "$completion_dir/_ngrok"
        fi
    fi
}

# ================================
# Platform-Specific Setup
# ================================

setup_macos_tools() {
    if ! is_macos; then
        return 0
    fi
    
    # Homebrew setup - disabled lazy loading to ensure immediate PATH availability
    # The Homebrew environment is now set up in .zprofile during login shell initialization
    # This prevents the lazy loading function from overriding the PATH setup
    
    # Note: Homebrew lazy loading was causing issues where 'brew' commands were available
    # but Homebrew paths were not in PATH until the first 'brew' command was executed.
    # The .zprofile now handles Homebrew setup immediately during login.
    
    # Uncomment below to re-enable lazy loading if needed:
    # if [[ -x /opt/homebrew/bin/brew ]]; then
    #     zinit ice wait"2" lucid \
    #         trigger-load'!brew' \
    #         atload'eval "$(/opt/homebrew/bin/brew shellenv)"'
    #     zinit light zdharma-continuum/null
    # fi
}

# ================================
# Theme Setup
# ================================

setup_theme() {
    # Powerlevel10k with instant prompt support
    zinit ice depth=1
    zinit light romkatv/powerlevel10k
    
    # Load p10k config if it exists
    [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
}

# ================================
# Custom Files
# ================================

setup_custom_files() {
    # Load custom configuration files through Zinit for better management
    local custom_files=(
        "$HOME/.zsh/aliases.zsh"
        "$HOME/.zsh/functions.zsh"
    )
    
    for file in $custom_files; do
        if [[ -f "$file" ]]; then
            zinit load "$file"
        fi
    done
}

# ================================
# Performance Optimization
# ================================

optimize_zinit() {
    # Compile Zinit for better performance
    if [[ -n "${ZINIT_OPTIMIZE:-}" ]]; then
        zinit compile --all
    fi
    
    # Set up completion caching
    zstyle ':completion:*' use-cache yes
    zstyle ':completion:*' cache-path "$HOME/.cache/zinit/zcompcache"
}

# ================================
# Main Initialization
# ================================

main() {
    # Initialize Zinit
    if ! init_zinit; then
        echo "Warning: Zinit initialization failed, falling back to basic setup" >&2
        return 1
    fi
    
    # Setup components
    setup_zinit_annexes
    setup_core_plugins
    setup_omz_libraries
    
    # Development tools
    setup_dev_tools
    
    # Cloud and container tools
    setup_cloud_tools
    setup_container_tools
    
    # Local completions
    setup_local_completions
    
    # Platform-specific
    setup_macos_tools
    
    # Theme
    setup_theme
    
    # Custom files
    setup_custom_files
    
    # Performance optimization
    optimize_zinit
    
    # Debug output
    if [[ -n "${ZSH_DEBUG:-}" ]]; then
        echo "Zinit setup completed successfully"
        zinit report --all | head -20
    fi
}

# Execute main setup
main