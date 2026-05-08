# ~/.zsh/zinit_setup.zsh
# Optimized Zinit setup with better performance and error handling

# Load utilities for command checking
[[ -f "$HOME/.zsh/utils.zsh" ]] && source "$HOME/.zsh/utils.zsh"

# ================================
# Zinit Installation and Setup
# ================================

# Zinit directories
[[ -z "$ZINIT_HOME" ]] && readonly ZINIT_HOME="$HOME/.zinit"
[[ -z "$ZINIT_BIN_DIR" ]] && readonly ZINIT_BIN_DIR="$ZINIT_HOME/bin"

# Create required directories
setup_zinit_dirs() {
    local dirs=(
        "${XDG_CACHE_HOME:-$HOME/.cache}/zinit/completions"
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
    
    # Git plugin (commented out due to gwt alias conflict with custom git-worktree.zsh)
    # zinit snippet OMZP::git/git.plugin.zsh
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
        # 補完ファイル ~/.asdf/completions/_asdf を遅延生成。
        # asdf 0.16+ では asdf binary が `asdf completion zsh` を提供するが、
        # 自動配置はされないため zsh 起動時に必要なら生成する。
        # asdf binary が補完ファイルより新しい場合のみ再生成（アップグレード追従）。
        # fpath への登録は .zsh/functions.zsh が担当。
        # 旧来の OMZP::asdf/asdf.plugin.zsh は asdf 0.x の asdf.sh を source する
        # 前提で、asdf 0.16+ では asdf.sh 自体が存在せず実質 no-op だったため除去。
        # shim PATH は .zprofile の setup_asdf() に集約済み（commit 7c7a033）。
        local _asdf_bin _asdf_comp_dir _asdf_comp_file
        _asdf_bin=$(command -v asdf 2>/dev/null)
        _asdf_comp_dir="${ASDF_DATA_DIR:-$HOME/.asdf}/completions"
        _asdf_comp_file="$_asdf_comp_dir/_asdf"
        if [[ -n "$_asdf_bin" ]] && \
           { [[ ! -f "$_asdf_comp_file" ]] || [[ "$_asdf_bin" -nt "$_asdf_comp_file" ]]; }; then
            # 失敗時に空 / 部分ファイルを残さないよう一時ファイル経由で atomic に置換。
            mkdir -p "$_asdf_comp_dir"
            if "$_asdf_bin" completion zsh > "$_asdf_comp_file.tmp" 2>/dev/null; then
                mv "$_asdf_comp_file.tmp" "$_asdf_comp_file"
            else
                rm -f "$_asdf_comp_file.tmp"
            fi
        fi

        # plugin-index の週次自動更新（バックグラウンド・非同期）
        # sentinel mtime が 7 日以上前なら 'asdf plugin update --all' を disown 起動。
        # stat: BSD (-f %m) → GNU (-c %Y) フォールバックでクロスプラットフォーム対応。
        local _asdf_idx_stamp="${ASDF_DATA_DIR:-$HOME/.asdf}/.plugin-index-updated"
        local _asdf_idx_week=$((7 * 24 * 60 * 60))
        local _asdf_idx_mtime
        _asdf_idx_mtime=$(stat -f %m "$_asdf_idx_stamp" 2>/dev/null \
            || stat -c %Y "$_asdf_idx_stamp" 2>/dev/null \
            || echo 0)
        if (( $(date +%s) - _asdf_idx_mtime > _asdf_idx_week )); then
            ( asdf plugin update --all >/dev/null 2>&1 && touch "$_asdf_idx_stamp" ) &!
        fi
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
# Modern CLI Tools Integration
# ================================

setup_modern_cli_tools() {
    # fzf key bindings and completion
    if has_command fzf; then
        zinit ice wait lucid
        zinit snippet https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh
        zinit ice wait lucid
        zinit snippet https://github.com/junegunn/fzf/blob/master/shell/completion.zsh
    fi

    # zoxide initialization
    if has_command zoxide; then
        eval "$(zoxide init zsh)"
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
    # Powerlevel10k with instant prompt support and gitstatus fix
    # Set gitstatus environment variables before loading p10k
    export GITSTATUS_LOG_LEVEL=${GITSTATUS_LOG_LEVEL:-}
    
    # Fix for interactive shell detection
    [[ -o interactive ]] || return 0
    
    zinit ice depth=1
    zinit light romkatv/powerlevel10k
    
    # Load p10k config if it exists
    [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
}

# ================================
# Custom Files
# ================================

setup_custom_files() {
    # Load custom configuration files (最後に読み込んでalias競合を解決)
    local custom_files=(
        "$HOME/.zsh/aliases.zsh"
        "$HOME/.zsh/functions.zsh"
    )
    
    for file in $custom_files; do
        if [[ -f "$file" ]]; then
            source "$file"
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
    zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zinit/zcompcache"
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

    # Modern CLI tools (fzf, zoxide)
    setup_modern_cli_tools

    # Platform-specific
    setup_macos_tools
    
    # Theme
    setup_theme
    
    # Performance optimization
    optimize_zinit
    
    # Custom files (最後に読み込んでalias競合を解決)
    setup_custom_files
    
    # Debug output
    if [[ -n "${ZSH_DEBUG:-}" ]]; then
        echo "Zinit setup completed successfully"
        zinit report --all | head -20
    fi
}

# Execute main setup
main