#!/bin/bash
# TMux統一設定ジェネレーター
# YAML設定から各プラットフォーム固有のtmux設定を自動生成
# Version: 1.0

set -euo pipefail

# ====================================
# 定数定義
# ====================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TMUX_DIR="$(dirname "$SCRIPT_DIR")"
readonly YAML_CONFIG="$TMUX_DIR/config/tmux-unified.yaml"
readonly OUTPUT_DIR="$TMUX_DIR/generated"
readonly PLATFORM_UTILS="$TMUX_DIR/claude/core/platform_utils.sh"

# ログ設定
readonly LOG_FILE="$TMUX_DIR/claude/logs/config-generator.log"
readonly LOG_LEVEL="${CONFIG_LOG_LEVEL:-INFO}"

# ====================================
# ユーティリティ関数
# ====================================

# ログ出力
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

log_info() { log "INFO" "$@"; }
log_warn() { log "WARN" "$@"; }
log_error() { log "ERROR" "$@"; }
log_debug() { 
    [[ "$LOG_LEVEL" == "DEBUG" ]] && log "DEBUG" "$@" || true
}

# エラーハンドリング
error_exit() {
    log_error "$1"
    exit 1
}

# 前提条件チェック
check_prerequisites() {
    log_info "前提条件をチェック中..."
    
    # YAML設定ファイルの存在確認
    [[ -f "$YAML_CONFIG" ]] || error_exit "YAML設定ファイルが見つかりません: $YAML_CONFIG"
    
    # 出力ディレクトリの作成
    mkdir -p "$OUTPUT_DIR"
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # yqコマンドの確認（YAML解析に必要）
    if ! command -v yq &> /dev/null; then
        log_warn "yqコマンドが見つかりません。代替YAMLパーサーを使用します"
    fi
    
    # プラットフォーム検出スクリプトの確認
    if [[ -f "$PLATFORM_UTILS" ]]; then
        source "$PLATFORM_UTILS"
    else
        log_warn "プラットフォーム検出スクリプトが見つかりません: $PLATFORM_UTILS"
    fi
    
    log_info "前提条件チェック完了"
}

# ====================================
# YAML解析機能
# ====================================

# シンプルなYAML値取得（yqが利用できない場合のフォールバック）
get_yaml_value() {
    local key="$1"
    local file="$2"
    local default="${3:-}"
    
    if command -v yq &> /dev/null; then
        # yqを使用した正確な解析
        yq eval ".$key // \"$default\"" "$file" 2>/dev/null || echo "$default"
    else
        # 基本的な正規表現による解析（制限あり）
        local result
        result=$(grep -E "^[[:space:]]*${key//./.*}:" "$file" | head -1 | sed 's/.*:[[:space:]]*//' | sed 's/["'\'']*//g' || echo "$default")
        echo "${result:-$default}"
    fi
}

# YAML配列の取得
get_yaml_array() {
    local key="$1"
    local file="$2"
    
    if command -v yq &> /dev/null; then
        yq eval ".$key[]" "$file" 2>/dev/null | grep -v "^null$" || true
    else
        # 基本的な配列解析（制限あり）
        grep -A 20 "^[[:space:]]*${key}:" "$file" | grep "^[[:space:]]*-" | sed 's/^[[:space:]]*-[[:space:]]*//' | sed 's/["'\'']*//g' || true
    fi
}

# プラットフォーム検出
detect_platform() {
    if [[ -f "$PLATFORM_UTILS" ]]; then
        source "$PLATFORM_UTILS"
        detect_wsl_environment
    else
        # 基本的なプラットフォーム検出
        case "$(uname -s)" in
            Linux)
                if grep -qi microsoft /proc/version 2>/dev/null; then
                    echo "wsl"
                else
                    echo "linux"
                fi
                ;;
            Darwin) echo "macos" ;;
            CYGWIN*|MINGW*|MSYS*) echo "windows" ;;
            FreeBSD) echo "freebsd" ;;
            *) echo "unknown" ;;
        esac
    fi
}

# ====================================
# 設定生成機能
# ====================================

# 基本設定の生成
generate_base_config() {
    local output_file="$1"
    
    log_info "基本設定を生成中..."
    
    cat > "$output_file" << 'EOF'
# Generated TMux Configuration
# Auto-generated from tmux-unified.yaml
# DO NOT EDIT MANUALLY - Changes will be overwritten

EOF
    
    # シェル設定
    local default_command
    default_command=$(get_yaml_value "base.shell.default_command" "$YAML_CONFIG" "zsh")
    echo "# Shell configuration" >> "$output_file"
    echo "set -g default-command $default_command" >> "$output_file"
    echo "" >> "$output_file"
    
    # プレフィックス設定
    local prefix_key
    prefix_key=$(get_yaml_value "base.prefix.key" "$YAML_CONFIG" "C-z")
    echo "# Prefix configuration" >> "$output_file"
    echo "unbind C-b" >> "$output_file"
    echo "set -g prefix $prefix_key" >> "$output_file"
    echo "bind $prefix_key send-prefix" >> "$output_file"
    
    local enable_double_tap
    enable_double_tap=$(get_yaml_value "base.prefix.enable_double_tap" "$YAML_CONFIG" "true")
    if [[ "$enable_double_tap" == "true" ]]; then
        echo "bind z send-prefix" >> "$output_file"
    fi
    echo "" >> "$output_file"
    
    # インデックス設定
    local window_base pane_base
    window_base=$(get_yaml_value "base.indexing.window_base" "$YAML_CONFIG" "1")
    pane_base=$(get_yaml_value "base.indexing.pane_base" "$YAML_CONFIG" "1")
    echo "# Indexing configuration" >> "$output_file"
    echo "set -g base-index $window_base" >> "$output_file"
    echo "set -g pane-base-index $pane_base" >> "$output_file"
    echo "" >> "$output_file"
    
    # ターミナル設定
    local default_terminal
    default_terminal=$(get_yaml_value "base.terminal.default_terminal" "$YAML_CONFIG" "xterm-256color")
    echo "# Terminal configuration" >> "$output_file"
    echo "set-option -g default-terminal \"$default_terminal\"" >> "$output_file"
    
    # ターミナルオーバーライド
    local overrides
    overrides=$(get_yaml_array "base.terminal.overrides" "$YAML_CONFIG")
    if [[ -n "$overrides" ]]; then
        while IFS= read -r override; do
            [[ -n "$override" ]] && echo "set -as terminal-overrides \",$override\"" >> "$output_file"
        done <<< "$overrides"
    fi
    echo "" >> "$output_file"
    
    # 表示設定
    local display_time titles_enabled auto_rename auto_rename_format
    display_time=$(get_yaml_value "base.display.time_ms" "$YAML_CONFIG" "2000")
    titles_enabled=$(get_yaml_value "base.display.titles_enabled" "$YAML_CONFIG" "true")
    auto_rename=$(get_yaml_value "base.display.automatic_rename" "$YAML_CONFIG" "true")
    auto_rename_format=$(get_yaml_value "base.display.automatic_rename_format" "$YAML_CONFIG" "#{=|-22|..;b:pane_current_path}")
    
    echo "# Display configuration" >> "$output_file"
    echo "set -g display-time $display_time" >> "$output_file"
    [[ "$titles_enabled" == "true" ]] && echo "set -g set-titles on" >> "$output_file"
    [[ "$auto_rename" == "true" ]] && echo "set-option -g automatic-rename on" >> "$output_file"
    echo "set-option -g automatic-rename-format '$auto_rename_format'" >> "$output_file"
    echo "" >> "$output_file"
    
    # マウス設定
    local mouse_enabled version_compat
    mouse_enabled=$(get_yaml_value "base.mouse.enabled" "$YAML_CONFIG" "true")
    version_compat=$(get_yaml_value "base.mouse.version_compatibility" "$YAML_CONFIG" "true")
    
    if [[ "$mouse_enabled" == "true" ]]; then
        echo "# Mouse configuration" >> "$output_file"
        if [[ "$version_compat" == "true" ]]; then
            cat >> "$output_file" << 'EOF'
# Get tmux version for conditional settings
run-shell "tmux setenv -g TMUX_VERSION $(tmux -V | cut -c 6-|sed 's/[a-z]*-//')"

# Mouse settings based on version
if-shell '[ "$(echo "$TMUX_VERSION < 2.1" | bc)" = 1 ]' " \
         set -g mouse-select-pane on; set -g mode-mouse on; \
         set -g mouse-resize-pane on; set -g mouse-select-window on"

if-shell '[ "$(echo "$TMUX_VERSION >= 2.1" | bc)" = 1 ]' \
         "set -g mouse on"

# UTF8 settings for older versions
if-shell '[ "$(echo "$TMUX_VERSION < 2.2" | bc)" = 1 ]' \
  "set -g utf8 on; set -g status-utf8 on; set -g mouse-utf8 on"
EOF
        else
            echo "set -g mouse on" >> "$output_file"
        fi
        echo "" >> "$output_file"
    fi
    
    log_info "基本設定生成完了"
}

# プラットフォーム固有設定の生成
generate_platform_config() {
    local platform="$1"
    local output_file="$2"
    
    log_info "プラットフォーム固有設定を生成中: $platform"
    
    echo "# Platform-specific configuration: $platform" >> "$output_file"
    
    case "$platform" in
        "wsl")
            generate_wsl_config "$output_file"
            ;;
        "macos")
            generate_macos_config "$output_file"
            ;;
        "linux")
            generate_linux_config "$output_file"
            ;;
        *)
            log_warn "サポートされていないプラットフォーム: $platform"
            ;;
    esac
    
    echo "" >> "$output_file"
}

# WSL固有設定の生成
generate_wsl_config() {
    local output_file="$1"
    
    echo "# WSL-specific configuration (Windows Subsystem for Linux)" >> "$output_file"
    echo "# This configuration is ONLY applied in WSL environment" >> "$output_file"
    echo "" >> "$output_file"
    
    # WSL環境検証の追加
    echo "# WSL environment verification" >> "$output_file"
    echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null' 'display-message \"WSL environment detected\"' 'display-message \"WARNING: WSL config loaded on non-WSL system\"'" >> "$output_file"
    echo "" >> "$output_file"
    
    # 環境変数設定
    local wsl_integration windows_host_path
    wsl_integration=$(get_yaml_value "platforms.wsl.environment.WSL_INTEGRATION" "$YAML_CONFIG" "enabled")
    windows_host_path=$(get_yaml_value "platforms.wsl.environment.WINDOWS_HOST_PATH" "$YAML_CONFIG" "/mnt/c")
    
    echo "# WSL environment variables" >> "$output_file"
    echo "set-environment -g WSL_INTEGRATION '$wsl_integration'" >> "$output_file"
    echo "set-environment -g WINDOWS_HOST_PATH '$windows_host_path'" >> "$output_file"
    echo "" >> "$output_file"
    
    # ターミナル設定
    local wsl_terminal
    wsl_terminal=$(get_yaml_value "platforms.wsl.terminal.default_terminal" "$YAML_CONFIG" "tmux-256color")
    echo "# WSL terminal configuration" >> "$output_file"
    echo "set-option -g default-terminal \"$wsl_terminal\"" >> "$output_file"
    
    local wsl_overrides
    wsl_overrides=$(get_yaml_array "platforms.wsl.terminal.overrides" "$YAML_CONFIG")
    if [[ -n "$wsl_overrides" ]]; then
        while IFS= read -r override; do
            [[ -n "$override" ]] && echo "set-option -sa terminal-overrides \",$override\"" >> "$output_file"
        done <<< "$wsl_overrides"
    fi
    echo "" >> "$output_file"
    
    # クリップボード統合（Windows依存コマンドの条件付き実行）
    local clipboard_enabled clipboard_cmd
    clipboard_enabled=$(get_yaml_value "platforms.wsl.clipboard.enabled" "$YAML_CONFIG" "true")
    clipboard_cmd=$(get_yaml_value "platforms.wsl.clipboard.command" "$YAML_CONFIG" "clip.exe")
    
    if [[ "$clipboard_enabled" == "true" ]]; then
        echo "# WSL clipboard integration (Windows-specific commands)" >> "$output_file"
        echo "# Only bind if running in WSL AND Windows commands are available" >> "$output_file"
        echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v $clipboard_cmd' 'bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel \"$clipboard_cmd\"'" >> "$output_file"
        echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v $clipboard_cmd' 'bind-key -T copy-mode C-w send-keys -X copy-pipe-and-cancel \"$clipboard_cmd\"'" >> "$output_file"
        
        local mouse_integration
        mouse_integration=$(get_yaml_value "platforms.wsl.clipboard.mouse_integration" "$YAML_CONFIG" "true")
        if [[ "$mouse_integration" == "true" ]]; then
            echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v $clipboard_cmd' 'bind-key -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel \"$clipboard_cmd\"'" >> "$output_file"
            echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v $clipboard_cmd' 'bind-key -T copy-mode MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel \"$clipboard_cmd\"'" >> "$output_file"
        fi
        echo "" >> "$output_file"
    fi
    
    # パフォーマンス設定
    local history_limit display_time display_panes_time status_interval
    history_limit=$(get_yaml_value "platforms.wsl.performance.history_limit" "$YAML_CONFIG" "50000")
    display_time=$(get_yaml_value "platforms.wsl.performance.display_time_ms" "$YAML_CONFIG" "3000")
    display_panes_time=$(get_yaml_value "platforms.wsl.performance.display_panes_time_ms" "$YAML_CONFIG" "3000")
    status_interval=$(get_yaml_value "platforms.wsl.performance.status_interval_sec" "$YAML_CONFIG" "5")
    
    echo "# WSL performance configuration" >> "$output_file"
    echo "set -g history-limit $history_limit" >> "$output_file"
    echo "set -g display-time $display_time" >> "$output_file"
    echo "set -g display-panes-time $display_panes_time" >> "$output_file"
    echo "set -g status-interval $status_interval" >> "$output_file"
    echo "" >> "$output_file"
    
    # ウィンドウステータス設定
    local current_style style activity_style bell_style format current_format
    current_style=$(get_yaml_value "platforms.wsl.window_status.current_style" "$YAML_CONFIG" "fg=colour235,bg=colour117,bold")
    style=$(get_yaml_value "platforms.wsl.window_status.style" "$YAML_CONFIG" "fg=colour250,bg=colour238")
    activity_style=$(get_yaml_value "platforms.wsl.window_status.activity_style" "$YAML_CONFIG" "fg=colour16,bg=colour228,bold")
    bell_style=$(get_yaml_value "platforms.wsl.window_status.bell_style" "$YAML_CONFIG" "fg=colour16,bg=colour202,bold")
    format=$(get_yaml_value "platforms.wsl.window_status.format" "$YAML_CONFIG" " #I:#W#F ")
    current_format=$(get_yaml_value "platforms.wsl.window_status.current_format" "$YAML_CONFIG" " #I:#W#F ")
    
    echo "# WSL window status configuration" >> "$output_file"
    echo "set-window-option -g window-status-current-style '$current_style'" >> "$output_file"
    echo "set-window-option -g window-status-style '$style'" >> "$output_file"
    echo "set-window-option -g window-status-activity-style '$activity_style'" >> "$output_file"
    echo "set-window-option -g window-status-bell-style '$bell_style'" >> "$output_file"
    echo "set-window-option -g window-status-format '$format'" >> "$output_file"
    echo "set-window-option -g window-status-current-format '$current_format'" >> "$output_file"
    echo "" >> "$output_file"
    
    # ステータスバー設定
    local status_right status_right_length
    status_right=$(get_yaml_value "platforms.wsl.status_bar.right" "$YAML_CONFIG")
    status_right_length=$(get_yaml_value "platforms.wsl.status_bar.right_length" "$YAML_CONFIG" "120")
    
    if [[ -n "$status_right" ]]; then
        echo "# WSL status bar configuration" >> "$output_file"
        echo "set -g status-right \"$status_right\"" >> "$output_file"
        echo "set -g status-right-length $status_right_length" >> "$output_file"
        echo "" >> "$output_file"
    fi
}

# macOS固有設定の生成
generate_macos_config() {
    local output_file="$1"
    
    log_info "macOS固有設定を生成中..."
    
    # 環境変数設定
    echo "# macOS environment variables" >> "$output_file"
    echo "set-environment -g MACOS_INTEGRATION 'enabled'" >> "$output_file"
    echo "" >> "$output_file"
    
    # パフォーマンス設定
    local history_limit status_interval
    history_limit=$(get_yaml_value "platforms.macos.performance.history_limit" "$YAML_CONFIG" "30000")
    status_interval=$(get_yaml_value "platforms.macos.performance.status_interval_sec" "$YAML_CONFIG" "2")
    
    echo "# macOS performance configuration" >> "$output_file"
    echo "set -g history-limit $history_limit" >> "$output_file"
    echo "set -g status-interval $status_interval" >> "$output_file"
    echo "" >> "$output_file"
}

# Linux固有設定の生成
generate_linux_config() {
    local output_file="$1"
    
    log_info "Linux固有設定を生成中..."
    
    # 環境変数設定
    echo "# Linux environment variables" >> "$output_file"
    echo "set-environment -g LINUX_INTEGRATION 'enabled'" >> "$output_file"
    echo "" >> "$output_file"
}

# Claude Voice設定の生成
generate_claude_voice_config() {
    local output_file="$1"
    
    log_info "Claude Voice設定を生成中..."
    
    echo "# Claude Voice integration configuration" >> "$output_file"
    
    # マスター設定
    local auto_summary notification_sound voice_engine
    auto_summary=$(get_yaml_value "claude_voice.master.auto_summary_enabled" "$YAML_CONFIG" "true")
    notification_sound=$(get_yaml_value "claude_voice.master.notification_sound_enabled" "$YAML_CONFIG" "true")
    voice_engine=$(get_yaml_value "claude_voice.master.voice_engine_version" "$YAML_CONFIG" "v3.0")
    
    echo "# Claude Voice master settings" >> "$output_file"
    echo "set-environment -g CLAUDE_VOICE_AUTO_SUMMARY \"$auto_summary\"" >> "$output_file"
    echo "set-environment -g CLAUDE_NOTIFICATION_SOUND \"$notification_sound\"" >> "$output_file"
    echo "set-environment -g CLAUDE_VOICE_ENGINE_VERSION \"$voice_engine\"" >> "$output_file"
    echo "" >> "$output_file"
    
    # 状態別設定
    local complete_enabled waiting_enabled busy_enabled
    complete_enabled=$(get_yaml_value "claude_voice.states.complete.enabled" "$YAML_CONFIG" "true")
    waiting_enabled=$(get_yaml_value "claude_voice.states.waiting.enabled" "$YAML_CONFIG" "true")
    busy_enabled=$(get_yaml_value "claude_voice.states.busy.enabled" "$YAML_CONFIG" "false")
    
    echo "# Claude Voice state-specific settings" >> "$output_file"
    echo "set-environment -g CLAUDE_VOICE_ON_COMPLETE \"$complete_enabled\"" >> "$output_file"
    echo "set-environment -g CLAUDE_VOICE_ON_WAITING \"$waiting_enabled\"" >> "$output_file"
    echo "set-environment -g CLAUDE_VOICE_ON_BUSY \"$busy_enabled\"" >> "$output_file"
    echo "" >> "$output_file"
    
    # 通知設定
    local notification_mode max_text_length timeout concurrent_limit
    notification_mode=$(get_yaml_value "claude_voice.notification.mode" "$YAML_CONFIG" "sound")
    max_text_length=$(get_yaml_value "claude_voice.notification.max_text_length" "$YAML_CONFIG" "5000")
    timeout=$(get_yaml_value "claude_voice.notification.timeout_sec" "$YAML_CONFIG" "30")
    concurrent_limit=$(get_yaml_value "claude_voice.notification.concurrent_limit" "$YAML_CONFIG" "1")
    
    echo "# Claude Voice notification settings" >> "$output_file"
    echo "set-environment -g CLAUDE_NOTIFICATION_MODE \"$notification_mode\"" >> "$output_file"
    echo "set-environment -g CLAUDE_MAX_TEXT_LENGTH \"$max_text_length\"" >> "$output_file"
    echo "set-environment -g CLAUDE_NOTIFICATION_TIMEOUT \"$timeout\"" >> "$output_file"
    echo "set-environment -g CLAUDE_CONCURRENT_LIMIT \"$concurrent_limit\"" >> "$output_file"
    echo "" >> "$output_file"
}

# キーバインド設定の生成
generate_key_bindings() {
    local platform="$1"
    local output_file="$2"
    
    log_info "キーバインド設定を生成中: $platform"
    
    echo "# Key bindings configuration" >> "$output_file"
    
    # Claude Voiceキーバインド
    local prefix
    prefix=$(get_yaml_value "claude_voice.key_bindings.prefix" "$YAML_CONFIG" "v")
    
    echo "# Claude Voice key bindings (Prefix + $prefix + [action])" >> "$output_file"
    
    # 各種キーバインド
    local test_key toggle_auto_key toggle_complete_key toggle_waiting_key toggle_busy_key
    test_key=$(get_yaml_value "claude_voice.key_bindings.test" "$YAML_CONFIG" "t")
    toggle_auto_key=$(get_yaml_value "claude_voice.key_bindings.toggle_auto_summary" "$YAML_CONFIG" "v")
    toggle_complete_key=$(get_yaml_value "claude_voice.key_bindings.toggle_complete" "$YAML_CONFIG" "1")
    toggle_waiting_key=$(get_yaml_value "claude_voice.key_bindings.toggle_waiting" "$YAML_CONFIG" "2")
    toggle_busy_key=$(get_yaml_value "claude_voice.key_bindings.toggle_busy" "$YAML_CONFIG" "3")
    
    # テストキーバインド
    echo "bind-key $prefix$test_key run-shell 'echo \"Testing Claude Voice integration...\" && ~/.tmux/claude/platforms/$platform/wsl_voice_engine.sh test'" >> "$output_file"
    
    # トグルキーバインド
    echo "bind-key $prefix$toggle_auto_key run-shell 'if [ \"\$CLAUDE_VOICE_AUTO_SUMMARY\" = \"true\" ]; then tmux set-environment -g CLAUDE_VOICE_AUTO_SUMMARY \"false\" && tmux display-message \"Claude Voice auto-summary disabled\"; else tmux set-environment -g CLAUDE_VOICE_AUTO_SUMMARY \"true\" && tmux display-message \"Claude Voice auto-summary enabled\"; fi'" >> "$output_file"
    
    echo "bind-key $prefix$toggle_complete_key run-shell 'if [ \"\$CLAUDE_VOICE_ON_COMPLETE\" = \"true\" ]; then tmux set-environment -g CLAUDE_VOICE_ON_COMPLETE \"false\" && tmux display-message \"Claude Voice on completion: OFF\"; else tmux set-environment -g CLAUDE_VOICE_ON_COMPLETE \"true\" && tmux display-message \"Claude Voice on completion: ON\"; fi'" >> "$output_file"
    
    echo "bind-key $prefix$toggle_waiting_key run-shell 'if [ \"\$CLAUDE_VOICE_ON_WAITING\" = \"true\" ]; then tmux set-environment -g CLAUDE_VOICE_ON_WAITING \"false\" && tmux display-message \"Claude Voice on waiting: OFF\"; else tmux set-environment -g CLAUDE_VOICE_ON_WAITING \"true\" && tmux display-message \"Claude Voice on waiting: ON\"; fi'" >> "$output_file"
    
    echo "bind-key $prefix$toggle_busy_key run-shell 'if [ \"\$CLAUDE_VOICE_ON_BUSY\" = \"true\" ]; then tmux set-environment -g CLAUDE_VOICE_ON_BUSY \"false\" && tmux display-message \"Claude Voice on busy: OFF\"; else tmux set-environment -g CLAUDE_VOICE_ON_BUSY \"true\" && tmux display-message \"Claude Voice on busy: ON\"; fi'" >> "$output_file"
    
    echo "" >> "$output_file"
    
    # プラットフォーム固有キーバインド
    case "$platform" in
        "wsl")
            generate_wsl_key_bindings "$output_file"
            ;;
        "macos")
            generate_macos_key_bindings "$output_file"
            ;;
    esac
}

# WSL固有キーバインドの生成
generate_wsl_key_bindings() {
    local output_file="$1"
    
    echo "# WSL-specific key bindings (Windows-dependent commands)" >> "$output_file"
    echo "# These bindings only work in WSL with Windows interop enabled" >> "$output_file"
    
    local explorer_key copy_path_key git_status_key split_h_key split_v_key
    explorer_key=$(get_yaml_value "platforms.wsl.key_bindings.explorer_current_dir" "$YAML_CONFIG" "C-e")
    copy_path_key=$(get_yaml_value "platforms.wsl.key_bindings.copy_path_to_clipboard" "$YAML_CONFIG" "C-p")
    git_status_key=$(get_yaml_value "platforms.wsl.key_bindings.git_status_display" "$YAML_CONFIG" "C-g")
    split_h_key=$(get_yaml_value "platforms.wsl.key_bindings.pane_split_horizontal" "$YAML_CONFIG" "|")
    split_v_key=$(get_yaml_value "platforms.wsl.key_bindings.pane_split_vertical" "$YAML_CONFIG" "-")
    
    # Windows依存コマンドは条件付きでバインド
    echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v explorer.exe' 'bind-key $explorer_key run-shell \"explorer.exe .\"' 'bind-key $explorer_key display-message \"explorer.exe not available\"'" >> "$output_file"
    echo "if-shell 'grep -qi microsoft /proc/version 2>/dev/null && command -v clip.exe' 'bind-key $copy_path_key run-shell \"pwd | tr -d \\\"\\\\\\\\n\\\" | clip.exe && tmux display-message \\\"Path copied to Windows clipboard\\\"\"' 'bind-key $copy_path_key display-message \"clip.exe not available\"'" >> "$output_file"
    
    # git status は汎用的なのでそのまま
    echo "bind-key $git_status_key run-shell 'cd #{pane_current_path} && git status --porcelain | head -10 | tmux display-message \"\$(cat)\"'" >> "$output_file"
    
    # pane split は汎用的
    echo "bind-key $split_h_key split-window -h -c \"#{pane_current_path}\"" >> "$output_file"
    echo "bind-key $split_v_key split-window -v -c \"#{pane_current_path}\"" >> "$output_file"
    
    echo "" >> "$output_file"
}

# macOS固有キーバインドの生成
generate_macos_key_bindings() {
    local output_file="$1"
    
    echo "# macOS-specific key bindings" >> "$output_file"
    echo "# (Add macOS-specific bindings here)" >> "$output_file"
    echo "" >> "$output_file"
}

# ====================================
# メイン処理
# ====================================

# 設定ファイル生成メイン関数
generate_config() {
    local platform="$1"
    local output_file="$OUTPUT_DIR/tmux-${platform}.conf"
    
    log_info "設定ファイル生成開始: $platform -> $output_file"
    
    # 出力ファイルの初期化
    : > "$output_file"
    
    # 各種設定の生成
    generate_base_config "$output_file"
    generate_platform_config "$platform" "$output_file"
    generate_claude_voice_config "$output_file"
    generate_key_bindings "$platform" "$output_file"
    
    # 生成完了メッセージ
    echo "# Configuration generated successfully" >> "$output_file"
    echo "# Generated at: $(date)" >> "$output_file"
    echo "# Platform: $platform" >> "$output_file"
    echo "# Source: $YAML_CONFIG" >> "$output_file"
    
    log_info "設定ファイル生成完了: $output_file"
}

# 使用方法の表示
show_usage() {
    cat << EOF
TMux統一設定ジェネレーター

使用方法:
  $0 [OPTIONS] [PLATFORM]

オプション:
  -h, --help     このヘルプを表示
  -v, --verbose  詳細ログを有効化
  -a, --all      全プラットフォーム対応設定を生成
  -c, --check    設定ファイルの検証のみ実行

プラットフォーム:
  wsl            Windows Subsystem for Linux
  macos          macOS
  linux          Linux
  auto           自動検出 (デフォルト)

例:
  $0 auto              # 自動検出して設定生成
  $0 wsl               # WSL用設定を生成
  $0 --all             # 全プラットフォーム用設定を生成
  $0 --check           # 設定ファイルの検証のみ

EOF
}

# 設定検証
validate_config() {
    log_info "設定ファイルの検証を開始..."
    
    if ! [[ -f "$YAML_CONFIG" ]]; then
        log_error "YAML設定ファイルが見つかりません: $YAML_CONFIG"
        return 1
    fi
    
    # YAML構文チェック
    if command -v yq &> /dev/null; then
        if ! yq eval '.' "$YAML_CONFIG" > /dev/null; then
            log_error "YAML構文エラーが検出されました"
            return 1
        fi
        log_info "YAML構文チェック: OK"
    else
        log_warn "yqが利用できません。YAML構文チェックをスキップします"
    fi
    
    # 必須キーの存在確認
    local required_keys=(
        "base.shell.default_command"
        "base.prefix.key"
        "metadata.version"
    )
    
    for key in "${required_keys[@]}"; do
        local value
        value=$(get_yaml_value "$key" "$YAML_CONFIG")
        if [[ -z "$value" || "$value" == "null" ]]; then
            log_error "必須キーが見つかりません: $key"
            return 1
        fi
        log_debug "必須キー確認: $key = $value"
    done
    
    log_info "設定ファイル検証完了"
    return 0
}

# メイン処理
main() {
    local platform="auto"
    local verbose=false
    local generate_all=false
    local check_only=false
    
    # コマンドライン引数の解析
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--verbose)
                export LOG_LEVEL="DEBUG"
                verbose=true
                shift
                ;;
            -a|--all)
                generate_all=true
                shift
                ;;
            -c|--check)
                check_only=true
                shift
                ;;
            wsl|macos|linux|auto)
                platform="$1"
                shift
                ;;
            *)
                log_error "未知のオプション: $1"
                show_usage
                exit 1
                ;;
        esac
    done
    
    # ログ開始
    log_info "TMux統一設定ジェネレーター開始"
    log_info "Version: 1.0, Platform: $platform, Verbose: $verbose"
    
    # 前提条件チェック
    check_prerequisites
    
    # 設定検証
    if ! validate_config; then
        error_exit "設定ファイルの検証に失敗しました"
    fi
    
    # 検証のみの場合は終了
    if [[ "$check_only" == "true" ]]; then
        log_info "設定ファイル検証のみ実行しました"
        exit 0
    fi
    
    # プラットフォーム自動検出
    if [[ "$platform" == "auto" ]]; then
        platform=$(detect_platform)
        log_info "自動検出されたプラットフォーム: $platform"
    fi
    
    # 設定生成
    if [[ "$generate_all" == "true" ]]; then
        log_info "全プラットフォーム対応設定を生成中..."
        for p in wsl macos linux; do
            generate_config "$p"
        done
    else
        generate_config "$platform"
    fi
    
    log_info "TMux統一設定ジェネレーター完了"
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi