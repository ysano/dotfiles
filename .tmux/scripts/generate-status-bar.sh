#!/bin/bash
# Dynamic Status Bar Generator for tmux
# OS固有コマンドを抽象化した動的ステータスバー生成

set -euo pipefail

# OS検出
detect_os() {
    case "$(uname)" in
        "Darwin")
            echo "macos"
            ;;
        "Linux")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                echo "wsl"
            else
                echo "linux"
            fi
            ;;
        "FreeBSD")
            echo "freebsd"
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# OS固有コマンド定義
get_os_commands() {
    local os="$1"
    
    case "$os" in
        "macos")
            cat << 'EOF'
# macOS用コマンド
LOAD_CMD_PRIMARY="tmux-mem-cpu-load --colors --interval 2 --graph-lines 0 --mem-mode 2"
LOAD_CMD_FALLBACK="sysctl -n vm.loadavg | cut -d' ' -f2"
LOAD_CMD_CHECK="command -v tmux-mem-cpu-load"
MEMORY_CMD=""
DISK_CMD=""
OS_INFO_CMD=""
EXTRA_INFO=""
COLORS_PRIMARY="#[fg=colour250,bg=colour238]"
COLORS_SECONDARY="#[fg=colour226,bg=colour238,bold]"
STATUS_LENGTH="100"
EOF
            ;;
        "wsl")
            cat << 'EOF'
# WSL用コマンド
LOAD_CMD_PRIMARY="cat /proc/loadavg | cut -d ' ' -f 1-3"
LOAD_CMD_FALLBACK="cat /proc/loadavg | cut -d ' ' -f 1-3"
LOAD_CMD_CHECK="test -f /proc/loadavg"
MEMORY_CMD="free | awk 'NR==2{printf \"%.0f%%\", \$3*100/\$2}'"
DISK_CMD="df -h / | awk 'NR==2{print \$5}'"
OS_INFO_CMD="lsb_release -rs 2>/dev/null || echo 'WSL'"
EXTRA_INFO="💻"
COLORS_PRIMARY="#[fg=colour250,bg=colour238]"
COLORS_SECONDARY="#[fg=yellow,bold]"
STATUS_LENGTH="120"
EOF
            ;;
        "linux")
            cat << 'EOF'
# Linux用コマンド
LOAD_CMD_PRIMARY="cut -d ' ' -f 1-3 /proc/loadavg"
LOAD_CMD_FALLBACK="cut -d ' ' -f 1-3 /proc/loadavg"
LOAD_CMD_CHECK="test -f /proc/loadavg"
MEMORY_CMD="free | awk 'NR==2{printf \"%.0f%%\", \$3*100/\$2}'"
DISK_CMD=""
OS_INFO_CMD=""
EXTRA_INFO="🧠"
SWAP_CMD="free | awk 'NR==3{if(\$2>0) printf \"%.0f%%\", \$3*100/\$2; else print \"0%\"}'"
COLORS_PRIMARY="#[fg=yellow,bold]"
COLORS_SECONDARY="#[fg=cyan]"
STATUS_LENGTH="100"
EOF
            ;;
        "freebsd")
            cat << 'EOF'
# FreeBSD用コマンド
LOAD_CMD_PRIMARY="sysctl vm.loadavg | cut -d ' ' -f 3-5"
LOAD_CMD_FALLBACK="sysctl vm.loadavg | cut -d ' ' -f 3-5"
LOAD_CMD_CHECK="sysctl vm.loadavg >/dev/null 2>&1"
MEMORY_CMD=""
DISK_CMD=""
OS_INFO_CMD=""
EXTRA_INFO=""
COLORS_PRIMARY="#[fg=yellow,bold]"
COLORS_SECONDARY="#[fg=cyan,bold]"
STATUS_LENGTH="100"
EOF
            ;;
        *)
            cat << 'EOF'
# デフォルト/未知のOS用
LOAD_CMD_PRIMARY="uptime | awk -F'load average:' '{print \$2}'"
LOAD_CMD_FALLBACK="uptime | awk -F'load average:' '{print \$2}'"
LOAD_CMD_CHECK="command -v uptime"
MEMORY_CMD=""
DISK_CMD=""
OS_INFO_CMD=""
EXTRA_INFO=""
COLORS_PRIMARY="#[fg=white]"
COLORS_SECONDARY="#[fg=white]"
STATUS_LENGTH="80"
EOF
            ;;
    esac
}

# ステータスバー生成
generate_status_bar() {
    local os="$1"
    local template_type="${2:-full}"  # full, minimal, custom
    
    # OS固有コマンドを読み込み
    eval "$(get_os_commands "$os")"
    
    case "$template_type" in
        "full")
            generate_full_status_bar
            ;;
        "minimal")  
            generate_minimal_status_bar
            ;;
        "custom")
            generate_custom_status_bar
            ;;
        *)
            generate_full_status_bar
            ;;
    esac
}

# フルステータスバー生成
generate_full_status_bar() {
    local status_right=""
    local load_section=""
    local memory_section=""
    local disk_section=""
    local os_info_section=""
    local time_section=""
    
    # 負荷情報セクション
    if [[ -n "$LOAD_CMD_CHECK" ]]; then
        load_section="if-shell '$LOAD_CMD_CHECK' \
          '$COLORS_PRIMARY $EXTRA_INFO #($LOAD_CMD_PRIMARY)#[default]' \
          '$COLORS_SECONDARY Load:#($LOAD_CMD_FALLBACK)#[default]'"
    else
        load_section="$COLORS_SECONDARY ⚡ #($LOAD_CMD_PRIMARY)#[default]"
    fi
    
    # メモリ情報セクション
    if [[ -n "$MEMORY_CMD" ]]; then
        memory_section=" #[fg=cyan]🧠 #($MEMORY_CMD)#[default]"
    fi
    
    # ディスク情報セクション
    if [[ -n "$DISK_CMD" ]]; then
        disk_section=" #[fg=green]💾 #($DISK_CMD)#[default]"
    fi
    
    # OS情報セクション
    if [[ -n "$OS_INFO_CMD" ]]; then
        os_info_section=" 💻 #($OS_INFO_CMD)"
    fi
    
    # スワップ情報セクション（Linux特有）
    local swap_section=""
    if [[ -n "${SWAP_CMD:-}" ]]; then
        swap_section=" 🔄 #[fg=cyan]#($SWAP_CMD)#[default]"
    fi
    
    # 時刻セクション
    time_section=" #[fg=colour117,bg=colour238]#[fg=colour235,bg=colour117,bold] %m/%d(%a) %H:%M "
    
    # 完全なステータスバー組み立て
    if [[ "$os" == "macos" ]]; then
        echo "set -g status-right \"#[fg=colour238,bg=colour235]$load_section$time_section\""
    else
        echo "set -g status-right \"#[fg=colour238,bg=colour235]$COLORS_PRIMARY$os_info_section #[fg=yellow,bold]⚡ #($LOAD_CMD_PRIMARY)#[default]$memory_section$disk_section$swap_section$time_section\""
    fi
    
    echo "set -g status-right-length $STATUS_LENGTH"
}

# ミニマルステータスバー生成
generate_minimal_status_bar() {
    echo "set -g status-right \"$COLORS_PRIMARY #($LOAD_CMD_PRIMARY)#[default] #[fg=colour117,bold]%H:%M\""
    echo "set -g status-right-length 50"
}

# カスタムステータスバー生成（設定ファイルから）
generate_custom_status_bar() {
    local config_file="$HOME/.tmux/status-config.yaml"
    if [[ -f "$config_file" ]]; then
        # YAML設定があれば使用（将来の拡張）
        generate_full_status_bar
    else
        generate_full_status_bar
    fi
}

# tmux設定への適用
apply_to_tmux() {
    local os="$1"
    local template_type="${2:-full}"
    
    echo "# 動的生成されたステータスバー設定 (OS: $os, Template: $template_type)"
    echo "# Generated at: $(date)"
    echo ""
    generate_status_bar "$os" "$template_type"
}

# テスト用関数
test_commands() {
    local os="$1"
    echo "=== Testing OS commands for: $os ==="
    
    eval "$(get_os_commands "$os")"
    
    echo "Load command: $LOAD_CMD_PRIMARY"
    if [[ -n "$LOAD_CMD_CHECK" ]]; then
        if eval "$LOAD_CMD_CHECK" 2>/dev/null; then
            echo "✓ Primary load command available"
            echo "  Result: $(eval "$LOAD_CMD_PRIMARY" 2>/dev/null || echo 'Failed')"
        else
            echo "⚠ Primary load command not available, using fallback"
            echo "  Result: $(eval "$LOAD_CMD_FALLBACK" 2>/dev/null || echo 'Failed')"
        fi
    fi
    
    if [[ -n "$MEMORY_CMD" ]]; then
        echo "Memory command: $MEMORY_CMD"
        echo "  Result: $(eval "$MEMORY_CMD" 2>/dev/null || echo 'Failed')"
    fi
    
    if [[ -n "$DISK_CMD" ]]; then
        echo "Disk command: $DISK_CMD"
        echo "  Result: $(eval "$DISK_CMD" 2>/dev/null || echo 'Failed')"
    fi
    
    echo ""
}

# メイン処理
main() {
    local command="${1:-generate}"
    local os="${2:-$(detect_os)}"
    local template="${3:-full}"
    
    case "$command" in
        "generate"|"")
            apply_to_tmux "$os" "$template"
            ;;
        "test")
            test_commands "$os"
            ;;
        "detect")
            echo "Detected OS: $(detect_os)"
            ;;
        *)
            cat << 'EOF'
Dynamic Status Bar Generator for tmux

Usage:
  generate-status-bar.sh [command] [os] [template]

Commands:
  generate     Generate status bar configuration (default)
  test         Test OS-specific commands
  detect       Detect current OS

OS Options:
  macos        macOS configuration
  wsl          Windows Subsystem for Linux
  linux        Pure Linux configuration  
  freebsd      FreeBSD configuration
  auto         Auto-detect (default)

Template Options:
  full         Full status bar with all available info (default)
  minimal      Minimal status bar with load and time only
  custom       Custom configuration (future enhancement)

Examples:
  generate-status-bar.sh
  generate-status-bar.sh generate wsl full
  generate-status-bar.sh test macos
  generate-status-bar.sh detect
EOF
            ;;
    esac
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi