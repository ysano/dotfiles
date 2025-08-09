#!/bin/bash
# TMux Scripts Platform Library
# Version: 1.0.0
#
# OS検出とプラットフォーム固有操作を提供
# - OS/プラットフォーム検出
# - プラットフォーム固有パス
# - システム情報取得

# インポートガード
[[ -n "${_TMUX_PLATFORM_LOADED}" ]] && return 0
declare -gr _TMUX_PLATFORM_LOADED=1

# core.sh依存
source "$(dirname "${BASH_SOURCE[0]}")/core.sh"

# === プラットフォーム定数 ===
readonly PLATFORM_CACHE_FILE="/tmp/.tmux_platform_cache_$$"
readonly PLATFORM_CACHE_TTL=300  # 5分間キャッシュ

# プラットフォーム名定数
readonly PLATFORM_MACOS="macos"
readonly PLATFORM_LINUX="linux"
readonly PLATFORM_WSL="wsl"
readonly PLATFORM_WINDOWS="windows"
readonly PLATFORM_FREEBSD="freebsd"
readonly PLATFORM_OPENBSD="openbsd"
readonly PLATFORM_NETBSD="netbsd"
readonly PLATFORM_UNKNOWN="unknown"

# === プラットフォーム検出 ===

# キャッシュ付きプラットフォーム検出
detect_platform() {
    # キャッシュチェック
    if [[ -f "$PLATFORM_CACHE_FILE" ]]; then
        local cache_age=$(( $(date +%s) - $(stat -f%m "$PLATFORM_CACHE_FILE" 2>/dev/null || stat -c%Y "$PLATFORM_CACHE_FILE" 2>/dev/null || echo 0) ))
        if [[ $cache_age -lt $PLATFORM_CACHE_TTL ]]; then
            cat "$PLATFORM_CACHE_FILE"
            return 0
        fi
    fi
    
    local platform=$(_detect_platform_internal)
    
    # キャッシュ保存
    echo "$platform" > "$PLATFORM_CACHE_FILE"
    register_cleanup "$PLATFORM_CACHE_FILE"
    
    echo "$platform"
}

# 内部プラットフォーム検出
_detect_platform_internal() {
    local platform="$PLATFORM_UNKNOWN"
    
    # OSTYPE変数から判定（最優先）
    if [[ -n "${OSTYPE:-}" ]]; then
        case "$OSTYPE" in
            darwin*)  platform="$PLATFORM_MACOS" ;;
            linux*)   
                # WSLチェック
                if [[ -n "${WSL_DISTRO_NAME:-}" ]] || \
                   [[ -f /proc/sys/fs/binfmt_misc/WSLInterop ]] || \
                   grep -qi microsoft /proc/version 2>/dev/null; then
                    platform="$PLATFORM_WSL"
                else
                    platform="$PLATFORM_LINUX"
                fi
                ;;
            msys*|cygwin*|mingw*) platform="$PLATFORM_WINDOWS" ;;
            freebsd*) platform="$PLATFORM_FREEBSD" ;;
            openbsd*) platform="$PLATFORM_OPENBSD" ;;
            netbsd*)  platform="$PLATFORM_NETBSD" ;;
        esac
    fi
    
    # unameから判定（フォールバック）
    if [[ "$platform" == "$PLATFORM_UNKNOWN" ]] && command_exists uname; then
        local uname_s=$(uname -s 2>/dev/null)
        case "$uname_s" in
            Darwin)  platform="$PLATFORM_MACOS" ;;
            Linux)   
                # WSLチェック
                if [[ -n "${WSL_DISTRO_NAME:-}" ]] || \
                   [[ -f /proc/sys/fs/binfmt_misc/WSLInterop ]] || \
                   grep -qi microsoft /proc/version 2>/dev/null; then
                    platform="$PLATFORM_WSL"
                else
                    platform="$PLATFORM_LINUX"
                fi
                ;;
            FreeBSD) platform="$PLATFORM_FREEBSD" ;;
            OpenBSD) platform="$PLATFORM_OPENBSD" ;;
            NetBSD)  platform="$PLATFORM_NETBSD" ;;
            CYGWIN*|MINGW*|MSYS*) platform="$PLATFORM_WINDOWS" ;;
        esac
    fi
    
    log_debug "検出されたプラットフォーム: $platform"
    echo "$platform"
}

# プラットフォーム判定ヘルパー
is_macos() {
    [[ "$(detect_platform)" == "$PLATFORM_MACOS" ]]
}

is_linux() {
    [[ "$(detect_platform)" == "$PLATFORM_LINUX" ]]
}

is_wsl() {
    [[ "$(detect_platform)" == "$PLATFORM_WSL" ]]
}

is_windows() {
    [[ "$(detect_platform)" == "$PLATFORM_WINDOWS" ]]
}

is_bsd() {
    local platform=$(detect_platform)
    [[ "$platform" == "$PLATFORM_FREEBSD" ]] || \
    [[ "$platform" == "$PLATFORM_OPENBSD" ]] || \
    [[ "$platform" == "$PLATFORM_NETBSD" ]]
}

# === WSL固有機能 ===

# WSLバージョン検出
get_wsl_version() {
    if ! is_wsl; then
        echo ""
        return 1
    fi
    
    if [[ -f /proc/version ]]; then
        if grep -qi "microsoft.*WSL2" /proc/version; then
            echo "2"
        else
            echo "1"
        fi
    else
        echo ""
    fi
}

# WSLディストリビューション名取得
get_wsl_distro() {
    if is_wsl; then
        echo "${WSL_DISTRO_NAME:-unknown}"
    else
        echo ""
    fi
}

# WindowsホストのPowerShellパス検出
find_powershell() {
    local powershell_cache="/tmp/.tmux_powershell_cache_$$"
    
    # キャッシュチェック
    if [[ -f "$powershell_cache" ]]; then
        local cached_path=$(cat "$powershell_cache")
        if [[ -x "$cached_path" ]]; then
            echo "$cached_path"
            return 0
        fi
    fi
    
    # PowerShell検索候補
    local candidates=(
        "powershell.exe"
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
        "/mnt/c/Program Files/PowerShell/6/pwsh.exe"
        "/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/c/Program Files/PowerShell/7/pwsh.exe"
        "pwsh.exe"
    )
    
    for candidate in "${candidates[@]}"; do
        if command_exists "$candidate" || [[ -x "$candidate" ]]; then
            echo "$candidate" > "$powershell_cache"
            register_cleanup "$powershell_cache"
            echo "$candidate"
            return 0
        fi
    done
    
    return 1
}

# === システム情報取得 ===

# OS詳細情報取得
get_os_info() {
    local platform=$(detect_platform)
    local info=""
    
    case "$platform" in
        "$PLATFORM_MACOS")
            info=$(sw_vers 2>/dev/null | paste -s -d' ' -)
            ;;
        "$PLATFORM_LINUX"|"$PLATFORM_WSL")
            if [[ -f /etc/os-release ]]; then
                info=$(grep -E "^(NAME|VERSION)=" /etc/os-release | cut -d= -f2 | tr -d '"' | paste -s -d' ' -)
            elif command_exists lsb_release; then
                info=$(lsb_release -d 2>/dev/null | cut -f2)
            fi
            ;;
        "$PLATFORM_FREEBSD"|"$PLATFORM_OPENBSD"|"$PLATFORM_NETBSD")
            info=$(uname -sr 2>/dev/null)
            ;;
    esac
    
    echo "${info:-$platform}"
}

# CPU情報取得
get_cpu_info() {
    local platform=$(detect_platform)
    local cpu_info=""
    
    case "$platform" in
        "$PLATFORM_MACOS")
            cpu_info=$(sysctl -n machdep.cpu.brand_string 2>/dev/null)
            ;;
        "$PLATFORM_LINUX"|"$PLATFORM_WSL")
            cpu_info=$(grep "model name" /proc/cpuinfo 2>/dev/null | head -1 | cut -d: -f2 | xargs)
            ;;
        "$PLATFORM_FREEBSD"|"$PLATFORM_OPENBSD"|"$PLATFORM_NETBSD")
            cpu_info=$(sysctl -n hw.model 2>/dev/null)
            ;;
    esac
    
    echo "${cpu_info:-unknown}"
}

# メモリ情報取得
get_memory_info() {
    local platform=$(detect_platform)
    local memory_info=""
    
    case "$platform" in
        "$PLATFORM_MACOS")
            local total=$(sysctl -n hw.memsize 2>/dev/null)
            if [[ -n "$total" ]]; then
                memory_info="$((total / 1024 / 1024 / 1024)) GB"
            fi
            ;;
        "$PLATFORM_LINUX"|"$PLATFORM_WSL")
            local total=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}')
            if [[ -n "$total" ]]; then
                memory_info="$((total / 1024 / 1024)) GB"
            fi
            ;;
        "$PLATFORM_FREEBSD"|"$PLATFORM_OPENBSD"|"$PLATFORM_NETBSD")
            local total=$(sysctl -n hw.physmem 2>/dev/null)
            if [[ -n "$total" ]]; then
                memory_info="$((total / 1024 / 1024 / 1024)) GB"
            fi
            ;;
    esac
    
    echo "${memory_info:-unknown}"
}

# === プラットフォーム固有パス ===

# ホームディレクトリパス取得
get_home_dir() {
    echo "${HOME:-$(eval echo ~$(whoami))}"
}

# 設定ディレクトリパス取得
get_config_dir() {
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            echo "${XDG_CONFIG_HOME:-$HOME/Library/Application Support}"
            ;;
        *)
            echo "${XDG_CONFIG_HOME:-$HOME/.config}"
            ;;
    esac
}

# キャッシュディレクトリパス取得
get_cache_dir() {
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            echo "${XDG_CACHE_HOME:-$HOME/Library/Caches}"
            ;;
        *)
            echo "${XDG_CACHE_HOME:-$HOME/.cache}"
            ;;
    esac
}

# 一時ディレクトリパス取得
get_temp_dir() {
    echo "${TMPDIR:-${TMP:-${TEMP:-/tmp}}}"
}

# === プラットフォーム固有コマンド ===

# ファイルを開く（OS固有）
open_file() {
    local file="$1"
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            open "$file"
            ;;
        "$PLATFORM_LINUX"|"$PLATFORM_WSL")
            if command_exists xdg-open; then
                xdg-open "$file"
            elif is_wsl && command_exists explorer.exe; then
                explorer.exe "$file"
            else
                log_error "ファイルを開くコマンドが見つかりません"
                return 1
            fi
            ;;
        "$PLATFORM_WINDOWS")
            start "$file"
            ;;
        *)
            log_error "サポートされていないプラットフォーム: $platform"
            return 1
            ;;
    esac
}

# クリップボードにコピー
copy_to_clipboard() {
    local text="$1"
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            echo -n "$text" | pbcopy
            ;;
        "$PLATFORM_LINUX")
            if command_exists xclip; then
                echo -n "$text" | xclip -selection clipboard
            elif command_exists xsel; then
                echo -n "$text" | xsel --clipboard --input
            else
                log_error "クリップボードコマンドが見つかりません"
                return 1
            fi
            ;;
        "$PLATFORM_WSL")
            if command_exists clip.exe; then
                echo -n "$text" | clip.exe
            else
                log_error "clip.exeが見つかりません"
                return 1
            fi
            ;;
        *)
            log_error "サポートされていないプラットフォーム: $platform"
            return 1
            ;;
    esac
}

# クリップボードから貼り付け
paste_from_clipboard() {
    local platform=$(detect_platform)
    
    case "$platform" in
        "$PLATFORM_MACOS")
            pbpaste
            ;;
        "$PLATFORM_LINUX")
            if command_exists xclip; then
                xclip -selection clipboard -o
            elif command_exists xsel; then
                xsel --clipboard --output
            else
                log_error "クリップボードコマンドが見つかりません"
                return 1
            fi
            ;;
        "$PLATFORM_WSL")
            if find_powershell >/dev/null; then
                $(find_powershell) -NoProfile -Command "Get-Clipboard" 2>/dev/null | tr -d '\r'
            else
                log_error "PowerShellが見つかりません"
                return 1
            fi
            ;;
        *)
            log_error "サポートされていないプラットフォーム: $platform"
            return 1
            ;;
    esac
}

# === 初期化 ===

# ライブラリ初期化
_init_platform() {
    if [[ "$TMUX_SCRIPTS_DEBUG" == "true" ]]; then
        local platform=$(detect_platform)
        log_debug "TMux Scripts Platform Library loaded"
        log_debug "Current platform: $platform"
        
        if is_wsl; then
            log_debug "WSL version: $(get_wsl_version)"
            log_debug "WSL distro: $(get_wsl_distro)"
            local ps_path=$(find_powershell 2>/dev/null)
            [[ -n "$ps_path" ]] && log_debug "PowerShell: $ps_path"
        fi
    fi
}

# 自動初期化
_init_platform