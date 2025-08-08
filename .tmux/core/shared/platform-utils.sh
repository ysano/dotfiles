#!/bin/bash
# Platform Utilities - Centralized platform detection and management
# プラットフォームユーティリティ - 統一プラットフォーム検出・管理

set -euo pipefail

readonly PLATFORM_UTILS_VERSION="1.0.0"

# キャッシュ設定
readonly PLATFORM_CACHE_TTL=60
readonly PLATFORM_CACHE_FILE="/tmp/.tmux_platform_cache_$$"

# === プラットフォーム検出 (キャッシュ対応) ===
detect_platform() {
    # キャッシュチェック
    if [[ -f "$PLATFORM_CACHE_FILE" ]]; then
        local file_age=$(( $(date +%s) - $(stat -c %Y "$PLATFORM_CACHE_FILE" 2>/dev/null || echo 0) ))
        if [[ $file_age -lt $PLATFORM_CACHE_TTL ]]; then
            cat "$PLATFORM_CACHE_FILE"
            return 0
        fi
    fi
    
    # プラットフォーム検出
    local platform
    case "$(uname)" in
        "Darwin")
            platform="macos"
            ;;
        "Linux")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                platform="wsl"
            else
                platform="linux"
            fi
            ;;
        "FreeBSD")
            platform="freebsd"
            ;;
        *)
            platform="unknown"
            ;;
    esac
    
    # キャッシュ保存
    echo "$platform" > "$PLATFORM_CACHE_FILE"
    echo "$platform"
}

# === プラットフォーム固有機能チェック ===
platform_has_capability() {
    local capability="$1"
    local platform=$(detect_platform)
    
    case "$platform" in
        "macos")
            case "$capability" in
                "speech_synthesis") command -v say >/dev/null 2>&1 ;;
                "audio_playback") command -v afplay >/dev/null 2>&1 ;;
                "notifications") [[ -d "/Applications" ]] ;;
                *) return 1 ;;
            esac
            ;;
        "linux")
            case "$capability" in
                "speech_synthesis") command -v espeak >/dev/null 2>&1 || command -v festival >/dev/null 2>&1 ;;
                "audio_playback") command -v paplay >/dev/null 2>&1 || command -v aplay >/dev/null 2>&1 ;;
                "notifications") command -v notify-send >/dev/null 2>&1 || command -v zenity >/dev/null 2>&1 ;;
                *) return 1 ;;
            esac
            ;;
        "wsl")
            case "$capability" in
                "speech_synthesis") _wsl_has_powershell ;;
                "audio_playback") _wsl_has_powershell ;;
                "notifications") _wsl_has_powershell ;;
                *) return 1 ;;
            esac
            ;;
        "freebsd")
            case "$capability" in
                "speech_synthesis") command -v espeak >/dev/null 2>&1 ;;
                "audio_playback") command -v play >/dev/null 2>&1 ;;
                "notifications") command -v notify-send >/dev/null 2>&1 ;;
                *) return 1 ;;
            esac
            ;;
        *)
            return 1
            ;;
    esac
}

# === WSL専用ヘルパー ===
_wsl_has_powershell() {
    command -v powershell.exe >/dev/null 2>&1 || \
    [[ -x "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" ]]
}

get_wsl_powershell_path() {
    if command -v powershell.exe >/dev/null 2>&1; then
        echo "powershell.exe"
    elif [[ -x "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" ]]; then
        echo "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
    else
        return 1
    fi
}

# === 環境検出 ===
is_ssh_session() {
    [[ -n "${SSH_CLIENT:-}" ]] || [[ -n "${SSH_TTY:-}" ]] || [[ "${SSH_CONNECTION:-}" ]]
}

is_local_session() {
    ! is_ssh_session
}

# === クリーンアップ ===
cleanup_platform_cache() {
    rm -f "$PLATFORM_CACHE_FILE" 2>/dev/null || true
}

# === ユーティリティ関数 ===
get_platform_info() {
    local platform=$(detect_platform)
    
    echo "Platform: $platform"
    echo "Kernel: $(uname -s)"
    echo "Version: $(uname -r)"
    echo "Architecture: $(uname -m)"
    
    case "$platform" in
        "wsl")
            echo "WSL Distribution: ${WSL_DISTRO_NAME:-unknown}"
            if _wsl_has_powershell; then
                echo "PowerShell: Available"
            else
                echo "PowerShell: Not available"
            fi
            ;;
        "macos")
            echo "macOS Version: $(sw_vers -productVersion 2>/dev/null || echo 'unknown')"
            ;;
        "linux")
            if [[ -f /etc/os-release ]]; then
                echo "Distribution: $(grep '^NAME=' /etc/os-release | cut -d'=' -f2 | tr -d '"')"
            fi
            ;;
    esac
    
    echo ""
    echo "Capabilities:"
    for cap in speech_synthesis audio_playback notifications; do
        if platform_has_capability "$cap"; then
            echo "  ✅ $cap"
        else
            echo "  ❌ $cap"
        fi
    done
}

# === メイン実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-detect}" in
        "detect")
            detect_platform
            ;;
        "info")
            get_platform_info
            ;;
        "capability")
            if [[ -n "${2:-}" ]]; then
                if platform_has_capability "$2"; then
                    echo "yes"
                    exit 0
                else
                    echo "no"
                    exit 1
                fi
            else
                echo "Usage: $0 capability <capability_name>"
                exit 1
            fi
            ;;
        "cleanup")
            cleanup_platform_cache
            echo "Platform cache cleaned"
            ;;
        *)
            echo "Usage: $0 {detect|info|capability|cleanup}"
            exit 1
            ;;
    esac
fi