#!/bin/bash
# WSL Environment Detection Module (Refactored)
# WSL環境検出とWindows統合機能 - 統一プラットフォームユーティリティ使用版

set -euo pipefail

# 統一プラットフォームユーティリティの読み込み
readonly PLATFORM_UTILS_PATH="$(dirname "$(dirname "${BASH_SOURCE[0]}")")/core/platform_utils.sh"
if [[ -f "$PLATFORM_UTILS_PATH" ]]; then
    source "$PLATFORM_UTILS_PATH"
else
    echo "ERROR: Platform utilities not found: $PLATFORM_UTILS_PATH" >&2
    exit 1
fi

# === WSL固有の拡張機能 ===
# platform_utilsの基本機能に加えて、WSL固有の機能を提供

# WSL統合状態の詳細チェック
check_wsl_integration_status() {
    local integration_status="unknown"
    
    # Windows相互運用性の確認
    if command -v explorer.exe >/dev/null 2>&1; then
        integration_status="full"
    elif command -v powershell.exe >/dev/null 2>&1; then
        integration_status="partial"
    else
        integration_status="limited"
    fi
    
    echo "$integration_status"
}

# WSLバージョン固有の機能確認
get_wsl_capabilities() {
    local wsl_type=$(detect_wsl_environment)
    
    case "$wsl_type" in
        "wsl2")
            echo "gpu_support,systemd,networking,full_filesystem"
            ;;
        "wsl1")
            echo "filesystem,limited_networking"
            ;;
        "wsl_compatible")
            echo "powershell_bridge"
            ;;
        *)
            echo "none"
            ;;
    esac
}

# Windows音声デバイスの詳細情報取得
get_windows_audio_info() {
    if ! has_powershell; then
        echo "powershell_unavailable"
        return 1
    fi
    
    get_windows_audio_devices | head -5  # 最初の5デバイスのみ
}

# === 初期化チェック ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "WSL Environment Detection Module"
    echo "Functions: detect_wsl_environment, find_powershell, check_windows_speech"
fi