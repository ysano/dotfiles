#!/bin/bash
# Platform Utilities - Unified Platform Detection and PowerShell Management
# プラットフォーム共通ユーティリティ - 統一されたOS検出とPowerShell管理

set -euo pipefail

# === 基本設定 ===
readonly PLATFORM_UTILS_VERSION="1.0.0"
readonly CACHE_DURATION=300  # 5分間キャッシュ

# キャッシュファイルパス
readonly WSL_CACHE_FILE="/tmp/.claude_wsl_detection_$$"
readonly POWERSHELL_CACHE_FILE="/tmp/.claude_powershell_path_$$"

# === ログ出力 ===
platform_utils_log() {
    local level="$1"
    shift
    if [[ "${CLAUDE_DEBUG:-false}" == "true" ]]; then
        echo "[$(date '+%H:%M:%S')] [PLATFORM_UTILS] [$level] $*" >&2
    fi
}

# === WSL環境検出（統一実装） ===
detect_wsl_environment() {
    # キャッシュチェック
    if [[ -f "$WSL_CACHE_FILE" ]]; then
        local cache_age=$(( $(date +%s) - $(stat -c %Y "$WSL_CACHE_FILE" 2>/dev/null || echo 0) ))
        if [[ $cache_age -lt $CACHE_DURATION ]]; then
            cat "$WSL_CACHE_FILE" 2>/dev/null && return 0
        fi
    fi

    local wsl_type="not_wsl"
    
    # 最優先: WSL環境変数による検出
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        wsl_type="wsl2"
        platform_utils_log "DEBUG" "WSL detected via WSL_DISTRO_NAME: $WSL_DISTRO_NAME"
    elif [[ -n "${WSLENV:-}" ]]; then
        wsl_type="wsl2"
        platform_utils_log "DEBUG" "WSL detected via WSLENV"
    
    # /proc/version による詳細検出
    elif [[ -f /proc/version ]] && grep -qi microsoft /proc/version 2>/dev/null; then
        if grep -qi "microsoft-standard-wsl2" /proc/version 2>/dev/null; then
            wsl_type="wsl2"
        elif grep -qi "microsoft" /proc/version 2>/dev/null; then
            # レガシー検出パターン
            if [[ -f /proc/sys/kernel/osrelease ]] && grep -qi microsoft /proc/sys/kernel/osrelease 2>/dev/null; then
                wsl_type="wsl1"
            else
                wsl_type="wsl2"
            fi
        fi
        platform_utils_log "DEBUG" "WSL detected via /proc/version: $wsl_type"
    
    # PowerShell相互運用による検出
    elif command -v powershell.exe >/dev/null 2>&1; then
        wsl_type="wsl_compatible"
        platform_utils_log "DEBUG" "WSL-compatible environment detected via powershell.exe"
    fi
    
    # 結果をキャッシュ
    echo "$wsl_type" > "$WSL_CACHE_FILE"
    echo "$wsl_type"
    
    if [[ "$wsl_type" != "not_wsl" ]]; then
        return 0
    else
        return 1
    fi
}

# === PowerShell実行ファイル検出（統一実装） ===
find_powershell() {
    # キャッシュチェック
    if [[ -f "$POWERSHELL_CACHE_FILE" ]]; then
        local cache_age=$(( $(date +%s) - $(stat -c %Y "$POWERSHELL_CACHE_FILE" 2>/dev/null || echo 0) ))
        if [[ $cache_age -lt $CACHE_DURATION ]]; then
            local cached_path=$(cat "$POWERSHELL_CACHE_FILE" 2>/dev/null)
            if [[ -n "$cached_path" ]] && command -v "$cached_path" >/dev/null 2>&1; then
                echo "$cached_path"
                return 0
            fi
        fi
    fi

    # PowerShell候補パスの優先順位
    local ps_candidates=(
        "powershell.exe"                                                    # PATH内のPowerShell
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"    # Windows PowerShell 5.x
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"                       # PowerShell Core 7+ (64bit)
        "/mnt/c/Program Files (x86)/PowerShell/7/pwsh.exe"                # PowerShell Core 7+ (32bit)
        "/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"       # Git Bash/Cygwin
        "pwsh.exe"                                                          # PowerShell Core (PATH内)
        "pwsh"                                                              # PowerShell Core (Linux native)
    )

    platform_utils_log "DEBUG" "Searching for PowerShell executable..."
    
    for ps_path in "${ps_candidates[@]}"; do
        platform_utils_log "DEBUG" "Trying: $ps_path"
        
        # commandで検索
        if command -v "$ps_path" >/dev/null 2>&1; then
            echo "$ps_path" > "$POWERSHELL_CACHE_FILE"
            platform_utils_log "DEBUG" "Found PowerShell via command: $ps_path"
            echo "$ps_path"
            return 0
        fi
        
        # 直接ファイル確認
        if [[ -f "$ps_path" && -x "$ps_path" ]]; then
            echo "$ps_path" > "$POWERSHELL_CACHE_FILE"
            platform_utils_log "DEBUG" "Found PowerShell via file: $ps_path"
            echo "$ps_path"
            return 0
        fi
    done

    platform_utils_log "WARN" "PowerShell executable not found"
    return 1
}

# === Windows音声システム検証（統一実装） ===
check_windows_speech() {
    local powershell_path
    powershell_path=$(find_powershell) || {
        platform_utils_log "ERROR" "PowerShell not found for speech check"
        return 1
    }
    
    platform_utils_log "DEBUG" "Checking Windows speech system via: $powershell_path"
    
    # Windows音声合成エンジンの可用性テスト
    local speech_test_script='
    try {
        Add-Type -AssemblyName System.Speech
        $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
        $voices = $synth.GetInstalledVoices()
        if ($voices.Count -gt 0) {
            Write-Host "SPEECH_AVAILABLE"
            $synth.Dispose()
            exit 0
        } else {
            Write-Host "SPEECH_NO_VOICES"
            $synth.Dispose()
            exit 1
        }
    } catch {
        Write-Host "SPEECH_ERROR: $($_.Exception.Message)"
        exit 1
    }'
    
    local result
    if result=$("$powershell_path" -NoProfile -Command "$speech_test_script" 2>/dev/null); then
        platform_utils_log "DEBUG" "Speech system check result: $result"
        echo "$result"
        [[ "$result" == "SPEECH_AVAILABLE" ]]
    else
        platform_utils_log "WARN" "Speech system check failed"
        echo "SPEECH_UNAVAILABLE"
        return 1
    fi
}

# === Windows音声デバイス取得（統一実装） ===
get_windows_audio_devices() {
    local powershell_path
    powershell_path=$(find_powershell) || {
        platform_utils_log "ERROR" "PowerShell not found for audio device enumeration"
        return 1
    }
    
    local device_script='
    try {
        Add-Type -TypeDefinition @"
        using System;
        using System.Runtime.InteropServices;
        public class AudioDevices {
            [DllImport("winmm.dll")]
            public static extern uint waveOutGetNumDevs();
            
            [DllImport("winmm.dll")]
            public static extern uint waveOutGetDevCaps(uint deviceId, out WAVEOUTCAPS caps, uint size);
            
            [StructLayout(LayoutKind.Sequential)]
            public struct WAVEOUTCAPS {
                public ushort wMid;
                public ushort wPid;
                public uint vDriverVersion;
                [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
                public string szPname;
                public uint dwFormats;
                public ushort wChannels;
                public ushort wReserved1;
                public uint dwSupport;
            }
        }
"@
        
        $deviceCount = [AudioDevices]::waveOutGetNumDevs()
        Write-Host "DEVICE_COUNT:$deviceCount"
        
        for ($i = 0; $i -lt $deviceCount; $i++) {
            $caps = New-Object AudioDevices+WAVEOUTCAPS
            $result = [AudioDevices]::waveOutGetDevCaps($i, [ref]$caps, [System.Runtime.InteropServices.Marshal]::SizeOf($caps))
            if ($result -eq 0) {
                Write-Host "DEVICE:$i:$($caps.szPname)"
            }
        }
    } catch {
        Write-Host "DEVICE_ERROR: $($_.Exception.Message)"
    }'
    
    "$powershell_path" -NoProfile -Command "$device_script" 2>/dev/null || {
        platform_utils_log "WARN" "Audio device enumeration failed"
        return 1
    }
}

# === プラットフォーム情報取得（統一実装） ===
get_platform_info() {
    local platform_data=()
    
    # OS基本情報
    platform_data+=("OS:$(uname -s)")
    platform_data+=("KERNEL:$(uname -r)")
    platform_data+=("ARCH:$(uname -m)")
    
    # WSL情報
    local wsl_type
    if wsl_type=$(detect_wsl_environment 2>/dev/null); then
        platform_data+=("WSL:$wsl_type")
        if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
            platform_data+=("WSL_DISTRO:$WSL_DISTRO_NAME")
        fi
    fi
    
    # PowerShell情報
    local ps_path
    if ps_path=$(find_powershell 2>/dev/null); then
        platform_data+=("POWERSHELL:$ps_path")
        
        # PowerShellバージョン
        local ps_version
        if ps_version=$("$ps_path" -NoProfile -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null); then
            platform_data+=("POWERSHELL_VERSION:$ps_version")
        fi
    fi
    
    # 音声システム情報
    local speech_status
    if speech_status=$(check_windows_speech 2>/dev/null); then
        platform_data+=("SPEECH:$speech_status")
    fi
    
    # 出力
    for info in "${platform_data[@]}"; do
        echo "$info"
    done
}

# === キャッシュクリア ===
clear_platform_cache() {
    rm -f "$WSL_CACHE_FILE" "$POWERSHELL_CACHE_FILE" 2>/dev/null
    platform_utils_log "INFO" "Platform detection cache cleared"
}

# === ユーティリティ関数 ===
is_wsl() {
    local wsl_type
    wsl_type=$(detect_wsl_environment 2>/dev/null) && [[ "$wsl_type" != "not_wsl" ]]
}

has_powershell() {
    find_powershell >/dev/null 2>&1
}

has_windows_speech() {
    local speech_status
    speech_status=$(check_windows_speech 2>/dev/null) && [[ "$speech_status" == "SPEECH_AVAILABLE" ]]
}

# === 診断関数 ===
diagnose_platform() {
    echo "=== Platform Utils Diagnostics ==="
    echo "Version: $PLATFORM_UTILS_VERSION"
    echo
    
    echo "Environment Detection:"
    if is_wsl; then
        echo "  WSL: $(detect_wsl_environment)"
    else
        echo "  WSL: Not detected"
    fi
    
    echo
    echo "PowerShell Detection:"
    if has_powershell; then
        local ps_path=$(find_powershell)
        echo "  Path: $ps_path"
        
        # バージョン確認
        local ps_version=$("$ps_path" -NoProfile -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null || echo "Unknown")
        echo "  Version: $ps_version"
    else
        echo "  PowerShell: Not found"
    fi
    
    echo
    echo "Windows Speech System:"
    if has_windows_speech; then
        echo "  Status: Available"
        local speech_details=$(check_windows_speech)
        echo "  Details: $speech_details"
    else
        echo "  Status: Unavailable"
    fi
    
    echo
    echo "Full Platform Information:"
    get_platform_info | sed 's/^/  /'
    
    echo
    echo "Cache Files:"
    echo "  WSL Cache: ${WSL_CACHE_FILE}"
    echo "  PowerShell Cache: ${POWERSHELL_CACHE_FILE}"
    
    echo
    echo "=== End Diagnostics ==="
}

# === メイン実行部 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-diagnose}" in
        "detect-wsl")
            detect_wsl_environment
            ;;
        "find-powershell")
            find_powershell
            ;;
        "check-speech")
            check_windows_speech
            ;;
        "audio-devices")
            get_windows_audio_devices
            ;;
        "platform-info")
            get_platform_info
            ;;
        "is-wsl")
            is_wsl && echo "true" || echo "false"
            ;;
        "has-powershell")
            has_powershell && echo "true" || echo "false"
            ;;
        "has-speech")
            has_windows_speech && echo "true" || echo "false"
            ;;
        "clear-cache")
            clear_platform_cache
            ;;
        "diagnose")
            diagnose_platform
            ;;
        *)
            echo "Usage: $0 {detect-wsl|find-powershell|check-speech|audio-devices|platform-info|is-wsl|has-powershell|has-speech|clear-cache|diagnose}"
            exit 1
            ;;
    esac
fi