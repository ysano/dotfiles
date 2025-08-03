#!/bin/bash
# WSL Environment Detection Module
# WSL環境検出とWindows統合機能

set -euo pipefail

# === WSL環境検出 ===
detect_wsl_environment() {
    local wsl_version=""
    
    # WSL_DISTRO_NAMEが設定されているかチェック
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        wsl_version="2"
        log "DEBUG" "WSL detected via WSL_DISTRO_NAME: $WSL_DISTRO_NAME"
        echo "wsl2"
        return 0
    fi
    
    # /proc/versionでMicrosoftが含まれるかチェック
    if grep -qi microsoft /proc/version 2>/dev/null; then
        if grep -qi "microsoft-standard-wsl2" /proc/version 2>/dev/null; then
            wsl_version="2"
        else
            wsl_version="1"
        fi
        log "DEBUG" "WSL detected via /proc/version: WSL$wsl_version"
        echo "wsl$wsl_version"
        return 0
    fi
    
    # Windows相互運用確認
    if command -v powershell.exe >/dev/null 2>&1; then
        log "DEBUG" "WSL detected via powershell.exe availability"
        echo "wsl"
        return 0
    fi
    
    # WSL環境ではない
    log "DEBUG" "Not running in WSL environment"
    echo "none"
    return 1
}

# === PowerShell実行ファイル検索 ===
find_powershell() {
    local powershell_paths=(
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
        "/mnt/c/Windows/sysnative/WindowsPowerShell/v1.0/powershell.exe"
        "$(command -v powershell.exe 2>/dev/null || true)"
        "$(command -v pwsh.exe 2>/dev/null || true)"
    )
    
    for ps_path in "${powershell_paths[@]}"; do
        if [[ -n "$ps_path" ]] && [[ -x "$ps_path" ]]; then
            log "DEBUG" "PowerShell found: $ps_path"
            echo "$ps_path"
            return 0
        fi
    done
    
    log "ERROR" "PowerShell executable not found"
    return 1
}

# === Windows音声システム確認 ===
check_windows_speech() {
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    # Windows Speech API利用可能性確認
    local speech_test='
    try {
        Add-Type -AssemblyName System.Speech;
        $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
        $synth.GetInstalledVoices() | Select-Object -First 1 | Out-Null;
        Write-Output "available";
    } catch {
        Write-Output "unavailable";
    }'
    
    local result
    result=$("$powershell_path" -Command "$speech_test" 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" == "available" ]]; then
        log "DEBUG" "Windows Speech API available"
        return 0
    else
        log "WARN" "Windows Speech API unavailable"
        return 1
    fi
}

# === 初期化チェック ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "WSL Environment Detection Module"
    echo "Functions: detect_wsl_environment, find_powershell, check_windows_speech"
fi