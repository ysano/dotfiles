#!/bin/bash
# PowerShell Interface Module
# PowerShell実行と音声APIインターフェース

set -euo pipefail

# 依存関係読み込み
source "$(dirname "${BASH_SOURCE[0]}")/wsl_environment.sh"

# === PowerShell音声合成実行 ===
execute_powershell_speech() {
    local text="$1"
    local voice="${2:-}"
    local rate="${3:-0}"
    local volume="${4:-100}"
    
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    # PowerShellスクリプト生成
    local ps_script="
    try {
        Add-Type -AssemblyName System.Speech;
        \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
        
        if ('$voice' -ne '') {
            \$synth.SelectVoice('$voice');
        }
        
        \$synth.Rate = $rate;
        \$synth.Volume = $volume;
        \$synth.Speak('$text');
        
        Write-Output 'speech_success';
    } catch {
        Write-Output ('speech_error: ' + \$_.Exception.Message);
    }"
    
    local result
    result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | grep -E "(speech_success|speech_error)" | head -1)
    
    if [[ "$result" == "speech_success" ]]; then
        log "DEBUG" "PowerShell speech synthesis completed"
        return 0
    else
        log "ERROR" "PowerShell speech synthesis failed: $result"
        return 1
    fi
}

# === PowerShell音声効果実行 ===
execute_powershell_beep() {
    local frequency="$1"
    local duration="$2"
    
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    local ps_script="
    try {
        [console]::beep($frequency, $duration);
        Write-Output 'beep_success';
    } catch {
        Write-Output ('beep_error: ' + \$_.Exception.Message);
    }"
    
    local result
    result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" == "beep_success" ]]; then
        return 0
    else
        log "ERROR" "PowerShell beep failed: $result"
        return 1
    fi
}

# === PowerShell WAVファイル再生 ===
execute_powershell_wav() {
    local wav_file="$1"
    local sync="${2:-true}"
    
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    local play_method
    if [[ "$sync" == "true" ]]; then
        play_method="PlaySync"
    else
        play_method="Play"
    fi
    
    local ps_script="
    try {
        \$player = New-Object Media.SoundPlayer '$wav_file';
        \$player.$play_method();
        Write-Output 'wav_success';
    } catch {
        Write-Output ('wav_error: ' + \$_.Exception.Message);
    }"
    
    local result
    result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" == "wav_success" ]]; then
        return 0
    else
        log "ERROR" "PowerShell WAV playback failed: $result"
        return 1
    fi
}

# === MessageBeep API実行 ===
execute_powershell_messagebeep() {
    local beep_type="${1:-0}"
    
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    local ps_script="
    try {
        Add-Type -TypeDefinition '
        using System;
        using System.Runtime.InteropServices;
        public class WinAPI {
            [DllImport(\"user32.dll\")]
            public static extern bool MessageBeep(uint uType);
        }';
        
        \$result = [WinAPI]::MessageBeep($beep_type);
        Write-Output 'messagebeep_success';
    } catch {
        Write-Output ('messagebeep_error: ' + \$_.Exception.Message);
    }"
    
    local result
    result=$("$powershell_path" -Command "$ps_script" 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" == "messagebeep_success" ]]; then
        return 0
    else
        log "ERROR" "PowerShell MessageBeep failed: $result"
        return 1
    fi
}

# === 初期化チェック ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "PowerShell Interface Module"
    echo "Functions: execute_powershell_speech, execute_powershell_beep, execute_powershell_wav, execute_powershell_messagebeep"
fi