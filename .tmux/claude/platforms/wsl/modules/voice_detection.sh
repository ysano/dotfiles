#!/bin/bash
# Voice Detection Module
# Windows音声合成エンジンの検出と選択

set -euo pipefail

# 依存関係読み込み
source "$(dirname "${BASH_SOURCE[0]}")/powershell_interface.sh"

# === 日本語音声優先順位 ===
declare -a JAPANESE_VOICES=(
    "Microsoft Haruka Desktop"
    "Microsoft Ayumi Desktop"
    "Microsoft Ichiro Desktop"
    "Microsoft Haruka"
    "Microsoft Ayumi" 
    "Microsoft Ichiro"
)

# === 英語音声フォールバック ===
declare -a ENGLISH_VOICES=(
    "Microsoft Zira Desktop"
    "Microsoft David Desktop"
    "Microsoft Mark Desktop"
    "Microsoft Zira"
    "Microsoft David"
    "Microsoft Mark"
)

# === 利用可能音声検出 ===
detect_available_voices() {
    local powershell_path
    powershell_path=$(find_powershell) || return 1
    
    local ps_script='
    try {
        Add-Type -AssemblyName System.Speech;
        $synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
        $voices = $synth.GetInstalledVoices();
        foreach ($voice in $voices) {
            if ($voice.Enabled) {
                Write-Output $voice.VoiceInfo.Name;
            }
        }
    } catch {
        Write-Error "Failed to detect voices: $_"
    }'
    
    "$powershell_path" -Command "$ps_script" 2>/dev/null | tr -d '\r'
}

# === 日本語音声検出 ===
detect_japanese_voices() {
    detect_available_voices | grep -i "haruka\|ayumi\|ichiro" || true
}

# === 最適日本語音声選択 ===
select_best_japanese_voice() {
    local available_voices
    available_voices=$(detect_japanese_voices)
    
    if [[ -z "$available_voices" ]]; then
        log "WARN" "No Japanese voices available"
        return 1
    fi
    
    # 優先順位に従って選択
    for priority_voice in "${JAPANESE_VOICES[@]}"; do
        if echo "$available_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected Japanese voice: $priority_voice"
            return 0
        fi
    done
    
    # フォールバック: 最初の利用可能な日本語音声
    local first_voice
    first_voice=$(echo "$available_voices" | head -1)
    echo "$first_voice"
    log "DEBUG" "Using first available Japanese voice: $first_voice"
    return 0
}

# === フォールバック英語音声選択 ===
select_fallback_english_voice() {
    local available_voices
    available_voices=$(detect_available_voices)
    
    # 英語音声を優先順位で選択
    for priority_voice in "${ENGLISH_VOICES[@]}"; do
        if echo "$available_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected English voice: $priority_voice"
            return 0
        fi
    done
    
    # デフォルト音声
    local default_voice
    default_voice=$(echo "$available_voices" | head -1)
    if [[ -n "$default_voice" ]]; then
        echo "$default_voice"
        log "DEBUG" "Using default voice: $default_voice"
        return 0
    fi
    
    log "ERROR" "No voices available"
    return 1
}

# === 最適音声自動選択 ===
auto_select_voice() {
    local language="${1:-japanese}"
    
    case "$language" in
        "japanese"|"ja")
            if select_best_japanese_voice; then
                return 0
            else
                log "INFO" "Japanese voices not available, falling back to English"
                select_fallback_english_voice
            fi
            ;;
        "english"|"en")
            select_fallback_english_voice
            ;;
        *)
            log "ERROR" "Unsupported language: $language"
            return 1
            ;;
    esac
}

# === 音声テスト ===
test_voice() {
    local voice_name="$1"
    local test_text="${2:-テスト音声です}"
    
    if execute_powershell_speech "$test_text" "$voice_name" 0 100; then
        log "INFO" "Voice test successful: $voice_name"
        return 0
    else
        log "ERROR" "Voice test failed: $voice_name"
        return 1
    fi
}

# === 音声情報表示 ===
show_voice_info() {
    echo "=== Available Voices ==="
    detect_available_voices
    echo
    
    echo "=== Japanese Voices ==="
    detect_japanese_voices
    echo
    
    echo "=== Recommended Voice ==="
    auto_select_voice japanese
    echo
}

# === 初期化チェック ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "Voice Detection Module"
    echo "Functions: detect_available_voices, select_best_japanese_voice, auto_select_voice, test_voice"
    show_voice_info
fi