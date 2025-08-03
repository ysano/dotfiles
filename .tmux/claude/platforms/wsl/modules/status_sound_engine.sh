#!/bin/bash
# Status Sound Engine Module
# Claude Codeステータス別音響効果システム

set -euo pipefail

# 依存関係読み込み
source "$(dirname "${BASH_SOURCE[0]}")/powershell_interface.sh"

# === ステータス別効果音設定（Claude Code公式3状態） ===
declare -A STATUS_SOUND_CONFIGS=(
    ["⚡"]="beep_pattern:alert|frequency:800,800,600|duration:80,80,100|interval:20|wav:Windows Exclamation.wav"
    ["⌛"]="beep_pattern:waiting|frequency:659,880,1175|duration:100,150,100|interval:50|wav:Windows Notify System Generic.wav"
    ["✅"]="beep_pattern:success|frequency:523,659,783,1046|duration:80,80,80,120|interval:30|wav:Windows Ding.wav"
)

# === ステータス音声再生 ===
play_status_sound() {
    local status_icon="$1"
    local method="${2:-wav}"  # wav, beep, messagebeep
    
    log "DEBUG" "Playing status sound for: $status_icon (method: $method)"
    
    # 設定取得
    local sound_config="${STATUS_SOUND_CONFIGS[$status_icon]:-}"
    if [[ -z "$sound_config" ]]; then
        log "WARN" "No sound configuration for status: $status_icon"
        return 1
    fi
    
    case "$method" in
        "wav")
            _play_status_wav "$status_icon" "$sound_config"
            ;;
        "beep")
            _play_status_beep "$status_icon" "$sound_config"
            ;;
        "messagebeep")
            _play_status_messagebeep "$status_icon"
            ;;
        *)
            log "ERROR" "Unknown sound method: $method"
            return 1
            ;;
    esac
}

# === WAVファイル方式 ===
_play_status_wav() {
    local status_icon="$1"
    local sound_config="$2"
    
    # WAVファイル名抽出
    local wav_file
    wav_file=$(echo "$sound_config" | grep -o "wav:[^|]*" | cut -d: -f2)
    
    if [[ -z "$wav_file" ]]; then
        log "ERROR" "No WAV file specified for status: $status_icon"
        return 1
    fi
    
    # Windows音声ファイルパス
    local full_path="C:\\Windows\\Media\\$wav_file"
    
    if execute_powershell_wav "$full_path" "true"; then
        log "INFO" "Status sound completed: $status_icon (WAV)"
        return 0
    else
        log "ERROR" "WAV playback failed for: $status_icon"
        return 1
    fi
}

# === Beep方式 ===
_play_status_beep() {
    local status_icon="$1"
    local sound_config="$2"
    
    # 設定解析
    local frequency_list duration_list interval
    frequency_list=$(echo "$sound_config" | grep -o "frequency:[^|]*" | cut -d: -f2)
    duration_list=$(echo "$sound_config" | grep -o "duration:[^|]*" | cut -d: -f2)
    interval=$(echo "$sound_config" | grep -o "interval:[^|]*" | cut -d: -f2)
    
    # デフォルト値
    interval="${interval:-50}"
    
    # 配列作成
    IFS=',' read -ra frequencies <<< "$frequency_list"
    IFS=',' read -ra durations <<< "$duration_list"
    
    # Beep実行
    for i in "${!frequencies[@]}"; do
        local freq="${frequencies[i]}"
        local dur="${durations[i]:-100}"
        
        if ! execute_powershell_beep "$freq" "$dur"; then
            log "WARN" "Beep failed: ${freq}Hz ${dur}ms"
        fi
        
        # インターバル（最後以外）
        if [[ $((i + 1)) -lt ${#frequencies[@]} ]]; then
            sleep "0.$(printf "%03d" "$interval")"
        fi
    done
    
    log "INFO" "Status sound completed: $status_icon (Beep)"
    return 0
}

# === MessageBeep方式 ===
_play_status_messagebeep() {
    local status_icon="$1"
    
    local beep_type
    case "$status_icon" in
        "⚡") beep_type="0x30" ;;  # MB_ICONEXCLAMATION
        "⌛") beep_type="0x40" ;;  # MB_ICONASTERISK
        "✅") beep_type="0x10" ;;  # MB_ICONHAND
        *) beep_type="0" ;;       # Default
    esac
    
    if execute_powershell_messagebeep "$beep_type"; then
        log "INFO" "Status sound completed: $status_icon (MessageBeep)"
        return 0
    else
        log "ERROR" "MessageBeep failed for: $status_icon"
        return 1
    fi
}

# === フォールバック音声再生 ===
play_status_sound_with_fallback() {
    local status_icon="$1"
    
    # 優先順序: WAV -> MessageBeep -> Beep
    local methods=("wav" "messagebeep" "beep")
    
    for method in "${methods[@]}"; do
        if play_status_sound "$status_icon" "$method"; then
            log "DEBUG" "Status sound succeeded with method: $method"
            return 0
        fi
        log "DEBUG" "Status sound method failed: $method"
    done
    
    log "ERROR" "All status sound methods failed for: $status_icon"
    return 1
}

# === 利用可能ステータス一覧 ===
list_available_statuses() {
    echo "Available status icons:"
    for status in "${!STATUS_SOUND_CONFIGS[@]}"; do
        echo "  $status"
    done
}

# === 初期化チェック ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "Status Sound Engine Module"
    echo "Functions: play_status_sound, play_status_sound_with_fallback, list_available_statuses"
    list_available_statuses
fi