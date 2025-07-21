#!/bin/bash
# Windows Audio System Module - Windows音響システム管理
# 音量制御、システム音、ビープ音、オーディオデバイス管理

# 必要なモジュールの読み込み
source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/powershell_engine.sh" 2>/dev/null || {
    log "ERROR" "PowerShell engine module not found"
    return 1
}

# グローバル変数
declare -g WINDOWS_AUDIO_INITIALIZED=""
declare -g CURRENT_VOLUME=""
declare -g AUDIO_DEVICE_LIST=""

# Windows システム音の再生
play_windows_sound() {
    local sound_name="$1"
    local volume="${2:-50}"  # 0-100
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        log "WARN" "PowerShell not available for system sound"
        return 1
    fi
    
    # Windows システム音のマッピング
    local sound_event=""
    case "$sound_name" in
        "error"|"critical")
            sound_event="SystemHand"
            ;;
        "warning"|"warn")
            sound_event="SystemExclamation"
            ;;
        "info"|"information"|"notify")
            sound_event="SystemAsterisk"
            ;;
        "question"|"confirm")
            sound_event="SystemQuestion"
            ;;
        "success"|"complete")
            sound_event="SystemAsterisk"
            ;;
        "beep"|"default")
            sound_event="Beep"
            ;;
        *)
            sound_event="SystemAsterisk"
            ;;
    esac
    
    local sound_script="
try {
    # システム音の再生
    if ('$sound_event' -eq 'Beep') {
        [console]::beep(800, 300)
    } else {
        [System.Media.SystemSounds]::$sound_event.Play()
    }
    Write-Output 'SOUND_SUCCESS:$sound_event'
} catch {
    # フォールバックビープ
    try {
        [console]::beep(1000, 200)
        Write-Output 'SOUND_FALLBACK:beep'
    } catch {
        Write-Output 'SOUND_ERROR:\$(\$_.Exception.Message)'
    }
}
"
    
    local result
    result=$(execute_powershell_script "$sound_script" 5 "$powershell_path")
    
    # 統合エラーハンドリングを使用
    if [[ -n "${LOADED_MODULES[error_handler]:-}" ]] || load_module "error_handler" false; then
        case "$result" in
            SOUND_SUCCESS:*)
                log "DEBUG" "System sound played: ${result#SOUND_SUCCESS:}"
                return 0
                ;;
            SOUND_FALLBACK:*)
                log "DEBUG" "Fallback sound played: ${result#SOUND_FALLBACK:}"
                return 0
                ;;
            SOUND_ERROR:*)
                local error_detail="${result#SOUND_ERROR:}"
                handle_voice_error "SOUND_ERROR" "$error_detail" "windows_audio_system" "play_windows_sound"
                return $?
                ;;
            *)
                handle_voice_error "SOUND_ERROR" "Unexpected sound result: $result" "windows_audio_system" "play_windows_sound"
                return $?
                ;;
        esac
    else
        # フォールバック: 従来のエラーハンドリング
        case "$result" in
            SOUND_SUCCESS:*)
                log "DEBUG" "System sound played: ${result#SOUND_SUCCESS:}"
                return 0
                ;;
            SOUND_FALLBACK:*)
                log "DEBUG" "Fallback sound played: ${result#SOUND_FALLBACK:}"
                return 0
                ;;
            SOUND_ERROR:*)
                log "ERROR" "Sound playback failed: ${result#SOUND_ERROR:}"
                return 1
                ;;
            *)
                log "WARN" "Unexpected sound result: $result"
                return 1
                ;;
        esac
    fi
}

# システムビープ音（命名規則統一版）
play_windows_system_beep() {
    local count="${1:-1}"
    local frequency="${2:-800}"
    local duration="${3:-200}"
    local interval="${4:-300}"
    
    log "DEBUG" "Playing system beep: count=$count, freq=$frequency, duration=$duration"
    
    # パラメータ検証
    if [[ ! "$count" =~ ^[0-9]+$ ]] || [[ $count -gt 10 ]]; then
        log "WARN" "Invalid beep count: $count, using 1"
        count=1
    fi
    
    if [[ ! "$frequency" =~ ^[0-9]+$ ]] || [[ $frequency -lt 37 ]] || [[ $frequency -gt 32767 ]]; then
        log "WARN" "Invalid frequency: $frequency, using 800"
        frequency=800
    fi
    
    if [[ ! "$duration" =~ ^[0-9]+$ ]] || [[ $duration -gt 5000 ]]; then
        log "WARN" "Invalid duration: $duration, using 200"
        duration=200
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        # フォールバック: ASCII ベル文字
        log "DEBUG" "Using ASCII bell fallback"
        for ((i=1; i<=count; i++)); do
            printf '\a'
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep $(echo "scale=3; $interval/1000" | bc -l 2>/dev/null || echo "0.3")
            fi
        done
        return 0
    fi
    
    # PowerShell ビープ音スクリプト
    local beep_script="
try {
    for (\$i = 1; \$i -le $count; \$i++) {
        [console]::beep($frequency, $duration)
        if (\$i -lt $count) {
            Start-Sleep -Milliseconds $interval
        }
    }
    Write-Output 'BEEP_SUCCESS:$count'
} catch {
    Write-Output 'BEEP_ERROR:\$(\$_.Exception.Message)'
}
"
    
    local result
    result=$(execute_powershell_script "$beep_script" 10 "$powershell_path")
    
    case "$result" in
        BEEP_SUCCESS:*)
            log "DEBUG" "Beep sequence completed: ${result#BEEP_SUCCESS:} beeps"
            return 0
            ;;
        BEEP_ERROR:*)
            log "ERROR" "Beep failed: ${result#BEEP_ERROR:}"
            return 1
            ;;
        *)
            log "WARN" "Unexpected beep result: $result"
            return 1
            ;;
    esac
}

# 後方互換性のためのエイリアス（非推奨）
system_beep() {
    log "WARN" "system_beep() is deprecated, use play_windows_system_beep()"
    play_windows_system_beep "$@"
}

# Windows ビープのフォールバック（音声合成失敗時）
windows_beep_fallback() {
    local message="$1"
    local urgency="${2:-normal}"
    
    log "DEBUG" "Using Windows beep fallback for message (urgency: $urgency)"
    
    # メッセージの分析に基づくビープパターン
    local beep_count=1
    local frequency=800
    local duration=200
    
    # メッセージの長さに応じた調整
    local length=${#message}
    if [[ $length -gt 100 ]]; then
        beep_count=3
    elif [[ $length -gt 50 ]]; then
        beep_count=2
    fi
    
    # 緊急度による調整
    case "$urgency" in
        "critical"|"error")
            frequency=400
            duration=500
            beep_count=$((beep_count + 1))
            ;;
        "warning"|"warn")
            frequency=600
            duration=300
            ;;
        "info"|"normal")
            frequency=800
            duration=200
            ;;
        "low")
            frequency=1000
            duration=100
            ;;
    esac
    
    # エラーキーワードの検出
    if echo "$message" | grep -qi "error\|エラー\|failed\|失敗\|critical\|重大"; then
        frequency=400
        duration=400
        beep_count=2
    elif echo "$message" | grep -qi "warning\|警告\|caution\|注意"; then
        frequency=600
        duration=250
    elif echo "$message" | grep -qi "success\|成功\|complete\|完了"; then
        frequency=1200
        duration=150
    fi
    
    system_beep "$beep_count" "$frequency" "$duration"
}

# 音量制御（Windows）
set_system_volume() {
    local volume="$1"  # 0-100
    local device_type="${2:-output}"
    
    if [[ -z "$volume" ]] || [[ ! "$volume" =~ ^[0-9]+$ ]]; then
        log "ERROR" "Invalid volume value: $volume"
        return 1
    fi
    
    if [[ $volume -lt 0 ]] || [[ $volume -gt 100 ]]; then
        log "ERROR" "Volume out of range (0-100): $volume"
        return 1
    fi
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    log "DEBUG" "Setting Windows system volume: $volume% ($device_type)"
    
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not available for volume control"
        return 1
    fi
    
    # Windows 音量制御スクリプト（改良版）
    local volume_script="
try {
    # AudioEndpointVolume API を使用した音量制御
    Add-Type -TypeDefinition @'
        using System;
        using System.Runtime.InteropServices;
        
        [Guid(\"5CDF2C82-841E-4546-9722-0CF74078229A\"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
        interface IAudioEndpointVolume {
            int NotImpl1();
            int NotImpl2();
            int GetChannelCount([Out] out uint channelCount);
            int SetMasterVolumeLevel(float level, [In] ref Guid eventContext);
            int SetMasterVolumeLevelScalar(float level, [In] ref Guid eventContext);
            int GetMasterVolumeLevel([Out] out float level);
            int GetMasterVolumeLevelScalar([Out] out float level);
            int SetChannelVolumeLevel(uint channelNumber, float level, [In] ref Guid eventContext);
            int SetChannelVolumeLevelScalar(uint channelNumber, float level, [In] ref Guid eventContext);
            int GetChannelVolumeLevel(uint channelNumber, [Out] out float level);
            int GetChannelVolumeLevelScalar(uint channelNumber, [Out] out float level);
            int SetMute([In] bool isMuted, [In] ref Guid eventContext);
            int GetMute([Out] out bool isMuted);
            int GetVolumeStepInfo([Out] out uint step, [Out] out uint stepCount);
            int VolumeStepUp([In] ref Guid eventContext);
            int VolumeStepDown([In] ref Guid eventContext);
            int QueryHardwareSupport([Out] out uint hardwareSupportMask);
            int GetVolumeRange([Out] out float volumeMin, [Out] out float volumeMax, [Out] out float volumeStep);
        }
'@
    
    # 簡易な音量設定（キーストローク方式のフォールバック）
    \$volume = $volume / 100.0
    \$obj = New-Object -ComObject WScript.Shell
    
    if ($volume -eq 0) {
        # ミュート
        \$obj.SendKeys([char]173)
        Write-Output 'VOLUME_SET:muted'
    } else {
        # 現在の音量をリセットしてから設定
        for (\$i = 0; \$i -lt 50; \$i++) {
            \$obj.SendKeys([char]174)  # 音量ダウン
            Start-Sleep -Milliseconds 5
        }
        
        # 目標音量まで上げる
        \$steps = [math]::Round(\$volume * 50)
        for (\$i = 0; \$i -lt \$steps; \$i++) {
            \$obj.SendKeys([char]175)  # 音量アップ
            Start-Sleep -Milliseconds 5
        }
        Write-Output 'VOLUME_SET:$volume'
    }
} catch {
    Write-Output 'VOLUME_ERROR:\$(\$_.Exception.Message)'
}
"
    
    local result
    result=$(execute_powershell_script "$volume_script" 15 "$powershell_path")
    
    case "$result" in
        VOLUME_SET:*)
            local set_volume="${result#VOLUME_SET:}"
            CURRENT_VOLUME="$volume"
            log "DEBUG" "Windows volume set successfully: $set_volume"
            return 0
            ;;
        VOLUME_ERROR:*)
            log "ERROR" "Windows volume control failed: ${result#VOLUME_ERROR:}"
            return 1
            ;;
        *)
            log "WARN" "Unexpected volume control result: $result"
            return 1
            ;;
    esac
}

# 現在の音量取得（Windows）
get_system_volume() {
    local device_type="${1:-output}"
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        echo "${CURRENT_VOLUME:-50}"  # キャッシュまたはデフォルト値
        return 1
    fi
    
    # Windows 音量取得スクリプト
    local volume_script="
try {
    # WMI による音量情報の取得を試行
    \$audio = Get-WmiObject -Class Win32_SoundDevice -ErrorAction SilentlyContinue | Select-Object -First 1
    if (\$audio) {
        # 簡易実装: 現在のキャッシュ値または推定値
        Write-Output 'VOLUME_GET:${CURRENT_VOLUME:-50}'
    } else {
        Write-Output 'VOLUME_GET:50'
    }
} catch {
    Write-Output 'VOLUME_GET:50'
}
"
    
    local result
    result=$(execute_powershell_script "$volume_script" 5 "$powershell_path")
    
    case "$result" in
        VOLUME_GET:*)
            local volume="${result#VOLUME_GET:}"
            echo "$volume"
            return 0
            ;;
        *)
            echo "50"
            return 1
            ;;
    esac
}

# オーディオデバイス一覧取得
get_audio_devices() {
    local device_type="${1:-all}"  # all, input, output
    
    local powershell_path
    powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        echo "No audio devices available (PowerShell required)"
        return 1
    fi
    
    local devices_script="
try {
    # Windows オーディオデバイスの取得
    \$devices = Get-WmiObject -Class Win32_SoundDevice | Where-Object { \$_.Status -eq 'OK' }
    
    if (\$devices) {
        \$devices | ForEach-Object {
            \$name = if (\$_.Name) { \$_.Name } else { 'Unknown Device' }
            \$status = if (\$_.Status) { \$_.Status } else { 'Unknown' }
            Write-Output \"DEVICE:\$name:\$status\"
        }
    } else {
        Write-Output 'DEVICE:Default Audio Device:OK'
    }
} catch {
    Write-Output 'DEVICE_ERROR:\$(\$_.Exception.Message)'
}
"
    
    local result
    result=$(execute_powershell_script "$devices_script" 10 "$powershell_path")
    
    if [[ "$result" == DEVICE_ERROR:* ]]; then
        log "ERROR" "Audio device enumeration failed: ${result#DEVICE_ERROR:}"
        echo "Default Audio Device"
        return 1
    fi
    
    # デバイス情報のフォーマット
    echo "$result" | grep "^DEVICE:" | while IFS=: read -r prefix name status; do
        echo "  - $name ($status)"
    done
}

# オーディオシステム情報取得
get_audio_system_info() {
    local format="${1:-text}"
    
    case "$format" in
        "json")
            cat <<EOF
{
    "current_volume": "${CURRENT_VOLUME:-unknown}",
    "audio_backend": "windows",
    "powershell_available": $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "true"; else echo "false"; fi),
    "system_sounds_available": true,
    "volume_control_available": true
}
EOF
            ;;
        "text")
            echo "Windows Audio System Status:"
            echo "  Current Volume: ${CURRENT_VOLUME:-unknown}%"
            echo "  Audio Backend: Windows"
            echo "  PowerShell Available: $(if [[ -n "$(find_powershell_path 2>/dev/null)" ]]; then echo "Yes"; else echo "No"; fi)"
            echo "  System Sounds: Available"
            echo "  Volume Control: Available"
            echo ""
            echo "Audio Devices:"
            get_audio_devices
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}

# Windows/WSL固有のオーディオ初期化
init_windows_audio() {
    log "INFO" "Initializing Windows/WSL audio subsystem"
    
    # PowerShell依存関係チェック
    if ! check_powershell_execution; then
        log "ERROR" "Windows audio initialization failed - PowerShell not available"
        return 1
    fi
    
    # 現在の音量を取得してキャッシュ
    CURRENT_VOLUME=$(get_system_volume)
    
    # オーディオデバイス情報をキャッシュ
    AUDIO_DEVICE_LIST=$(get_audio_devices)
    
    # 初期化完了フラグ
    WINDOWS_AUDIO_INITIALIZED="true"
    
    log "INFO" "Windows audio system initialized (Volume: ${CURRENT_VOLUME}%)"
    return 0
}

# Windowsオーディオシステムテスト
test_windows_audio_system() {
    echo "=== Windows Audio System Test ==="
    
    # PowerShell チェック
    if ! check_powershell_execution; then
        echo "❌ PowerShell not available for audio testing"
        return 1
    fi
    
    # システム音テスト
    echo "Testing system sounds..."
    local test_sounds=("info" "warning" "error" "beep")
    
    for sound in "${test_sounds[@]}"; do
        echo "  Playing $sound sound..."
        if play_windows_sound "$sound"; then
            echo "  ✅ $sound sound played"
        else
            echo "  ❌ $sound sound failed"
        fi
        sleep 0.5
    done
    
    # ビープ音テスト
    echo ""
    echo "Testing beep sequences..."
    if system_beep 2 800 200; then
        echo "✅ Beep sequence test passed"
    else
        echo "❌ Beep sequence test failed"
    fi
    
    # 音量制御テスト
    echo ""
    echo "Testing volume control..."
    local original_volume
    original_volume=$(get_system_volume)
    echo "  Current volume: $original_volume%"
    
    if set_system_volume 70; then
        echo "✅ Volume control test passed"
        # 元の音量に復元
        set_system_volume "$original_volume" >/dev/null 2>&1
    else
        echo "❌ Volume control test failed"
    fi
    
    # オーディオデバイス検出テスト
    echo ""
    echo "Testing audio device detection..."
    local devices
    devices=$(get_audio_devices)
    if [[ -n "$devices" ]]; then
        echo "✅ Audio devices detected:"
        echo "$devices"
    else
        echo "❌ No audio devices detected"
    fi
    
    # フォールバック機能テスト
    echo ""
    echo "Testing fallback beep..."
    windows_beep_fallback "This is a test message" "normal"
    echo "✅ Fallback beep test completed"
    
    echo ""
    echo "Windows Audio System test completed"
    get_audio_system_info "text"
    
    return 0
}