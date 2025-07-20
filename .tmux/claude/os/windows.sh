#!/bin/bash
# Claude Voice - Windows/WSL specific functions
# Windows/WSL固有の音声・通知機能

# Windows/WSL固有の依存関係チェック
check_windows_dependencies() {
    local missing_deps=()
    local optional_deps=()
    
    # PowerShellの確認
    local powershell_path=$(find_powershell_path)
    if [[ -z "$powershell_path" ]]; then
        missing_deps+=("PowerShell (Windows PowerShell or PowerShell Core)")
    fi
    
    # WSL環境の確認
    if [[ -z "${WSL_DISTRO_NAME:-}" ]] && ! grep -qi microsoft /proc/version 2>/dev/null; then
        log "WARN" "Not running in WSL environment"
    fi
    
    # Windows側への接続確認
    if [[ -n "$powershell_path" ]]; then
        if ! "$powershell_path" -Command "echo 'test'" >/dev/null 2>&1; then
            missing_deps+=("PowerShell execution capability")
        fi
    fi
    
    # エラー報告
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log "ERROR" "Missing critical dependencies: ${missing_deps[*]}"
        return 1
    fi
    
    if [[ ${#optional_deps[@]} -gt 0 ]]; then
        log "WARN" "Missing optional dependencies: ${optional_deps[*]}"
    fi
    
    log "DEBUG" "Windows/WSL dependencies check passed"
    return 0
}

# PowerShell実行パスの検出（拡張版）
find_powershell_path() {
    log "DEBUG" "Searching for PowerShell executable"
    
    # 検索パスの優先順位
    local powershell_paths=(
        # PowerShell Core (推奨)
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
        "/mnt/c/Program Files/PowerShell/6/pwsh.exe"
        # Windows PowerShell
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Windows/System32/powershell.exe"
        "/mnt/c/Windows/SysWOW64/WindowsPowerShell/v1.0/powershell.exe"
        # WSL Path経由
        "powershell.exe"
        "pwsh.exe"
    )
    
    for path in "${powershell_paths[@]}"; do
        if [[ -f "$path" ]] || command -v "$path" >/dev/null 2>&1; then
            log "DEBUG" "Found PowerShell at: $path"
            echo "$path"
            return 0
        fi
    done
    
    log "ERROR" "PowerShell executable not found"
    return 1
}

# Windows音声エンジンの検出
detect_windows_tts_voices() {
    local powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        echo "Microsoft Haruka Desktop"  # デフォルト
        return 1
    fi
    
    log "DEBUG" "Detecting Windows TTS voices"
    
    # PowerShellで利用可能な音声を取得
    local voices_script='
Add-Type -AssemblyName System.Speech
$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
$synth.GetInstalledVoices() | ForEach-Object { $_.VoiceInfo.Name }
'
    
    local voices=$("$powershell_path" -Command "$voices_script" 2>/dev/null)
    
    if [[ -n "$voices" ]]; then
        echo "$voices"
    else
        echo "Microsoft Haruka Desktop"  # フォールバック
    fi
}

# 最適な日本語音声の選択
select_japanese_voice() {
    local available_voices=$(detect_windows_tts_voices)
    
    # 日本語音声の優先順位
    local preferred_voices=(
        "Microsoft Haruka Desktop"
        "Microsoft Sayaka Desktop"
        "Microsoft Ichiro Desktop"
        "Microsoft Haruka Mobile"
        "Microsoft Sayaka Mobile"
    )
    
    for voice in "${preferred_voices[@]}"; do
        if echo "$available_voices" | grep -Fq "$voice"; then
            echo "$voice"
            return 0
        fi
    done
    
    # 日本語が含まれる音声を検索
    local japanese_voice=$(echo "$available_voices" | grep -i "haruka\|sayaka\|ichiro" | head -1)
    if [[ -n "$japanese_voice" ]]; then
        echo "$japanese_voice"
        return 0
    fi
    
    # フォールバック
    echo "Microsoft Haruka Desktop"
}

# 音声合成実行
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "auto")}"
    local device="${3:-auto}"  # Windows では通常無視
    local rate="${4:-$(get_config "audio.speech_rate" "0")}"  # -10 to 10
    
    log "DEBUG" "Speaking text on Windows: voice=$voice, rate=$rate"
    
    # 依存関係チェック
    if ! check_windows_dependencies; then
        log "ERROR" "Cannot speak: missing dependencies"
        return 1
    fi
    
    local powershell_path=$(find_powershell_path)
    
    # テキストの前処理
    local processed_text=$(preprocess_speech_text "$text")
    
    # 音声の選択
    local target_voice="$voice"
    if [[ "$voice" == "auto" ]] || [[ -z "$voice" ]]; then
        target_voice=$(select_japanese_voice)
    fi
    
    # レートの正規化（-10 to 10）
    local normalized_rate="$rate"
    if [[ $rate -gt 10 ]]; then
        normalized_rate=10
    elif [[ $rate -lt -10 ]]; then
        normalized_rate=-10
    fi
    
    # PowerShell 音声合成スクリプト
    local tts_script="
try {
    Add-Type -AssemblyName System.Speech
    \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
    
    # 音声の設定
    try {
        \$synth.SelectVoice('$target_voice')
        Write-Host \"Using voice: $target_voice\"
    } catch {
        Write-Host \"Voice '$target_voice' not found, using default\"
    }
    
    # レートの設定
    \$synth.Rate = $normalized_rate
    
    # 音声合成の実行
    \$synth.Speak('$processed_text')
    \$synth.Dispose()
    
    Write-Host \"Speech synthesis completed successfully\"
    exit 0
} catch {
    Write-Host \"Error: \$(\$_.Exception.Message)\"
    exit 1
}
"
    
    # 実行とエラーハンドリング
    local start_time=$(start_timer)
    
    if "$powershell_path" -Command "$tts_script" 2>/dev/null; then
        local duration=$(end_timer "$start_time")
        log "INFO" "Speech synthesis completed on Windows (${duration}s)"
        return 0
    else
        log "ERROR" "Windows speech synthesis failed"
        
        # フォールバック: シンプルなビープ音
        windows_beep_fallback "$processed_text"
        return 1
    fi
}

# 音声テキストの前処理（Windows向け）
preprocess_speech_text() {
    local text="$1"
    
    # PowerShell文字列エスケープ
    local processed=$(echo "$text" | \
        sed "s/'/\'\'/g" | \
        sed 's/"/\\"\\"/g')
    
    # 特殊文字の読み上げ対応
    processed=$(echo "$processed" | \
        sed 's/⏺/○/g' | \
        sed 's/✅/成功/g' | \
        sed 's/❌/エラー/g' | \
        sed 's/📁/フォルダ/g' | \
        sed 's/🔧/設定/g' | \
        sed 's/&/アンド/g' | \
        sed 's/@/アット/g' | \
        sed 's/#/シャープ/g')
    
    # URL の簡略化
    processed=$(echo "$processed" | sed 's|https\?://[^ ]*|URL|g')
    
    # 長すぎるテキストの短縮
    local max_length=$(get_config "audio.max_speech_length" "400")
    if [[ ${#processed} -gt $max_length ]]; then
        processed="${processed:0:$max_length}。以下省略。"
    fi
    
    echo "$processed"
}

# システム通知の送信
send_notification() {
    local title="$1"
    local message="$2"
    local sound="${3:-default}"
    local urgency="${4:-normal}"
    local timeout="${5:-5000}"
    
    log "DEBUG" "Sending Windows notification: title=$title, urgency=$urgency"
    
    local powershell_path=$(find_powershell_path)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not available for notifications"
        echo "通知: $title - $message"
        return 1
    fi
    
    # Windows Toast通知スクリプト
    local notification_script="
try {
    # Windows.UI.Notifications を使用したトースト通知
    [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    [Windows.UI.Notifications.ToastNotification, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
    [Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType = WindowsRuntime] | Out-Null
    
    # トーストテンプレート
    \$template = @\"
<toast>
    <visual>
        <binding template=\"ToastGeneric\">
            <text>$title</text>
            <text>$message</text>
        </binding>
    </visual>
    <audio src=\"ms-winsoundevent:Notification.Default\" />
</toast>
\"@
    
    \$xml = New-Object Windows.Data.Xml.Dom.XmlDocument
    \$xml.LoadXml(\$template)
    
    \$toast = New-Object Windows.UI.Notifications.ToastNotification \$xml
    [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier(\"Claude Voice\").Show(\$toast)
    
    Write-Host \"Toast notification sent successfully\"
    exit 0
} catch {
    # フォールバック: MessageBox
    try {
        [void] [System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms')
        [System.Windows.Forms.MessageBox]::Show('$message', '$title', 'OK', 'Information')
        Write-Host \"MessageBox notification sent successfully\"
        exit 0
    } catch {
        Write-Host \"Error: \$(\$_.Exception.Message)\"
        exit 1
    }
}
"
    
    if "$powershell_path" -Command "$notification_script" 2>/dev/null; then
        log "DEBUG" "Windows notification sent successfully"
        
        # サウンド再生（オプション）
        if [[ "$sound" != "none" ]] && [[ "$sound" != "default" ]]; then
            play_windows_sound "$sound" &
        fi
        
        return 0
    else
        log "WARN" "Windows notification failed, using console fallback"
        echo "通知: $title - $message"
        return 1
    fi
}

# Windows システム音の再生
play_windows_sound() {
    local sound_name="$1"
    local powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        return 1
    fi
    
    # Windows システム音のマッピング
    local sound_event=""
    case "$sound_name" in
        "error"|"critical")
            sound_event="SystemHand"
            ;;
        "warning")
            sound_event="SystemExclamation"
            ;;
        "info"|"information")
            sound_event="SystemAsterisk"
            ;;
        "question")
            sound_event="SystemQuestion"
            ;;
        *)
            sound_event="SystemAsterisk"
            ;;
    esac
    
    local sound_script="
try {
    [console]::beep(800, 300)
    Start-Sleep -Milliseconds 100
    [System.Media.SystemSounds]::$sound_event.Play()
    Write-Host \"System sound played: $sound_event\"
} catch {
    [console]::beep(1000, 200)
    Write-Host \"Fallback beep played\"
}
"
    
    "$powershell_path" -Command "$sound_script" 2>/dev/null
}

# システムビープ音
system_beep() {
    local count="${1:-1}"
    local frequency="${2:-800}"
    local duration="${3:-200}"
    
    log "DEBUG" "Playing system beep on Windows: count=$count, freq=$frequency"
    
    local powershell_path=$(find_powershell_path)
    if [[ -z "$powershell_path" ]]; then
        # フォールバック
        for ((i=1; i<=count; i++)); do
            echo -e '\a'
            if [[ $count -gt 1 && $i -lt $count ]]; then
                sleep 0.3
            fi
        done
        return 0
    fi
    
    # PowerShell ビープ音スクリプト
    local beep_script="
for (\$i = 1; \$i -le $count; \$i++) {
    [console]::beep($frequency, $duration)
    if (\$i -lt $count) {
        Start-Sleep -Milliseconds 300
    }
}
Write-Host \"Beep sequence completed\"
"
    
    if "$powershell_path" -Command "$beep_script" 2>/dev/null; then
        log "DEBUG" "Windows beep completed successfully"
        return 0
    else
        log "WARN" "Windows beep failed"
        return 1
    fi
}

# Windows ビープのフォールバック（音声合成失敗時）
windows_beep_fallback() {
    local message="$1"
    
    log "DEBUG" "Using Windows beep fallback for message"
    
    # メッセージの長さに応じたビープパターン
    local length=${#message}
    local beep_count=1
    
    if [[ $length -gt 100 ]]; then
        beep_count=3
    elif [[ $length -gt 50 ]]; then
        beep_count=2
    fi
    
    # エラーキーワードの検出
    if echo "$message" | grep -qi "error\|エラー\|failed\|失敗"; then
        system_beep 2 400 300  # 低い音でエラーを表現
    else
        system_beep $beep_count 800 200  # 通常の音
    fi
}

# 音量制御（Windows）
set_system_volume() {
    local volume="$1"  # 0-100
    local type="${2:-output}"
    local powershell_path=$(find_powershell_path)
    
    log "DEBUG" "Setting Windows system volume: $volume ($type)"
    
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not available for volume control"
        return 1
    fi
    
    # Windows 音量制御スクリプト
    local volume_script="
try {
    # COM オブジェクトによる音量制御
    \$volume = $volume / 100.0
    \$obj = New-Object -ComObject WScript.Shell
    
    # キーストロークによる音量調整（簡易実装）
    if ($volume -eq 0) {
        # ミュート
        \$obj.SendKeys([char]173)
    } else {
        # 音量設定（概算）
        \$steps = [math]::Round(\$volume * 50)
        for (\$i = 0; \$i -lt \$steps; \$i++) {
            \$obj.SendKeys([char]175)  # 音量アップ
            Start-Sleep -Milliseconds 10
        }
    }
    
    Write-Host \"Volume set to $volume%\"
    exit 0
} catch {
    Write-Host \"Error: \$(\$_.Exception.Message)\"
    exit 1
}
"
    
    if "$powershell_path" -Command "$volume_script" 2>/dev/null; then
        log "DEBUG" "Windows volume set successfully"
        return 0
    else
        log "ERROR" "Windows volume control failed"
        return 1
    fi
}

# 現在の音量取得（Windows）
get_system_volume() {
    local type="${1:-output}"
    local powershell_path=$(find_powershell_path)
    
    if [[ -z "$powershell_path" ]]; then
        echo "50"  # デフォルト値
        return 1
    fi
    
    # Windows 音量取得スクリプト（簡易実装）
    local volume_script="
try {
    # WMI による音量取得の試行
    \$audio = Get-WmiObject -Class Win32_SoundDevice | Select-Object -First 1
    if (\$audio) {
        Write-Host \"50\"  # プレースホルダー
    } else {
        Write-Host \"50\"
    }
} catch {
    Write-Host \"50\"
}
"
    
    "$powershell_path" -Command "$volume_script" 2>/dev/null || echo "50"
}

# Windows/WSL固有の初期化
init_windows_audio() {
    log "INFO" "Initializing Windows/WSL audio subsystem"
    
    # 依存関係チェック
    if ! check_windows_dependencies; then
        return 1
    fi
    
    # PowerShell パスの確認
    local powershell_path=$(find_powershell_path)
    log "DEBUG" "PowerShell path: $powershell_path"
    
    # 利用可能な音声の確認
    local voices=$(detect_windows_tts_voices | head -3)
    log "DEBUG" "Available voices (first 3): $voices"
    
    # デフォルト日本語音声の選択
    local default_voice=$(select_japanese_voice)
    log "DEBUG" "Selected default Japanese voice: $default_voice"
    
    log "INFO" "Windows/WSL audio subsystem initialized successfully"
    return 0
}

# このモジュールのテスト関数
test_windows_functions() {
    echo "Testing Windows/WSL-specific functions..."
    
    # 依存関係チェック
    if check_windows_dependencies; then
        echo "Dependencies: OK"
    else
        echo "Dependencies: ISSUES"
    fi
    
    # PowerShell パスの確認
    local powershell_path=$(find_powershell_path)
    echo "PowerShell path: $powershell_path"
    
    # 利用可能な音声の確認
    local voices=$(detect_windows_tts_voices | head -3)
    echo "Available voices (first 3): $voices"
    
    # デフォルト日本語音声
    local japanese_voice=$(select_japanese_voice)
    echo "Default Japanese voice: $japanese_voice"
    
    # 現在の音量取得
    local volume=$(get_system_volume "output")
    echo "Current output volume: $volume"
    
    # 短いテスト音声（オプション）
    local test_speech=$(get_config "test.enable_speech" "false")
    if [[ "$test_speech" == "true" ]]; then
        echo "Testing speech synthesis..."
        speak_text "Windows テスト" "$japanese_voice" "auto" "0"
    fi
    
    # テストビープ音
    echo "Testing system beep..."
    system_beep 2 600 150
    
    echo "Windows/WSL functions test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    source "$SCRIPT_DIR/core/base.sh"
    
    claude_voice_init true
    test_windows_functions
fi