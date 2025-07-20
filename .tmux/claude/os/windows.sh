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
    
    # WSL環境の確認と強化された検出
    if [[ -z "${WSL_DISTRO_NAME:-}" ]] && ! grep -qi microsoft /proc/version 2>/dev/null; then
        log "WARN" "Not running in WSL environment - some features may not work"
        return 1
    fi
    
    # WSL2の確認
    if grep -qi "WSL2" /proc/version 2>/dev/null; then
        log "DEBUG" "Running in WSL2 environment"
        export WSL_VERSION="2"
    else
        log "DEBUG" "Running in WSL1 environment"
        export WSL_VERSION="1"
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
    
    # 検索パスの優先順位（WSL用に拡張）
    local powershell_paths=(
        # PowerShell Core (推奨) - 複数バージョン対応
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
        "/mnt/c/Program Files/PowerShell/6/pwsh.exe"
        # Windows PowerShell (システムデフォルト)
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Windows/System32/powershell.exe"
        "/mnt/c/Windows/SysWOW64/WindowsPowerShell/v1.0/powershell.exe"
        # ユーザーディレクトリのPowerShell
        "/mnt/c/Users/*/AppData/Local/Microsoft/PowerShell/*/pwsh.exe"
        # WSL Path経由（Windows PATH統合）
        "powershell.exe"
        "pwsh.exe"
        # Windows Store版PowerShell
        "/mnt/c/Users/*/AppData/Local/Microsoft/WindowsApps/pwsh.exe"
        "/mnt/c/Users/*/AppData/Local/Microsoft/WindowsApps/powershell.exe"
    )
    
    for path in "${powershell_paths[@]}"; do
        # ワイルドカード展開
        if [[ "$path" == *"*"* ]]; then
            # ワイルドカードパスの展開と検索
            for expanded_path in $path; do
                if [[ -f "$expanded_path" ]] && [[ -x "$expanded_path" ]]; then
                    log "DEBUG" "Found PowerShell at: $expanded_path"
                    echo "$expanded_path"
                    return 0
                fi
            done
        else
            # 通常のパス検索
            if [[ -f "$path" ]] && [[ -x "$path" ]]; then
                log "DEBUG" "Found PowerShell at: $path"
                echo "$path"
                return 0
            elif command -v "$path" >/dev/null 2>&1; then
                log "DEBUG" "Found PowerShell via command: $path"
                echo "$path"
                return 0
            fi
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

# === Windows固有の音声合成 ===
speak_windows() {
    local text="$1"
    local voice="${2:-auto}"
    
    # WSL環境での音声合成
    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        # WSLユニバーサル音声システムを使用
        if source "$CLAUDE_HOME/core/universal_voice.sh" 2>/dev/null; then
            universal_speak "$text" "$voice"
            return $?
        fi
    fi
    
    # フォールバック: テキスト出力
    log "WARN" "Windows native speech synthesis not available"
    echo "[VOICE] $text"
    return 1
}

# 音声合成実行（後方互換性のため保持）
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "auto")}"
    local device="${3:-auto}"  # Windows では通常無視
    local rate="${4:-$(get_config "audio.speech_rate" "0")}"  # -10 to 10
    
    log "DEBUG" "Speaking text on Windows: voice=$voice, rate=$rate"
    
    # 新しいWindows音声システムを呼び出し
    speak_windows "$text" "$voice"
    return $?
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

# WSL クリップボード統合機能
wsl_clipboard_copy() {
    local text="$1"
    
    if command -v clip.exe >/dev/null 2>&1; then
        echo -n "$text" | clip.exe
        log "DEBUG" "Text copied to Windows clipboard via clip.exe"
        return 0
    elif command -v powershell.exe >/dev/null 2>&1; then
        echo -n "$text" | powershell.exe -Command "Set-Clipboard -Value (Get-Content -Raw)"
        log "DEBUG" "Text copied to Windows clipboard via PowerShell"
        return 0
    else
        log "WARN" "No clipboard integration available"
        return 1
    fi
}

# WSL クリップボードからの貼り付け
wsl_clipboard_paste() {
    if command -v powershell.exe >/dev/null 2>&1; then
        powershell.exe -Command "Get-Clipboard" 2>/dev/null | sed 's/\r$//'
        return 0
    else
        log "WARN" "No clipboard paste capability available"
        return 1
    fi
}

# WSL固有のシステム情報取得
get_wsl_info() {
    local info_type="${1:-all}"
    
    case "$info_type" in
        "version")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
                echo "$WSL_DISTRO_NAME"
            elif grep -qi "WSL2" /proc/version 2>/dev/null; then
                echo "WSL2"
            elif grep -qi microsoft /proc/version 2>/dev/null; then
                echo "WSL1"
            else
                echo "Not WSL"
            fi
            ;;
        "windows_build")
            if command -v cmd.exe >/dev/null 2>&1; then
                cmd.exe /c "ver" 2>/dev/null | grep -oP "Version \K[0-9]+\.[0-9]+\.[0-9]+" || echo "Unknown"
            else
                echo "Unknown"
            fi
            ;;
        "memory")
            # WSL固有のメモリ情報
            if [[ -f /proc/meminfo ]]; then
                awk '/MemTotal:/ {total=$2} /MemAvailable:/ {avail=$2} END {printf "%.1fGB/%.1fGB", (total-avail)/1024/1024, total/1024/1024}' /proc/meminfo
            fi
            ;;
        "all")
            echo "WSL Version: $(get_wsl_info version)"
            echo "Windows Build: $(get_wsl_info windows_build)"
            echo "Memory Usage: $(get_wsl_info memory)"
            if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
                echo "Distribution: $WSL_DISTRO_NAME"
            fi
            ;;
    esac
}

# WSL環境最適化
optimize_wsl_environment() {
    log "INFO" "Optimizing WSL environment for Claude Voice"
    
    # WSL固有の環境変数設定
    export CLAUDE_WSL_MODE="true"
    export CLAUDE_AUDIO_BACKEND="windows"
    
    # Windows側アプリケーションパスの確認
    if [[ -d "/mnt/c/Windows/System32" ]]; then
        export WINDOWS_SYSTEM32="/mnt/c/Windows/System32"
        log "DEBUG" "Windows System32 path detected: $WINDOWS_SYSTEM32"
    fi
    
    # PowerShellの事前キャッシュ
    local powershell_path=$(find_powershell_path)
    if [[ -n "$powershell_path" ]]; then
        export CLAUDE_POWERSHELL_PATH="$powershell_path"
        log "DEBUG" "PowerShell path cached: $CLAUDE_POWERSHELL_PATH"
    fi
    
    # WSL用の音量設定（Windows側）
    if [[ -n "$powershell_path" ]]; then
        # Windows側の音量確認
        local windows_volume=$(get_system_volume "output")
        log "DEBUG" "Windows system volume: $windows_volume%"
    fi
    
    log "INFO" "WSL environment optimization completed"
}

# WSL環境でのフォールバック通知（PowerShell不可時）
wsl_fallback_notification() {
    local text="$1"
    local max_length=100
    
    log "INFO" "WSL fallback notification mode"
    
    # テキストの短縮
    if [[ ${#text} -gt $max_length ]]; then
        text="${text:0:$max_length}..."
    fi
    
    # ターミナルビープ音（複数回）
    local beep_count=3
    if echo "$text" | grep -qi "error\|エラー\|failed\|失敗"; then
        beep_count=5  # エラーの場合は多めに
    elif echo "$text" | grep -qi "complete\|完了\|success\|成功"; then
        beep_count=2  # 成功の場合は少なめに
    fi
    
    for ((i=1; i<=beep_count; i++)); do
        echo -e '\a'
        sleep 0.2
    done
    
    # tmux用の表示通知
    if command -v tmux >/dev/null 2>&1 && [[ -n "${TMUX:-}" ]]; then
        tmux display-message "🔊 Claude Voice (WSL): $text"
    fi
    
    # コンソール出力
    echo "🔊 Claude Voice Notification: $text"
    
    # ログ出力
    log "INFO" "Fallback notification sent: $text"
    
    return 0
}

# WSL環境での簡易システム情報取得（PowerShell不要版）
get_wsl_simple_info() {
    echo "WSL Distribution: ${WSL_DISTRO_NAME:-$(lsb_release -si 2>/dev/null || echo "Unknown")}"
    echo "WSL Version: $(grep -qi "WSL2" /proc/version 2>/dev/null && echo "WSL2" || echo "WSL1")"
    echo "Memory: $(free -h | awk 'NR==2{printf "%s/%s", $3, $2}')"
    echo "Uptime: $(uptime -p 2>/dev/null || echo "Unknown")"
}

# WSL用の軽量テスト（PowerShell不要）
test_wsl_basic_functions() {
    echo "=== WSL基本機能テスト ==="
    
    # WSL環境検出
    if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
        echo "✅ WSL環境検出"
    else
        echo "❌ WSL環境検出"
    fi
    
    # 基本コマンド確認
    if command -v tmux >/dev/null 2>&1; then
        echo "✅ tmux利用可能"
    else
        echo "❌ tmux利用不可"
    fi
    
    # ファイルシステムアクセス
    if [[ -d "/mnt/c" ]]; then
        echo "✅ Windows Cドライブアクセス可能"
    else
        echo "❌ Windows Cドライブアクセス不可"
    fi
    
    # フォールバック通知テスト
    echo "🔊 フォールバック通知をテスト中..."
    wsl_fallback_notification "WSL環境での通知テストです"
    
    echo "WSL基本機能テスト完了"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    source "$SCRIPT_DIR/core/base.sh"
    
    claude_voice_init true
    optimize_wsl_environment
    
    # PowerShell利用可能性に応じたテスト
    if check_windows_dependencies 2>/dev/null; then
        echo "PowerShell利用可能 - フルテスト実行"
        test_windows_functions
    else
        echo "PowerShell利用不可 - 基本テスト実行"
        test_wsl_basic_functions
    fi
fi