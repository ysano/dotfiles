#!/bin/bash
# WSL Voice Engine - Windows Subsystem for Linux音声出力システム
# PowerShell Speech Synthesis統合モジュール

# === グローバル設定 ===
readonly WSL_VOICE_VERSION="1.0.0"

# 日本語音声の優先順位（Windows 10/11標準音声）
readonly JAPANESE_VOICES=(
    "Microsoft Haruka Desktop"
    "Microsoft Sayaka Desktop" 
    "Microsoft Ichiro Desktop"
    "Microsoft Ayumi Desktop"
    "Microsoft Nanami Desktop"
    "Microsoft Zira Desktop"
)

# 英語フォールバック音声
readonly ENGLISH_VOICES=(
    "Microsoft Zira Desktop"
    "Microsoft David Desktop"
    "Microsoft Mark Desktop"
)

# === WSL環境検出 ===
detect_wsl_environment() {
    # WSL環境の詳細検出
    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        # WSLバージョンの判定
        if [[ -f /proc/sys/kernel/osrelease ]] && grep -qi microsoft /proc/sys/kernel/osrelease; then
            echo "wsl1"
        else
            echo "wsl2"
        fi
        return 0
    fi
    
    # 環境変数による検出
    if [[ -n "$WSL_DISTRO_NAME" ]] || [[ -n "$WSLENV" ]]; then
        echo "wsl2"
        return 0
    fi
    
    # PowerShell可用性による検出
    if command -v powershell.exe >/dev/null 2>&1; then
        echo "wsl_compatible"
        return 0
    fi
    
    echo "not_wsl"
    return 1
}

# === PowerShell実行ファイルの検出 ===
find_powershell() {
    # 優先順位に従ってPowerShellを検索
    local ps_candidates=(
        "powershell.exe"
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
        "/mnt/c/Program Files (x86)/PowerShell/7/pwsh.exe"
    )
    
    for ps_path in "${ps_candidates[@]}"; do
        if command -v "$ps_path" >/dev/null 2>&1 || [[ -f "$ps_path" ]]; then
            echo "$ps_path"
            return 0
        fi
    done
    
    return 1
}

# === Windows音声システム検証 ===
check_windows_speech() {
    log "DEBUG" "Checking Windows Speech availability"
    
    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found"
        echo "powershell_not_found"
        return 1
    fi
    
    log "DEBUG" "Using PowerShell: $powershell_path"
    
    local check_result=$("$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech -ErrorAction Stop;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$voices = \$synth.GetInstalledVoices() | Where-Object {\$_.Enabled};
            if (\$voices.Count -gt 0) {
                Write-Output 'available';
            } else {
                Write-Output 'no_voices';
            }
        } catch {
            Write-Output 'unavailable';
        }
    " 2>/dev/null | tr -d '\r\n')
    
    log "DEBUG" "Windows Speech check result: $check_result"
    echo "$check_result"
}

# === 利用可能音声の検出 ===
detect_available_voices() {
    log "DEBUG" "Detecting available voices"
    
    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found for voice detection"
        return 1
    fi
    
    "$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$voices = \$synth.GetInstalledVoices() | Where-Object {\$_.Enabled};
            \$voices | ForEach-Object {
                \$voice = \$_.VoiceInfo;
                Write-Output (\$voice.Name + '|' + \$voice.Culture.Name + '|' + \$voice.Gender);
            }
        } catch {
            Write-Output 'error: voice detection failed';
        }
    " 2>/dev/null | tr -d '\r'
}

# === 日本語音声の検出と選択 ===
detect_japanese_voices() {
    local all_voices=$(detect_available_voices)
    echo "$all_voices" | grep "|ja-JP|" | cut -d'|' -f1
}

select_best_japanese_voice() {
    local available_voices=$(detect_japanese_voices)
    
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
    local first_voice=$(echo "$available_voices" | head -1)
    echo "$first_voice"
    log "DEBUG" "Using first available Japanese voice: $first_voice"
    return 0
}

# === 英語フォールバック音声選択 ===
select_fallback_english_voice() {
    local all_voices=$(detect_available_voices)
    local english_voices=$(echo "$all_voices" | grep "|en-US|" | cut -d'|' -f1)
    
    # 優先順位に従って選択
    for priority_voice in "${ENGLISH_VOICES[@]}"; do
        if echo "$english_voices" | grep -q "^$priority_voice$"; then
            echo "$priority_voice"
            log "DEBUG" "Selected English fallback voice: $priority_voice"
            return 0
        fi
    done
    
    # 最初の利用可能な英語音声
    local first_voice=$(echo "$english_voices" | head -1)
    if [[ -n "$first_voice" ]]; then
        echo "$first_voice"
        log "DEBUG" "Using first available English voice: $first_voice"
        return 0
    fi
    
    return 1
}

# === メイン音声合成関数 ===
wsl_speak() {
    local text="$1"
    local voice="${2:-auto}"
    local rate="${3:-0}"    # -10 to 10
    local volume="${4:-80}" # 0 to 100
    
    log "DEBUG" "WSL speak request: text='${text:0:50}...', voice=$voice"
    
    # 音声の自動選択
    if [[ "$voice" == "auto" ]]; then
        voice=$(select_best_japanese_voice)
        if [[ -z "$voice" ]]; then
            voice=$(select_fallback_english_voice)
            if [[ -z "$voice" ]]; then
                log "ERROR" "No suitable voice available"
                return 1
            fi
        fi
    fi
    
    # テキストのサニタイズ（PowerShell特殊文字のエスケープ）
    local sanitized_text=$(echo "$text" | sed "s/'/\`'/g" | sed 's/"/\\"/g')
    
    log "DEBUG" "Using voice: $voice, rate: $rate, volume: $volume"
    
    # PowerShell実行ファイルの取得
    local powershell_path=$(find_powershell)
    if [[ -z "$powershell_path" ]]; then
        log "ERROR" "PowerShell not found for speech synthesis"
        return 1
    fi
    
    # PowerShell音声合成の実行
    local result=$("$powershell_path" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$synth.SelectVoice('$voice');
            \$synth.Rate = $rate;
            \$synth.Volume = $volume;
            \$synth.Speak('$sanitized_text');
            Write-Output 'success';
        } catch {
            Write-Output ('error: ' + \$_.Exception.Message);
        }
    " 2>/dev/null | tr -d '\r\n')
    
    if [[ "$result" == "success" ]]; then
        log "INFO" "Speech synthesis completed successfully"
        return 0
    else
        log "ERROR" "Speech synthesis failed: $result"
        return 1
    fi
}

# === 非同期音声出力 ===
wsl_speak_async() {
    local text="$1"
    local voice="${2:-auto}"
    local timeout="${3:-30}"
    
    log "DEBUG" "Starting async speech synthesis"
    
    # バックグラウンドで音声出力
    (
        timeout "$timeout" wsl_speak "$text" "$voice" 2>/dev/null
        local exit_code=$?
        if [[ $exit_code -eq 124 ]]; then
            log "WARN" "Speech synthesis timed out after ${timeout}s"
        elif [[ $exit_code -ne 0 ]]; then
            log "WARN" "Speech synthesis failed with exit code $exit_code"
        fi
    ) &
    
    local bg_pid=$!
    log "DEBUG" "Speech synthesis started in background (PID: $bg_pid)"
    
    # 同時音声出力の制限
    manage_voice_processes
    
    return 0
}

# === 音声プロセス管理 ===
manage_voice_processes() {
    local max_processes="${1:-1}"
    
    # 現在実行中の音声プロセス数を取得
    local current_count=$(pgrep -f "powershell.*Speech" 2>/dev/null | wc -l)
    
    log "DEBUG" "Current voice processes: $current_count, max allowed: $max_processes"
    
    if [[ $current_count -gt $max_processes ]]; then
        log "WARN" "Too many voice processes ($current_count), terminating oldest"
        # 古いプロセスを終了（最新のものを残す）
        local pids=($(pgrep -f "powershell.*Speech" 2>/dev/null))
        for ((i=0; i<${#pids[@]}-$max_processes; i++)); do
            kill "${pids[i]}" 2>/dev/null
            log "DEBUG" "Terminated voice process: ${pids[i]}"
        done
        sleep 0.5
    fi
}

# === 音声システム診断 ===
diagnose_wsl_voice() {
    echo "=== WSL Voice Engine Diagnostics ==="
    echo "Version: $WSL_VOICE_VERSION"
    echo ""
    
    # WSL環境検出
    local wsl_type=$(detect_wsl_environment)
    echo "WSL Environment: $wsl_type"
    
    # PowerShell可用性
    local powershell_path=$(find_powershell)
    if [[ -n "$powershell_path" ]]; then
        echo "PowerShell: Available ($powershell_path)"
        local ps_version=$("$powershell_path" -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null | tr -d '\r\n')
        echo "PowerShell Version: $ps_version"
    else
        echo "PowerShell: Not Available"
        return 1
    fi
    
    # Windows Speech System
    local speech_status=$(check_windows_speech)
    echo "Windows Speech: $speech_status"
    
    if [[ "$speech_status" == "available" ]]; then
        echo ""
        echo "Available Voices:"
        detect_available_voices | while IFS='|' read -r name culture gender; do
            echo "  $name ($culture, $gender)"
        done
        
        echo ""
        echo "Recommended Japanese Voice:"
        local jp_voice=$(select_best_japanese_voice)
        if [[ -n "$jp_voice" ]]; then
            echo "  $jp_voice"
        else
            echo "  None available"
        fi
        
        echo ""
        echo "Fallback English Voice:"
        local en_voice=$(select_fallback_english_voice)
        if [[ -n "$en_voice" ]]; then
            echo "  $en_voice"
        else
            echo "  None available"
        fi
    fi
    
    echo ""
    echo "=== End Diagnostics ==="
}

# === テスト関数 ===
test_wsl_voice() {
    echo "Testing WSL Voice Engine..."
    
    # 診断実行
    diagnose_wsl_voice
    echo ""
    
    # 音声テスト
    echo "Testing Japanese speech synthesis..."
    if wsl_speak "WSL音声エンジンのテストです。日本語の読み上げが正常に動作しています。"; then
        echo "✅ Japanese speech test: PASSED"
    else
        echo "❌ Japanese speech test: FAILED"
    fi
    
    echo ""
    echo "Testing English speech synthesis..."
    if wsl_speak "This is a test of the WSL voice engine. English speech synthesis is working."; then
        echo "✅ English speech test: PASSED"
    else
        echo "❌ English speech test: FAILED"
    fi
    
    echo ""
    echo "WSL Voice Engine test completed"
}

# === このスクリプトが直接実行された場合 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    if [[ -f "$SCRIPT_DIR/base.sh" ]]; then
        source "$SCRIPT_DIR/base.sh"
        claude_voice_init true
    else
        # スタンドアロン実行時の簡易ログ関数
        log() {
            local level="$1"
            local message="$2"
            echo "[$level] $message" >&2
        }
    fi
    
    # コマンドライン引数の処理
    case "${1:-test}" in
        "test")
            test_wsl_voice
            ;;
        "diagnose")
            diagnose_wsl_voice
            ;;
        "speak")
            if [[ -n "$2" ]]; then
                wsl_speak "$2" "${3:-auto}"
            else
                echo "Usage: $0 speak <text> [voice]"
                exit 1
            fi
            ;;
        *)
            echo "Usage: $0 {test|diagnose|speak}"
            echo "  test     - Run comprehensive tests"
            echo "  diagnose - Show system diagnostics"
            echo "  speak    - Test speech synthesis"
            exit 1
            ;;
    esac
fi