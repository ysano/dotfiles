#!/bin/bash
# ファイル名: sound_utils.sh
# 説明: tmux-claude-voice 音声エンジン

# 依存ファイルの存在確認
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR

# functions.shからログ機能を利用
if [[ -f "${SCRIPT_DIR}/functions.sh" ]]; then
    source "${SCRIPT_DIR}/functions.sh"
elif command -v log_info >/dev/null 2>&1; then
    # 既に読み込み済み
    :
else
    # ログ機能のフォールバック
    log_info() { echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2; }
    log_error() { echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2; }
    log_debug() { [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]] && echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2 || true; }
fi

# OS種別の取得
get_os_type() {
    uname
}

# 設定値の取得（デフォルト値付き）
get_tmux_sound_option() {
    local option="$1"
    local default_value="$2"
    
    local value
    if value=$(tmux show-option -gqv "@$option" 2>/dev/null); then
        echo "$value"
    else
        echo "$default_value"
    fi
}

# プラットフォーム固有の通知音ファイルパスを取得
get_system_sound_path() {
    local sound_name="$1"
    local os_type=$(get_os_type)
    
    if [[ -z "$sound_name" ]]; then
        log_error "通知音名が指定されていません"
        return 1
    fi

    log_debug "通知音パスを取得中: $sound_name (OS: $os_type)"

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: システム通知音のパス
        local sound_paths=(
            "/System/Library/Sounds/${sound_name}.aiff"
            "/System/Library/PrivateFrameworks/ToneLibrary.framework/Versions/A/Resources/AlertTones/${sound_name}.aiff"
            "${HOME}/Library/Sounds/${sound_name}.aiff"
        )

        for path in "${sound_paths[@]}"; do
            if [[ -f "$path" ]]; then
                log_debug "通知音ファイルを発見: $path"
                echo "$path"
                return 0
            fi
        done

        # デフォルトのシステム音
        local default_path="/System/Library/Sounds/Submarine.aiff"
        log_debug "デフォルト通知音を使用: $default_path"
        echo "$default_path"
    else
        # WSL/Windows: Windowsシステム通知音のパス
        local windows_media_path="/mnt/c/Windows/Media"
        local sound_path="${windows_media_path}/${sound_name}.wav"

        if [[ -f "$sound_path" ]]; then
            log_debug "通知音ファイルを発見: $sound_path"
            echo "$sound_path"
        else
            # デフォルトのWindows通知音
            local default_path="${windows_media_path}/Windows Notify Messaging.wav"
            log_debug "デフォルト通知音を使用: $default_path"
            echo "$default_path"
        fi
    fi
}

# 利用可能なmacOS音声を取得
get_available_macos_voices() {
    if command -v say >/dev/null 2>&1; then
        log_debug "利用可能なmacOS音声を検索中..."
        say -v "?" 2>/dev/null | grep -E "(Kyoko|Otoya)" | grep -E "(enhanced|compact)" | head -n 4 || {
            log_debug "Enhanced音声が見つからない場合の基本音声を検索..."
            say -v "?" 2>/dev/null | grep -E "(Kyoko|Otoya)" | head -n 2
        }
    else
        log_error "sayコマンドが見つかりません"
        return 1
    fi
}

# PowerShellのパスを取得
get_powershell_path() {
    # 複数のPowerShellパスを試行
    local powershell_paths=(
        "powershell.exe"
        "pwsh.exe"
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
        "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
    )
    
    for path in "${powershell_paths[@]}"; do
        if command -v "$path" >/dev/null 2>&1 || [[ -f "$path" ]]; then
            echo "$path"
            return 0
        fi
    done
    
    return 1
}

# 利用可能なWindows音声を取得
get_available_windows_voices() {
    log_debug "利用可能なWindows音声を検索中..."
    
    local powershell_path
    if powershell_path=$(get_powershell_path); then
        "$powershell_path" -Command "
            try {
                Add-Type -AssemblyName System.Speech
                \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
                \$synth.GetInstalledVoices() | ForEach-Object {
                    \$voice = \$_.VoiceInfo
                    if (\$voice.Culture.Name -eq 'ja-JP') {
                        Write-Output \$voice.Name
                    }
                }
            } catch {
                Write-Error 'Failed to get installed voices'
            }
        " 2>/dev/null || {
            log_error "Windows音声の取得に失敗しました"
            return 1
        }
    else
        log_error "PowerShellが見つかりません"
        return 1
    fi
}

# 音声合成でテキストを読み上げ
speak_text() {
    local text="$1"
    local os_type=$(get_os_type)
    
    if [[ -z "$text" ]]; then
        log_error "読み上げテキストが指定されていません"
        return 1
    fi

    log_debug "テキストを読み上げ中: $text (OS: $os_type)"

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: sayコマンド（設定可能な音声）
        local speech_rate=$(get_tmux_sound_option "claude_voice_speech_rate" "200")
        local voice_name=$(get_tmux_sound_option "claude_voice_macos_voice" "Kyoko")
        local voice_quality=$(get_tmux_sound_option "claude_voice_voice_quality" "enhanced")

        # 音声品質に応じた音声名の選択
        if [[ "$voice_quality" == "enhanced" ]]; then
            case "$voice_name" in
                "Kyoko") voice_name="Kyoko Enhanced" ;;
                "Otoya") voice_name="Otoya Enhanced" ;;
            esac
        fi

        log_debug "macOS音声設定: voice=$voice_name, rate=$speech_rate, quality=$voice_quality"

        if command -v say >/dev/null 2>&1; then
            say -v "$voice_name" -r "$speech_rate" "$text" 2>/dev/null &
            local say_pid=$!
            log_debug "音声再生開始 (PID: $say_pid)"
        else
            log_error "sayコマンドが見つかりません"
            return 1
        fi
    else
        # WSL: ffplayを使用した直接再生（優先）またはPowerShell + System.Speech
        local volume=$(get_tmux_sound_option "claude_voice_volume_wsl" "80")
        local voice_name=$(get_tmux_sound_option "claude_voice_wsl_voice" "Haruka")

        # PowerShellが利用可能な場合はWindowsネイティブTTSを使用（優先）
        if powershell_path=$(get_powershell_path); then
            # WindowsネイティブのTTS
            # Windows音声名の完全指定
            case "$voice_name" in
                "Haruka") voice_name="Microsoft Haruka Desktop" ;;
                "Ayumi") voice_name="Microsoft Ayumi Desktop" ;;
                "Ichiro") voice_name="Microsoft Ichiro Desktop" ;;
            esac

            log_debug "Windows音声設定: voice=$voice_name, volume=$volume"

            "$powershell_path" -Command "
                try {
                    Add-Type -AssemblyName System.Speech
                    \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
                    \$synth.SelectVoice('$voice_name')
                    \$synth.Volume = $volume
                    \$synth.Speak('$text')
                } catch {
                    Write-Error 'Failed to synthesize speech: \$_'
                }
            " 2>/dev/null &
            local ps_pid=$!
            log_debug "音声再生開始 (PowerShell PID: $ps_pid)"
        elif command -v ffplay >/dev/null 2>&1; then
            # フォールバック: シンプルな音声合成として通知音を再生
            log_debug "WSL音声合成 (ffplay): 通知音で代替"
            play_notification_sound "complete"
            # フォールバック: PowerShell + System.Speech
            # Windows音声名の完全指定
            case "$voice_name" in
                "Haruka") voice_name="Microsoft Haruka Desktop" ;;
                "Ayumi") voice_name="Microsoft Ayumi Desktop" ;;
                "Ichiro") voice_name="Microsoft Ichiro Desktop" ;;
            esac

            log_debug "Windows音声設定: voice=$voice_name, volume=$volume"

            "$powershell_path" -Command "
                try {
                    Add-Type -AssemblyName System.Speech
                    \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
                    \$synth.SelectVoice('$voice_name')
                    \$synth.Volume = $volume
                    \$synth.Speak('$text')
                } catch {
                    Write-Error 'Failed to synthesize speech: \$_'
                }
            " 2>/dev/null &
            local ps_pid=$!
            log_debug "音声再生開始 (PowerShell PID: $ps_pid)"
        else
            log_error "音声合成に必要なコマンドが見つかりません (ffplay, powershell.exe)"
            return 1
        fi
    fi
}

# 通知音を再生
play_notification_sound() {
    local sound_type="$1"  # start, complete, waiting, error
    local session_window="$2"  # セッション:ウィンドウ（オプション）
    local os_type=$(get_os_type)
    
    if [[ -z "$sound_type" ]]; then
        log_error "通知音タイプが指定されていません"
        return 1
    fi

    log_debug "通知音を再生中: type=$sound_type (OS: $os_type)"

    # 設定から音声名を取得
    local sound_name=$(get_tmux_sound_option "claude_voice_sound_${sound_type}" "")
    if [[ -z "$sound_name" ]]; then
        case "$sound_type" in
            "start") sound_name="Ping" ;;
            "complete") sound_name="Glass" ;;
            "waiting") sound_name="Funk" ;;
            "error") sound_name="Sosumi" ;;
            *) sound_name="Submarine" ;;
        esac
    fi

    local sound_file
    if sound_file=$(get_system_sound_path "$sound_name"); then
        log_debug "通知音ファイル: $sound_file"
        
        if [[ ! -f "$sound_file" ]]; then
            log_error "通知音ファイルが見つかりません: $sound_file"
            return 1
        fi

        # パンニング機能の統合
        local panning_enabled=$(tmux show-option -gqv @claude_voice_panning_enabled 2>/dev/null)
        panning_enabled="${panning_enabled:-true}"
        
        if [[ "$panning_enabled" == "true" && -n "$session_window" ]]; then
            # パンニングエンジンが利用可能な場合は使用
            if [[ -f "$SCRIPT_DIR/panning_engine.sh" ]]; then
                source "$SCRIPT_DIR/panning_engine.sh"
                apply_panning "$sound_file" "$session_window" "$sound_type" &
                return 0
            fi
        fi

        if [[ "$os_type" == "Darwin" ]]; then
            # macOS: afplayを使用
            if command -v afplay >/dev/null 2>&1; then
                local volume=$(get_tmux_sound_option "claude_voice_volume_macos" "0.8")
                log_debug "macOS通知音再生: volume=$volume"
                afplay -v "$volume" "$sound_file" 2>/dev/null &
                local afplay_pid=$!
                log_debug "通知音再生開始 (PID: $afplay_pid)"
            else
                log_error "afplayコマンドが見つかりません"
                return 1
            fi
        else
            # WSL: ffplayを使用した直接再生（優先）またはWindowsのSoundPlayer
            local volume=$(get_tmux_sound_option "claude_voice_volume_wsl" "80")
            
            # PowerShellが利用可能な場合はWindowsネイティブ機能を使用（優先）
            if powershell_path=$(get_powershell_path); then
                # Windowsネイティブの通知音再生
                local windows_path=$(echo "$sound_file" | sed 's|/mnt/c/|C:/|g' | sed 's|/|\\|g')
                log_debug "WSL通知音再生 (PowerShell): volume=$volume, path=$windows_path"

                "$powershell_path" -Command "
                    try {
                        Add-Type -AssemblyName System.Windows.Forms
                        \$player = New-Object System.Media.SoundPlayer
                        \$player.SoundLocation = '$windows_path'
                        \$player.Play()
                    } catch {
                        Write-Error 'Failed to play sound: \$_'
                    }
                " 2>/dev/null &
                local ps_pid=$!
                log_debug "通知音再生開始 (PowerShell PID: $ps_pid)"
            elif command -v ffplay >/dev/null 2>&1; then
                # フォールバック: ffplayを使用した直接再生
                # 音量を0.0-1.0の範囲に変換
                local ffplay_volume=$(echo "scale=2; $volume / 100" | bc 2>/dev/null || echo "0.8")
                log_debug "WSL通知音再生 (ffplay): volume=$ffplay_volume, file=$sound_file"

                ffplay -nodisp -autoexit -volume "$ffplay_volume" "$sound_file" 2>/dev/null &
                local ffplay_pid=$!
                log_debug "通知音再生開始 (ffplay PID: $ffplay_pid)"
                # フォールバック: WindowsのSoundPlayerを使用
                local windows_path=$(echo "$sound_file" | sed 's|/mnt/c/|C:/|g' | sed 's|/|\\|g')
                log_debug "WSL通知音再生 (PowerShell): volume=$volume, path=$windows_path"
                
                "$powershell_path" -Command "
                    try {
                        Add-Type -AssemblyName System.Windows.Forms
                        \$player = New-Object System.Media.SoundPlayer
                        \$player.SoundLocation = '$windows_path'
                        \$player.Play()
                    } catch {
                        Write-Error 'Failed to play sound: \$_'
                    }
                " 2>/dev/null &
                local ps_pid=$!
                log_debug "通知音再生開始 (PowerShell PID: $ps_pid)"
            else
                log_error "音声再生に必要なコマンドが見つかりません (ffplay, powershell.exe)"
                return 1
            fi
        fi
    else
        log_error "通知音パスの取得に失敗しました"
        return 1
    fi
}

# 音声エンジンのテスト
test_sound_engine() {
    local test_type="${1:-all}"
    
    echo "=== 音声エンジンテスト開始 ($test_type) ==="
    
    # OS種別テスト
    if [[ "$test_type" == "all" || "$test_type" == "platform" ]]; then
        echo "プラットフォーム検出テスト..."
        local os_type=$(get_os_type)
        echo "✓ OS種別検出: $os_type"
    fi
    
    # 利用可能音声テスト
    if [[ "$test_type" == "all" || "$test_type" == "voices" ]]; then
        echo "利用可能音声テスト..."
        local os_type=$(get_os_type)
        
        if [[ "$os_type" == "Darwin" ]]; then
            echo "macOS音声を検索中..."
            if get_available_macos_voices; then
                echo "✓ macOS音声検出: 成功"
            else
                echo "✗ macOS音声検出: 失敗"
            fi
        else
            echo "Windows音声を検索中..."
            if get_available_windows_voices; then
                echo "✓ Windows音声検出: 成功"
            else
                echo "✗ Windows音声検出: 失敗"
            fi
        fi
    fi
    
    # 通知音ファイルテスト
    if [[ "$test_type" == "all" || "$test_type" == "sounds" ]]; then
        echo "通知音ファイルテスト..."
        local test_sounds=("Submarine" "Funk" "Basso")
        
        for sound in "${test_sounds[@]}"; do
            local sound_path
            if sound_path=$(get_system_sound_path "$sound"); then
                if [[ -f "$sound_path" ]]; then
                    echo "✓ 通知音ファイル ($sound): $sound_path"
                else
                    echo "✗ 通知音ファイル ($sound): ファイルが存在しません - $sound_path"
                fi
            else
                echo "✗ 通知音ファイル ($sound): パス取得失敗"
            fi
        done
    fi
    
    # 音声再生テスト（オプション）
    if [[ "$test_type" == "playback" ]]; then
        echo "音声再生テスト..."
        echo "音声合成テスト（3秒後に実行）..."
        sleep 3
        speak_text "テスト音声です。Claude Voice システムが正常に動作しています。"
        echo "✓ 音声合成テスト実行"
        
        echo "通知音テスト（3秒後に実行）..."
        sleep 3
        play_notification_sound "complete"
        echo "✓ 通知音テスト実行"
    fi
    
    echo "=== 音声エンジンテスト完了 ==="
    return 0
}

# 音声エンジンの依存関係チェック
check_sound_dependencies() {
    local os_type=$(get_os_type)
    local missing_deps=()
    
    echo "音声エンジン依存関係チェック..."
    
    if [[ "$os_type" == "Darwin" ]]; then
        # macOS依存関係
        if ! command -v say >/dev/null 2>&1; then
            missing_deps+=("say (macOS音声合成)")
        fi
        
        if ! command -v afplay >/dev/null 2>&1; then
            missing_deps+=("afplay (macOS音声再生)")
        fi
    else
        # WSL依存関係
        # PowerShellの存在チェック
        if powershell_path=$(get_powershell_path); then
            echo "✓ PowerShell: 利用可能 ($powershell_path)"
        else
            echo "⚠️ PowerShellが見つかりません（WSL環境では正常な場合があります）"
        fi
        
        # Windows Mediaディレクトリの存在確認
        if [[ ! -d "/mnt/c/Windows/Media" ]]; then
            missing_deps+=("/mnt/c/Windows/Media (Windows通知音ディレクトリ)")
        fi
        
        # ffplayの存在確認（音声再生に必要）
        if ! command -v ffplay >/dev/null 2>&1; then
            missing_deps+=("ffplay (ffmpeg)")
        else
            echo "✓ ffplay (ffmpeg): 利用可能"
        fi
    fi
    
    if [[ ${#missing_deps[@]} -eq 0 ]]; then
        echo "✓ すべての依存関係が満たされています"
        return 0
    else
        echo "✗ 不足している依存関係:"
        for dep in "${missing_deps[@]}"; do
            echo "  - $dep"
        done
        return 1
    fi
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            TMUX_CLAUDE_VOICE_DEBUG=1 test_sound_engine "all"
            ;;
        "test-platform")
            test_sound_engine "platform"
            ;;
        "test-voices")
            test_sound_engine "voices"
            ;;
        "test-sounds")
            test_sound_engine "sounds"
            ;;
        "test-playback")
            echo "警告: 音声が再生されます"
            test_sound_engine "playback"
            ;;
        "deps")
            check_sound_dependencies
            ;;
        "speak")
            speak_text "${2:-テストメッセージです}"
            ;;
        "play")
            play_notification_sound "${2:-complete}"
            ;;
        *)
            echo "使用方法: $0 [test|test-platform|test-voices|test-sounds|test-playback|deps|speak|play]"
            echo "  test          - 全テスト実行（音声再生なし）"
            echo "  test-platform - プラットフォーム検出テスト"
            echo "  test-voices   - 利用可能音声テスト"
            echo "  test-sounds   - 通知音ファイルテスト"
            echo "  test-playback - 音声再生テスト（実際に音声が再生されます）"
            echo "  deps          - 依存関係チェック"
            echo "  speak <text>  - テキストを音声合成"
            echo "  play <type>   - 通知音を再生"
            exit 1
            ;;
    esac
fi