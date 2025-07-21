#!/bin/bash
# Claude Voice - Windows/WSL specific functions
# Windows/WSL固有の音声・通知機能

# モジュールローダーの読み込み
source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/module_loader.sh" 2>/dev/null || {
    echo "ERROR: Module loader not found" >&2
    exit 1
}

# Windows関連モジュールの一括初期化
if ! load_windows_modules; then
    log "ERROR" "Failed to initialize Windows modules"
fi

# Windows/WSL固有の依存関係チェック（最適化版）
check_windows_dependencies() {
    # モジュールが既に読み込み済みの場合は直接呼び出し
    if [[ -n "${LOADED_MODULES[powershell_engine]}" ]]; then
        check_windows_dependencies "$@"
    else
        log "ERROR" "PowerShell engine module not loaded"
        return 1
    fi
}

# PowerShell実行パスの検出（最適化版）
find_powershell_path() {
    if [[ -n "${LOADED_MODULES[powershell_engine]}" ]]; then
        find_powershell_path "$@"
    else
        log "ERROR" "PowerShell engine module not loaded"
        return 1
    fi
}

# Windows音声エンジンの検出（最適化版）
detect_windows_tts_voices() {
    if [[ -n "${LOADED_MODULES[windows_tts_engine]}" ]]; then
        detect_windows_tts_voices "$@"
    else
        echo "Microsoft Haruka Desktop" # フォールバック
        return 1
    fi
}

# 最適な日本語音声の選択（統合版 - windows_tts_engine.shに移行済み）
select_japanese_voice() {
    # windows_tts_engine.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_tts_engine.sh" 2>/dev/null || {
        echo "Microsoft Haruka Desktop" # フォールバック
        return 1
    }
    select_japanese_voice "$@"
}

# === Windows固有の音声合成 ===（統合版 - windows_tts_engine.shに移行済み）
speak_windows() {
    # windows_tts_engine.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_tts_engine.sh" 2>/dev/null || {
        log "WARN" "Windows TTS engine module not found"
        echo "[VOICE] $1"
        return 1
    }
    speak_windows "$@"
}

# 音声合成実行（後方互換性のため保持）
speak_text() {
    local text="$1"
    local voice="${2:-$(get_config "audio.default_voice" "auto")}"
    local device="${3:-auto}"                                # Windows では通常無視
    local rate="${4:-$(get_config "audio.speech_rate" "0")}" # -10 to 10

    log "DEBUG" "Speaking text on Windows: voice=$voice, rate=$rate"

    # 新しいWindows音声システムを呼び出し
    speak_windows "$text" "$voice"
    return $?
}

# 音声テキストの前処理（Windows向け）（統合版 - windows_tts_engine.shに移行済み）
preprocess_speech_text() {
    # windows_tts_engine.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_tts_engine.sh" 2>/dev/null || {
        log "WARN" "Windows TTS engine module not found"
        echo "$1"
        return 1
    }
    preprocess_speech_text "$@"
}

# システム通知の送信（統合版 - windows_notification_system.shに移行済み）
send_notification() {
    # windows_notification_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_notification_system.sh" 2>/dev/null || {
        log "WARN" "Windows notification system module not found"
        echo "通知: $1 - $2"
        return 1
    }
    send_notification "$@"
}

# Windows システム音の再生（統合版 - windows_audio_system.shに移行済み）
play_windows_sound() {
    # windows_audio_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_audio_system.sh" 2>/dev/null || {
        log "WARN" "Windows audio system module not found"
        return 1
    }
    play_windows_sound "$@"
}

# システムビープ音（統合版 - windows_audio_system.shに移行済み）
system_beep() {
    # windows_audio_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_audio_system.sh" 2>/dev/null || {
        log "WARN" "Windows audio system module not found"
        for ((i = 1; i <= count; i++)); do
            echo -e '\a'
            if [[ ${1:-1} -gt 1 && $i -lt ${1:-1} ]]; then
                sleep 0.3
            fi
        done
        return 1
    }
    system_beep "$@"
}

# Windows ビープのフォールバック（音声合成失敗時）（統合版 - windows_audio_system.shに移行済み）
windows_beep_fallback() {
    # windows_audio_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_audio_system.sh" 2>/dev/null || {
        log "WARN" "Windows audio system module not found"
        echo -e '\a'
        return 1
    }
    windows_beep_fallback "$@"
}

# 音量制御（Windows）（統合版 - windows_audio_system.shに移行済み）
set_system_volume() {
    # windows_audio_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_audio_system.sh" 2>/dev/null || {
        log "WARN" "Windows audio system module not found"
        return 1
    }
    set_system_volume "$@"
}

# 現在の音量取得（Windows）（統合版 - windows_audio_system.shに移行済み）
get_system_volume() {
    # windows_audio_system.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/windows_audio_system.sh" 2>/dev/null || {
        echo "50" # デフォルト値
        return 1
    }
    get_system_volume "$@"
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

# WSL クリップボード統合機能（統合版 - wsl_integration.shに移行済み）
wsl_clipboard_copy() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        log "WARN" "WSL integration module not found"
        return 1
    }
    wsl_clipboard_copy "$@"
}

# WSL クリップボードからの貼り付け（統合版 - wsl_integration.shに移行済み）
wsl_clipboard_paste() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        log "WARN" "WSL integration module not found"
        return 1
    }
    wsl_clipboard_paste "$@"
}

# WSL固有のシステム情報取得（統合版 - wsl_integration.shに移行済み）
get_wsl_info() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        echo "WSL information unavailable (module not found)"
        return 1
    }
    get_wsl_info "$@"
}

# WSL環境最適化（統合版 - wsl_integration.shに移行済み）
optimize_wsl_environment() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        log "WARN" "WSL integration module not found"
        return 1
    }
    optimize_wsl_environment "$@"
}

# WSL環境でのフォールバック通知（PowerShell不可時）（統合版 - wsl_integration.shに移行済み）
wsl_fallback_notification() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        log "INFO" "WSL integration module not found - using simple fallback"
        echo "🔊 Claude Voice Notification: $1"
        return 1
    }
    wsl_fallback_notification "$@"
}

# WSL環境での簡易システム情報取得（PowerShell不要版）（統合版 - wsl_integration.shに移行済み）
get_wsl_simple_info() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        echo "WSL information unavailable (module not found)"
        return 1
    }
    get_wsl_simple_info "$@"
}

# WSL用の軽量テスト（PowerShell不要）（統合版 - wsl_integration.shに移行済み）
test_wsl_basic_functions() {
    # wsl_integration.sh の関数を呼び出し
    source "${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}/core/wsl_integration.sh" 2>/dev/null || {
        echo "❌ WSL integration module not found for testing"
        return 1
    }
    test_wsl_basic_functions "$@"
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
