#!/bin/bash
# Voice Engine Registry - 音声エンジン統合管理
# 循環参照を避けるための中央レジストリ

# === Voice Engine Registry ===
readonly VOICE_ENGINE_REGISTRY_VERSION="1.0.0"

# スタンドアロン用の簡易log関数
if ! command -v log >/dev/null 2>&1; then
    log() { 
        local level="$1"
        local message="$2"
        echo "[$level] $message" >&2
    }
fi

# グローバルなエンジン設定
declare -gA VOICE_ENGINES
declare -gA ENGINE_AVAILABILITY

# === エンジン登録 ===
register_voice_engine() {
    local engine_name="$1"
    local engine_function="$2"
    local availability_check="$3"
    
    VOICE_ENGINES["$engine_name"]="$engine_function"
    ENGINE_AVAILABILITY["$engine_name"]="$availability_check"
    
    log "DEBUG" "Registered voice engine: $engine_name"
}

# === エンジン利用可能性確認 ===
is_engine_available() {
    local engine_name="$1"
    local check_function="${ENGINE_AVAILABILITY[$engine_name]}"
    
    if [[ -n "$check_function" ]]; then
        eval "$check_function" >/dev/null 2>&1
        return $?
    else
        return 1
    fi
}

# === 最適エンジン選択 ===
select_best_engine() {
    local os_type=$(uname)
    local engine_preferences=()
    
    log "DEBUG" "Selecting best engine for OS: $os_type"
    
    # WSL環境の検出
    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        log "DEBUG" "WSL environment detected"
        engine_preferences=(
            "wsl_powershell"
            "simple_fallback"
        )
    # macOS環境
    elif [[ "$os_type" == "Darwin" ]]; then
        log "DEBUG" "macOS environment detected"
        engine_preferences=(
            "osascript"
            "simple_fallback"
        )
    # Linux環境
    elif [[ "$os_type" == "Linux" ]]; then
        log "DEBUG" "Linux environment detected"
        engine_preferences=(
            "espeak"
            "festival"
            "simple_fallback"
        )
    else
        log "DEBUG" "Unknown environment, using fallback"
        engine_preferences=("simple_fallback")
    fi
    
    # 優先順位に従って利用可能なエンジンを選択
    for engine in "${engine_preferences[@]}"; do
        log "DEBUG" "Checking engine availability: $engine"
        if is_engine_available "$engine"; then
            log "DEBUG" "Selected engine: $engine"
            echo "$engine"
            return 0
        fi
    done
    
    # フォールバックエンジン
    log "WARN" "No engines available, using simple_fallback"
    echo "simple_fallback"
}

# === エンジン実行 ===
execute_voice_engine() {
    local engine_name="$1"
    local text="$2"
    local voice_setting="${3:-auto}"
    
    local engine_function="${VOICE_ENGINES[$engine_name]}"
    
    if [[ -n "$engine_function" ]]; then
        log "DEBUG" "Executing engine: $engine_name"
        eval "$engine_function" "'$text'" "'$voice_setting'"
        return $?
    else
        log "ERROR" "Engine not found: $engine_name"
        return 1
    fi
}

# === Built-in エンジン実装 ===

# WSL PowerShell エンジン
wsl_powershell_engine() {
    local text="$1"
    local voice_setting="${2:-auto}"
    
    # log関数が定義されていない場合は簡易版を提供
    if ! command -v log >/dev/null 2>&1; then
        log() { 
            local level="$1"
            local message="$2"
            echo "[$level] $message" >&2
        }
    fi
    
    # wsl_voice_engine.sh の関数を直接呼び出す
    source "$(dirname "${BASH_SOURCE[0]}")/wsl_voice_engine.sh"
    wsl_speak "$text" "$voice_setting"
}

# WSL PowerShell 利用可能性チェック
wsl_powershell_check() {
    # log関数が定義されていない場合は簡易版を提供
    if ! command -v log >/dev/null 2>&1; then
        log() { 
            local level="$1"
            local message="$2"
            echo "[$level] $message" >&2
        }
    fi
    
    source "$(dirname "${BASH_SOURCE[0]}")/wsl_voice_engine.sh"
    local result=$(check_windows_speech 2>/dev/null)
    [[ "$result" == "available" ]]
}

# macOS osascript エンジン
osascript_engine() {
    local text="$1"
    local voice_setting="${2:-Kyoko}"
    
    osascript -e "say \"$text\" using \"$voice_setting\""
}

# osascript 利用可能性チェック
osascript_check() {
    command -v osascript >/dev/null 2>&1
}

# Linux espeak エンジン
espeak_engine() {
    local text="$1"
    local voice_setting="${2:-ja}"
    
    espeak -v "$voice_setting" "$text" 2>/dev/null || espeak "$text"
}

# espeak 利用可能性チェック
espeak_check() {
    command -v espeak >/dev/null 2>&1
}

# Linux festival エンジン
festival_engine() {
    local text="$1"
    local voice_setting="${2:-auto}"
    
    echo "$text" | festival --tts
}

# festival 利用可能性チェック
festival_check() {
    command -v festival >/dev/null 2>&1
}

# Simple fallback エンジン
simple_fallback_engine() {
    local text="$1"
    local voice_setting="${2:-auto}"
    
    echo "[VOICE] $text"
    log "INFO" "Voice output (text): $text"
}

# Simple fallback 利用可能性チェック（常に利用可能）
simple_fallback_check() {
    return 0
}

# === レジストリ初期化 ===
init_voice_engine_registry() {
    log "INFO" "Initializing voice engine registry"
    
    # Built-in エンジンの登録
    register_voice_engine "wsl_powershell" "wsl_powershell_engine" "wsl_powershell_check"
    register_voice_engine "osascript" "osascript_engine" "osascript_check"
    register_voice_engine "espeak" "espeak_engine" "espeak_check"
    register_voice_engine "festival" "festival_engine" "festival_check"
    register_voice_engine "simple_fallback" "simple_fallback_engine" "simple_fallback_check"
    
    log "DEBUG" "Voice engine registry initialized with ${#VOICE_ENGINES[@]} engines"
}

# === 診断関数 ===
diagnose_engine_registry() {
    echo "=== Voice Engine Registry Diagnostics ==="
    echo ""
    
    echo "Registered Engines:"
    for engine in "${!VOICE_ENGINES[@]}"; do
        printf "  %-20s" "$engine"
        if is_engine_available "$engine"; then
            echo "✅ Available"
        else
            echo "❌ Unavailable"
        fi
    done
    
    echo ""
    local best_engine
    best_engine=$(select_best_engine)
    echo "Selected Engine: $best_engine"
    
    echo ""
    echo "=== End Registry Diagnostics ==="
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
    
    init_voice_engine_registry
    
    case "${1:-diagnose}" in
        "diagnose")
            diagnose_engine_registry
            ;;
        "test")
            init_voice_engine_registry
            best_engine=$(select_best_engine)
            echo "Testing best engine: $best_engine"
            execute_voice_engine "$best_engine" "音声エンジンレジストリのテストです"
            ;;
        *)
            echo "Usage: $0 {diagnose|test}"
            exit 1
            ;;
    esac
fi