#!/bin/bash
# Cross-Platform Detection Engine for Claude Voice
# クロスプラットフォーム検出エンジン

# プラットフォーム検出結果をキャッシュする変数
DETECTED_PLATFORM=""
PLATFORM_CACHE_FILE="/tmp/.claude_platform_cache_$$"

# プラットフォーム検出関数
detect_platform() {
    # キャッシュが存在し、有効期間内（60秒）の場合はキャッシュを使用
    if [[ -f "$PLATFORM_CACHE_FILE" && $(( $(date +%s) - $(stat -c %Y "$PLATFORM_CACHE_FILE" 2>/dev/null || echo 0) )) -lt 60 ]]; then
        DETECTED_PLATFORM=$(cat "$PLATFORM_CACHE_FILE" 2>/dev/null)
        if [[ -n "$DETECTED_PLATFORM" ]]; then
            echo "$DETECTED_PLATFORM"
            return 0
        fi
    fi

    local platform=""
    local uname_s=$(uname -s)
    
    case "$uname_s" in
        "Linux")
            # WSL環境の検出
            if [[ -n "$WSL_DISTRO_NAME" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                platform="wsl"
            else
                platform="linux"
            fi
            ;;
        "Darwin")
            platform="macos"
            ;;
        "FreeBSD")
            platform="freebsd"
            ;;
        CYGWIN*|MINGW*|MSYS*)
            platform="windows"
            ;;
        *)
            platform="unknown"
            ;;
    esac
    
    # 結果をキャッシュ
    echo "$platform" > "$PLATFORM_CACHE_FILE"
    DETECTED_PLATFORM="$platform"
    echo "$platform"
}

# プラットフォーム固有のパスを取得
get_platform_path() {
    local platform=$(detect_platform)
    echo "$HOME/.tmux/claude/platforms/$platform"
}

# プラットフォーム固有のスクリプトパスを取得
get_platform_script() {
    local script_name="$1"
    local platform_path=$(get_platform_path)
    echo "$platform_path/$script_name"
}

# プラットフォーム固有機能の可用性チェック
check_platform_capability() {
    local capability="$1"
    local platform=$(detect_platform)
    
    case "$platform" in
        "wsl")
            case "$capability" in
                "audio") 
                    # PowerShellアクセスとWindows音声システム
                    command -v powershell.exe >/dev/null 2>&1
                    ;;
                "speech")
                    # Windows音声合成エンジン
                    [[ -f "$(get_platform_script "wsl_voice_engine_v2.sh")" ]]
                    ;;
                *) 
                    return 1
                    ;;
            esac
            ;;
        "macos")
            case "$capability" in
                "audio")
                    command -v afplay >/dev/null 2>&1
                    ;;
                "speech")
                    command -v say >/dev/null 2>&1
                    ;;
                *)
                    return 1
                    ;;
            esac
            ;;
        "linux")
            case "$capability" in
                "audio")
                    command -v aplay >/dev/null 2>&1 || command -v paplay >/dev/null 2>&1
                    ;;
                "speech")
                    command -v espeak >/dev/null 2>&1 || command -v festival >/dev/null 2>&1
                    ;;
                *)
                    return 1
                    ;;
            esac
            ;;
        *)
            return 1
            ;;
    esac
}

# プラットフォーム固有コマンドの実行
execute_platform_command() {
    local command_type="$1"
    shift
    local platform=$(detect_platform)
    
    case "$platform" in
        "wsl")
            case "$command_type" in
                "audio")
                    local platform_script=$(get_platform_script "wsl_voice_engine_v2.sh")
                    [[ -f "$platform_script" ]] && source "$platform_script" && play_sound "$@"
                    ;;
                "speech")
                    local platform_script=$(get_platform_script "wsl_voice_engine_v2.sh")
                    [[ -f "$platform_script" ]] && source "$platform_script" && speak "$@"
                    ;;
            esac
            ;;
        "macos")
            case "$command_type" in
                "audio")
                    [[ -f "$1" ]] && afplay "$1"
                    ;;
                "speech")
                    say "$@"
                    ;;
            esac
            ;;
        "linux")
            case "$command_type" in
                "audio")
                    if command -v paplay >/dev/null 2>&1; then
                        [[ -f "$1" ]] && paplay "$1"
                    elif command -v aplay >/dev/null 2>&1; then
                        [[ -f "$1" ]] && aplay "$1"
                    fi
                    ;;
                "speech")
                    if command -v espeak >/dev/null 2>&1; then
                        espeak "$@"
                    elif command -v festival >/dev/null 2>&1; then
                        echo "$@" | festival --tts
                    fi
                    ;;
            esac
            ;;
    esac
}

# メイン関数として実行された場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-detect}" in
        "detect")
            detect_platform
            ;;
        "path")
            get_platform_path
            ;;
        "script")
            get_platform_script "$2"
            ;;
        "check")
            check_platform_capability "$2"
            ;;
        "exec")
            shift
            execute_platform_command "$@"
            ;;
        *)
            echo "Usage: $0 {detect|path|script <name>|check <capability>|exec <command> [args...]}"
            exit 1
            ;;
    esac
fi