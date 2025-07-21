#!/bin/bash
# Module Loader - モジュール依存関係管理システム
# 重複読み込み防止とパフォーマンス最適化

# グローバル変数 - 読み込み済みモジュール追跡
declare -g -A LOADED_MODULES=()
declare -g MODULE_LOADER_INITIALIZED="false"

# モジュールローダーの初期化
init_module_loader() {
    if [[ "$MODULE_LOADER_INITIALIZED" == "true" ]]; then
        return 0
    fi

    # 基本パス設定
    export CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
    export CLAUDE_CORE_DIR="$CLAUDE_VOICE_HOME/core"
    export CLAUDE_OS_DIR="$CLAUDE_VOICE_HOME/os"

    MODULE_LOADER_INITIALIZED="true"
    log "DEBUG" "Module loader initialized"
}

# 安全なモジュール読み込み
load_module() {
    local module_name="$1"
    local required="${2:-true}"

    if [[ -z "$module_name" ]]; then
        log "ERROR" "Module name not provided"
        return 1
    fi

    # 既に読み込み済みかチェック
    if [[ -n "${LOADED_MODULES[$module_name]}" ]]; then
        log "DEBUG" "Module $module_name already loaded"
        return 0
    fi

    # モジュールパスの解決
    local module_path
    module_path=$(resolve_module_path "$module_name")

    if [[ ! -f "$module_path" ]]; then
        if [[ "$required" == "true" ]]; then
            log "ERROR" "Required module not found: $module_name at $module_path"
            return 1
        else
            log "WARN" "Optional module not found: $module_name"
            return 1
        fi
    fi

    # モジュール読み込み実行（エラーハンドリング統合版）
    local load_error=""
    if load_error=$(source "$module_path" 2>&1); then
        LOADED_MODULES["$module_name"]="$module_path"
        log "DEBUG" "Module loaded successfully: $module_name"
        return 0
    else
        # 統合エラーハンドリングを使用（可能な場合）
        if declare -f handle_module_error >/dev/null 2>&1; then
            handle_module_error "$module_name" "$load_error" "load_module"
            return $?
        else
            # フォールバック: 従来のエラーハンドリング
            log "ERROR" "Failed to load module: $module_name from $module_path - $load_error"
            return 1
        fi
    fi
}

# モジュールパス解決
resolve_module_path() {
    local module_name="$1"

    # 相対パス指定の場合
    if [[ "$module_name" == *"/"* ]]; then
        echo "$CLAUDE_VOICE_HOME/$module_name"
        return 0
    fi

    # 標準的なモジュール名の場合
    local possible_paths=(
        "$CLAUDE_CORE_DIR/${module_name}.sh"
        "$CLAUDE_CORE_DIR/$module_name"
        "$CLAUDE_OS_DIR/${module_name}.sh"
        "$CLAUDE_OS_DIR/$module_name"
        "$CLAUDE_VOICE_HOME/${module_name}.sh"
        "$CLAUDE_VOICE_HOME/$module_name"
    )

    for path in "${possible_paths[@]}"; do
        if [[ -f "$path" ]]; then
            echo "$path"
            return 0
        fi
    done

    # 見つからない場合はcore内の推測パス
    echo "$CLAUDE_CORE_DIR/${module_name}.sh"
}

# 複数モジュールの一括読み込み
load_modules() {
    local modules=("$@")
    local failed_modules=()

    for module in "${modules[@]}"; do
        if ! load_module "$module" false; then
            failed_modules+=("$module")
        fi
    done

    if [[ ${#failed_modules[@]} -gt 0 ]]; then
        log "WARN" "Failed to load modules: ${failed_modules[*]}"
        return 1
    fi

    return 0
}

# 必須モジュールの読み込み
require_module() {
    local module_name="$1"
    load_module "$module_name" true
}

# オプションモジュールの読み込み
load_optional_module() {
    local module_name="$1"
    load_module "$module_name" false
}

# Windows関連モジュール群の読み込み（依存関係解決付き）
load_windows_modules() {
    log "DEBUG" "Loading Windows module suite with dependency resolution"

    # 依存関係解決システムを読み込み
    if ! load_module "dependency_resolver" false; then
        log "DEBUG" "Dependency resolver not available, using fallback loading"
        load_windows_modules_fallback
        return $?
    fi

    # 関数が利用可能になったことを確認
    if ! declare -F load_modules_with_dependencies >/dev/null 2>&1; then
        log "DEBUG" "Dependency resolution function not loaded, using fallback"
        load_windows_modules_fallback
        return $?
    fi

    local windows_modules=(
        "powershell_engine"
        "windows_tts_engine"
        "windows_audio_system"
        "windows_notification_system"
        "wsl_integration"
    )

    # 依存関係を考慮した順序で読み込み
    if command -v load_modules_with_dependencies >/dev/null 2>&1; then
        load_modules_with_dependencies "${windows_modules[@]}"
    else
        log "WARN" "Advanced dependency resolution not available, using fallback"
        load_windows_modules_fallback
    fi
}

# Windows関連モジュール群の読み込み（フォールバック）
load_windows_modules_fallback() {
    log "DEBUG" "Loading Windows modules with fallback method"

    local windows_modules=(
        "powershell_engine"
        "windows_tts_engine"
        "windows_audio_system"
        "windows_notification_system"
        "wsl_integration"
    )

    # PowerShellエンジンを最初に読み込み（他のモジュールの依存関係）
    if ! require_module "powershell_engine"; then
        log "ERROR" "Failed to load critical PowerShell engine module"
        return 1
    fi

    # 残りのWindowsモジュールを読み込み
    local remaining_modules=("${windows_modules[@]:1}")
    load_modules "${remaining_modules[@]}"
}

# WSL関連モジュール群の読み込み
load_wsl_modules() {
    log "DEBUG" "Loading WSL module suite"

    local wsl_modules=(
        "wsl_integration"
        "wsl_voice_engine"
    )

    load_modules "${wsl_modules[@]}"
}

# 音声関連モジュール群の読み込み
load_voice_modules() {
    log "DEBUG" "Loading voice module suite"

    local voice_modules=(
        "voice_engine_registry"
        "universal_voice"
    )

    # 音声エンジンレジストリを最初に読み込み
    if ! require_module "voice_engine_registry"; then
        log "ERROR" "Failed to load voice engine registry"
        return 1
    fi

    # 残りの音声モジュールを読み込み
    load_optional_module "universal_voice"
}

# 読み込み済みモジュール情報取得
get_loaded_modules() {
    local format="${1:-text}"

    case "$format" in
        "json")
            echo "{"
            local first=true
            for module in "${!LOADED_MODULES[@]}"; do
                if [[ "$first" == "true" ]]; then
                    first=false
                else
                    echo ","
                fi
                echo "  \"$module\": \"${LOADED_MODULES[$module]}\""
            done
            echo "}"
            ;;
        "text")
            local module_count=0
            if [[ -n "${LOADED_MODULES[*]:-}" ]]; then
                module_count=${#LOADED_MODULES[@]}
            fi
            echo "Loaded Modules ($module_count):"
            if [[ $module_count -gt 0 ]]; then
                for module in "${!LOADED_MODULES[@]}"; do
                    echo "  - $module: ${LOADED_MODULES[$module]}"
                done
            fi
            ;;
        "count")
            if [[ -n "${LOADED_MODULES[*]:-}" ]]; then
                echo "${#LOADED_MODULES[@]}"
            else
                echo "0"
            fi
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}

# モジュールアンロード（テスト用）
unload_module() {
    local module_name="$1"

    if [[ -n "${LOADED_MODULES[$module_name]}" ]]; then
        unset LOADED_MODULES["$module_name"]
        log "DEBUG" "Module unloaded: $module_name"
        return 0
    else
        log "WARN" "Module not loaded: $module_name"
        return 1
    fi
}

# 全モジュールアンロード（テスト用）
unload_all_modules() {
    LOADED_MODULES=()
    MODULE_LOADER_INITIALIZED="false"
    log "DEBUG" "All modules unloaded"
}

# モジュール依存関係チェック
check_module_dependencies() {
    local module_name="$1"

    case "$module_name" in
        "windows_tts_engine" | "windows_audio_system" | "windows_notification_system" | "wsl_integration")
            if [[ -z "${LOADED_MODULES[powershell_engine]}" ]]; then
                log "WARN" "Module $module_name requires powershell_engine"
                return 1
            fi
            ;;
        "universal_voice")
            if [[ -z "${LOADED_MODULES[voice_engine_registry]}" ]]; then
                log "WARN" "Module $module_name requires voice_engine_registry"
                return 1
            fi
            ;;
    esac

    return 0
}

# 高レベルプラットフォーム初期化
init_platform_modules() {
    local platform="${1:-auto}"

    # 基本モジュールを最初に読み込み
    require_module "base" || {
        log "ERROR" "Failed to load base module"
        return 1
    }

    # プラットフォーム検出
    if [[ "$platform" == "auto" ]]; then
        if [[ -f /proc/version ]] && grep -qi microsoft /proc/version 2>/dev/null; then
            platform="wsl"
        elif [[ "$OSTYPE" == "darwin"* ]]; then
            platform="macos"
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            platform="linux"
        else
            platform="unknown"
        fi
    fi

    log "INFO" "Initializing modules for platform: $platform"

    # プラットフォーム別モジュール読み込み
    case "$platform" in
        "wsl")
            load_windows_modules
            load_wsl_modules
            ;;
        "windows")
            load_windows_modules
            ;;
        "macos" | "linux")
            load_voice_modules
            ;;
        *)
            log "WARN" "Unknown platform: $platform"
            load_voice_modules # フォールバック
            ;;
    esac

    log "INFO" "Platform module initialization completed"
    return 0
}

# モジュールローダーテスト
test_module_loader() {
    echo "=== Module Loader Test ==="

    # 初期化テスト
    if init_module_loader; then
        echo "✅ Module loader initialization successful"
    else
        echo "❌ Module loader initialization failed"
        return 1
    fi

    # 基本読み込みテスト
    if load_module "base"; then
        echo "✅ Base module loading successful"
    else
        echo "❌ Base module loading failed"
    fi

    # 重複読み込みテスト
    if load_module "base"; then
        echo "✅ Duplicate loading prevention working"
    else
        echo "❌ Duplicate loading prevention failed"
    fi

    # 存在しないモジュールテスト
    if ! load_optional_module "nonexistent_module"; then
        echo "✅ Nonexistent module handling working"
    else
        echo "❌ Nonexistent module handling failed"
    fi

    # 読み込み状況表示
    echo ""
    get_loaded_modules "text"

    echo ""
    echo "Module loader test completed"
    return 0
}

# モジュールローダーの自動初期化
if [[ -z "$MODULE_LOADER_INITIALIZED" ]] || [[ "$MODULE_LOADER_INITIALIZED" != "true" ]]; then
    init_module_loader
fi
