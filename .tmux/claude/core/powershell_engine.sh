#!/bin/bash
# PowerShell Engine - PowerShell検出・実行・管理モジュール
# Windows/WSL環境でのPowerShell統合機能

# グローバル変数
declare -g POWERSHELL_PATH=""
declare -g POWERSHELL_VERSION=""
declare -g POWERSHELL_TYPE=""

# PowerShell実行パスの検出（統合版）
find_powershell_path() {
    log "DEBUG" "Searching for PowerShell executable"

    # キャッシュされたパスがあれば使用
    if [[ -n "$POWERSHELL_PATH" ]] && [[ -x "$POWERSHELL_PATH" ]]; then
        echo "$POWERSHELL_PATH"
        return 0
    fi

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
                    POWERSHELL_PATH="$expanded_path"
                    detect_powershell_info "$expanded_path"
                    echo "$expanded_path"
                    return 0
                fi
            done
        else
            # 通常のパス検索
            if [[ -f "$path" ]] && [[ -x "$path" ]]; then
                log "DEBUG" "Found PowerShell at: $path"
                POWERSHELL_PATH="$path"
                detect_powershell_info "$path"
                echo "$path"
                return 0
            elif command -v "$path" >/dev/null 2>&1; then
                local resolved_path=$(command -v "$path")
                log "DEBUG" "Found PowerShell via command: $path -> $resolved_path"
                POWERSHELL_PATH="$resolved_path"
                detect_powershell_info "$resolved_path"
                echo "$resolved_path"
                return 0
            fi
        fi
    done

    log "ERROR" "PowerShell executable not found"
    return 1
}

# PowerShell情報の検出
detect_powershell_info() {
    local ps_path="$1"

    if [[ -z "$ps_path" ]]; then
        return 1
    fi

    # バージョン情報取得
    local version_output
    version_output=$("$ps_path" -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null || echo "unknown")
    POWERSHELL_VERSION="$version_output"

    # PowerShellタイプの判定
    if [[ "$ps_path" == *"pwsh.exe"* ]]; then
        POWERSHELL_TYPE="PowerShell Core"
    elif [[ "$ps_path" == *"powershell.exe"* ]]; then
        POWERSHELL_TYPE="Windows PowerShell"
    else
        POWERSHELL_TYPE="Unknown"
    fi

    log "DEBUG" "PowerShell detected: $POWERSHELL_TYPE v$POWERSHELL_VERSION"
}

# PowerShell実行可能性チェック
check_powershell_execution() {
    local ps_path="${1:-$(find_powershell_path)}"

    if [[ -z "$ps_path" ]]; then
        log "ERROR" "PowerShell path not provided and not found"
        return 1
    fi

    # 基本実行テスト
    if ! "$ps_path" -Command "echo 'test'" >/dev/null 2>&1; then
        log "ERROR" "PowerShell execution test failed"
        return 1
    fi

    # 権限チェック
    local execution_policy
    execution_policy=$("$ps_path" -Command "Get-ExecutionPolicy" 2>/dev/null || echo "Unknown")

    if [[ "$execution_policy" == "Restricted" ]]; then
        log "WARN" "PowerShell execution policy is Restricted - some features may not work"
        return 2
    fi

    log "DEBUG" "PowerShell execution check passed (Policy: $execution_policy)"
    return 0
}

# PowerShellスクリプト実行（エラーハンドリング統合版）
execute_powershell_script() {
    local script="$1"
    local timeout="${2:-30}"
    local ps_path="${3:-$(find_powershell_path)}"

    # エラーハンドラーの読み込み
    if [[ -n "${LOADED_MODULES[error_handler]:-}" ]] || load_module "error_handler" false; then
        # 統合エラーハンドリングを使用
        if [[ -z "$ps_path" ]]; then
            report_error "POWERSHELL_NOT_FOUND" "powershell_engine" "execute_powershell_script" "PowerShell executable not available"
            return 101
        fi

        if [[ -z "$script" ]]; then
            report_error "CONFIGURATION_ERROR" "powershell_engine" "execute_powershell_script" "No PowerShell script provided"
            return 4
        fi

        log "DEBUG" "Executing PowerShell script (timeout: ${timeout}s)"

        # タイムアウト付き実行
        local result
        if command -v timeout >/dev/null 2>&1; then
            if ! result=$(timeout "$timeout" "$ps_path" -Command "$script" 2>&1); then
                handle_powershell_error "$result" "powershell_engine" "execute_powershell_script" "Script: ${script:0:50}..."
                return $?
            fi
        else
            if ! result=$("$ps_path" -Command "$script" 2>&1); then
                handle_powershell_error "$result" "powershell_engine" "execute_powershell_script" "Script: ${script:0:50}..."
                return $?
            fi
        fi

        echo "$result"
        return 0
    else
        # フォールバック: 従来のエラーハンドリング
        if [[ -z "$ps_path" ]]; then
            log "ERROR" "PowerShell not available for script execution"
            return 1
        fi

        if [[ -z "$script" ]]; then
            log "ERROR" "No PowerShell script provided"
            return 1
        fi

        log "DEBUG" "Executing PowerShell script (timeout: ${timeout}s)"

        # タイムアウト付き実行
        if command -v timeout >/dev/null 2>&1; then
            timeout "$timeout" "$ps_path" -Command "$script" 2>/dev/null
        else
            "$ps_path" -Command "$script" 2>/dev/null
        fi
    fi
}

# PowerShellファイル実行
execute_powershell_file() {
    local script_file="$1"
    local timeout="${2:-30}"
    local ps_path="${3:-$(find_powershell_path)}"

    if [[ -z "$ps_path" ]]; then
        log "ERROR" "PowerShell not available for file execution"
        return 1
    fi

    if [[ ! -f "$script_file" ]]; then
        log "ERROR" "PowerShell script file not found: $script_file"
        return 1
    fi

    log "DEBUG" "Executing PowerShell file: $script_file"

    # Windows形式のパスに変換
    local windows_path
    windows_path=$(wslpath -w "$script_file" 2>/dev/null || echo "$script_file")

    # タイムアウト付き実行
    if command -v timeout >/dev/null 2>&1; then
        timeout "$timeout" "$ps_path" -File "$windows_path" 2>/dev/null
    else
        "$ps_path" -File "$windows_path" 2>/dev/null
    fi
}

# PowerShell COM オブジェクト利用可能性チェック
check_powershell_com_support() {
    local ps_path="${1:-$(find_powershell_path)}"

    if [[ -z "$ps_path" ]]; then
        return 1
    fi

    # COM オブジェクト作成テスト
    local com_test_script='
    try {
        $shell = New-Object -ComObject Shell.Application
        if ($shell) { 
            Write-Output "COM_SUPPORTED"
            $shell = $null
        }
    } catch {
        Write-Output "COM_NOT_SUPPORTED"
    }
    '

    local result
    result=$(execute_powershell_script "$com_test_script" 10 "$ps_path")

    if [[ "$result" == "COM_SUPPORTED" ]]; then
        log "DEBUG" "PowerShell COM support verified"
        return 0
    else
        log "WARN" "PowerShell COM support not available"
        return 1
    fi
}

# PowerShell .NET Framework チェック
check_powershell_dotnet_support() {
    local ps_path="${1:-$(find_powershell_path)}"

    if [[ -z "$ps_path" ]]; then
        return 1
    fi

    # .NET Framework バージョンチェック
    local dotnet_test_script='
    try {
        $version = [System.Environment]::Version
        Write-Output "DOTNET_VERSION:$($version.ToString())"
        
        # System.Speech アセンブリチェック
        Add-Type -AssemblyName System.Speech -ErrorAction Stop
        Write-Output "SPEECH_AVAILABLE"
    } catch {
        Write-Output "SPEECH_NOT_AVAILABLE"
    }
    '

    local result
    result=$(execute_powershell_script "$dotnet_test_script" 10 "$ps_path")

    if echo "$result" | grep -q "SPEECH_AVAILABLE"; then
        log "DEBUG" "PowerShell .NET Speech support verified"
        return 0
    else
        log "WARN" "PowerShell .NET Speech support not available"
        return 1
    fi
}

# Windows/WSL固有の依存関係チェック（命名規則統一版）
check_powershell_dependencies() {
    local missing_deps=()
    local optional_deps=()

    # PowerShellの確認
    local powershell_path
    powershell_path=$(find_powershell_path)
    if [[ -z "$powershell_path" ]]; then
        missing_deps+=("PowerShell (Windows PowerShell or PowerShell Core)")
    else
        # PowerShell実行確認
        if ! check_powershell_execution "$powershell_path"; then
            missing_deps+=("PowerShell execution capability")
        fi

        # COM サポート確認
        if ! check_powershell_com_support "$powershell_path"; then
            optional_deps+=("PowerShell COM support (for advanced features)")
        fi

        # .NET Speech サポート確認
        if ! check_powershell_dotnet_support "$powershell_path"; then
            optional_deps+=("PowerShell .NET Speech support (for TTS)")
        fi
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

    # エラー報告
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log "ERROR" "Missing critical dependencies: ${missing_deps[*]}"
        return 1
    fi

    if [[ ${#optional_deps[@]} -gt 0 ]]; then
        log "WARN" "Missing optional dependencies: ${optional_deps[*]}"
    fi

    log "DEBUG" "PowerShell dependencies check passed"
    return 0
}

# 後方互換性のためのエイリアス（非推奨）
check_windows_dependencies() {
    log "WARN" "check_windows_dependencies() is deprecated, use check_powershell_dependencies()"
    check_powershell_dependencies "$@"
}

# PowerShell エンジン初期化
init_powershell_engine() {
    log "DEBUG" "Initializing PowerShell engine"

    # 依存関係チェック
    if ! check_windows_dependencies; then
        log "ERROR" "PowerShell engine initialization failed - dependencies not met"
        return 1
    fi

    # PowerShell情報表示
    if [[ -n "$POWERSHELL_PATH" ]]; then
        log "INFO" "PowerShell Engine: $POWERSHELL_TYPE v$POWERSHELL_VERSION"
        log "INFO" "PowerShell Path: $POWERSHELL_PATH"
    fi

    return 0
}

# PowerShell エンジン情報取得
get_powershell_info() {
    local format="${1:-json}"

    case "$format" in
        "json")
            cat <<EOF
{
    "path": "${POWERSHELL_PATH:-}",
    "version": "${POWERSHELL_VERSION:-}",
    "type": "${POWERSHELL_TYPE:-}",
    "available": $(if [[ -n "$POWERSHELL_PATH" ]]; then echo "true"; else echo "false"; fi)
}
EOF
            ;;
        "text")
            echo "PowerShell Engine Status:"
            echo "  Path: ${POWERSHELL_PATH:-Not found}"
            echo "  Version: ${POWERSHELL_VERSION:-Unknown}"
            echo "  Type: ${POWERSHELL_TYPE:-Unknown}"
            echo "  Available: $(if [[ -n "$POWERSHELL_PATH" ]]; then echo "Yes"; else echo "No"; fi)"
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}

# PowerShell エンジンテスト
test_powershell_engine() {
    echo "=== PowerShell Engine Test ==="

    # 基本検出テスト
    local ps_path
    ps_path=$(find_powershell_path)
    if [[ -n "$ps_path" ]]; then
        echo "✅ PowerShell detected: $ps_path"
    else
        echo "❌ PowerShell not found"
        return 1
    fi

    # 実行テスト
    if check_powershell_execution "$ps_path"; then
        echo "✅ PowerShell execution test passed"
    else
        echo "❌ PowerShell execution test failed"
        return 1
    fi

    # 機能テスト
    local test_script='Write-Output "Hello from PowerShell"'
    local result
    result=$(execute_powershell_script "$test_script" 5 "$ps_path")

    if [[ "$result" == "Hello from PowerShell" ]]; then
        echo "✅ PowerShell script execution test passed"
    else
        echo "❌ PowerShell script execution test failed"
        return 1
    fi

    # COM サポートテスト
    if check_powershell_com_support "$ps_path"; then
        echo "✅ PowerShell COM support available"
    else
        echo "⚠️  PowerShell COM support not available"
    fi

    # .NET Speech サポートテスト
    if check_powershell_dotnet_support "$ps_path"; then
        echo "✅ PowerShell .NET Speech support available"
    else
        echo "⚠️  PowerShell .NET Speech support not available"
    fi

    echo "PowerShell Engine test completed"
    get_powershell_info "text"

    return 0
}
