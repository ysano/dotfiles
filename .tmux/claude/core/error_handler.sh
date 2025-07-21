#!/bin/bash
# Error Handler - 統一エラーハンドリングシステム
# 標準化されたエラー処理、復旧戦略、ログ管理

# グローバル変数
declare -g ERROR_HANDLER_INITIALIZED="false"
declare -g -A ERROR_STATS=()
declare -g -A ERROR_RECOVERY_ATTEMPTS=()
declare -g ERROR_LOG_FILE=""
declare -g MAX_RECOVERY_ATTEMPTS=3

# エラーハンドラーの初期化
init_error_handler() {
    if [[ "$ERROR_HANDLER_INITIALIZED" == "true" ]]; then
        return 0
    fi

    # エラーログファイルの設定
    ERROR_LOG_FILE="${CLAUDE_LOG_DIR:-/tmp}/claude_voice_errors.log"
    mkdir -p "$(dirname "$ERROR_LOG_FILE")"

    # エラー統計の初期化
    ERROR_STATS["total_errors"]=0
    ERROR_STATS["recoverable_errors"]=0
    ERROR_STATS["critical_errors"]=0

    ERROR_HANDLER_INITIALIZED="true"
    log "DEBUG" "Error handler initialized"
}

# エラーコード定義
declare -g -A ERROR_CODES=(
    # 基盤エラー (1-99)
    ["MODULE_NOT_FOUND"]=1
    ["DEPENDENCY_MISSING"]=2
    ["INITIALIZATION_FAILED"]=3
    ["CONFIGURATION_ERROR"]=4
    ["PERMISSION_DENIED"]=5

    # PowerShellエラー (100-199)
    ["POWERSHELL_NOT_FOUND"]=101
    ["POWERSHELL_EXECUTION_FAILED"]=102
    ["POWERSHELL_POLICY_RESTRICTED"]=103
    ["POWERSHELL_TIMEOUT"]=104
    ["POWERSHELL_COM_ERROR"]=105

    # 音声エラー (200-299)
    ["TTS_ENGINE_FAILED"]=201
    ["VOICE_NOT_FOUND"]=202
    ["AUDIO_DEVICE_ERROR"]=203
    ["SPEECH_SYNTHESIS_FAILED"]=204
    ["VOLUME_CONTROL_FAILED"]=205

    # 通知エラー (300-399)
    ["NOTIFICATION_FAILED"]=301
    ["TOAST_NOT_SUPPORTED"]=302
    ["MESSAGEBOX_ERROR"]=303
    ["NOTIFICATION_QUEUE_FULL"]=304

    # WSLエラー (400-499)
    ["WSL_NOT_DETECTED"]=401
    ["WSL_INTEGRATION_FAILED"]=402
    ["CLIPBOARD_ERROR"]=403
    ["WINDOWS_BRIDGE_ERROR"]=404

    # システムエラー (500-599)
    ["RESOURCE_EXHAUSTED"]=501
    ["TIMEOUT_ERROR"]=502
    ["NETWORK_ERROR"]=503
    ["FILE_SYSTEM_ERROR"]=504
)

# 統一エラー報告関数
report_error() {
    local error_code="$1"
    local module_name="$2"
    local function_name="$3"
    local error_message="$4"
    local context="${5:-}"

    # エラーコードの解決
    local numeric_code="${ERROR_CODES[$error_code]:-999}"

    # エラーメッセージの標準化
    local formatted_message="[${module_name}:${function_name}] ${error_message}"
    if [[ -n "$context" ]]; then
        formatted_message="${formatted_message} (Context: ${context})"
    fi

    # ログ出力
    log "ERROR" "$formatted_message (Code: $error_code/$numeric_code)"

    # エラーログファイルへの記録
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR [$error_code/$numeric_code] [$module_name:$function_name] $error_message" >>"$ERROR_LOG_FILE"

    # 統計更新
    ((ERROR_STATS["total_errors"]++))

    # エラーの重要度判定
    if is_critical_error "$error_code"; then
        ((ERROR_STATS["critical_errors"]++))
        handle_critical_error "$error_code" "$module_name" "$function_name" "$error_message"
    else
        ((ERROR_STATS["recoverable_errors"]++))
    fi

    return "$numeric_code"
}

# 重要度判定
is_critical_error() {
    local error_code="$1"

    case "$error_code" in
        "MODULE_NOT_FOUND" | "DEPENDENCY_MISSING" | "INITIALIZATION_FAILED")
            return 0 # 重要
            ;;
        "POWERSHELL_NOT_FOUND" | "WSL_NOT_DETECTED")
            return 0 # 重要
            ;;
        "PERMISSION_DENIED" | "RESOURCE_EXHAUSTED")
            return 0 # 重要
            ;;
        *)
            return 1 # 一般的
            ;;
    esac
}

# 重要エラーの処理
handle_critical_error() {
    local error_code="$1"
    local module_name="$2"
    local function_name="$3"
    local error_message="$4"

    log "ERROR" "CRITICAL ERROR DETECTED: $error_code in $module_name:$function_name"

    # 緊急通知（可能な場合）
    if command -v notify-send >/dev/null 2>&1; then
        notify-send "Claude Voice Critical Error" "$error_message" --urgency=critical 2>/dev/null || true
    fi

    # 復旧戦略の実行
    attempt_error_recovery "$error_code" "$module_name" "$function_name"
}

# エラー復旧の試行
attempt_error_recovery() {
    local error_code="$1"
    local module_name="$2"
    local function_name="$3"

    local recovery_key="${module_name}:${function_name}:${error_code}"
    local attempts="${ERROR_RECOVERY_ATTEMPTS[$recovery_key]:-0}"

    if [[ $attempts -ge $MAX_RECOVERY_ATTEMPTS ]]; then
        log "ERROR" "Maximum recovery attempts reached for $recovery_key"
        return 1
    fi

    ((ERROR_RECOVERY_ATTEMPTS["$recovery_key"]++))
    log "INFO" "Attempting error recovery for $error_code (attempt $((attempts + 1)))"

    case "$error_code" in
        "MODULE_NOT_FOUND")
            # モジュール再検索の試行
            if [[ -n "${LOADED_MODULES:-}" ]]; then
                unset LOADED_MODULES["$module_name"] 2>/dev/null || true
                load_module "$module_name" false
            fi
            ;;
        "POWERSHELL_NOT_FOUND")
            # PowerShellパスの再検索
            unset POWERSHELL_PATH 2>/dev/null || true
            find_powershell_path >/dev/null 2>&1
            ;;
        "TTS_ENGINE_FAILED")
            # 音声エンジンのリセット
            unset TTS_VOICE_LIST 2>/dev/null || true
            unset SELECTED_JAPANESE_VOICE 2>/dev/null || true
            ;;
        "NOTIFICATION_FAILED")
            # 通知システムのリセット
            unset TOAST_NOTIFICATION_AVAILABLE 2>/dev/null || true
            ;;
        *)
            log "DEBUG" "No specific recovery strategy for $error_code"
            return 1
            ;;
    esac

    log "INFO" "Error recovery attempt completed for $error_code"
    return 0
}

# PowerShell関連エラーの統一処理
handle_powershell_error() {
    local result="$1"
    local module_name="$2"
    local function_name="$3"
    local context="${4:-}"

    case "$result" in
        *"execution policy"* | *"ExecutionPolicy"*)
            report_error "POWERSHELL_POLICY_RESTRICTED" "$module_name" "$function_name" "PowerShell execution policy restricted" "$context"
            return 103
            ;;
        *"timeout"* | *"Timeout"*)
            report_error "POWERSHELL_TIMEOUT" "$module_name" "$function_name" "PowerShell execution timeout" "$context"
            return 104
            ;;
        *"COM"* | *"ComObject"*)
            report_error "POWERSHELL_COM_ERROR" "$module_name" "$function_name" "PowerShell COM object error" "$context"
            return 105
            ;;
        "")
            report_error "POWERSHELL_EXECUTION_FAILED" "$module_name" "$function_name" "PowerShell execution failed - no output" "$context"
            return 102
            ;;
        *)
            report_error "POWERSHELL_EXECUTION_FAILED" "$module_name" "$function_name" "PowerShell execution failed: $result" "$context"
            return 102
            ;;
    esac
}

# 音声関連エラーの統一処理
handle_voice_error() {
    local error_type="$1"
    local error_detail="$2"
    local module_name="$3"
    local function_name="$4"

    case "$error_type" in
        "TTS_ERROR")
            report_error "SPEECH_SYNTHESIS_FAILED" "$module_name" "$function_name" "$error_detail"
            return 204
            ;;
        "VOICE_NOT_FOUND")
            report_error "VOICE_NOT_FOUND" "$module_name" "$function_name" "$error_detail"
            return 202
            ;;
        "SOUND_ERROR" | "BEEP_ERROR")
            report_error "AUDIO_DEVICE_ERROR" "$module_name" "$function_name" "$error_detail"
            return 203
            ;;
        "VOLUME_ERROR")
            report_error "VOLUME_CONTROL_FAILED" "$module_name" "$function_name" "$error_detail"
            return 205
            ;;
        *)
            report_error "TTS_ENGINE_FAILED" "$module_name" "$function_name" "$error_detail"
            return 201
            ;;
    esac
}

# 通知関連エラーの統一処理
handle_notification_error() {
    local error_type="$1"
    local error_detail="$2"
    local module_name="$3"
    local function_name="$4"

    case "$error_type" in
        "TOAST_ERROR")
            report_error "TOAST_NOT_SUPPORTED" "$module_name" "$function_name" "$error_detail"
            return 302
            ;;
        "MESSAGEBOX_ERROR")
            report_error "MESSAGEBOX_ERROR" "$module_name" "$function_name" "$error_detail"
            return 303
            ;;
        "BALLOON_ERROR")
            report_error "NOTIFICATION_FAILED" "$module_name" "$function_name" "$error_detail"
            return 301
            ;;
        *)
            report_error "NOTIFICATION_FAILED" "$module_name" "$function_name" "$error_detail"
            return 301
            ;;
    esac
}

# WSL関連エラーの統一処理
handle_wsl_error() {
    local error_type="$1"
    local error_detail="$2"
    local module_name="$3"
    local function_name="$4"

    case "$error_type" in
        "WSL_NOT_DETECTED")
            report_error "WSL_NOT_DETECTED" "$module_name" "$function_name" "$error_detail"
            return 401
            ;;
        "CLIPBOARD_ERROR")
            report_error "CLIPBOARD_ERROR" "$module_name" "$function_name" "$error_detail"
            return 403
            ;;
        "WINDOWS_BRIDGE_ERROR")
            report_error "WINDOWS_BRIDGE_ERROR" "$module_name" "$function_name" "$error_detail"
            return 404
            ;;
        *)
            report_error "WSL_INTEGRATION_FAILED" "$module_name" "$function_name" "$error_detail"
            return 402
            ;;
    esac
}

# モジュール読み込みエラーの統一処理
handle_module_error() {
    local module_name="$1"
    local error_detail="$2"
    local function_name="${3:-load_module}"

    if [[ "$error_detail" == *"not found"* ]] || [[ "$error_detail" == *"No such file"* ]]; then
        report_error "MODULE_NOT_FOUND" "module_loader" "$function_name" "Module not found: $module_name"
        return 1
    elif [[ "$error_detail" == *"dependency"* ]]; then
        report_error "DEPENDENCY_MISSING" "module_loader" "$function_name" "Module dependency missing: $module_name"
        return 2
    else
        report_error "INITIALIZATION_FAILED" "module_loader" "$function_name" "Module initialization failed: $module_name - $error_detail"
        return 3
    fi
}

# エラー統計取得
get_error_stats() {
    local format="${1:-text}"

    case "$format" in
        "json")
            echo "{"
            echo "  \"total_errors\": ${ERROR_STATS[total_errors]:-0},"
            echo "  \"recoverable_errors\": ${ERROR_STATS[recoverable_errors]:-0},"
            echo "  \"critical_errors\": ${ERROR_STATS[critical_errors]:-0},"
            echo "  \"recovery_attempts\": $(echo "${!ERROR_RECOVERY_ATTEMPTS[@]}" | wc -w),"
            echo "  \"log_file\": \"$ERROR_LOG_FILE\""
            echo "}"
            ;;
        "text")
            echo "Error Statistics:"
            echo "  Total Errors: ${ERROR_STATS[total_errors]:-0}"
            echo "  Recoverable Errors: ${ERROR_STATS[recoverable_errors]:-0}"
            echo "  Critical Errors: ${ERROR_STATS[critical_errors]:-0}"
            echo "  Recovery Attempts: $(echo "${!ERROR_RECOVERY_ATTEMPTS[@]}" | wc -w)"
            echo "  Error Log: $ERROR_LOG_FILE"
            ;;
    esac
}

# エラーログのクリーンアップ
cleanup_error_logs() {
    local days_to_keep="${1:-7}"

    if [[ -f "$ERROR_LOG_FILE" ]]; then
        # 指定日数より古いログエントリを削除
        local cutoff_date=$(date -d "$days_to_keep days ago" '+%Y-%m-%d')
        grep "^[0-9]" "$ERROR_LOG_FILE" | while read -r line; do
            local log_date=$(echo "$line" | cut -d' ' -f1)
            if [[ "$log_date" < "$cutoff_date" ]]; then
                continue
            fi
            echo "$line"
        done >"${ERROR_LOG_FILE}.tmp"

        mv "${ERROR_LOG_FILE}.tmp" "$ERROR_LOG_FILE"
        log "DEBUG" "Error log cleaned up (kept last $days_to_keep days)"
    fi
}

# 安全な関数実行（エラートラップ付き）
safe_execute() {
    local function_name="$1"
    local module_name="$2"
    shift 2

    # エラートラップの設定（無限ループ防止）
    local error_occurred=false
    local original_trap=$(trap -p ERR)

    trap 'error_occurred=true' ERR

    # 関数実行
    if "$function_name" "$@"; then
        # 成功時の処理
        trap "$original_trap" ERR
        return 0
    else
        local exit_code=$?
        if [[ "$error_occurred" == "true" ]]; then
            report_error "FUNCTION_EXECUTION_FAILED" "$module_name" "$function_name" "Function execution failed with exit code $exit_code"
        fi
        trap "$original_trap" ERR
        return $exit_code
    fi
}

# エラーハンドラーテスト
test_error_handler() {
    echo "=== Error Handler Test ==="

    # 初期化テスト
    if init_error_handler; then
        echo "✅ Error handler initialization successful"
    else
        echo "❌ Error handler initialization failed"
        return 1
    fi

    # エラー報告テスト
    report_error "MODULE_NOT_FOUND" "test_module" "test_function" "Test error message"
    echo "✅ Error reporting test completed"

    # PowerShellエラー処理テスト
    handle_powershell_error "execution policy restricted" "test_module" "test_function"
    echo "✅ PowerShell error handling test completed"

    # 統計表示
    echo ""
    get_error_stats "text"

    echo ""
    echo "Error handler test completed"
    return 0
}

# エラーハンドラーの自動初期化
if [[ -z "$ERROR_HANDLER_INITIALIZED" ]] || [[ "$ERROR_HANDLER_INITIALIZED" != "true" ]]; then
    init_error_handler
fi
