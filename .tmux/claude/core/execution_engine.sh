#!/bin/bash
# Execution Engine Module - 実行エンジン機能
# メイン実行ロジック、引数検証、音声サブシステム初期化を担当

# === 実行エンジン機能 ===

# メイン処理ワークフローの実行
main_execution_workflow() {
    local summary_type="${1:-brief}"
    local lines="${2:-${DEFAULT_LINES:-50}}"
    local voice="${3:-${DEFAULT_VOICE:-auto}}"
    local model="${4:-${DEFAULT_MODEL:-auto}}"
    local device="${5:-${DEFAULT_DEVICE:-system_default}}"

    log "INFO" "Starting claude-voice execution workflow"
    log "DEBUG" "Parameters: type=$summary_type, lines=$lines, voice=$voice, model=$model, device=$device"

    # 引数の検証
    if ! validate_execution_arguments "$summary_type" "$lines" "$voice" "$model"; then
        log "ERROR" "Argument validation failed"
        return 1
    fi

    # 音声サブシステムの初期化
    if ! initialize_audio_subsystem; then
        log "ERROR" "Failed to initialize audio subsystem"
        return 1
    fi

    # 実行ワークフローのタイマー開始
    local start_time=$(start_execution_timer)

    # ワークフロー実行
    if execute_core_workflow "$summary_type" "$lines" "$voice" "$model" "$device"; then
        local total_duration=$(end_execution_timer "$start_time")
        log "INFO" "Workflow completed successfully in ${total_duration}s"

        # 統計記録
        record_execution_stats "$summary_type" "$model" "$(detect_os)" "$total_duration" "true"

        # クリーンアップ
        cleanup_execution_environment
        return 0
    else
        local total_duration=$(end_execution_timer "$start_time")
        log "ERROR" "Workflow failed after ${total_duration}s"

        # 失敗統計記録
        record_execution_stats "$summary_type" "$model" "$(detect_os)" "$total_duration" "false"

        # エラー時クリーンアップ
        cleanup_execution_environment
        return 1
    fi
}

# コアワークフローの実行
execute_core_workflow() {
    local summary_type="$1"
    local lines="$2"
    local voice="$3"
    local model="$4"
    local device="$5"

    log "DEBUG" "Executing core workflow steps"

    # 1. 画面キャプチャ
    local screen_text
    if ! screen_text=$(execute_screen_capture "$lines"); then
        return 1
    fi

    # 2. コンテキスト情報の収集
    local context
    if ! context=$(execute_context_collection); then
        log "WARN" "Context collection failed, continuing without context"
        context=""
    fi

    # 3. 要約生成
    local summary
    if ! summary=$(execute_summary_generation "$screen_text" "$summary_type" "$model" "$context"); then
        return 1
    fi

    # 4. 音声出力
    if ! execute_voice_output "$summary" "$voice" "$device"; then
        log "WARN" "Voice output failed, displaying text instead"
        display_text_output "$summary"
    fi

    # 5. 完了通知
    display_completion_notification "$summary_type"

    return 0
}

# 画面キャプチャの実行
execute_screen_capture() {
    local lines="$1"

    echo "📺 画面内容を取得中..."
    log "DEBUG" "Starting screen capture with $lines lines"

    local screen_text
    if command -v capture_screen_text >/dev/null 2>&1; then
        if ! screen_text=$(capture_screen_text "." "$lines"); then
            local error_msg="画面内容の取得に失敗しました。"
            echo "❌ $error_msg"
            log "ERROR" "Screen capture failed"
            return 1
        fi
    else
        log "ERROR" "Screen capture function not available"
        echo "❌ 画面キャプチャ機能が利用できません"
        return 1
    fi

    local char_count=${#screen_text}
    echo "✅ ${char_count}文字のテキストを取得"
    log "DEBUG" "Captured $char_count characters"
    log "DEBUG" "Text preview: ${screen_text:0:100}..."

    echo "$screen_text"
    return 0
}

# コンテキスト情報の収集
execute_context_collection() {
    log "DEBUG" "Collecting context information"

    local context=""

    # Git情報の収集
    if command -v git >/dev/null 2>&1 && git rev-parse --git-dir >/dev/null 2>&1; then
        local git_branch=$(git branch --show-current 2>/dev/null || echo "unknown")
        local git_status=$(git status --porcelain 2>/dev/null | wc -l)
        context="Git: $git_branch (${git_status} changes)"
        log "DEBUG" "Git context: $context"
    fi

    # tmux情報の収集
    if [[ -n "${TMUX:-}" ]] && command -v tmux >/dev/null 2>&1; then
        local tmux_session=$(tmux display-message -p '#S' 2>/dev/null || echo "unknown")
        local tmux_window=$(tmux display-message -p '#W' 2>/dev/null || echo "unknown")
        local tmux_info="tmux: $tmux_session:$tmux_window"
        context="${context:+$context, }$tmux_info"
        log "DEBUG" "tmux context: $tmux_info"
    fi

    # 作業ディレクトリ情報
    local pwd_context="pwd: $(basename "$PWD")"
    context="${context:+$context, }$pwd_context"

    echo "$context"
    return 0
}

# 要約生成の実行
execute_summary_generation() {
    local screen_text="$1"
    local summary_type="$2"
    local model="$3"
    local context="$4"

    echo "🤖 ${model}で要約を生成中..."
    log "DEBUG" "Generating summary with model: $model, type: $summary_type"

    local summary
    if command -v generate_summary >/dev/null 2>&1; then
        if ! summary=$(generate_summary "$screen_text" "$summary_type" "$model"); then
            local error_msg="要約の生成に失敗しました。"
            echo "❌ $error_msg"
            log "ERROR" "Summary generation failed"
            return 1
        fi
    else
        log "ERROR" "Summary generation function not available"
        echo "❌ 要約生成機能が利用できません"
        return 1
    fi

    echo "✅ 要約生成完了"
    log "DEBUG" "Summary generated successfully (${#summary} characters)"

    echo "$summary"
    return 0
}

# 音声出力の実行
execute_voice_output() {
    local summary="$1"
    local voice="$2"
    local device="$3"

    echo "🔊 音声で読み上げ中..."
    log "DEBUG" "Starting voice output with voice: $voice, device: $device"

    # ユニバーサル音声システムを優先使用
    if command -v universal_speak >/dev/null 2>&1; then
        if universal_speak "$summary" "$voice"; then
            log "DEBUG" "Universal voice output successful"
            return 0
        else
            log "WARN" "Universal voice output failed, trying fallback"
        fi
    fi

    # OS固有の音声出力にフォールバック
    if command -v speak_text >/dev/null 2>&1; then
        if speak_text "$summary" "$voice" "$device"; then
            log "DEBUG" "OS-specific voice output successful"
            return 0
        else
            log "WARN" "OS-specific voice output failed"
        fi
    fi

    log "ERROR" "All voice output methods failed"
    return 1
}

# テキスト出力の表示
display_text_output() {
    local summary="$1"

    echo ""
    echo "📝 要約内容:"
    echo "----------------------------------------"
    echo "$summary"
    echo "----------------------------------------"
    log "INFO" "Displayed text output as fallback"
}

# 完了通知の表示
display_completion_notification() {
    local summary_type="$1"

    echo "✅ 処理完了 ($summary_type要約)"
    log "INFO" "Execution completed successfully"
}

# === 引数検証機能 ===

# 実行引数の検証
validate_execution_arguments() {
    local summary_type="$1"
    local lines="$2"
    local voice="$3"
    local model="$4"

    log "DEBUG" "Validating execution arguments"

    # 要約タイプの検証
    if ! validate_summary_type "$summary_type"; then
        return 1
    fi

    # 行数の検証
    if ! validate_lines_parameter "$lines"; then
        return 1
    fi

    # 音声パラメータの検証（オプション）
    if ! validate_voice_parameter "$voice"; then
        log "WARN" "Voice parameter validation failed, but continuing"
    fi

    # モデルパラメータの検証（オプション）
    if ! validate_model_parameter "$model"; then
        log "WARN" "Model parameter validation failed, but continuing"
    fi

    log "DEBUG" "All arguments validated successfully"
    return 0
}

# 要約タイプの検証
validate_summary_type() {
    local summary_type="$1"
    local valid_types=("brief" "detailed" "technical")

    for type in "${valid_types[@]}"; do
        if [[ "$summary_type" == "$type" ]]; then
            log "DEBUG" "Valid summary type: $summary_type"
            return 0
        fi
    done

    log "ERROR" "Invalid summary type: $summary_type (valid: ${valid_types[*]})"
    return 1
}

# 行数パラメータの検証
validate_lines_parameter() {
    local lines="$1"

    if ! [[ "$lines" =~ ^[0-9]+$ ]]; then
        log "ERROR" "Lines must be a number: $lines"
        return 1
    fi

    if [[ $lines -lt 1 ]] || [[ $lines -gt 1000 ]]; then
        log "ERROR" "Lines must be between 1 and 1000: $lines"
        return 1
    fi

    log "DEBUG" "Valid lines parameter: $lines"
    return 0
}

# 音声パラメータの検証
validate_voice_parameter() {
    local voice="$1"

    # "auto"は常に有効
    if [[ "$voice" == "auto" ]]; then
        log "DEBUG" "Using automatic voice selection"
        return 0
    fi

    # 空文字列チェック
    if [[ -z "$voice" ]]; then
        log "WARN" "Empty voice parameter, using auto"
        return 1
    fi

    # 特殊文字チェック（セキュリティ）
    if [[ "$voice" =~ [\;\&\|] ]]; then
        log "ERROR" "Voice parameter contains invalid characters"
        return 1
    fi

    log "DEBUG" "Voice parameter appears valid: $voice"
    return 0
}

# モデルパラメータの検証
validate_model_parameter() {
    local model="$1"

    # "auto"は常に有効
    if [[ "$model" == "auto" ]]; then
        log "DEBUG" "Using automatic model selection"
        return 0
    fi

    # 空文字列チェック
    if [[ -z "$model" ]]; then
        log "WARN" "Empty model parameter, using auto"
        return 1
    fi

    # 基本的な形式チェック（モデル名:タグ）
    if [[ "$model" =~ ^[a-zA-Z0-9._-]+(:latest|:[a-zA-Z0-9._-]+)?$ ]]; then
        log "DEBUG" "Model parameter appears valid: $model"
        return 0
    else
        log "WARN" "Model parameter format may be invalid: $model"
        return 1
    fi
}

# === 音声サブシステム初期化 ===

# 音声サブシステムの初期化
initialize_audio_subsystem() {
    local os_type=$(detect_os 2>/dev/null || echo "unknown")

    log "DEBUG" "Initializing audio subsystem for OS: $os_type"

    # ユニバーサル音声システムの初期化を優先
    if [[ -f "$CLAUDE_VOICE_HOME/core/universal_voice.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/universal_voice.sh" 2>/dev/null; then
            log "DEBUG" "Universal voice system loaded successfully"
            return 0
        else
            log "WARN" "Failed to load universal voice system"
        fi
    fi

    # OS固有の音声初期化にフォールバック
    case "$os_type" in
        "darwin")
            initialize_macos_audio
            ;;
        "linux")
            initialize_linux_audio
            ;;
        "windows")
            initialize_windows_audio
            ;;
        *)
            log "WARN" "Unknown OS type: $os_type, using basic audio initialization"
            initialize_basic_audio
            ;;
    esac
}

# macOS音声初期化
initialize_macos_audio() {
    log "DEBUG" "Initializing macOS audio"

    if command -v osascript >/dev/null 2>&1; then
        log "DEBUG" "macOS osascript available"
        return 0
    else
        log "ERROR" "macOS osascript not available"
        return 1
    fi
}

# Linux音声初期化
initialize_linux_audio() {
    log "DEBUG" "Initializing Linux audio"

    local audio_available=false

    if command -v espeak >/dev/null 2>&1; then
        log "DEBUG" "Linux espeak available"
        audio_available=true
    fi

    if command -v festival >/dev/null 2>&1; then
        log "DEBUG" "Linux festival available"
        audio_available=true
    fi

    if [[ "$audio_available" == "true" ]]; then
        return 0
    else
        log "WARN" "No Linux audio engines available"
        return 1
    fi
}

# Windows音声初期化
initialize_windows_audio() {
    log "DEBUG" "Initializing Windows audio"

    # WSL環境での音声初期化
    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        if [[ -f "$CLAUDE_VOICE_HOME/core/wsl_voice_engine.sh" ]]; then
            if source "$CLAUDE_VOICE_HOME/core/wsl_voice_engine.sh" 2>/dev/null; then
                local speech_status=$(check_windows_speech 2>/dev/null || echo "unavailable")
                if [[ "$speech_status" == "available" ]]; then
                    log "DEBUG" "WSL PowerShell Speech available"
                    return 0
                fi
            fi
        fi
    fi

    log "WARN" "Windows audio not available"
    return 1
}

# 基本音声初期化（フォールバック）
initialize_basic_audio() {
    log "DEBUG" "Using basic audio initialization (text output only)"
    return 0
}

# === タイマー機能 ===

# 実行タイマーの開始
start_execution_timer() {
    date +%s
}

# 実行タイマーの終了
end_execution_timer() {
    local start_time="$1"
    local end_time=$(date +%s)
    echo $((end_time - start_time))
}

# === 統計・クリーンアップ機能 ===

# 実行統計の記録
record_execution_stats() {
    local summary_type="$1"
    local model="$2"
    local os_type="$3"
    local duration="$4"
    local success="$5"

    # stats_monitor.shが利用可能な場合
    if [[ -f "$CLAUDE_VOICE_HOME/core/stats_monitor.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/stats_monitor.sh" 2>/dev/null; then
            record_usage_stats "$summary_type" "$model" "$os_type" "$duration" "$success"
            log "DEBUG" "Execution stats recorded"
            return 0
        fi
    fi

    # フォールバック統計記録
    log "INFO" "Execution completed: type=$summary_type, model=$model, duration=${duration}s, success=$success"
}

# 実行環境のクリーンアップ
cleanup_execution_environment() {
    log "DEBUG" "Cleaning up execution environment"

    # 一時ファイルのクリーンアップ
    if [[ -d "/tmp" ]]; then
        find /tmp -name "claude_voice_*" -mtime +1 -delete 2>/dev/null || true
    fi

    # プロセスのクリーンアップ
    if command -v claude_voice_cleanup >/dev/null 2>&1; then
        claude_voice_cleanup 2>/dev/null || true
    fi

    log "DEBUG" "Cleanup completed"
}

# === OS検出ユーティリティ ===

# OS検出関数（簡易版）
detect_os() {
    local os_type=$(uname -s)
    case "$os_type" in
        "Darwin")
            echo "darwin"
            ;;
        "Linux")
            if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
                echo "windows" # WSL
            else
                echo "linux"
            fi
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# === ログ機能 ===

# 簡易ログ関数（base.shが利用できない場合）
log() {
    local level="$1"
    local message="$2"

    if command -v logger >/dev/null 2>&1; then
        echo "[$level] $message" >&2
    fi
}

# === このモジュールが直接実行された場合のテスト ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # テスト用の環境変数設定
    CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
    DEFAULT_LINES=50
    DEFAULT_VOICE="auto"
    DEFAULT_MODEL="auto"
    DEFAULT_DEVICE="system_default"

    echo "Execution Engine Module Test"
    echo "============================"
    echo ""

    case "${1:-validate}" in
        "validate")
            echo "Testing argument validation..."
            if validate_execution_arguments "brief" "30" "auto" "phi4-mini:latest"; then
                echo "✅ Argument validation: PASSED"
            else
                echo "❌ Argument validation: FAILED"
            fi
            ;;
        "audio")
            echo "Testing audio subsystem initialization..."
            if initialize_audio_subsystem; then
                echo "✅ Audio initialization: PASSED"
            else
                echo "❌ Audio initialization: FAILED"
            fi
            ;;
        "timer")
            echo "Testing timer functions..."
            local start=$(start_execution_timer)
            sleep 1
            local duration=$(end_execution_timer "$start")
            echo "Timer test: ${duration}s (expected: ~1s)"
            ;;
        "context")
            echo "Testing context collection..."
            local context=$(execute_context_collection)
            echo "Context: $context"
            ;;
        "workflow")
            echo "Testing core workflow (dry run)..."
            echo "⚠️  This would execute the full workflow with real dependencies"
            echo "Use individual component tests instead"
            ;;
        *)
            echo "Available tests:"
            echo "  validate - Argument validation"
            echo "  audio    - Audio subsystem initialization"
            echo "  timer    - Timer functions"
            echo "  context  - Context collection"
            echo "  workflow - Full workflow (requires dependencies)"
            ;;
    esac
fi
