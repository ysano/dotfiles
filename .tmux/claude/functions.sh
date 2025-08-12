#!/bin/bash
# ファイル名: functions.sh
# 説明: tmux-claude-voice 基本機能関数群

# ログ機能（ファイル出力に変更してループを防止）
CLAUDE_VOICE_LOG_FILE="${TMPDIR:-/tmp}/tmux-claude-voice.log"

log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
}

log_debug() {
    if [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]]; then
        echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
    fi
}

# Claude Codeウィンドウの検出
detect_claude_windows() {
    local pattern="${1:-Claude|claude|CLAUDE}"
    
    log_debug "Claudeウィンドウを検索中: パターン='$pattern'"
    
    # tmuxウィンドウリストを取得し、パターンにマッチするウィンドウを検索
    local windows
    if windows=$(tmux list-windows -F "#{session_name}:#{window_index}:#{window_name}" 2>/dev/null); then
        local claude_windows
        claude_windows=$(echo "$windows" | grep -E ":.*($pattern)" | cut -d':' -f1,2)
        
        if [[ -n "$claude_windows" ]]; then
            log_debug "検出されたClaudeウィンドウ: $claude_windows"
            echo "$claude_windows"
        else
            log_debug "Claudeウィンドウが見つかりません"
            echo ""
        fi
    else
        log_error "tmuxウィンドウリストの取得に失敗しました"
        return 1
    fi
}

# ペインコンテンツからステータス判定
analyze_pane_content() {
    local session_window="$1"
    
    if [[ -z "$session_window" ]]; then
        log_error "セッション:ウィンドウが指定されていません"
        return 1
    fi
    
    log_debug "ペインコンテンツを解析中: $session_window"
    
    # tmux capture-paneでペインのコンテンツを取得
    local pane_content
    if pane_content=$(tmux capture-pane -t "$session_window" -p 2>/dev/null); then
        log_debug "ペインコンテンツを取得しました (文字数: ${#pane_content})"
        
        # ステータス判定（優先順位: Busy → Waiting → Idle）
        if echo "$pane_content" | grep -iq "tokens.*esc to interrupt"; then
            echo "Busy"
        elif echo "$pane_content" | grep -iE "(Do you want to proceed\?|Continue\?|Proceed\?|❯ [123]|Choose an option|tell Claude what|Should I|Would you like|Yes, and|No, keep|Error:|Failed:|Exception:)"; then
            echo "Waiting"
        else
            echo "Idle"
        fi
    else
        log_error "ペインコンテンツの取得に失敗しました: $session_window"
        return 1
    fi
}

# 前回の状態を取得
get_previous_status() {
    local session_window="$1"
    local option_name="@claude_voice_status_${session_window//[:\.]/_}"
    
    local previous_status
    previous_status=$(tmux show-option -gqv "$option_name" 2>/dev/null)
    
    if [[ -z "$previous_status" ]]; then
        echo "Unknown"
    else
        echo "$previous_status"
    fi
}

# 現在の状態を保存
save_current_status() {
    local session_window="$1"
    local current_status="$2"
    local option_name="@claude_voice_status_${session_window//[:\.]/_}"
    
    if tmux set-option -g "$option_name" "$current_status" 2>/dev/null; then
        log_debug "ステータスを保存しました: $session_window = $current_status"
    else
        log_error "ステータスの保存に失敗しました: $session_window"
        return 1
    fi
}

# ウィンドウアイコンの更新
update_window_icon() {
    local session_window="$1"
    local status="$2"
    
    if [[ -z "$session_window" || -z "$status" ]]; then
        log_error "ウィンドウアイコン更新: パラメータが不足しています"
        return 1
    fi
    
    # ステータスに対応するアイコン
    local icon
    case "$status" in
        "Busy")     icon="⚡" ;;
        "Waiting")  icon="⌛" ;;
        "Idle")     icon="✅" ;;
        *)          icon="❓" ;;
    esac
    
    # 現在のウィンドウ名を取得
    local current_name
    if current_name=$(tmux display-message -t "$session_window" -p "#{window_name}" 2>/dev/null); then
        # 既存のアイコンを削除（⚡⌛✅❓のいずれか）
        local clean_name
        clean_name=$(echo "$current_name" | sed -E 's/^[⚡⌛✅❓] *//')
        
        # 新しいアイコンとウィンドウ名を設定
        local new_name="${icon} ${clean_name}"
        
        if tmux rename-window -t "$session_window" "$new_name" 2>/dev/null; then
            log_debug "ウィンドウアイコンを更新しました: $session_window -> $new_name"
        else
            log_error "ウィンドウアイコンの更新に失敗しました: $session_window"
            return 1
        fi
    else
        log_error "現在のウィンドウ名の取得に失敗しました: $session_window"
        return 1
    fi
}

# 状態変化の処理
handle_status_change() {
    local session_window="$1"
    local previous_status="$2"
    local current_status="$3"
    
    log_info "ステータス変化を検出: $session_window ($previous_status -> $current_status)"
    
    # 通知モードを取得
    local notify_mode=$(tmux show-option -gqv @claude_voice_notify_mode 2>/dev/null)
    notify_mode="${notify_mode:-sound_summary}"
    
    # 音声有効化状態を取得
    local sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    sound_enabled="${sound_enabled:-true}"
    
    # 要約有効化状態を取得
    local summary_enabled=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
    summary_enabled="${summary_enabled:-true}"
    
    log_debug "通知モード: $notify_mode, 音声: $sound_enabled, 要約: $summary_enabled"
    
    # 状態変化に応じた通知処理
    case "$previous_status -> $current_status" in
        "Idle -> Busy" | "Waiting -> Busy")
            log_debug "処理開始通知をトリガー"
            if [[ "$sound_enabled" == "true" ]]; then
                # 開始通知音の再生
                if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
                    source "$SCRIPT_DIR/sound_utils.sh"
                    play_notification_sound "start" "$session_window" 2>/dev/null &
                fi
            fi
            ;;
        "Busy -> Idle")
            log_debug "処理完了通知をトリガー"
            if [[ "$sound_enabled" == "true" ]]; then
                # 完了通知音の再生
                if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
                    source "$SCRIPT_DIR/sound_utils.sh"
                    play_notification_sound "complete" "$session_window" 2>/dev/null &
                fi
            fi
            
            # 要約読み上げの処理
            if [[ "$summary_enabled" == "true" ]]; then
                handle_summary_reading "$session_window" "complete"
            fi
            ;;
        "Busy -> Waiting")
            log_debug "問い合わせ通知をトリガー"
            if [[ "$sound_enabled" == "true" ]]; then
                # 注意通知音の再生
                if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
                    source "$SCRIPT_DIR/sound_utils.sh"
                    play_notification_sound "waiting" "$session_window" 2>/dev/null &
                fi
            fi
            
            # 要約読み上げの処理
            if [[ "$summary_enabled" == "true" ]]; then
                handle_summary_reading "$session_window" "waiting"
            fi
            ;;
    esac
    
    return 0
}

# Claudeウィンドウの処理
process_claude_window() {
    local session_window="$1"
    
    if [[ -z "$session_window" ]]; then
        log_error "ウィンドウ処理: セッション:ウィンドウが指定されていません"
        return 1
    fi
    
    log_debug "Claudeウィンドウを処理中: $session_window"
    
    # 現在のステータスを解析
    local current_status
    if current_status=$(analyze_pane_content "$session_window"); then
        log_debug "現在のステータス: $current_status"
        
        # 前回のステータスを取得
        local previous_status
        previous_status=$(get_previous_status "$session_window")
        log_debug "前回のステータス: $previous_status"
        
        # ステータスが変化した場合の処理
        if [[ "$previous_status" != "$current_status" ]]; then
            # 状態変化を処理
            handle_status_change "$session_window" "$previous_status" "$current_status"
            
            # 現在のステータスを保存
            save_current_status "$session_window" "$current_status"
        fi
        
        # ウィンドウアイコンを更新
        update_window_icon "$session_window" "$current_status"
        
    else
        log_error "ステータス解析に失敗しました: $session_window"
        return 1
    fi
}

# 実行時間の測定
measure_execution_time() {
    local start_time=$(date +%s.%N)
    "$@"
    local exit_code=$?
    local end_time=$(date +%s.%N)
    local execution_time
    execution_time=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "0")
    log_debug "実行時間: ${execution_time}秒"
    return $exit_code
}

# 関数の安全実行
safe_execute() {
    local func_name="$1"
    shift
    
    log_debug "関数実行開始: $func_name"
    
    if "$func_name" "$@"; then
        log_debug "関数実行成功: $func_name"
        return 0
    else
        local exit_code=$?
        log_error "関数実行失敗: $func_name (終了コード: $exit_code)"
        return $exit_code
    fi
}

# テスト関数
test_functions() {
    echo "=== functions.sh 単体テスト開始 ==="
    
    # ログ機能のテスト
    echo "ログ機能テスト..."
    log_info "テスト情報ログ"
    log_error "テストエラーログ"
    
    # デバッグログのテスト
    echo "デバッグログテスト（通常）..."
    log_debug "通常時は出力されないデバッグログ"
    
    echo "デバッグログテスト（DEBUG有効）..."
    TMUX_CLAUDE_VOICE_DEBUG=1 log_debug "DEBUG有効時の出力テスト"
    
    # ステータス判定のテスト
    echo "ステータス判定テスト..."
    
    # Busyステータスのテスト
    local test_content_busy="tokens 500/1000 esc to interrupt"
    local status_busy
    if echo "$test_content_busy" | grep -iq "tokens.*esc to interrupt"; then
        status_busy="Busy"
    elif echo "$test_content_busy" | grep -iE "(Do you want to proceed\?|Continue\?|Proceed\?)"; then
        status_busy="Waiting"
    else
        status_busy="Idle"
    fi
    
    if [[ "$status_busy" == "Busy" ]]; then
        echo "✓ Busyステータス判定: 成功"
    else
        echo "✗ Busyステータス判定: 失敗 (実際: $status_busy)"
        return 1
    fi
    
    # Waitingステータスのテスト
    local test_content_waiting="Do you want to proceed?"
    local status_waiting
    if echo "$test_content_waiting" | grep -iq "tokens.*esc to interrupt"; then
        status_waiting="Busy"
    elif echo "$test_content_waiting" | grep -iE "(Do you want to proceed\?|Continue\?|Proceed\?)"; then
        status_waiting="Waiting"
    else
        status_waiting="Idle"
    fi
    
    if [[ "$status_waiting" == "Waiting" ]]; then
        echo "✓ Waitingステータス判定: 成功"
    else
        echo "✗ Waitingステータス判定: 失敗 (実際: $status_waiting)"
        return 1
    fi
    
    # Idleステータスのテスト
    local test_content_idle="Ready to help you with your next task."
    local status_idle
    if echo "$test_content_idle" | grep -iq "tokens.*esc to interrupt"; then
        status_idle="Busy"
    elif echo "$test_content_idle" | grep -iE "(Do you want to proceed\?|Continue\?|Proceed\?)"; then
        status_idle="Waiting"
    else
        status_idle="Idle"
    fi
    
    if [[ "$status_idle" == "Idle" ]]; then
        echo "✓ Idleステータス判定: 成功"
    else
        echo "✗ Idleステータス判定: 失敗 (実際: $status_idle)"
        return 1
    fi
    
    # 実行時間測定のテスト
    echo "実行時間測定テスト..."
    if command -v bc >/dev/null 2>&1; then
        TMUX_CLAUDE_VOICE_DEBUG=1 measure_execution_time sleep 0.1
        echo "✓ 実行時間測定: 成功"
    else
        echo "✓ 実行時間測定: スキップ (bc未インストール)"
    fi
    
    echo "=== functions.sh 単体テスト完了 ==="
    return 0
}

# 依存関係チェック機能
check_dependencies() {
    local deps=("tmux" "grep" "awk" "curl" "jq")
    local missing_deps=()
    
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" >/dev/null 2>&1; then
            missing_deps+=("$dep")
        fi
    done
    
    if [[ ${#missing_deps[@]} -eq 0 ]]; then
        return 0
    else
        log_error "依存関係が見つかりません: ${missing_deps[*]}"
        return 1
    fi
}

# 設定値バリデーション機能
validate_config() {
    local config_name="$1"
    local config_value="$2"
    local expected_type="$3"

    case "$expected_type" in
        "number")
            if ! [[ "$config_value" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
                log_error "設定値が数値ではありません: $config_name = $config_value"
                return 1
            fi
            ;;
        "boolean")
            if ! [[ "$config_value" =~ ^(true|false)$ ]]; then
                log_error "設定値が真偽値ではありません: $config_name = $config_value"
                return 1
            fi
            ;;
        "string")
            if [[ -z "$config_value" ]]; then
                log_error "設定値が空です: $config_name"
                return 1
            fi
            ;;
    esac

    return 0
}

# 要約読み上げ処理
handle_summary_reading() {
    local session_window="$1"
    local change_type="$2"  # complete または waiting
    
    log_debug "要約読み上げ処理開始: $session_window ($change_type)"
    
    # ペインコンテンツを取得
    local pane_content
    if ! pane_content=$(tmux capture-pane -t "$session_window" -p 2>/dev/null); then
        log_error "ペインコンテンツの取得に失敗: $session_window"
        return 1
    fi
    
    # 要約行数を取得
    local summary_lines=$(tmux show-option -gqv @claude_voice_summary_lines 2>/dev/null)
    summary_lines="${summary_lines:-20}"
    
    # 最後のN行を取得
    local last_lines=$(echo "$pane_content" | tail -n "$summary_lines")
    
    # Ollama連携スクリプトが存在するかチェック
    if [[ -f "$SCRIPT_DIR/ollama_utils.sh" ]]; then
        source "$SCRIPT_DIR/ollama_utils.sh"
        
        # 要約を生成
        local summary
        if summary=$(summarize_with_ollama "$last_lines" "$change_type"); then
            log_debug "要約生成成功: $summary"
            
            # 音声合成スクリプトが存在するかチェック
            if [[ -f "$SCRIPT_DIR/sound_utils.sh" ]]; then
                source "$SCRIPT_DIR/sound_utils.sh"
                
                # 要約を音声で読み上げ
                speak "$summary" 2>/dev/null &
                log_debug "要約読み上げ開始"
            else
                log_error "sound_utils.shが見つかりません"
                return 1
            fi
        else
            log_error "要約生成に失敗しました"
            return 1
        fi
    else
        log_error "ollama_utils.shが見つかりません"
        return 1
    fi
    
    return 0
}

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "$1" in
        "test")
            test_functions
            ;;
        "deps")
            if check_dependencies; then
                echo "✓ 基本依存関係: 満足"
                exit 0
            else
                echo "✗ 基本依存関係: 不足"
                exit 1
            fi
            ;;
        *)
            echo "このスクリプトは直接実行できません。polling_monitor.shから読み込んでください。"
            exit 1
            ;;
    esac
fi