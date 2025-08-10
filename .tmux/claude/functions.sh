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
    
    # 音声通知の処理（後のフェーズで実装）
    case "$previous_status -> $current_status" in
        "Idle -> Busy" | "Waiting -> Busy")
            log_debug "処理開始通知をトリガー"
            # TODO: 開始通知音の再生
            ;;
        "Busy -> Idle")
            log_debug "処理完了通知をトリガー"
            # TODO: 完了通知音 + 要約の読み上げ
            ;;
        "Busy -> Waiting")
            log_debug "問い合わせ通知をトリガー"
            # TODO: 注意通知音 + 問い合わせ内容の要約読み上げ
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

# スクリプト実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    if [[ "$1" == "test" ]]; then
        test_functions
    else
        echo "このスクリプトは直接実行できません。main.shから読み込んでください。"
        exit 1
    fi
fi