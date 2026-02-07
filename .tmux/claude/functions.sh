#!/bin/bash
# ファイル名: functions.sh
# 説明: tmux-claude-voice 基本機能関数群

# 多重読み込み防止
[[ -n "$_CLAUDE_FUNCTIONS_LOADED" ]] && return 0 2>/dev/null
_CLAUDE_FUNCTIONS_LOADED=1

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

# Claude Codeペインの検出（プロセスベース、ペインレベル）
# 出力形式: session:window.pane（例: main:0.1）
detect_claude_panes() {
    log_debug "Claude Codeプロセスを検索中..."

    # すべてのペインを調査してClaude Codeの実行を検出
    local claude_panes=""
    local panes_list
    panes_list=$(tmux list-panes -a -F "#{session_name}:#{window_index}:#{pane_index} #{pane_current_command} #{pane_pid}" 2>/dev/null)

    while IFS=' ' read -r pane_info cmd pid; do
        if [[ -z "$pane_info" ]]; then
            continue
        fi

        local session=$(echo "$pane_info" | cut -d':' -f1)
        local window=$(echo "$pane_info" | cut -d':' -f2)
        local pane=$(echo "$pane_info" | cut -d':' -f3)
        local pane_target="${session}:${window}.${pane}"

        # Claude Code プロセスの検出
        # 1. コマンド名が "claude" なら即検出（最も確実）
        # 2. コマンド名が "node" の場合はペイン内容で判定（間接起動対応）
        if [[ "$cmd" == "claude" ]]; then
            log_debug "Claude Codeを検出(cmd): $pane_target (cmd=$cmd, pid=$pid)"
            if [[ -z "$claude_panes" ]]; then
                claude_panes="$pane_target"
            else
                claude_panes="$claude_panes\n$pane_target"
            fi
        elif [[ "$cmd" == "node" ]]; then
            local pane_content
            pane_content=$(tmux capture-pane -t "$pane_target" -p 2>/dev/null | head -50)

            # Claude Code特有のパターン（tokens, esc to interrupt, K tokens, claude.ai等）
            if echo "$pane_content" | grep -qE "(tokens.*esc to interrupt|K tokens|claude\.ai|Claude Code|⏺|⚡|⌛|✅|Update Todos|Update\(|esc to|▶|◀)"; then
                log_debug "Claude Codeを検出(content): $pane_target (cmd=$cmd, pid=$pid)"
                if [[ -z "$claude_panes" ]]; then
                    claude_panes="$pane_target"
                else
                    claude_panes="$claude_panes\n$pane_target"
                fi
            fi
        fi
    done <<< "$panes_list"

    if [[ -n "$claude_panes" ]]; then
        log_debug "検出されたClaude Codeペイン: $claude_panes"
        echo -e "$claude_panes" | sort -u
    else
        log_debug "Claude Codeプロセスが見つかりません"
        echo ""
    fi
}

# 後方互換ラッパー: ウィンドウレベルに集約して返す
# 出力形式: session:window（例: main:0）
detect_claude_windows() {
    detect_claude_panes | sed 's/\.[0-9]*$//' | sort -u
}

# ペインコンテンツからステータス判定
# 引数: session:window.pane 形式（session:window 形式も後方互換で受け付ける）
analyze_pane_content() {
    local pane_target="$1"

    if [[ -z "$pane_target" ]]; then
        log_error "ペインターゲットが指定されていません"
        return 1
    fi

    log_debug "ペインコンテンツを解析中: $pane_target"

    # tmux capture-paneでペインのコンテンツを取得
    local pane_content
    if pane_content=$(tmux capture-pane -t "$pane_target" -p 2>/dev/null); then
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
        log_error "ペインコンテンツの取得に失敗しました: $pane_target"
        return 1
    fi
}

# 前回の状態を取得
get_previous_status() {
    local pane_target="$1"
    local option_name="@claude_voice_status_${pane_target//[:\.]/_}"
    
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
    local pane_target="$1"
    local current_status="$2"
    local option_name="@claude_voice_status_${pane_target//[:\.]/_}"

    if tmux set-option -g "$option_name" "$current_status" 2>/dev/null; then
        log_debug "ステータスを保存しました: $pane_target = $current_status"
    else
        log_error "ステータスの保存に失敗しました: $pane_target"
        return 1
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