#!/bin/bash
# Claude Code Status Detection - Enhanced Precision v4.0
# 過去の高精度実装を統合し、検出精度を大幅改善

set -euo pipefail

# === 設定 ===
readonly CACHE_FILE="/tmp/.claude_status_cache_$$"
readonly CACHE_DURATION=1  # 1秒キャッシュ（レスポンス性向上）
readonly DEBUG="${CLAUDE_STATUS_DEBUG:-0}"

# === デバッグ関数 ===
debug_log() {
    [[ "$DEBUG" == "1" ]] && echo "[DEBUG] $(date '+%H:%M:%S') $1" >&2
}

# === キャッシュ管理 ===
is_cache_valid() {
    [[ -f "$CACHE_FILE" ]] && \
    [[ $(( $(date +%s) - $(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0) )) -lt $CACHE_DURATION ]]
}

# === 強化されたClaude Code検出 ===
is_claude_code_session() {
    local output="$1"
    local pane_pid="$2"
    
    # 方法1: プロセス名チェック（最も確実）
    if [[ -n "$pane_pid" ]]; then
        local process_tree=$(pstree -p "$pane_pid" 2>/dev/null || echo "")
        if echo "$process_tree" | grep -q "claude"; then
            debug_log "Claude process confirmed in process tree"
            return 0
        fi
    fi
    
    # 方法2: UI要素の詳細チェック
    if echo "$output" | grep -qE '(╭─|╰─|\? for shortcuts|claude\.ai|Claude Code|Generated with.*Claude)'; then
        debug_log "Claude Code UI elements confirmed"
        return 0
    fi
    
    # 方法3: 特徴的なプロンプトパターン
    if echo "$output" | grep -qE '(tokens.*interrupt|esc to interrupt|Finagling|Ruminating)'; then
        debug_log "Claude Code behavior patterns confirmed"
        return 0
    fi
    
    debug_log "Not a Claude Code session"
    return 1
}

# === 3状態検出の高精度実装 ===
detect_claude_status_precision() {
    local output="$1"
    local lines="${2:-50}"
    
    debug_log "Analyzing ${#output} chars of output"
    
    # 出力を最新部分と全体に分けて分析
    local recent_output=$(echo "$output" | tail -10)
    local context_output=$(echo "$output" | tail -30)
    
    debug_log "Recent output (${#recent_output} chars): $(echo "$recent_output" | tr '\n' ' ' | cut -c1-100)..."
    
    # === 1. BUSY状態の検出（処理中） ===
    
    # パターン1: アクティブな処理状態（トークン生成中）
    if echo "$recent_output" | grep -qE '\([0-9]+s\s*[·•]\s*[0-9,.]+[km]?\s*tokens\s*[·•]\s*(esc to interrupt|interrupt)\)'; then
        debug_log "BUSY: Active token processing detected"
        echo "Busy"
        return
    fi
    
    # パターン2: 処理中のメッセージ
    if echo "$recent_output" | grep -qE '(Finagling|Ruminating|Thinking|Processing|Working|Generating|Analyzing)\.\.\.' || \
       echo "$recent_output" | grep -qE '✻\s*(Finagling|Ruminating|Thinking|Processing)'; then
        debug_log "BUSY: Processing message detected"
        echo "Busy"
        return
    fi
    
    # パターン3: ツール実行中
    if echo "$recent_output" | grep -qE '(Running|Executing|Using)\s+(bash|edit|read|grep)' || \
       echo "$recent_output" | grep -qE 'Tool:\s*(bash|edit|read)'; then
        debug_log "BUSY: Tool execution detected"
        echo "Busy"
        return
    fi
    
    # === 2. WAITING状態の検出（ユーザー入力待ち） ===
    
    # パターン1: 明示的な選択肢プロンプト
    if echo "$recent_output" | grep -qE '(Do you want|Would you like|Should I|Continue\?|Proceed\?)' && \
       echo "$recent_output" | grep -qE '❯\s*[0-9]+\.\s*(Yes|No|Continue|Stop)'; then
        debug_log "WAITING: Choice prompt detected"
        echo "Waiting"
        return
    fi
    
    # パターン2: プラン承認待ち
    if echo "$recent_output" | grep -qE 'plan mode.*exit.*approve' && \
       ! echo "$recent_output" | grep -qE '(plan mode.*on|auto-accept.*on)' && \
       echo "$recent_output" | grep -qE '>\s*$'; then
        debug_log "WAITING: Plan approval needed"
        echo "Waiting"
        return
    fi
    
    # パターン3: エラー状態での入力待ち
    if echo "$recent_output" | grep -qE '(Error|Failed|Exception).*:' && \
       echo "$recent_output" | grep -qE '(❯|>)\s*$'; then
        debug_log "WAITING: Error state requiring input"
        echo "Waiting"
        return
    fi
    
    # パターン4: ファイル選択やパス入力待ち
    if echo "$recent_output" | grep -qE '(Select.*file|Enter.*path|Choose.*option)' && \
       echo "$recent_output" | grep -qE '>\s*$'; then
        debug_log "WAITING: File/path selection"
        echo "Waiting"
        return
    fi
    
    # パターン5: インタラクティブモード（実際の入力待ち）
    if echo "$recent_output" | grep -qE '(❯.*)?>\s*$' && \
       ! echo "$recent_output" | grep -qE '(Finagling|tokens.*interrupt|completed|finished)'; then
        # 処理中でない場合のみ入力待ちとみなす
        local has_processing=$(echo "$context_output" | grep -cE '(Finagling|tokens.*interrupt|Working)')
        if [[ "$has_processing" -eq 0 ]]; then
            debug_log "WAITING: Interactive input prompt"
            echo "Waiting"
            return
        fi
    fi
    
    # === 3. IDLE状態の検出（完了・待機） ===
    
    # パターン1: 明示的な完了メッセージ
    if echo "$recent_output" | grep -qE '(✅.*完了|✅.*completed|Task completed|Successfully|Finished)'; then
        debug_log "IDLE: Task completion detected"
        echo "Idle"
        return
    fi
    
    # パターン2: ショートカット案内付きの待機状態
    if echo "$recent_output" | grep -qE '\?\s*for shortcuts' && \
       echo "$recent_output" | grep -qE '>\s*$'; then
        debug_log "IDLE: Ready with shortcuts available"
        echo "Idle"
        return
    fi
    
    # パターン3: 詳細トランスクリプト表示（完了状態）
    if echo "$recent_output" | grep -qE '(Showing detailed transcript|Ctrl\+R to toggle)'; then
        debug_log "IDLE: Transcript display mode"
        echo "Idle"
        return
    fi
    
    # パターン4: 単純なプロンプト（処理なし）
    if echo "$recent_output" | grep -qE '>\s*$' && \
       ! echo "$context_output" | grep -qE '(Finagling|tokens.*interrupt|Do you want|Would you like)'; then
        debug_log "IDLE: Simple prompt ready"
        echo "Idle"
        return
    fi
    
    # === デフォルト状態の決定 ===
    
    # 最新の行にプロンプトがある場合はIdle
    local last_line=$(echo "$output" | tail -1)
    if echo "$last_line" | grep -qE '>\s*$'; then
        debug_log "IDLE: Default - prompt available"
        echo "Idle"
        return
    fi
    
    # 処理の痕跡がある場合はBusy
    if echo "$context_output" | grep -qE '(Finagling|tokens|Working|Processing)'; then
        debug_log "BUSY: Default - processing traces found"
        echo "Busy"
        return
    fi
    
    # 最終的なデフォルト
    debug_log "IDLE: Final default"
    echo "Idle"
}

# === ステータスアイコン変換 ===
get_status_icon() {
    case "$1" in
        "Busy") echo "⚡" ;;
        "Waiting") echo "⌛" ;;
        "Idle") echo "✅" ;;
        *) echo "" ;;
    esac
}

# === メイン検出関数 ===
detect_claude_status() {
    local window_id="${1:-$(tmux display-message -p '#I' 2>/dev/null || echo "1")}"
    local pane_id="${2:-$(tmux display-message -p '#P' 2>/dev/null || echo "0")}"
    local mode="${3:-icon}"
    
    debug_log "Starting precision detection for window $window_id, pane $pane_id"
    
    # キャッシュチェック
    if [[ "$mode" != "force" ]] && is_cache_valid; then
        local cached_result=$(cat "$CACHE_FILE" 2>/dev/null)
        debug_log "Cache hit: $cached_result"
        echo "$cached_result"
        return 0
    fi
    
    # Claude プロセスの全体確認
    local claude_processes=$(pgrep -f "claude" 2>/dev/null | wc -l)
    if [[ "$claude_processes" -eq 0 ]]; then
        debug_log "No Claude processes found globally"
        echo ""
        return 0
    fi
    
    # tmux pane情報の取得
    local pane_target="${window_id}.${pane_id}"
    local pane_pid=$(tmux display-message -t "$pane_target" -p '#{pane_pid}' 2>/dev/null)
    
    if [[ -z "$pane_pid" ]]; then
        debug_log "Failed to get pane PID for $pane_target"
        echo ""
        return 0
    fi
    
    # ターミナル出力の取得（より多くの行を取得）
    local terminal_output=$(tmux capture-pane -p -S -50 -t "$pane_target" 2>/dev/null)
    
    if [[ -z "$terminal_output" ]]; then
        debug_log "Failed to capture terminal output"
        echo ""
        return 0
    fi
    
    debug_log "Captured ${#terminal_output} chars from $pane_target"
    
    # Claude Code セッションの確認
    if ! is_claude_code_session "$terminal_output" "$pane_pid"; then
        debug_log "Not a Claude Code session"
        echo ""
        return 0
    fi
    
    # 高精度ステータス検出
    local status=$(detect_claude_status_precision "$terminal_output")
    debug_log "Detected status: $status"
    
    # アイコン変換
    local result=""
    if [[ "$mode" == "text" ]]; then
        result="$status"
    else
        result=$(get_status_icon "$status")
    fi
    
    # 結果をキャッシュして出力
    echo "$result" | tee "$CACHE_FILE"
    debug_log "Final result: $result"
}

# === テスト機能 ===
run_precision_test() {
    echo "=== Claude Status Detection Precision Test ==="
    
    local test_cases=(
        "✻ Finagling… (15s · 2.8k tokens · esc to interrupt)|Busy|⚡"
        "Do you want to proceed?\n❯ 1. Yes\n  2. No|Waiting|⌛"
        "> \n? for shortcuts|Idle|✅"
        "✅ Task completed successfully|Idle|✅"
        "Error: File not found\n> |Waiting|⌛"
        "Working on your request...|Busy|⚡"
    )
    
    for test_case in "${test_cases[@]}"; do
        IFS='|' read -r input expected_status expected_icon <<< "$test_case"
        local result=$(detect_claude_status_precision "$input")
        local icon=$(get_status_icon "$result")
        
        if [[ "$result" == "$expected_status" ]]; then
            echo "✅ PASS: $expected_status ($icon)"
        else
            echo "❌ FAIL: Expected $expected_status, got $result"
            echo "   Input: $(echo "$input" | tr '\n' ' ')"
        fi
    done
}

# === クリーンアップ ===
cleanup() {
    [[ -f "$CACHE_FILE" ]] && rm -f "$CACHE_FILE"
}

trap cleanup EXIT INT TERM

# === メイン実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-detect}" in
        "test")
            run_precision_test
            ;;
        "debug")
            export CLAUDE_STATUS_DEBUG=1
            detect_claude_status "${2:-}" "${3:-}" "${4:-}"
            ;;
        "force")
            detect_claude_status "${2:-}" "${3:-}" "force"
            ;;
        *)
            detect_claude_status "$@"
            ;;
    esac
fi