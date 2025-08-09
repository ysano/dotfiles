#!/bin/bash
# Dependency Injection Enabled Status Detector
# 依存性注入対応のステータス検出器

# インターフェース抽象化レイヤーの読み込み
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/interfaces.sh"

# デバッグログ関数
debug_log() {
    [[ "${CLAUDE_STATUS_DEBUG:-}" == "1" ]] && echo "[DEBUG] $1" >&2
}

# === コア検出ロジック（純粋関数） ===

# Claude Code ステータス検出（依存性なし）
detect_claude_status_pure() {
    local output="$1"
    
    debug_log "Analyzing output: ${#output} chars"
    
    # UI コンテキストの抽出（現在は簡略化）
    local ui_context="$output"
    debug_log "Analyzing UI context: ${#ui_context} chars"
    
    # 1. Waiting: ユーザー入力が必要（最優先）
    if echo "$ui_context" | grep -qE '(Do you want|Would you like|Continue\?|Proceed\?|❯.*Yes|Error:|Failed:|Exception:)'; then
        debug_log "Detected: Waiting (input required)"
        echo "Waiting"
        return
    fi
    
    # 2. Busy: トークン処理中で中断可能
    # 安定したパターン: "tokens" と "esc to interrupt" の組み合わせ
    # 例: ✽ Imagining… (108s · ⚒ 1.5k tokens · esc to interrupt)
    if echo "$ui_context" | grep -qE 'tokens.*esc to interrupt'; then
        debug_log "Detected: Busy (tokens + esc to interrupt pattern)"
        echo "Busy"
        return
    fi
    
    # 3. Idle: 入力準備完了（プロンプトパターン）
    if echo "$ui_context" | grep -qE '>\s*$'; then
        debug_log "Detected: Idle (prompt ready)"
        echo "Idle"
        return
    fi
    
    # デフォルトはIdle
    debug_log "Detected: Idle (default fallback)"
    echo "Idle"
}

# Claude Code セッション確認（純粋関数）
is_claude_code_session_pure() {
    local output="$1"
    
    # Claude Code UI要素のチェック
    if echo "$output" | grep -qE '(╭─|╰─|\? for shortcuts|claude\.ai|claude code)'; then
        debug_log "Claude Code UI confirmed"
        return 0
    fi
    
    debug_log "No Claude Code UI found"
    return 1
}

# ステータスアイコン取得（純粋関数）
get_state_icon_pure() {
    case "$1" in
        "Busy") echo "⚡" ;;
        "Waiting") echo "⌛" ;;
        "Idle") echo "✅" ;;
        *) echo "" ;;
    esac
}

# === 依存性注入対応のメイン検出器 ===

# Claude Code ステータス検出器（依存性注入対応）
detect_claude_status_with_deps() {
    local window_id="${1:-$(call_tmux get_window_id)}"
    local pane_id="${2:-$(call_tmux get_pane_id)}"
    
    debug_log "Starting Claude status detection for window $window_id, pane $pane_id"
    
    # ステップ1: Claude プロセスの確認
    local claude_process=$(call_process find_process "claude")
    if [[ -z "$claude_process" ]]; then
        debug_log "No Claude process found globally"
        echo ""
        return 0
    fi
    
    # ステップ2: pane PIDの取得
    local pane_pid=$(call_tmux get_pane_pid "$window_id" "$pane_id")
    if [[ -z "$pane_pid" ]]; then
        debug_log "Failed to get pane PID"
        echo ""
        return 0
    fi
    
    # ステップ3: プロセスツリーでClaudeを確認
    local pane_processes=$(call_process get_process_tree "$pane_pid")
    if ! echo "$pane_processes" | grep -q "claude"; then
        debug_log "Claude not found in pane process tree"
        echo ""
        return 0
    fi
    
    # ステップ4: ターミナル出力のキャプチャ
    local terminal_output=$(call_tmux capture_pane "$window_id" "$pane_id" 30)
    if [[ -z "$terminal_output" ]]; then
        debug_log "Failed to capture terminal output"
        echo ""
        return 0
    fi
    
    debug_log "Captured ${#terminal_output} chars of terminal output"
    
    # ステップ5: Claude Code セッションの確認とステータス検出
    if ! is_claude_code_session_pure "$terminal_output"; then
        debug_log "Not a Claude Code session"
        echo ""
        return 0
    fi
    
    # ステップ6: ステータス検出
    local status=$(detect_claude_status_pure "$terminal_output")
    debug_log "Detected status: $status"
    
    # ステップ7: アイコンの出力
    local status_icon=$(get_state_icon_pure "$status")
    if [[ -n "$status_icon" ]]; then
        echo "$status_icon"
    else
        echo ""
    fi
    
    debug_log "Final output: $status_icon"
}

# === バックワード互換性関数 ===

# 既存コードとの互換性のため
detect_claude_status() {
    if [[ $# -eq 1 ]]; then
        # 新しい純粋関数版（テスト用）
        detect_claude_status_pure "$1"
    else
        # 依存性注入版（実際の使用）
        detect_claude_status_with_deps "$@"
    fi
}

get_state_icon() {
    get_state_icon_pure "$1"
}

is_claude_code_session() {
    is_claude_code_session_pure "$1"
}

# === テスト用ユーティリティ ===

# テスト用のモック設定
setup_test_mocks() {
    # モック関数の定義
    tmux_mock_get_window_id() { echo "1"; }
    tmux_mock_get_pane_id() { echo "0"; }
    tmux_mock_get_pane_pid() { echo "12345"; }
    tmux_mock_capture_pane() { echo "$MOCK_TERMINAL_OUTPUT"; }
    
    process_mock_find() { echo "54321"; }
    process_mock_tree() { echo "claude(54321)"; }
    
    # モックの登録
    register_mock_interface "tmux" "get_window_id" "tmux_mock_get_window_id"
    register_mock_interface "tmux" "get_pane_id" "tmux_mock_get_pane_id"
    register_mock_interface "tmux" "get_pane_pid" "tmux_mock_get_pane_pid"
    register_mock_interface "tmux" "capture_pane" "tmux_mock_capture_pane"
    register_mock_interface "process" "find_process" "process_mock_find"
    register_mock_interface "process" "get_process_tree" "process_mock_tree"
}

# テスト後のクリーンアップ
cleanup_test_mocks() {
    reset_interface "all"
    unset MOCK_TERMINAL_OUTPUT
}

# テスト実行例
run_self_test() {
    echo "=== Status Detector Self Test ==="
    
    # テスト1: 純粋関数のテスト
    echo "Test 1: Pure function tests"
    # 新しいパターンと旧パターンの両方をテスト
    local test_busy_new="✽ Imagining… (108s · ⚒ 1.5k tokens · esc to interrupt)"
    local result=$(detect_claude_status_pure "$test_busy_new")
    echo "  Busy pattern (new): $result (expected: Busy)"
    
    local test_busy_old="✻ Ruminating… (6s · 2.8k tokens · esc to interrupt)"
    result=$(detect_claude_status_pure "$test_busy_old")
    echo "  Busy pattern (old): $result (expected: Busy)"
    
    local test_waiting="Do you want to proceed?\n❯ 1. Yes\n  2. No"
    result=$(detect_claude_status_pure "$test_waiting")
    echo "  Waiting pattern: $result (expected: Waiting)"
    
    local test_idle="> \n? for shortcuts"
    result=$(detect_claude_status_pure "$test_idle")
    echo "  Idle pattern: $result (expected: Idle)"
    
    # テスト2: 依存性注入のテスト
    echo "Test 2: Dependency injection test"
    setup_test_mocks
    export MOCK_TERMINAL_OUTPUT="✽ Imagining… (5s · ⚒ 100 tokens · esc to interrupt)"
    
    result=$(detect_claude_status_with_deps)
    echo "  With mocks: $result (expected: ⚡)"
    
    cleanup_test_mocks
    echo "Self test completed"
}

# このスクリプトが直接実行された場合はセルフテストを実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    run_self_test
fi