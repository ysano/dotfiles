#!/bin/bash
# Claude Status Detection Debug Tool
# ステータス検出のデバッグと精度検証ツール

set -euo pipefail

readonly CACHE_FILE="/tmp/.claude_status_debug_$$"

# デバッグレベル
DEBUG_LEVEL="${CLAUDE_DEBUG_LEVEL:-1}"

# デバッグログ関数
debug_log() {
    local level="$1"
    shift
    [[ "$DEBUG_LEVEL" -ge "$level" ]] && echo "[DEBUG-$level] $(date '+%H:%M:%S') $*" >&2
}

# Claude Code検出の詳細デバッグ
debug_claude_detection() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    
    echo "=== Claude Code Detection Debug ==="
    echo "Window: $window_id, Pane: $pane_id"
    echo
    
    # 1. プロセス確認
    echo "1. Process Detection:"
    local claude_processes=$(pgrep -f "claude" 2>/dev/null || echo "")
    if [[ -n "$claude_processes" ]]; then
        echo "   ✅ Claude processes found:"
        while read -r pid; do
            if [[ -n "$pid" ]]; then
                local cmd=$(ps -p "$pid" -o args= 2>/dev/null || echo "unknown")
                echo "      PID $pid: $cmd"
            fi
        done <<< "$claude_processes"
    else
        echo "   ❌ No Claude processes found"
        return 1
    fi
    echo
    
    # 2. Pane 情報
    echo "2. Pane Information:"
    local pane_target="${window_id}.${pane_id}"
    local pane_pid=$(tmux display-message -t "$pane_target" -p '#{pane_pid}' 2>/dev/null)
    local pane_command=$(tmux display-message -t "$pane_target" -p '#{pane_current_command}' 2>/dev/null)
    
    if [[ -n "$pane_pid" ]]; then
        echo "   ✅ Pane PID: $pane_pid"
        echo "   ✅ Current command: $pane_command"
        
        # プロセスツリー確認
        local process_tree=$(pstree -p "$pane_pid" 2>/dev/null || echo "")
        if echo "$process_tree" | grep -q "claude"; then
            echo "   ✅ Claude found in process tree"
            echo "      Tree: $process_tree"
        else
            echo "   ❌ Claude NOT found in process tree"
            echo "      Tree: $process_tree"
        fi
    else
        echo "   ❌ Failed to get pane PID"
        return 1
    fi
    echo
    
    # 3. ターミナル出力の取得と分析
    echo "3. Terminal Output Analysis:"
    local terminal_output=$(tmux capture-pane -p -S -50 -t "$pane_target" 2>/dev/null)
    
    if [[ -n "$terminal_output" ]]; then
        echo "   ✅ Captured ${#terminal_output} characters"
        
        # Claude Code UI要素チェック
        echo "   UI Elements Check:"
        local ui_elements=('╭─' '╰─' '\? for shortcuts' 'claude\.ai' 'Claude Code' 'tokens.*interrupt')
        for element in "${ui_elements[@]}"; do
            if echo "$terminal_output" | grep -qE "$element"; then
                echo "      ✅ Found: $element"
            else
                echo "      ❌ Missing: $element"
            fi
        done
        
        # 最新の10行を表示
        echo "   Recent Output (last 10 lines):"
        echo "$terminal_output" | tail -10 | sed 's/^/      /'
        
    else
        echo "   ❌ Failed to capture terminal output"
        return 1
    fi
    echo
    
    # 4. ステータス検出テスト
    echo "4. Status Detection Test:"
    local recent_output=$(echo "$terminal_output" | tail -10)
    
    # 各パターンのチェック
    echo "   Pattern Matching:"
    
    # BUSY パターン
    if echo "$recent_output" | grep -qE '\([0-9]+s\s*[·•]\s*[0-9,.]+[km]?\s*tokens\s*[·•]\s*(esc to interrupt|interrupt)\)'; then
        echo "      ✅ BUSY: Token processing pattern found"
    elif echo "$recent_output" | grep -qE '(Finagling|Ruminating|Thinking|Processing|Working)\.\.\.|✻\s*(Finagling|Ruminating)'; then
        echo "      ✅ BUSY: Processing message pattern found"
    else
        echo "      ❌ BUSY: No busy patterns found"
    fi
    
    # WAITING パターン
    if echo "$recent_output" | grep -qE '(Do you want|Would you like|Should I|Continue\?|Proceed\?)' && \
       echo "$recent_output" | grep -qE '❯\s*[0-9]+\.\s*(Yes|No|Continue)'; then
        echo "      ✅ WAITING: Choice prompt pattern found"
    elif echo "$recent_output" | grep -qE 'plan mode.*exit.*approve' && \
         ! echo "$recent_output" | grep -qE '(plan mode.*on|auto-accept.*on)' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        echo "      ✅ WAITING: Plan approval pattern found"
    elif echo "$recent_output" | grep -qE '(Error|Failed|Exception).*:' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        echo "      ✅ WAITING: Error state pattern found"
    else
        echo "      ❌ WAITING: No waiting patterns found"
    fi
    
    # IDLE パターン
    if echo "$recent_output" | grep -qE '(✅.*完了|✅.*completed|Task completed|Successfully)'; then
        echo "      ✅ IDLE: Completion pattern found"
    elif echo "$recent_output" | grep -qE '\?\s*for shortcuts' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        echo "      ✅ IDLE: Shortcuts available pattern found"
    elif echo "$recent_output" | grep -qE '>\s*$' && \
         ! echo "$terminal_output" | tail -30 | grep -qE '(Finagling|tokens.*interrupt|Do you want)'; then
        echo "      ✅ IDLE: Prompt ready pattern found"
    else
        echo "      ❌ IDLE: No idle patterns found"
    fi
    echo
    
    # 5. 最終検出結果
    echo "5. Final Detection Result:"
    export CLAUDE_STATUS_DEBUG=1
    local detected_status=$(~/.tmux/scripts/claude-status-smart.sh "$window_id" "$pane_id")
    echo "   Detected Icon: '$detected_status'"
    
    # アイコンの意味
    case "$detected_status" in
        "⚡") echo "   Status: BUSY (Processing)" ;;
        "⌛") echo "   Status: WAITING (User input required)" ;;
        "✅") echo "   Status: IDLE (Ready/Completed)" ;;
        "") echo "   Status: NO CLAUDE CODE DETECTED" ;;
        *) echo "   Status: UNKNOWN ($detected_status)" ;;
    esac
}

# 複数の改善案を比較テスト
comparison_test() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    
    echo "=== Detection Algorithm Comparison ==="
    echo
    
    echo "1. Smart Detection (current):"
    local smart_result=$(~/.tmux/scripts/claude-status-smart.sh "$window_id" "$pane_id" 2>/dev/null)
    echo "   Result: '$smart_result'"
    
    echo "2. Enhanced Detection:"
    if [[ -f ~/.tmux/scripts/claude-status-enhanced.sh ]]; then
        local enhanced_result=$(~/.tmux/scripts/claude-status-enhanced.sh "$window_id" "$pane_id" 2>/dev/null)
        echo "   Result: '$enhanced_result'"
    else
        echo "   Not available"
    fi
    
    echo "3. Precision Detection:"
    if [[ -f ~/.tmux/scripts/claude-status-precision.sh ]]; then
        local precision_result=$(~/.tmux/scripts/claude-status-precision.sh "$window_id" "$pane_id" 2>/dev/null)
        echo "   Result: '$precision_result'"
    else
        echo "   Not available"
    fi
    
    echo "4. Core Detector (with dependencies):"
    if [[ -f ~/.tmux/claude/core/status_detector.sh ]]; then
        local core_result=$(bash ~/.tmux/claude/core/status_detector.sh 2>/dev/null)
        echo "   Result: '$core_result'"
    else
        echo "   Not available"
    fi
}

# パフォーマンス測定
performance_test() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    local iterations="${3:-10}"
    
    echo "=== Performance Test ($iterations iterations) ==="
    echo
    
    local scripts=(
        "~/.tmux/scripts/claude-status-smart.sh"
        "~/.tmux/scripts/claude-status-precision.sh"
        "~/.tmux/scripts/claude-status-enhanced.sh"
    )
    
    for script in "${scripts[@]}"; do
        if [[ -f "$script" ]]; then
            echo "Testing: $(basename "$script")"
            
            local start_time=$(date +%s%N)
            for ((i=1; i<=iterations; i++)); do
                "$script" "$window_id" "$pane_id" >/dev/null 2>&1
            done
            local end_time=$(date +%s%N)
            
            local duration_ms=$(( (end_time - start_time) / 1000000 ))
            local avg_ms=$(( duration_ms / iterations ))
            
            echo "   Total: ${duration_ms}ms, Average: ${avg_ms}ms"
        else
            echo "Skipping: $(basename "$script") (not found)"
        fi
    done
}

# クリーンアップ
cleanup() {
    [[ -f "$CACHE_FILE" ]] && rm -f "$CACHE_FILE"
}

trap cleanup EXIT INT TERM

# メイン実行
case "${1:-debug}" in
    "debug")
        debug_claude_detection "${2:-}" "${3:-}"
        ;;
    "compare")
        comparison_test "${2:-}" "${3:-}"
        ;;
    "performance")
        performance_test "${2:-}" "${3:-}" "${4:-10}"
        ;;
    "help")
        echo "Usage: $0 [debug|compare|performance|help] [window_id] [pane_id]"
        echo ""
        echo "Commands:"
        echo "  debug      - Detailed detection debug (default)"
        echo "  compare    - Compare different detection algorithms"
        echo "  performance - Performance testing"
        echo "  help       - Show this help"
        ;;
    *)
        echo "Unknown command: $1"
        echo "Use '$0 help' for usage information"
        exit 1
        ;;
esac