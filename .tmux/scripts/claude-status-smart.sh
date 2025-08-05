#!/bin/bash
# Claude Status Smart Detection v2.0
# プラットフォーム分離対応の統合ステータス管理

# キャッシュ設定
readonly CACHE_FILE="/tmp/.claude_status_cache_$$"
readonly CACHE_DURATION=2  # 2秒キャッシュ

# プラットフォーム統合パス
readonly CORE_DIR="$HOME/.tmux/claude/core"
readonly PLATFORM_VOICE_ENGINE="$HOME/.tmux/claude/platforms/wsl/wsl_voice_engine_v2.sh"
readonly FALLBACK_VOICE_ENGINE="$HOME/.tmux/claude/platforms/wsl/wsl_voice_engine.sh"

# プラットフォーム検出器を読み込み
if [[ -f "$CORE_DIR/platform_detector.sh" ]]; then
    source "$CORE_DIR/platform_detector.sh"
fi

# キャッシュ有効性チェック
is_cache_valid() {
    [[ -f "$CACHE_FILE" ]] && \
    [[ $(( $(date +%s) - $(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0) )) -lt $CACHE_DURATION ]]
}

# 高精度Claude Codeステータス検出
detect_claude_status() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    
    # キャッシュチェック
    if is_cache_valid; then
        cat "$CACHE_FILE" 2>/dev/null
        return 0
    fi
    
    # Claude プロセスの確認
    local claude_processes=$(pgrep -f "claude" 2>/dev/null | wc -l)
    if [[ "$claude_processes" -eq 0 ]]; then
        echo ""
        return 0
    fi
    
    # pane情報の取得
    local pane_target="${window_id}.${pane_id}"
    local pane_pid=$(tmux display-message -t "$pane_target" -p '#{pane_pid}' 2>/dev/null)
    
    if [[ -z "$pane_pid" ]]; then
        echo ""
        return 0
    fi
    
    # プロセスツリーでClaude確認
    local process_tree=$(pstree -p "$pane_pid" 2>/dev/null || echo "")
    if ! echo "$process_tree" | grep -q "claude"; then
        echo ""
        return 0
    fi
    
    # ターミナル出力の詳細取得
    local terminal_output=$(tmux capture-pane -p -S -50 -t "$pane_target" 2>/dev/null)
    
    if [[ -z "$terminal_output" ]]; then
        echo ""
        return 0
    fi
    
    # Claude Code UI確認
    if ! echo "$terminal_output" | grep -qE '(╭─|╰─|\? for shortcuts|claude\.ai|Claude Code|tokens.*interrupt)'; then
        echo ""
        return 0
    fi
    
    # 高精度3状態検出
    local recent_output=$(echo "$terminal_output" | tail -10)
    local status_icon=""
    
    # BUSY: トークン処理中
    if echo "$recent_output" | grep -qE '\([0-9]+s\s*[·•]\s*[0-9,.]+[km]?\s*tokens\s*[·•]\s*(esc to interrupt|interrupt)\)'; then
        status_icon="⚡"
    # BUSY: 処理中メッセージ
    elif echo "$recent_output" | grep -qE '(Finagling|Ruminating|Thinking|Processing|Working)\.\.\.|✻\s*(Finagling|Ruminating)'; then
        status_icon="⚡"
    # WAITING: 選択肢プロンプト
    elif echo "$recent_output" | grep -qE '(Do you want|Would you like|Should I|Continue\?|Proceed\?)' && \
         echo "$recent_output" | grep -qE '❯\s*[0-9]+\.\s*(Yes|No|Continue)'; then
        status_icon="⌛"
    # WAITING: プラン承認待ち
    elif echo "$recent_output" | grep -qE 'plan mode.*exit.*approve' && \
         ! echo "$recent_output" | grep -qE '(plan mode.*on|auto-accept.*on)' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        status_icon="⌛"
    # WAITING: エラー状態
    elif echo "$recent_output" | grep -qE '(Error|Failed|Exception).*:' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        status_icon="⌛"
    # IDLE: 完了状態
    elif echo "$recent_output" | grep -qE '(✅.*完了|✅.*completed|Task completed|Successfully)'; then
        status_icon="✅"
    # IDLE: ショートカット案内
    elif echo "$recent_output" | grep -qE '\?\s*for shortcuts' && \
         echo "$recent_output" | grep -qE '>\s*$'; then
        status_icon="✅"
    # IDLE: プロンプト待機
    elif echo "$recent_output" | grep -qE '>\s*$' && \
         ! echo "$terminal_output" | tail -30 | grep -qE '(Finagling|tokens.*interrupt|Do you want)'; then
        status_icon="✅"
    else
        # デフォルト: 最新行にプロンプトがあればIdle
        local last_line=$(echo "$terminal_output" | tail -1)
        if echo "$last_line" | grep -qE '>\s*$'; then
            status_icon="✅"
        else
            status_icon="⚡"  # 処理中と推定
        fi
    fi
    
    # 結果をキャッシュして出力
    echo "$status_icon" | tee "$CACHE_FILE"
}

# プラットフォーム対応音声通知（高精度検出連携）
trigger_voice_notification() {
    local status_icon="$1"
    local window_id="${2:-1}"
    local last_status="${3:-}"
    
    # 音声機能が無効な場合はスキップ
    [[ "${CLAUDE_VOICE_ENABLED:-true}" == "false" ]] && return 0
    
    # ステータス変更時のみ通知（スパム防止）
    if [[ -n "$last_status" && "$status_icon" == "$last_status" ]]; then
        return 0
    fi
    
    # 通知対象ステータスかチェック
    case "$status_icon" in
        "⚡")  # Busy: より控えめに通知
            [[ "${CLAUDE_VOICE_BUSY_NOTIFICATIONS:-false}" == "true" ]] || return 0
            ;;
        "⌛")  # Waiting: 重要な通知
            ;;
        "✅")  # Idle/Complete: 完了通知
            ;;
        *)
            return 0  # 通知不要
            ;;
    esac
    
    # 高品質音声エンジンの優先使用
    if [[ -f "$CORE_DIR/audio-fallback.sh" ]]; then
        # 新しい統合音声システム使用
        "$CORE_DIR/audio-fallback.sh" play "$status_icon" "auto" "Claude status: $status_icon" &
    elif [[ -f "$CORE_DIR/cross_platform_voice.sh" ]]; then
        "$CORE_DIR/cross_platform_voice.sh" status "$status_icon" &
    elif check_platform_capability "audio" 2>/dev/null; then
        execute_platform_command "audio" "$status_icon" &
    elif [[ -f "$PLATFORM_VOICE_ENGINE" ]]; then
        # WSL v2.0エンジンを使用
        "$PLATFORM_VOICE_ENGINE" sound "$status_icon" &
    elif [[ -f "$FALLBACK_VOICE_ENGINE" ]]; then
        # フォールバック: v1.0エンジン
        "$FALLBACK_VOICE_ENGINE" sound "$status_icon" &
    fi
    
    return 0
}

# メイン処理
main() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    local mode="${3:-display}"  # display, notify, both
    
    # ステータス検出
    local status_icon
    status_icon=$(detect_claude_status "$window_id" "$pane_id")
    
    # モード別処理
    case "$mode" in
        "display")
            echo "$status_icon"
            ;;
        "notify")
            trigger_voice_notification "$status_icon" "$window_id"
            ;;
        "both")
            echo "$status_icon"
            trigger_voice_notification "$status_icon" "$window_id"
            ;;
        "test")
            echo "Testing Claude Status Detection v2.0"
            echo "Window ID: $window_id, Pane ID: $pane_id"
            echo "Detected Status: $status_icon"
            echo "Platform: $(detect_platform 2>/dev/null || echo "unknown")"
            ;;
        *)
            echo "Usage: $0 [window_id] [pane_id] [display|notify|both|test]"
            exit 1
            ;;
    esac
}

# クリーンアップ関数
cleanup() {
    [[ -f "$CACHE_FILE" ]] && rm -f "$CACHE_FILE"
}

# シグナルハンドラ設定
trap cleanup EXIT INT TERM

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi