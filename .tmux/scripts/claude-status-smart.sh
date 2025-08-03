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

# Claude Codeステータス検出
detect_claude_status() {
    local window_id="${1:-1}"
    local pane_id="${2:-0}"
    
    # キャッシュチェック
    if is_cache_valid; then
        cat "$CACHE_FILE" 2>/dev/null
        return 0
    fi
    
    local status_icon=""
    local has_activity=false
    
    # tmuxペイン情報取得
    local pane_info
    if pane_info=$(tmux list-panes -t "$window_id" -F "#{pane_active}:#{pane_current_command}:#{pane_pid}" 2>/dev/null); then
        
        while IFS=':' read -r is_active command pid; do
            [[ "$is_active" == "1" ]] || continue
            has_activity=true
            
            case "$command" in
                "claude")
                    # Claude Code実行中 - より詳細な状態検出
                    if ps -p "$pid" -o args= 2>/dev/null | grep -q "claude.*--"; then
                        status_icon="⚡"  # コマンド実行中
                    else
                        status_icon="⌛"  # 待機中
                    fi
                    ;;
                "python"|"node"|"npm"|"cargo"|"go")
                    status_icon="⚡"  # 開発ツール実行中
                    ;;
                "git")
                    status_icon="⚡"  # Git操作中
                    ;;
                "emacs"|"vim"|"code")
                    status_icon="✏️"   # エディタ
                    ;;
                *)
                    # その他のアクティビティ
                    if [[ -n "$command" && "$command" != "zsh" && "$command" != "bash" ]]; then
                        status_icon="⚡"
                    fi
                    ;;
            esac
        done <<< "$pane_info"
    fi
    
    # デフォルト状態
    if [[ -z "$status_icon" ]]; then
        if [[ "$has_activity" == "true" ]]; then
            status_icon="✅"  # アクティブだが特別な処理なし
        else
            status_icon=""    # 非アクティブ
        fi
    fi
    
    # 結果をキャッシュして出力
    echo "$status_icon" | tee "$CACHE_FILE"
}

# プラットフォーム対応音声通知
trigger_voice_notification() {
    local status_icon="$1"
    local window_id="${2:-1}"
    
    # 音声機能が無効な場合はスキップ
    [[ "${CLAUDE_VOICE_ENABLED:-true}" == "false" ]] && return 0
    
    # 通知対象ステータスかチェック
    case "$status_icon" in
        "⚡"|"⌛"|"✅") 
            ;;
        *)
            return 0  # 通知不要
            ;;
    esac
    
    # プラットフォーム統合音声エンジン使用
    if [[ -f "$CORE_DIR/cross_platform_voice.sh" ]]; then
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