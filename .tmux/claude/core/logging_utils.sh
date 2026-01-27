#!/bin/bash
# ファイル名: logging_utils.sh
# 説明: tmux-claude-voice 共通ログ機能
# 他のスクリプトからsourceして使用する統一ログ関数を提供

# 多重読み込み防止
[[ -n "$_CLAUDE_LOGGING_LOADED" ]] && return 0
readonly _CLAUDE_LOGGING_LOADED=1

# ログファイルパス
CLAUDE_VOICE_LOG_FILE="${TMPDIR:-/tmp}/tmux-claude-voice.log"

# INFOレベルログ
log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
}

# ERRORレベルログ
log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
}

# DEBUGレベルログ（TMUX_CLAUDE_VOICE_DEBUG=1のときのみ出力）
log_debug() {
    [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]] && \
        echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$CLAUDE_VOICE_LOG_FILE"
}

# ログファイルのローテーション（サイズ制限）
rotate_log_if_needed() {
    local max_size="${1:-1048576}"  # デフォルト1MB
    if [[ -f "$CLAUDE_VOICE_LOG_FILE" ]]; then
        local size
        size=$(stat -f%z "$CLAUDE_VOICE_LOG_FILE" 2>/dev/null || stat -c%s "$CLAUDE_VOICE_LOG_FILE" 2>/dev/null || echo 0)
        if [[ "$size" -gt "$max_size" ]]; then
            mv "$CLAUDE_VOICE_LOG_FILE" "${CLAUDE_VOICE_LOG_FILE}.old"
            log_info "ログファイルをローテーションしました"
        fi
    fi
}

# テスト関数
test_logging() {
    echo "=== logging_utils.sh テスト ==="
    log_info "INFOログテスト"
    log_error "ERRORログテスト"
    TMUX_CLAUDE_VOICE_DEBUG=1 log_debug "DEBUGログテスト"
    echo "ログファイル: $CLAUDE_VOICE_LOG_FILE"
    echo "最新のログ:"
    tail -3 "$CLAUDE_VOICE_LOG_FILE"
    echo "=== テスト完了 ==="
}

# スクリプト直接実行時の処理
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_logging
            ;;
        "view")
            tail -20 "$CLAUDE_VOICE_LOG_FILE"
            ;;
        "clear")
            > "$CLAUDE_VOICE_LOG_FILE"
            echo "ログファイルをクリアしました"
            ;;
        *)
            echo "使用方法: $0 [test|view|clear]"
            echo "  test  - ログ機能テスト"
            echo "  view  - 最新20行を表示"
            echo "  clear - ログファイルをクリア"
            ;;
    esac
fi
