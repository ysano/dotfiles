#!/bin/bash
# Claude Code Status Detection Script - Refactored Version
# Claude Voice統合アーキテクチャに準拠したステータス検出スクリプト

# ライブラリのロード
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
source "$CLAUDE_VOICE_HOME/core/lib/status_common.sh"

# 引数処理
WINDOW_ID=${1:-$(tmux display-message -p '#I')}

# メイン検出関数
detect_claude_status() {
    local window_id="$1"
    
    # ペインPIDの取得
    local pane_pid=$(tmux list-panes -t "$window_id" -F '#{pane_pid}' 2>/dev/null | head -1)
    
    if [[ -z "$pane_pid" ]]; then
        echo "$STATUS_EMPTY"
        return
    fi
    
    # Claude Codeプロセスの存在確認
    if ! detect_claude_process "$pane_pid"; then
        echo "$STATUS_EMPTY"
        return
    fi
    
    # ペインコンテンツの取得（最新10行）
    local pane_content=$(tmux capture-pane -t "$window_id" -p -S -10 2>/dev/null | tail -10 || echo "")
    
    # コンテンツからステータスを判定
    local status_name=$(analyze_pane_content "$pane_content")
    
    # ステータス名をアイコンに変換
    local status_icon=$(status_name_to_icon "$status_name")
    
    echo "$status_icon"
}

# バックグラウンドでクリーンアップ実行
cleanup_status_files &

# ステータス検出と出力
detect_claude_status "$WINDOW_ID"