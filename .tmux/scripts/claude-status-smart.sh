#!/bin/bash
# Claude Status Smart Detection Script
# v2.0対応のインテリジェントなステータス検出

set -euo pipefail

# === 設定 ===
readonly WINDOW_ID="${1:-$(tmux display-message -p '#I')}"
readonly PANE_ID="${2:-$(tmux display-message -p '#P')}"
readonly TMUX_CONFIG_DIR="$HOME/.tmux"
readonly CLAUDE_CORE_DIR="$TMUX_CONFIG_DIR/claude/core"

# === エンジン検出と優先順位 ===
detect_best_engine() {
    # v2.0 モジュラーエンジンを最優先
    if [[ -x "$CLAUDE_CORE_DIR/wsl_voice_engine_v2.sh" ]]; then
        echo "v2"
        return 0
    fi
    
    # レガシーエンジンでフォールバック
    if [[ -x "$CLAUDE_CORE_DIR/wsl_voice_engine.sh" ]]; then
        echo "v1"
        return 0
    fi
    
    # Claude統合なし
    echo "none"
    return 1
}

# === ステータス検出メイン ===
get_claude_status() {
    local engine
    engine=$(detect_best_engine)
    
    case "$engine" in
        "v2")
            # v2.0 診断機能使用
            if "$CLAUDE_CORE_DIR/wsl_voice_engine_v2.sh" diagnose >/dev/null 2>&1; then
                # 詳細ステータス取得の試行
                "$TMUX_CONFIG_DIR/scripts/claude-status-enhanced.sh" "$WINDOW_ID" "$PANE_ID" 2>/dev/null || echo ""
            else
                echo ""
            fi
            ;;
        "v1")
            # レガシーシステム使用
            "$TMUX_CONFIG_DIR/scripts/claude-status-enhanced.sh" "$WINDOW_ID" "$PANE_ID" 2>/dev/null || echo ""
            ;;
        "none")
            # Claude統合なし
            echo ""
            ;;
    esac
}

# === パフォーマンス最適化 ===
# キャッシュファイルの使用（オプション）
readonly CACHE_FILE="$HOME/.tmux/status/smart-cache-${WINDOW_ID}.status"
readonly CACHE_DURATION=2  # 2秒

use_cache() {
    if [[ -f "$CACHE_FILE" ]]; then
        local cache_age
        cache_age=$(( $(date +%s) - $(stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0) ))
        [[ $cache_age -lt $CACHE_DURATION ]]
    else
        return 1
    fi
}

# === メイン実行 ===
main() {
    # キャッシュチェック（パフォーマンス最適化）
    if use_cache; then
        cat "$CACHE_FILE" 2>/dev/null || get_claude_status
    else
        local status
        status=$(get_claude_status)
        
        # キャッシュ更新
        if [[ -n "$status" ]]; then
            mkdir -p "$(dirname "$CACHE_FILE")"
            echo "$status" > "$CACHE_FILE"
        fi
        
        echo "$status"
    fi
}

# === エラーハンドリング ===
trap 'echo ""' ERR

# === 実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi