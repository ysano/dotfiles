#!/bin/bash
# Claude Voice - Summary Engine Module
# スクリーンキャプチャと要約生成エンジン

# 依存モジュール
source "$CLAUDE_VOICE_HOME/core/screen_capture.sh" 2>/dev/null || true
source "$CLAUDE_VOICE_HOME/core/llm_manager.sh" 2>/dev/null || true
source "$CLAUDE_VOICE_HOME/core/summary_interface.sh" 2>/dev/null || true

# 要約生成の主要関数
generate_summary() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local pane_id="${2:-1}"
    local max_lines="${3:-20}"
    local context="${4:-auto}"
    
    # ウィンドウ.ペイン形式をサポート
    if [[ "$window_id" == *"."* ]]; then
        pane_id="${window_id#*.}"
        window_id="${window_id%.*}"
    fi
    
    # スクリーンコンテンツを取得（capture_screen_textを使用）
    local content
    if declare -f capture_screen_text >/dev/null; then
        content=$(capture_screen_text "${window_id}.${pane_id}" "$max_lines" true false)
    else
        # フォールバック: 直接tmuxから取得
        content=$(tmux capture-pane -t "${window_id}.${pane_id}" -p -S "-${max_lines}" 2>/dev/null || echo "")
    fi
    
    if [[ -z "$content" ]]; then
        log "WARN" "No content captured from window $window_id pane $pane_id"
        return 1
    fi
    
    # コンテキストの自動判定（統一インターフェースを使用）
    if [[ "$context" == "auto" ]]; then
        if declare -f detect_context_from_content >/dev/null; then
            context=$(detect_context_from_content "$content")
        else
            # フォールバック: ローカル判定
            if echo "$content" | grep -qE "(Error|Failed|Exception|失敗|エラー)" 2>/dev/null; then
                context="error"
            elif echo "$content" | grep -qE "(Complete|Done|Success|完了|成功)" 2>/dev/null; then
                context="complete"
            elif echo "$content" | grep -qE "(\\?|Continue|Proceed|Y/N|yes/no)" 2>/dev/null; then
                context="waiting"
            else
                context="general"
            fi
        fi
    fi
    
    # LLMで要約を生成
    local summary
    if declare -f summarize_screen_content >/dev/null; then
        summary=$(summarize_screen_content "$content" 30 "$context")
    else
        # フォールバック: 簡易要約
        summary=$(echo "$content" | tail -5 | head -1)
    fi
    
    echo "$summary"
}

# 音声用の短い要約を生成
generate_brief_summary() {
    local window_id="$1"
    local pane_id="${2:-1}"
    local lines="${3:-10}"
    local context="${4:-auto}"
    
    # より短い要約を生成
    local summary=$(generate_summary "$window_id" "$pane_id" "$lines" "$context")
    
    # 音声合成用に最適化（句読点を調整）
    echo "$summary" | sed 's/、/、 /g' | sed 's/。/。 /g'
}

# ステータス変更時の要約
generate_status_change_summary() {
    local old_status="$1"
    local new_status="$2"
    local window_id="${3:-$(tmux display-message -p '#I')}"
    
    # 統一インターフェースを使用
    local context
    if declare -f status_to_context >/dev/null; then
        context=$(status_to_context "$new_status")
    else
        # フォールバック
        case "$new_status" in
            "✅"|"Idle")
                context="complete"
                ;;
            "⌛"|"Waiting")
                context="waiting"
                ;;
            "⚡"|"Busy")
                context="busy"
                ;;
            *)
                context="general"
                ;;
        esac
    fi
    
    generate_brief_summary "$window_id" 1 15 "$context"
}

# エクスポート
export -f generate_summary
export -f generate_brief_summary
export -f generate_status_change_summary

# Context情報収集（スタブ）
collect_context_information() {
    local pane_id="${1:-1}"
    local window_id="${2:-$(tmux display-message -p '#I')}"
    
    # 基本的なコンテキスト情報を返す
    echo "Window: $window_id, Pane: $pane_id, Time: $(date +%H:%M)"
}

export -f collect_context_information < /dev/null