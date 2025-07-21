#!/bin/bash
# Claude Voice Core - Screen Capture Module
# tmuxペインからのテキスト取得（OS非依存）

# 依存関係の確認
if ! has_command tmux; then
    log "ERROR" "tmux is not installed or not in PATH"
    exit 1
fi

# tmuxペインのテキスト内容を取得
capture_screen_text() {
    local pane_id="${1:-.}"
    local lines="${2:-50}"
    local include_history="${3:-true}"

    log "DEBUG" "Capturing screen text: pane=$pane_id, lines=$lines, history=$include_history"

    # tmuxペインの存在確認
    if ! tmux list-panes -t "$pane_id" >/dev/null 2>&1; then
        log "ERROR" "Tmux pane not found: $pane_id"
        return 1
    fi

    # ペイン情報の取得
    local pane_info=$(tmux display-message -t "$pane_id" -p "#{pane_width}x#{pane_height}")
    log "DEBUG" "Pane dimensions: $pane_info"

    # tmuxペインの内容を取得
    local capture_args="-t $pane_id -p"

    if [[ "$include_history" == "true" ]]; then
        capture_args="$capture_args -S -${lines}"
    else
        capture_args="$capture_args -e" # 現在の画面のみ
    fi

    local screen_text
    if ! screen_text=$(tmux capture-pane $capture_args 2>/dev/null); then
        log "ERROR" "Failed to capture pane content"
        return 2
    fi

    if [[ -z "$screen_text" ]]; then
        log "WARN" "No content captured from tmux pane"
        return 3
    fi

    # テキストの前処理
    local processed_text=$(process_captured_text "$screen_text")

    log "DEBUG" "Captured ${#processed_text} characters"
    echo "$processed_text"
    return 0
}

# キャプチャしたテキストの前処理
process_captured_text() {
    local raw_text="$1"
    local max_chars=$(get_config "capture.max_chars" "2000")

    log "DEBUG" "Processing captured text (${#raw_text} chars)"

    # 制御文字とエスケープシーケンスの除去
    local cleaned_text=$(echo "$raw_text" |
        sed 's/\x1b\[[0-9;]*m//g' |
        sed 's/\x1b\[[0-9;]*[A-Za-z]//g' |
        sed 's/\x1b\[[@A-Z\\^_]//g' |
        sed 's/\x1b].*\x07//g' |
        tr -d '\000-\010\013\014\016-\037\177')

    # 空行の除去（連続する空行を1つにまとめる）
    cleaned_text=$(echo "$cleaned_text" | sed '/^[[:space:]]*$/d')

    # 文字数制限の適用
    if [[ ${#cleaned_text} -gt $max_chars ]]; then
        cleaned_text="${cleaned_text:0:$max_chars}..."
        log "DEBUG" "Text truncated to $max_chars characters"
    fi

    # 最終的な空白の正規化
    cleaned_text=$(echo "$cleaned_text" | sed 's/[[:space:]]\+/ /g')

    echo "$cleaned_text"
}

# 特定のtmuxウィンドウの情報を取得
get_window_context() {
    local window_id="${1:-@}"

    # ウィンドウの基本情報
    local window_name=$(tmux display-message -t "$window_id" -p "#{window_name}")
    local window_index=$(tmux display-message -t "$window_id" -p "#{window_index}")
    local pane_count=$(tmux display-message -t "$window_id" -p "#{window_panes}")
    local current_command=$(tmux display-message -t "$window_id" -p "#{pane_current_command}")
    local current_path=$(tmux display-message -t "$window_id" -p "#{pane_current_path}")

    log "DEBUG" "Window context: name=$window_name, index=$window_index, panes=$pane_count, command=$current_command"

    # JSON形式で情報を返す
    cat <<EOF
{
    "window_name": "$window_name",
    "window_index": $window_index,
    "pane_count": $pane_count,
    "current_command": "$current_command",
    "current_path": "$current_path"
}
EOF
}

# アクティブなペインの詳細情報を取得
get_active_pane_info() {
    local session_name=$(tmux display-message -p "#{session_name}")
    local window_index=$(tmux display-message -p "#{window_index}")
    local pane_index=$(tmux display-message -p "#{pane_index}")
    local pane_id=$(tmux display-message -p "#{pane_id}")
    local pane_title=$(tmux display-message -p "#{pane_title}")

    log "DEBUG" "Active pane: session=$session_name, window=$window_index, pane=$pane_index"

    cat <<EOF
{
    "session_name": "$session_name",
    "window_index": $window_index,
    "pane_index": $pane_index,
    "pane_id": "$pane_id",
    "pane_title": "$pane_title"
}
EOF
}

# 複数ペインの内容を結合して取得
capture_multiple_panes() {
    local window_id="${1:-@}"
    local max_panes="${2:-5}"
    local lines_per_pane="${3:-30}"

    log "DEBUG" "Capturing multiple panes: window=$window_id, max_panes=$max_panes"

    # ウィンドウ内の全ペインを取得
    local pane_list=$(tmux list-panes -t "$window_id" -F "#{pane_id}")
    local pane_count=0
    local combined_text=""

    while read -r pane_id && [[ $pane_count -lt $max_panes ]]; do
        if [[ -n "$pane_id" ]]; then
            log "DEBUG" "Capturing pane: $pane_id"

            local pane_text=$(capture_screen_text "$pane_id" "$lines_per_pane" false)
            if [[ -n "$pane_text" ]]; then
                combined_text+="=== Pane $pane_id ===\n"
                combined_text+="$pane_text\n\n"
                ((pane_count++))
            fi
        fi
    done <<<"$pane_list"

    echo -e "$combined_text"
}

# 最近のコマンド履歴から関連情報を抽出
extract_command_context() {
    local lines="${1:-10}"

    # シェル履歴ファイルの検出
    local history_file=""
    if [[ -n "$HISTFILE" && -f "$HISTFILE" ]]; then
        history_file="$HISTFILE"
    elif [[ -f "$HOME/.zsh_history" ]]; then
        history_file="$HOME/.zsh_history"
    elif [[ -f "$HOME/.bash_history" ]]; then
        history_file="$HOME/.bash_history"
    else
        log "DEBUG" "No shell history file found"
        return 1
    fi

    log "DEBUG" "Extracting command context from: $history_file"

    # 最近のコマンドを取得（zsh historyの場合はタイムスタンプを除去）
    local recent_commands=$(tail -n "$lines" "$history_file" |
        sed 's/^: [0-9]*:[0-9]*;//' |
        grep -v '^#' |
        tail -5)

    echo "$recent_commands"
}

# スクリーンキャプチャの品質評価
evaluate_capture_quality() {
    local captured_text="$1"
    local min_chars=50
    local max_empty_ratio=0.8

    # 基本的な品質チェック
    local char_count=${#captured_text}
    local line_count=$(echo "$captured_text" | wc -l)
    local empty_lines=$(echo "$captured_text" | grep -c '^[[:space:]]*$' || echo 0)

    log "DEBUG" "Capture quality: chars=$char_count, lines=$line_count, empty=$empty_lines"

    # 品質スコアの計算
    local quality_score=100

    # 文字数が少なすぎる場合
    if [[ $char_count -lt $min_chars ]]; then
        quality_score=$((quality_score - 30))
        log "WARN" "Capture quality: insufficient content"
    fi

    # 空行が多すぎる場合
    if [[ $line_count -gt 0 ]]; then
        local empty_ratio=$(echo "scale=2; $empty_lines / $line_count" | bc 2>/dev/null || echo "0")
        if [[ $(echo "$empty_ratio > $max_empty_ratio" | bc 2>/dev/null) == 1 ]]; then
            quality_score=$((quality_score - 20))
            log "WARN" "Capture quality: too many empty lines"
        fi
    fi

    # エラーパターンの検出
    if echo "$captured_text" | grep -qi "command not found\|permission denied\|no such file"; then
        quality_score=$((quality_score + 10)) # エラー情報は重要なので加点
        log "DEBUG" "Capture quality: error information detected"
    fi

    echo "$quality_score"
}

# このモジュールのテスト関数
test_screen_capture() {
    echo "Testing screen capture module..."

    # 現在のペインからテキストを取得
    local captured_text=$(capture_screen_text "." 20)
    echo "Captured ${#captured_text} characters"

    # ウィンドウコンテキストの取得
    local window_context=$(get_window_context)
    echo "Window context: $window_context"

    # 品質評価
    local quality=$(evaluate_capture_quality "$captured_text")
    echo "Capture quality score: $quality"

    echo "Screen capture test completed"
}

# このスクリプトが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    source "$SCRIPT_DIR/base.sh"

    claude_voice_init true
    test_screen_capture
fi
