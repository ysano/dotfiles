#!/bin/bash
# Claude Voice Core - Screen Capture Module
# tmuxペインからのテキスト取得（OS非依存）

# 依存関係の確認
if ! has_command tmux; then
    log "ERROR" "tmux is not installed or not in PATH"
    exit 1
fi

# キャラクタベースの枠パターンを検出
detect_character_frames() {
    local text="$1"
    
    local complete_frames=0
    local incomplete_frames=0
    
    # 改行を空白に置換してから検索（複数行パターン対応）
    local normalized_text=$(echo "$text" | tr '\n' ' ')
    
    # 丸角ボックスの検出
    local rounded_complete=$(echo "$text" | perl -0777 -ne 'print scalar(() = /╭[^\n]*╮.*?╰[^\n]*╯/gs)' 2>/dev/null || echo "0")
    local rounded_top=$(echo "$text" | grep -c '╭.*╮' 2>/dev/null || echo "0")
    local rounded_bottom=$(echo "$text" | grep -c '╰.*╯' 2>/dev/null || echo "0")
    
    # 角ボックスの検出
    local square_complete=$(echo "$text" | perl -0777 -ne 'print scalar(() = /┌[^\n]*┐.*?└[^\n]*┘/gs)' 2>/dev/null || echo "0")
    local square_top=$(echo "$text" | grep -c '┌.*┐' 2>/dev/null || echo "0")
    local square_bottom=$(echo "$text" | grep -c '└.*┘' 2>/dev/null || echo "0")
    
    # 太線ボックスの検出
    local thick_complete=$(echo "$text" | perl -0777 -ne 'print scalar(() = /┏[^\n]*┓.*?┗[^\n]*┛/gs)' 2>/dev/null || echo "0")
    local thick_top=$(echo "$text" | grep -c '┏.*┓' 2>/dev/null || echo "0")
    local thick_bottom=$(echo "$text" | grep -c '┗.*┛' 2>/dev/null || echo "0")
    
    # 数値検証
    [[ "$rounded_complete" =~ ^[0-9]+$ ]] || rounded_complete=0
    [[ "$rounded_top" =~ ^[0-9]+$ ]] || rounded_top=0
    [[ "$rounded_bottom" =~ ^[0-9]+$ ]] || rounded_bottom=0
    [[ "$square_complete" =~ ^[0-9]+$ ]] || square_complete=0
    [[ "$square_top" =~ ^[0-9]+$ ]] || square_top=0
    [[ "$square_bottom" =~ ^[0-9]+$ ]] || square_bottom=0
    [[ "$thick_complete" =~ ^[0-9]+$ ]] || thick_complete=0
    [[ "$thick_top" =~ ^[0-9]+$ ]] || thick_top=0
    [[ "$thick_bottom" =~ ^[0-9]+$ ]] || thick_bottom=0
    
    # 完全な枠の合計
    complete_frames=$((rounded_complete + square_complete + thick_complete))
    
    # 不完全な枠の検出
    if [[ $rounded_top -gt $rounded_complete || $rounded_bottom -gt $rounded_complete ]]; then
        incomplete_frames=$((incomplete_frames + 1))
    fi
    if [[ $square_top -gt $square_complete || $square_bottom -gt $square_complete ]]; then
        incomplete_frames=$((incomplete_frames + 1))
    fi
    if [[ $thick_top -gt $thick_complete || $thick_bottom -gt $thick_complete ]]; then
        incomplete_frames=$((incomplete_frames + 1))
    fi
    
    # 枠の状態を判定
    local frame_status="none"
    if [[ $complete_frames -gt 0 ]]; then
        frame_status="complete"
    elif [[ $incomplete_frames -gt 0 ]]; then
        frame_status="incomplete"
    fi
    
    log "DEBUG" "Frame detection: complete=$complete_frames, incomplete=$incomplete_frames, status=$frame_status"
    log "DEBUG" "Details: rounded=($rounded_complete/$rounded_top/$rounded_bottom), square=($square_complete/$square_top/$square_bottom), thick=($thick_complete/$thick_top/$thick_bottom)"
    
    # 結果をJSON形式で返す
    cat <<EOF
{
    "status": "$frame_status",
    "complete_frames": $complete_frames,
    "incomplete_frames": $incomplete_frames,
    "needs_expansion": $([ "$frame_status" = "incomplete" ] && echo "true" || echo "false")
}
EOF
}

# 智的キャプチャ範囲拡大
capture_with_smart_expansion() {
    local pane_id="${1:-.}"
    local initial_lines="${2:-50}"
    local include_history="${3:-true}"
    local max_attempts="${4:-3}"
    local expansion_increment="${5:-20}"
    
    log "DEBUG" "Smart capture: pane=$pane_id, initial_lines=$initial_lines, max_attempts=$max_attempts"
    
    local current_lines=$initial_lines
    local attempt=1
    
    while [[ $attempt -le $max_attempts ]]; do
        log "DEBUG" "Capture attempt $attempt with $current_lines lines"
        
        # 現在の範囲でキャプチャ実行
        local captured_text
        if ! captured_text=$(capture_basic_screen_text "$pane_id" "$current_lines" "$include_history"); then
            log "ERROR" "Basic capture failed on attempt $attempt"
            return 1
        fi
        
        # 枠の検出
        local frame_info=$(detect_character_frames "$captured_text")
        local frame_status=$(echo "$frame_info" | grep -o '"status": "[^"]*"' | cut -d'"' -f4)
        local needs_expansion=$(echo "$frame_info" | grep -o '"needs_expansion": [^,}]*' | cut -d':' -f2 | tr -d ' ')
        
        log "DEBUG" "Attempt $attempt: frame_status=$frame_status, needs_expansion=$needs_expansion"
        
        # 完全な枠が見つかった場合、または最大試行回数に達した場合は終了
        if [[ "$frame_status" == "complete" ]] || [[ $attempt -eq $max_attempts ]]; then
            local final_processed=$(process_captured_text "$captured_text")
            log "INFO" "Smart capture completed: attempts=$attempt, lines=$current_lines, status=$frame_status"
            echo "$final_processed"
            return 0
        fi
        
        # 不完全な枠が検出された場合は範囲を拡大
        if [[ "$needs_expansion" == "true" ]]; then
            current_lines=$((current_lines + expansion_increment))
            log "DEBUG" "Expanding capture range to $current_lines lines"
        else
            # 枠が全く検出されない場合は、デフォルトの拡大を実行
            current_lines=$((current_lines + expansion_increment / 2))
            log "DEBUG" "No frames detected, modest expansion to $current_lines lines"
        fi
        
        ((attempt++))
    done
    
    # フォールバック: 最後の結果を返す
    local final_text=$(process_captured_text "$captured_text")
    log "WARN" "Smart capture reached max attempts, returning last result"
    echo "$final_text"
    return 0
}

# 基本的なtmuxペインキャプチャ（内部使用）
capture_basic_screen_text() {
    local pane_id="${1:-.}"
    local lines="${2:-50}"
    local include_history="${3:-true}"

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

    echo "$screen_text"
    return 0
}

# メイン画面キャプチャ関数（智的拡大機能付き）
capture_screen_text() {
    local pane_id="${1:-.}"
    local lines="${2:-50}"
    local include_history="${3:-true}"
    local enable_smart_expansion="${4:-true}"

    log "DEBUG" "Capturing screen text: pane=$pane_id, lines=$lines, history=$include_history, smart_expansion=$enable_smart_expansion"

    # 智的拡大が有効な場合
    if [[ "$enable_smart_expansion" == "true" ]]; then
        capture_with_smart_expansion "$pane_id" "$lines" "$include_history"
        return $?
    fi
    
    # 従来の方式（後方互換性）
    local screen_text
    if ! screen_text=$(capture_basic_screen_text "$pane_id" "$lines" "$include_history"); then
        return $?
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

# 枠検出のテスト関数
test_frame_detection() {
    echo "=== Frame Detection Test ==="
    echo ""
    
    # テスト用のサンプルテキスト
    local test_samples=(
        $'完全な丸角ボックス:\n╭─────────────╮\n│ Complete Box │\n╰─────────────╯'
        $'不完全な丸角ボックス（上のみ）:\n╭─────────────╮\n│ Incomplete Box'
        $'不完全な丸角ボックス（下のみ）:\n│ Incomplete Box\n╰─────────────╯'
        $'完全な角ボックス:\n┌─────────────┐\n│ Complete Box │\n└─────────────┘'
        $'枠なしテキスト:\nJust plain text\nwithout any frames'
    )
    
    local test_names=(
        "Complete rounded box"
        "Incomplete rounded box (top only)"
        "Incomplete rounded box (bottom only)"
        "Complete square box"
        "No frames"
    )
    
    for i in "${!test_samples[@]}"; do
        echo "Test $((i+1)): ${test_names[$i]}"
        echo "Sample text:"
        echo "${test_samples[$i]}"
        echo ""
        
        local frame_result=$(detect_character_frames "${test_samples[$i]}")
        echo "Detection result: $frame_result"
        echo ""
        echo "---"
        echo ""
    done
    
    echo "✅ Frame detection test completed"
}

# 智的拡大機能のテスト
test_smart_expansion() {
    echo "=== Smart Expansion Test ==="
    echo ""
    
    # 現在のペインで智的拡大をテスト
    echo "Testing smart expansion on current pane..."
    
    # 複数の初期行数でテスト
    local test_lines=(10 20 30)
    
    for lines in "${test_lines[@]}"; do
        echo "Testing with initial $lines lines:"
        
        # 智的拡大有効でキャプチャ
        local smart_result=$(capture_screen_text "." "$lines" "true" "true")
        local smart_length=${#smart_result}
        
        # 通常キャプチャ
        local normal_result=$(capture_screen_text "." "$lines" "true" "false")
        local normal_length=${#normal_result}
        
        echo "  Smart expansion: $smart_length characters"
        echo "  Normal capture:  $normal_length characters"
        echo "  Difference:      $((smart_length - normal_length)) characters"
        
        # 枠検出結果
        local frame_info=$(detect_character_frames "$smart_result")
        local frame_status=$(echo "$frame_info" | grep -o '"status": "[^"]*"' | cut -d'"' -f4)
        echo "  Frame status:    $frame_status"
        echo ""
    done
    
    echo "✅ Smart expansion test completed"
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

    echo ""
    
    # 枠検出テスト
    test_frame_detection
    
    echo ""
    
    # 智的拡大テスト
    test_smart_expansion

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
