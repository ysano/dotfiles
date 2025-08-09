#!/bin/bash
# Claude Code Frame Detector
# Claude Codeのプロンプト枠全体を検出・取得

# ログ関数
log() {
    local level="$1"
    local message="$2"
    [[ "${CLAUDE_VOICE_DEBUG:-}" == "1" ]] && echo "[$(date '+%H:%M:%S')] [$level] $message" >&2
}

# 四角い枠の開始位置を検出
find_frame_start() {
    local content="$1"
    local max_lines="$2"
    
    # 枠の開始パターンを検索（╭─ で始まる行）
    local line_num=1
    while IFS= read -r line; do
        if [[ "$line" =~ ^[[:space:]]*╭─ ]]; then
            log "DEBUG" "Frame start found at line $line_num"
            echo "$line_num"
            return 0
        fi
        ((line_num++))
        if [[ $line_num -gt $max_lines ]]; then
            break
        fi
    done <<< "$content"
    
    echo "0"
    return 1
}

# 四角い枠の終了位置を検出
find_frame_end() {
    local content="$1"
    local start_line="$2"
    
    # 枠の終了パターンを検索（╰─ で始まる行）
    local line_num=1
    local found_start=false
    
    while IFS= read -r line; do
        if [[ $line_num -ge $start_line ]]; then
            found_start=true
        fi
        
        if [[ "$found_start" == "true" ]] && [[ "$line" =~ ^[[:space:]]*╰─ ]]; then
            log "DEBUG" "Frame end found at line $line_num"
            echo "$line_num"
            return 0
        fi
        
        ((line_num++))
    done <<< "$content"
    
    echo "0"
    return 1
}

# 完全な枠を取得
get_complete_frame() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local initial_lines="${2:-15}"
    local max_lines="${3:-100}"
    
    log "INFO" "Getting complete frame for window $window_id (initial: $initial_lines lines)"
    
    local current_lines=$initial_lines
    local frame_complete=false
    local content=""
    
    while [[ $current_lines -le $max_lines ]] && [[ "$frame_complete" != "true" ]]; do
        # 現在の行数で画面内容を取得
        content=$(tmux capture-pane -t "$window_id" -p -S -$current_lines 2>/dev/null)
        
        if [[ -z "$content" ]]; then
            log "ERROR" "Failed to capture pane content"
            echo "$content"
            return 1
        fi
        
        # 枠の開始位置を検索
        local frame_start=$(find_frame_start "$content" "$current_lines")
        
        if [[ $frame_start -eq 0 ]]; then
            log "DEBUG" "No frame start found in $current_lines lines"
            # 枠が見つからない場合は行数を増やす
            current_lines=$((current_lines + 10))
            continue
        fi
        
        # 枠の終了位置を検索
        local frame_end=$(find_frame_end "$content" "$frame_start")
        
        if [[ $frame_end -eq 0 ]]; then
            log "DEBUG" "Frame end not found in $current_lines lines, expanding..."
            # 枠の終了が見つからない場合は行数を増やす
            current_lines=$((current_lines + 10))
        else
            # 枠が完全に取得できた
            frame_complete=true
            log "INFO" "Complete frame found: lines $frame_start to $frame_end (total: $current_lines lines captured)"
            
            # 枠の部分だけを抽出
            local total_lines=$(echo "$content" | wc -l)
            local start_from=$((total_lines - current_lines + frame_start))
            local end_at=$((total_lines - current_lines + frame_end))
            
            # 枠全体を含む内容を返す
            echo "$content"
            return 0
        fi
    done
    
    log "WARN" "Could not find complete frame within $max_lines lines"
    echo "$content"
    return 1
}

# プロンプト内容を抽出（枠の中身のみ）
extract_prompt_content() {
    local frame_content="$1"
    
    # 枠の中身だけを抽出（│で始まる行）
    local inside_frame=false
    local prompt_content=""
    
    while IFS= read -r line; do
        if [[ "$line" =~ ^[[:space:]]*╭─ ]]; then
            inside_frame=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*╰─ ]]; then
            inside_frame=false
            break
        elif [[ "$inside_frame" == "true" ]] && [[ "$line" =~ ^[[:space:]]*│ ]]; then
            # 枠の装飾を除去して内容だけを抽出
            # 行の最初の│と最後の│を除去
            local content=$(echo "$line" | sed -E 's/^[[:space:]]*│[[:space:]]?//; s/[[:space:]]?│[[:space:]]*$//')
            if [[ -n "$prompt_content" ]]; then
                prompt_content="${prompt_content}
${content}"
            else
                prompt_content="$content"
            fi
        fi
    done <<< "$frame_content"
    
    echo "$prompt_content"
}

# Waiting状態用の枠取得
get_waiting_frame() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    
    log "INFO" "Getting waiting frame for window $window_id"
    
    # まず15行で試して、枠が不完全なら拡張
    local frame_content=$(get_complete_frame "$window_id" 15 100)
    
    if [[ $? -eq 0 ]]; then
        log "INFO" "Successfully captured complete frame"
        echo "$frame_content"
        return 0
    else
        log "WARN" "Frame might be incomplete, returning best effort"
        echo "$frame_content"
        return 1
    fi
}

# テスト関数
test_frame_detection() {
    echo "=== Frame Detection Test ==="
    echo ""
    
    # テスト用のコンテンツ
    local test_content="Some text before
╭─ Question ─────────────────╮
│ Do you want to continue?   │
│                             │
│ [Y]es  [N]o  [C]ancel      │
╰─────────────────────────────╯
Some text after"
    
    echo "Test content:"
    echo "$test_content"
    echo ""
    
    # 開始位置テスト
    local start=$(find_frame_start "$test_content" 10)
    echo "Frame start line: $start"
    
    # 終了位置テスト
    local end=$(find_frame_end "$test_content" "$start")
    echo "Frame end line: $end"
    
    # プロンプト内容抽出テスト
    echo ""
    echo "Extracted prompt content:"
    extract_prompt_content "$test_content"
    
    echo ""
    echo "✅ Test completed"
}

# エクスポート
export -f find_frame_start
export -f find_frame_end
export -f get_complete_frame
export -f extract_prompt_content
export -f get_waiting_frame

# 直接実行された場合
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-}" in
        "test")
            test_frame_detection
            ;;
        "get")
            get_waiting_frame "${2:-}"
            ;;
        *)
            echo "Usage: $0 [test|get [window_id]]"
            exit 1
            ;;
    esac
fi