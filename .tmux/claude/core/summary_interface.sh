#!/bin/bash
# Claude Voice - Summary Interface
# 統一された要約インターフェース定義

# SummaryOptionsクラス（連想配列で実装）
create_summary_options() {
    local -n options=$1
    
    # デフォルト値の設定
    options[content]=""
    options[max_length]=50
    options[context]="auto"
    options[source_type]="screen"  # screen, text, file
    options[format]="plain"  # plain, speech_optimized
    options[language]="ja"
}

# 統一要約インターフェース
generate_unified_summary() {
    local -n opts=$1
    
    # 必須パラメータチェック
    if [[ -z "${opts[content]}" ]]; then
        log "ERROR" "Summary content is required"
        return 1
    fi
    
    # コンテキストの自動判定（autoの場合）
    if [[ "${opts[context]}" == "auto" ]]; then
        opts[context]=$(detect_context_from_content "${opts[content]}")
    fi
    
    # 要約生成の委譲
    local summary
    if declare -f summarize_screen_content >/dev/null; then
        summary=$(summarize_screen_content "${opts[content]}" "${opts[max_length]}" "${opts[context]}")
    else
        log "ERROR" "Summary backend not available"
        return 1
    fi
    
    # フォーマット処理
    if [[ "${opts[format]}" == "speech_optimized" ]]; then
        summary=$(optimize_for_speech "$summary")
    fi
    
    echo "$summary"
    return 0
}

# コンテキスト判定の共通ロジック
detect_context_from_content() {
    local content="$1"
    
    # エラーパターン
    if echo "$content" | grep -qE "(Error|Failed|Exception|失敗|エラー|fatal|crash)" 2>/dev/null; then
        echo "error"
        return
    fi
    
    # 完了パターン
    if echo "$content" | grep -qE "(Complete|Done|Success|Finished|完了|成功|successfully)" 2>/dev/null; then
        echo "complete"
        return
    fi
    
    # 待機パターン
    if echo "$content" | grep -qE "(\\?|Continue|Proceed|Y/N|yes/no|waiting|入力|確認)" 2>/dev/null; then
        echo "waiting"
        return
    fi
    
    # 処理中パターン
    if echo "$content" | grep -qE "(Processing|Running|Working|実行中|処理中|building)" 2>/dev/null; then
        echo "busy"
        return
    fi
    
    echo "general"
}

# ステータスからコンテキストへのマッピング
status_to_context() {
    local status="$1"
    
    case "$status" in
        "✅"|"Idle"|"Complete")
            echo "complete"
            ;;
        "⌛"|"Waiting")
            echo "waiting"
            ;;
        "⚡"|"Busy"|"Running")
            echo "busy"
            ;;
        *)
            echo "general"
            ;;
    esac
}

# 音声最適化処理
optimize_for_speech() {
    local text="$1"
    
    # 句読点の後に適切な間を追加
    text=$(echo "$text" | sed 's/、/、 /g' | sed 's/。/。 /g')
    
    # 英数字の読みやすさ改善
    text=$(echo "$text" | sed 's/\([0-9]\)\([A-Za-z]\)/\1 \2/g')
    text=$(echo "$text" | sed 's/\([A-Za-z]\)\([0-9]\)/\1 \2/g')
    
    # 記号の置換
    text=$(echo "$text" | sed 's/&/ and /g')
    text=$(echo "$text" | sed 's/@/ at /g')
    text=$(echo "$text" | sed 's/#/ number /g')
    
    echo "$text"
}

# 要約タイプから最大長への変換
summary_type_to_length() {
    local type="$1"
    
    case "$type" in
        "brief")
            echo 50
            ;;
        "detailed")
            echo 150
            ;;
        "technical")
            echo 100
            ;;
        "minimal")
            echo 30
            ;;
        *)
            echo 50
            ;;
    esac
}

# エクスポート
export -f create_summary_options
export -f generate_unified_summary
export -f detect_context_from_content
export -f status_to_context
export -f optimize_for_speech
export -f summary_type_to_length