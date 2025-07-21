#!/bin/bash
# Stats Monitor Module - 統計・監視機能
# 使用統計の記録、分析、表示を担当

# === 統計記録機能 ===

# 使用統計の記録
record_usage_stats() {
    local summary_type="$1"
    local model="$2"
    local os_type="$3"
    local duration="$4"
    local success="$5"

    # 必要な関数が存在しない場合の簡易実装
    if ! command -v get_timestamp >/dev/null 2>&1; then
        get_timestamp() {
            date +%s
        }
    fi

    local timestamp=$(get_timestamp)
    local stats_entry=$(
        cat <<EOF
{
    "timestamp": $timestamp,
    "operation": "claude_voice_main",
    "summary_type": "$summary_type",
    "model": "$model", 
    "os_type": "$os_type",
    "duration": $duration,
    "success": $success,
    "version": "${CLAUDE_VOICE_VERSION:-"unknown"}"
}
EOF
    )

    # 統計ファイルに記録
    local stats_dir="$CLAUDE_VOICE_HOME/logs"
    mkdir -p "$stats_dir"
    echo "$stats_entry" >>"$stats_dir/usage_stats.jsonl"

    # ログローテーション（オプション）
    rotate_stats_file_if_needed "$stats_dir/usage_stats.jsonl"
}

# 統計ファイルのローテーション
rotate_stats_file_if_needed() {
    local stats_file="$1"
    local max_size=1048576 # 1MB

    if [[ -f "$stats_file" ]]; then
        local file_size=$(stat -f%z "$stats_file" 2>/dev/null || stat -c%s "$stats_file" 2>/dev/null || echo 0)

        if [[ $file_size -gt $max_size ]]; then
            local backup_file="${stats_file}.$(date +%Y%m%d-%H%M%S)"
            mv "$stats_file" "$backup_file"
            log "INFO" "Stats file rotated: $backup_file"

            # 古いバックアップファイルのクリーンアップ（30日以上）
            find "$(dirname "$stats_file")" -name "usage_stats.jsonl.*" -mtime +30 -delete 2>/dev/null
        fi
    fi
}

# === 統計表示機能 ===

# 使用統計の詳細表示
show_stats() {
    echo "=== Claude Voice 使用統計 ==="

    local stats_file="$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl"
    if [[ ! -f "$stats_file" ]]; then
        echo "統計データがありません。"
        echo "統計は以下の場所に保存されます: $stats_file"
        return 1
    fi

    # 基本統計
    show_basic_stats "$stats_file"

    # 時系列統計
    show_temporal_stats "$stats_file"

    # jq利用可能時の詳細統計
    if has_command jq; then
        show_detailed_stats "$stats_file"
    else
        echo ""
        echo "注意: jqコマンドが利用できないため、詳細統計は表示されません"
        echo "インストール: sudo apt install jq (Ubuntu) または brew install jq (macOS)"
    fi

    # 最近の使用履歴
    show_recent_usage "$stats_file"
}

# 基本統計の表示
show_basic_stats() {
    local stats_file="$1"

    local total_uses=$(wc -l <"$stats_file")
    local successful_uses=$(grep '"success":"true"' "$stats_file" | wc -l)
    local failed_uses=$((total_uses - successful_uses))

    local success_rate=0
    if [[ $total_uses -gt 0 ]]; then
        success_rate=$(echo "scale=1; $successful_uses * 100 / $total_uses" | bc 2>/dev/null || echo "0")
    fi

    echo ""
    echo "📊 基本統計:"
    echo "  総使用回数: $total_uses"
    echo "  成功: $successful_uses回"
    echo "  失敗: $failed_uses回"
    echo "  成功率: ${success_rate}%"
}

# 時系列統計の表示
show_temporal_stats() {
    local stats_file="$1"

    # 最近24時間、7日間、30日間の使用状況
    local now=$(date +%s)
    local day_ago=$((now - 86400))
    local week_ago=$((now - 604800))
    local month_ago=$((now - 2592000))

    local day_uses=$(awk -v threshold="$day_ago" 'BEGIN{count=0} /"timestamp":[0-9]+/ {match($0, /"timestamp":([0-9]+)/, arr); if(arr[1] >= threshold) count++} END{print count}' "$stats_file")
    local week_uses=$(awk -v threshold="$week_ago" 'BEGIN{count=0} /"timestamp":[0-9]+/ {match($0, /"timestamp":([0-9]+)/, arr); if(arr[1] >= threshold) count++} END{print count}' "$stats_file")
    local month_uses=$(awk -v threshold="$month_ago" 'BEGIN{count=0} /"timestamp":[0-9]+/ {match($0, /"timestamp":([0-9]+)/, arr); if(arr[1] >= threshold) count++} END{print count}' "$stats_file")

    echo ""
    echo "📅 時系列統計:"
    echo "  24時間以内: $day_uses回"
    echo "  7日以内: $week_uses回"
    echo "  30日以内: $month_uses回"
}

# 詳細統計の表示（jq使用）
show_detailed_stats() {
    local stats_file="$1"

    # 平均処理時間
    local avg_duration=$(grep '"success":"true"' "$stats_file" | jq -r '.duration' | awk '{sum+=$1; count++} END {if(count>0) printf("%.2f", sum/count); else print "0"}')
    echo ""
    echo "⏱️  パフォーマンス:"
    echo "  平均処理時間: ${avg_duration}秒"

    # 最高・最低処理時間
    local min_duration=$(grep '"success":"true"' "$stats_file" | jq -r '.duration' | awk 'BEGIN{min=999999} {if($1<min) min=$1} END {print min}')
    local max_duration=$(grep '"success":"true"' "$stats_file" | jq -r '.duration' | awk 'BEGIN{max=0} {if($1>max) max=$1} END {print max}')

    if [[ "$min_duration" != "999999" ]]; then
        echo "  最短処理時間: ${min_duration}秒"
        echo "  最長処理時間: ${max_duration}秒"
    fi

    # OS別統計
    echo ""
    echo "💻 OS別使用状況:"
    grep '"success":"true"' "$stats_file" | jq -r '.os_type' | sort | uniq -c | while read count os; do
        if [[ -n "$count" && -n "$os" ]]; then
            echo "  $os: $count回"
        fi
    done

    # 要約タイプ別統計
    echo ""
    echo "📋 要約タイプ別使用状況:"
    grep '"success":"true"' "$stats_file" | jq -r '.summary_type' | sort | uniq -c | while read count type; do
        if [[ -n "$count" && -n "$type" ]]; then
            echo "  $type: $count回"
        fi
    done

    # モデル別統計
    echo ""
    echo "🤖 モデル別使用状況:"
    grep '"success":"true"' "$stats_file" | jq -r '.model' | sort | uniq -c | while read count model; do
        if [[ -n "$count" && -n "$model" ]]; then
            echo "  $model: $count回"
        fi
    done
}

# 最近の使用履歴表示
show_recent_usage() {
    local stats_file="$1"
    local display_count="${2:-5}"

    echo ""
    echo "🕒 最近の${display_count}回の使用:"

    tail -"$display_count" "$stats_file" | while read line; do
        if has_command jq; then
            show_formatted_usage_entry "$line"
        else
            echo "  $line"
        fi
    done
}

# 使用履歴エントリーのフォーマット表示
show_formatted_usage_entry() {
    local line="$1"

    local timestamp=$(echo "$line" | jq -r '.timestamp' 2>/dev/null)
    local type=$(echo "$line" | jq -r '.summary_type' 2>/dev/null)
    local success=$(echo "$line" | jq -r '.success' 2>/dev/null)
    local duration=$(echo "$line" | jq -r '.duration' 2>/dev/null)
    local model=$(echo "$line" | jq -r '.model' 2>/dev/null)

    if [[ "$timestamp" == "null" || -z "$timestamp" ]]; then
        echo "  $line"
        return
    fi

    local date_str=$(date -r "$timestamp" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || echo "不明")

    local status_icon="✅"
    if [[ "$success" != "true" ]]; then
        status_icon="❌"
    fi

    echo "  $date_str - $type ($model) - $status_icon - ${duration}s"
}

# === 統計分析機能 ===

# 簡潔な統計サマリー
show_stats_summary() {
    local stats_file="$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl"

    if [[ ! -f "$stats_file" ]]; then
        echo "統計データなし"
        return
    fi

    local total=$(wc -l <"$stats_file")
    local success=$(grep '"success":"true"' "$stats_file" | wc -l)
    local rate=$(echo "scale=0; $success * 100 / $total" | bc 2>/dev/null || echo "0")

    echo "使用統計: ${total}回 (成功率: ${rate}%)"
}

# 統計ファイルのクリーンアップ
cleanup_old_stats() {
    local stats_dir="$CLAUDE_VOICE_HOME/logs"
    local days="${1:-90}" # デフォルト90日

    echo "統計ファイルのクリーンアップ（${days}日以上前のファイル）"

    # バックアップファイルのクリーンアップ
    local cleaned=0
    if [[ -d "$stats_dir" ]]; then
        while IFS= read -r -d '' file; do
            echo "削除: $(basename "$file")"
            rm "$file"
            ((cleaned++))
        done < <(find "$stats_dir" -name "usage_stats.jsonl.*" -mtime +"$days" -print0 2>/dev/null)
    fi

    echo "クリーンアップ完了: ${cleaned}ファイル削除"
}

# 統計のエクスポート
export_stats() {
    local stats_file="$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl"
    local output_file="${1:-claude_voice_stats_$(date +%Y%m%d).json}"

    if [[ ! -f "$stats_file" ]]; then
        echo "エクスポートする統計データがありません"
        return 1
    fi

    if has_command jq; then
        # 整形されたJSONで出力
        jq -s '.' "$stats_file" >"$output_file"
        echo "統計データをエクスポートしました: $output_file"
    else
        # jqがない場合は生ファイルをコピー
        cp "$stats_file" "$output_file"
        echo "統計データをコピーしました: $output_file"
    fi
}

# === ユーティリティ関数 ===

# コマンドの存在確認
has_command() {
    command -v "$1" >/dev/null 2>&1
}

# このモジュールが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # テスト用の環境変数設定
    CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"
    CLAUDE_VOICE_VERSION="1.0.0"

    # 簡易log関数
    log() {
        local level="$1"
        local message="$2"
        echo "[$level] $message" >&2
    }

    echo "Stats Monitor Module Test"
    echo "========================"
    echo ""

    case "${1:-summary}" in
        "record")
            echo "テスト統計を記録中..."
            record_usage_stats "brief" "test-model" "linux" "2.5" "true"
            echo "統計記録完了"
            ;;
        "show")
            show_stats
            ;;
        "summary")
            show_stats_summary
            ;;
        "cleanup")
            cleanup_old_stats 30
            ;;
        "export")
            export_stats "test_export.json"
            ;;
        *)
            echo "Available tests: record, show, summary, cleanup, export"
            ;;
    esac
fi

# 使用パターン分析機能
analyze_usage_patterns() {
    local stats_file="${1:-"$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl"}"
    
    if [[ ! -f "$stats_file" ]]; then
        log "WARN" "統計ファイルが見つかりません: $stats_file"
        return 1
    fi
    
    log "INFO" "使用パターン分析を実行中..."
    
    # 基本的な使用パターン分析
    echo ""
    echo "📈 使用パターン分析結果:"
    
    # 最も使用されているモデル
    local most_used_model=$(grep '"model"' "$stats_file" | grep -v '"auto"' | sort | uniq -c | sort -nr | head -1 | awk '{print $2}' | tr -d '",')
    if [[ -n "$most_used_model" ]]; then
        echo "  最頻使用モデル: $most_used_model"
    fi
    
    # 平均処理時間の推移
    local avg_duration=$(grep '"success":"true"' "$stats_file" | grep '"duration"' | sed 's/.*"duration": *\([0-9.]*\).*/\1/' | awk '{sum+=$1; count++} END {if(count>0) printf("%.2f", sum/count); else print "0"}')
    echo "  平均処理時間: ${avg_duration}秒"
    
    # 最近の処理時間の傾向
    local recent_avg=$(tail -10 "$stats_file" | grep '"success":"true"' | grep '"duration"' | sed 's/.*"duration": *\([0-9.]*\).*/\1/' | awk '{sum+=$1; count++} END {if(count>0) printf("%.2f", sum/count); else print "0"}')
    echo "  最近10回の平均: ${recent_avg}秒"
    
    # 成功率
    local total_uses=$(wc -l <"$stats_file")
    local successful_uses=$(grep '"success":"true"' "$stats_file" | wc -l)
    local success_rate=0
    if [[ $total_uses -gt 0 ]]; then
        success_rate=$(echo "scale=1; $successful_uses * 100 / $total_uses" | bc 2>/dev/null || echo "0")
    fi
    echo "  成功率: ${success_rate}%"
    
    # パフォーマンス推奨事項
    echo ""
    echo "💡 推奨事項:"
    if [[ $(echo "$avg_duration > 20" | bc -l 2>/dev/null || echo "0") -eq 1 ]]; then
        echo "  ⚠️  平均処理時間が20秒を超えています。gemma2:2bモデルの使用を推奨します"
    elif [[ $(echo "$avg_duration < 15" | bc -l 2>/dev/null || echo "1") -eq 1 ]]; then
        echo "  ✅ パフォーマンスは良好です"
    fi
    
    echo ""
    log "INFO" "パターン分析完了"
}

# === 統計サマリー計算機能 ===

# 統計サマリーの計算
calculate_stats_summary() {
    local stats_file="${1:-$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl}"
    
    log "DEBUG" "統計サマリー計算開始: $stats_file"
    
    if [[ ! -f "$stats_file" ]]; then
        echo "統計ファイルが見つかりません: $stats_file"
        return 1
    fi
    
    local total_operations=$(wc -l < "$stats_file" 2>/dev/null || echo "0")
    local successful_operations=$(grep '"success": true' "$stats_file" 2>/dev/null | wc -l)
    local failed_operations=$(grep '"success": false' "$stats_file" 2>/dev/null | wc -l)
    
    echo "=== Claude Voice 統計サマリー ==="
    echo "総実行回数: $total_operations"
    echo "成功: $successful_operations"
    echo "失敗: $failed_operations"
    
    if [[ $total_operations -gt 0 ]]; then
        local success_rate=$((successful_operations * 100 / total_operations))
        echo "成功率: ${success_rate}%"
        
        # 平均実行時間の計算
        local avg_duration=$(grep -o '"duration": [0-9]*' "$stats_file" 2>/dev/null | \
            awk -F': ' '{sum+=$2; count++} END {if(count>0) print int(sum/count); else print 0}')
        echo "平均実行時間: ${avg_duration}秒"
        
        # 最も使用されるモデル
        local top_model=$(grep -o '"model": "[^"]*"' "$stats_file" 2>/dev/null | \
            sort | uniq -c | sort -nr | head -1 | awk '{print $2}' | tr -d '"')
        echo "最頻使用モデル: ${top_model:-"不明"}"
        
        # 最も使用される要約タイプ
        local top_summary_type=$(grep -o '"summary_type": "[^"]*"' "$stats_file" 2>/dev/null | \
            sort | uniq -c | sort -nr | head -1 | awk '{print $2}' | tr -d '"')
        echo "最頻要約タイプ: ${top_summary_type:-"不明"}"
    else
        echo "成功率: 0%"
    fi
    
    log "DEBUG" "統計サマリー計算完了"
    return 0
}

# 統計出力のフォーマット
format_stats_output() {
    local format="${1:-text}" # text, json, csv
    local stats_file="${2:-$CLAUDE_VOICE_HOME/logs/usage_stats.jsonl}"
    
    case "$format" in
        "json")
            format_stats_as_json "$stats_file"
            ;;
        "csv")
            format_stats_as_csv "$stats_file"
            ;;
        "text"|*)
            format_stats_as_text "$stats_file"
            ;;
    esac
}

# テキスト形式での統計出力
format_stats_as_text() {
    local stats_file="$1"
    calculate_stats_summary "$stats_file"
}

# JSON形式での統計出力
format_stats_as_json() {
    local stats_file="$1"
    echo "{"
    echo '  "claude_voice_stats": {'
    if [[ -f "$stats_file" ]]; then
        local total=$(wc -l < "$stats_file" 2>/dev/null || echo "0")
        local success=$(grep '"success": true' "$stats_file" 2>/dev/null | wc -l)
        echo "    \"total_operations\": $total,"
        echo "    \"successful_operations\": $success,"
        echo "    \"failed_operations\": $((total - success))"
    else
        echo "    \"total_operations\": 0,"
        echo "    \"successful_operations\": 0,"
        echo "    \"failed_operations\": 0"
    fi
    echo '  }'
    echo "}"
}

# CSV形式での統計出力
format_stats_as_csv() {
    local stats_file="$1"
    echo "timestamp,operation,summary_type,model,os_type,duration,success"
    if [[ -f "$stats_file" ]]; then
        while IFS= read -r line; do
            echo "$line" | jq -r '[.timestamp, .operation, .summary_type, .model, .os_type, .duration, .success] | @csv' 2>/dev/null || echo "$line"
        done < "$stats_file"
    fi
}
