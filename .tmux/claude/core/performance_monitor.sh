#!/bin/bash
# Performance Monitor - System Performance Monitoring
# システムパフォーマンス監視・最適化モジュール

# 依存関係のインポート
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/cache_manager.sh"
source "$SCRIPT_DIR/config_manager_v2.sh"

# === Performance Configuration ===

declare -A PERF_CONFIG=(
    ["enable_monitoring"]="true"        # 監視有効化
    ["sample_interval"]="1"             # サンプリング間隔（秒）
    ["history_retention"]="300"         # 履歴保持期間（秒）
    ["alert_threshold_cpu"]="80"        # CPU使用率アラート閾値（%）
    ["alert_threshold_memory"]="90"     # メモリ使用率アラート閾値（%）
    ["alert_threshold_response"]="5000" # 応答時間アラート閾値（ms）
    ["enable_auto_optimization"]="true" # 自動最適化有効化
)

# === Performance Data Storage ===

declare -A PERFORMANCE_METRICS=()      # 現在のメトリクス
declare -A PERFORMANCE_HISTORY=()      # 履歴データ
declare -A PERFORMANCE_ALERTS=()       # アラート状態

# メトリクス初期化
PERFORMANCE_METRICS=(
    ["cpu_percent"]="0"
    ["memory_percent"]="0"
    ["response_time_ms"]="0"
    ["cache_hit_rate"]="0"
    ["active_processes"]="0"
    ["last_update"]="0"
)

# === Core Performance Functions ===

# システムメトリクスの収集
collect_system_metrics() {
    local timestamp=$(date +%s)
    
    # CPU使用率（簡易計算）
    local cpu_percent=0
    if command -v top >/dev/null 2>&1; then
        cpu_percent=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | sed 's/%us,//' | cut -d'%' -f1 | cut -d'.' -f1 2>/dev/null || echo "0")
        # 数値でない場合のフォールバック
        if ! [[ "$cpu_percent" =~ ^[0-9]+$ ]]; then
            cpu_percent=0
        fi
    fi
    
    # メモリ使用率
    local memory_percent=0
    if [[ -f /proc/meminfo ]]; then
        local mem_total=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        local mem_available=$(grep MemAvailable /proc/meminfo | awk '{print $2}' 2>/dev/null || echo "0")
        if [[ $mem_total -gt 0 && $mem_available -gt 0 ]]; then
            memory_percent=$(( (mem_total - mem_available) * 100 / mem_total ))
        fi
    fi
    
    # プロセス数
    local active_processes=0
    if command -v ps >/dev/null 2>&1; then
        active_processes=$(ps aux | wc -l)
    fi
    
    # メトリクス更新
    PERFORMANCE_METRICS["cpu_percent"]="$cpu_percent"
    PERFORMANCE_METRICS["memory_percent"]="$memory_percent"
    PERFORMANCE_METRICS["active_processes"]="$active_processes"
    PERFORMANCE_METRICS["last_update"]="$timestamp"
    
    # キャッシュ統計の取得
    if declare -f show_cache_statistics >/dev/null; then
        local cache_stats=$(show_cache_statistics 2>/dev/null)
        local hit_rate=$(echo "$cache_stats" | grep "Hit Rate:" | awk '{print $3}' | sed 's/%//' || echo "0")
        PERFORMANCE_METRICS["cache_hit_rate"]="$hit_rate"
    fi
    
    # 履歴への追加
    local history_key="$timestamp"
    PERFORMANCE_HISTORY["$history_key"]="$cpu_percent|$memory_percent|$active_processes|$hit_rate"
    
    # 古い履歴の削除
    cleanup_performance_history
}

# 応答時間の測定
measure_response_time() {
    local command_to_test="$1"
    local start_time=$(date +%s%N)
    
    # コマンド実行（タイムアウト付き）
    local timeout_duration="5"
    if timeout "${timeout_duration}s" bash -c "$command_to_test" >/dev/null 2>&1; then
        local end_time=$(date +%s%N)
        local response_time=$(( (end_time - start_time) / 1000000 ))
        
        PERFORMANCE_METRICS["response_time_ms"]="$response_time"
        return 0
    else
        # タイムアウトまたはエラー
        PERFORMANCE_METRICS["response_time_ms"]="9999"
        return 1
    fi
}

# Claude検出の応答時間測定
measure_claude_detection_performance() {
    local test_command="source '$SCRIPT_DIR/status_detector.sh' && detect_claude_status_pure 'Testing performance'"
    measure_response_time "$test_command"
}

# === Performance Analysis ===

# パフォーマンス分析
analyze_performance() {
    local analysis_result=""
    local current_time=$(date +%s)
    local cpu="${PERFORMANCE_METRICS[cpu_percent]}"
    local memory="${PERFORMANCE_METRICS[memory_percent]}"
    local response_time="${PERFORMANCE_METRICS[response_time_ms]}"
    local cache_hit_rate="${PERFORMANCE_METRICS[cache_hit_rate]}"
    
    # しきい値チェック
    local issues=()
    local recommendations=()
    
    # CPU使用率チェック
    if [[ $cpu -gt ${PERF_CONFIG[alert_threshold_cpu]} ]]; then
        issues+=("High CPU usage: ${cpu}%")
        recommendations+=("Consider reducing polling frequency")
    fi
    
    # メモリ使用率チェック
    if [[ $memory -gt ${PERF_CONFIG[alert_threshold_memory]} ]]; then
        issues+=("High memory usage: ${memory}%")
        recommendations+=("Enable cache compression or reduce cache size")
    fi
    
    # 応答時間チェック
    if [[ $response_time -gt ${PERF_CONFIG[alert_threshold_response]} ]]; then
        issues+=("Slow response time: ${response_time}ms")
        recommendations+=("Enable caching or optimize detection logic")
    fi
    
    # キャッシュ効率チェック
    if [[ $cache_hit_rate -lt 50 ]]; then
        issues+=("Low cache hit rate: ${cache_hit_rate}%")
        recommendations+=("Increase cache TTL or review caching strategy")
    fi
    
    # 分析結果の構築
    analysis_result="=== Performance Analysis ===\n"
    analysis_result+="Timestamp: $(date -d "@$current_time")\n"
    analysis_result+="CPU: ${cpu}%, Memory: ${memory}%, Response: ${response_time}ms\n"
    analysis_result+="Cache Hit Rate: ${cache_hit_rate}%\n\n"
    
    if [[ ${#issues[@]} -gt 0 ]]; then
        analysis_result+="Issues Found:\n"
        for issue in "${issues[@]}"; do
            analysis_result+="  ⚠️  $issue\n"
        done
        analysis_result+="\nRecommendations:\n"
        for rec in "${recommendations[@]}"; do
            analysis_result+="  💡 $rec\n"
        done
    else
        analysis_result+="✅ Performance is within acceptable limits\n"
    fi
    
    echo -e "$analysis_result"
    
    # アラート状態の更新
    update_alert_status "${issues[@]}"
    
    # 自動最適化の実行
    if [[ "${PERF_CONFIG[enable_auto_optimization]}" == "true" && ${#issues[@]} -gt 0 ]]; then
        apply_auto_optimizations "${issues[@]}"
    fi
}

# アラート状態の更新
update_alert_status() {
    local issues=("$@")
    local current_time=$(date +%s)
    
    # 既存アラートのクリア
    for alert_key in "${!PERFORMANCE_ALERTS[@]}"; do
        unset PERFORMANCE_ALERTS["$alert_key"]
    done
    
    # 新しいアラートの設定
    for issue in "${issues[@]}"; do
        local alert_key=$(echo "$issue" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
        PERFORMANCE_ALERTS["$alert_key"]="$current_time|$issue"
    done
}

# === Auto-Optimization ===

# 自動最適化の適用
apply_auto_optimizations() {
    local issues=("$@")
    local optimizations_applied=()
    
    for issue in "${issues[@]}"; do
        case "$issue" in
            *"High CPU usage"*)
                # ポーリング間隔を増加
                local current_interval=$(get_config "detection.polling_interval" "1")
                local new_interval=$((current_interval + 1))
                set_config "detection.polling_interval" "$new_interval" "false"
                optimizations_applied+=("Increased polling interval to ${new_interval}s")
                ;;
            *"High memory usage"*)
                # キャッシュ圧縮を有効化
                configure_cache "enable_compression" "true"
                optimizations_applied+=("Enabled cache compression")
                ;;
            *"Slow response time"*)
                # キャッシュTTLを増加
                local current_ttl=$(get_config "performance.cache_ttl" "5")
                local new_ttl=$((current_ttl + 2))
                set_config "performance.cache_ttl" "$new_ttl" "false"
                optimizations_applied+=("Increased cache TTL to ${new_ttl}s")
                ;;
            *"Low cache hit rate"*)
                # キャッシュサイズを増加
                local current_max=$(get_config "performance.max_concurrent_ops" "10")
                local new_max=$((current_max + 5))
                set_config "performance.max_concurrent_ops" "$new_max" "false"
                optimizations_applied+=("Increased cache capacity to $new_max")
                ;;
        esac
    done
    
    if [[ ${#optimizations_applied[@]} -gt 0 ]]; then
        echo "Auto-optimizations applied:"
        for opt in "${optimizations_applied[@]}"; do
            echo "  🔧 $opt"
        done
    fi
}

# === Historical Analysis ===

# 履歴データのクリーンアップ
cleanup_performance_history() {
    local current_time=$(date +%s)
    local retention_period="${PERF_CONFIG[history_retention]}"
    local cutoff_time=$((current_time - retention_period))
    local keys_to_delete=()
    
    # 古いエントリを特定
    for timestamp in "${!PERFORMANCE_HISTORY[@]}"; do
        if [[ $timestamp -lt $cutoff_time ]]; then
            keys_to_delete+=("$timestamp")
        fi
    done
    
    # 削除実行
    for key in "${keys_to_delete[@]}"; do
        unset PERFORMANCE_HISTORY["$key"]
    done
}

# 履歴統計の計算
calculate_historical_stats() {
    local metric_type="${1:-cpu_percent}"  # cpu_percent, memory_percent, etc.
    local time_window="${2:-60}"           # 秒
    
    local current_time=$(date +%s)
    local start_time=$((current_time - time_window))
    local values=()
    
    # 指定期間のデータを収集
    for timestamp in "${!PERFORMANCE_HISTORY[@]}"; do
        if [[ $timestamp -ge $start_time ]]; then
            local data="${PERFORMANCE_HISTORY[$timestamp]}"
            IFS='|' read -ra metrics <<< "$data"
            
            case "$metric_type" in
                "cpu_percent")
                    values+=("${metrics[0]}")
                    ;;
                "memory_percent")
                    values+=("${metrics[1]}")
                    ;;
                "active_processes")
                    values+=("${metrics[2]}")
                    ;;
                "cache_hit_rate")
                    values+=("${metrics[3]}")
                    ;;
            esac
        fi
    done
    
    # 統計計算
    if [[ ${#values[@]} -gt 0 ]]; then
        local sum=0
        local min=${values[0]}
        local max=${values[0]}
        
        for value in "${values[@]}"; do
            sum=$((sum + value))
            if [[ $value -lt $min ]]; then
                min=$value
            fi
            if [[ $value -gt $max ]]; then
                max=$value
            fi
        done
        
        local avg=$((sum / ${#values[@]}))
        
        echo "Metric: $metric_type (${time_window}s window)"
        echo "  Samples: ${#values[@]}"
        echo "  Average: $avg"
        echo "  Min: $min"
        echo "  Max: $max"
    else
        echo "No data available for $metric_type in the last ${time_window}s"
    fi
}

# === Monitoring and Reporting ===

# 継続的監視の開始
start_performance_monitoring() {
    local interval="${1:-${PERF_CONFIG[sample_interval]}}"
    
    echo "Starting performance monitoring (interval: ${interval}s)"
    echo "Press Ctrl+C to stop"
    
    while true; do
        collect_system_metrics
        measure_claude_detection_performance
        
        # 定期的な分析実行
        local current_time=$(date +%s)
        if [[ $((current_time % 30)) -eq 0 ]]; then
            analyze_performance
        fi
        
        sleep "$interval"
    done
}

# 現在のパフォーマンス状況表示
show_current_performance() {
    local last_update="${PERFORMANCE_METRICS[last_update]}"
    local age=$(($(date +%s) - last_update))
    
    echo "=== Current Performance Status ==="
    echo "Last Update: $(date -d "@$last_update" 2>/dev/null || echo "Never") (${age}s ago)"
    echo ""
    echo "System Metrics:"
    echo "  CPU Usage: ${PERFORMANCE_METRICS[cpu_percent]}%"
    echo "  Memory Usage: ${PERFORMANCE_METRICS[memory_percent]}%"
    echo "  Active Processes: ${PERFORMANCE_METRICS[active_processes]}"
    echo ""
    echo "Application Metrics:"
    echo "  Response Time: ${PERFORMANCE_METRICS[response_time_ms]}ms"
    echo "  Cache Hit Rate: ${PERFORMANCE_METRICS[cache_hit_rate]}%"
    echo ""
    
    # アクティブアラート
    if [[ ${#PERFORMANCE_ALERTS[@]} -gt 0 ]]; then
        echo "Active Alerts:"
        for alert_key in "${!PERFORMANCE_ALERTS[@]}"; do
            local alert_data="${PERFORMANCE_ALERTS[$alert_key]}"
            IFS='|' read -ra alert_parts <<< "$alert_data"
            local alert_time="${alert_parts[0]}"
            local alert_message="${alert_parts[1]}"
            local alert_age=$(( $(date +%s) - alert_time ))
            echo "  🚨 $alert_message (${alert_age}s ago)"
        done
    else
        echo "No active alerts"
    fi
}

# パフォーマンスレポートの生成
generate_performance_report() {
    local time_window="${1:-300}"  # 5分間のレポート
    
    echo "=== Performance Report ==="
    echo "Time Window: ${time_window} seconds"
    echo "Generated: $(date)"
    echo ""
    
    # 各メトリクスの統計
    calculate_historical_stats "cpu_percent" "$time_window"
    echo ""
    calculate_historical_stats "memory_percent" "$time_window"
    echo ""
    calculate_historical_stats "cache_hit_rate" "$time_window"
    echo ""
    
    # 分析実行
    collect_system_metrics
    measure_claude_detection_performance
    analyze_performance
}

# === Test Functions ===

# パフォーマンス監視テスト
test_performance_monitor() {
    echo "=== Performance Monitor Test ==="
    echo ""
    
    # メトリクス収集テスト
    echo "1. Metrics Collection Test:"
    collect_system_metrics
    echo "  ✅ System metrics collected"
    echo ""
    
    # 応答時間測定テスト
    echo "2. Response Time Test:"
    measure_claude_detection_performance
    local response_time="${PERFORMANCE_METRICS[response_time_ms]}"
    echo "  Detection response time: ${response_time}ms"
    echo ""
    
    # 現在状況表示
    echo "3. Current Status:"
    show_current_performance
    echo ""
    
    # 履歴統計テスト
    echo "4. Historical Analysis Test:"
    # いくつかの履歴データを作成
    for i in {1..5}; do
        collect_system_metrics
        sleep 1
    done
    calculate_historical_stats "cpu_percent" "10"
}

# === Main Execution ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_performance_monitor
            ;;
        "monitor")
            start_performance_monitoring "${2:-1}"
            ;;
        "status")
            collect_system_metrics
            measure_claude_detection_performance
            show_current_performance
            ;;
        "analyze")
            collect_system_metrics
            measure_claude_detection_performance
            analyze_performance
            ;;
        "report")
            generate_performance_report "${2:-300}"
            ;;
        "stats")
            calculate_historical_stats "${2:-cpu_percent}" "${3:-60}"
            ;;
        *)
            echo "Usage: $0 [test|monitor|status|analyze|report|stats]"
            ;;
    esac
fi