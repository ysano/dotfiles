#!/bin/bash
# Performance Monitor for tmux Claude Integration
# WSL改善をmacOS環境に適用したパフォーマンス監視システム

# メトリクス収集（macOS最適化版）
collect_macos_metrics() {
    local metrics_file="${1:-$HOME/.tmux/claude/logs/performance.log}"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # CPU使用率（より精密な測定）
    local cpu_percent=$(top -l1 -n0 | grep "CPU usage" | awk '{print $3}' | sed 's/%//g')
    [[ -z "$cpu_percent" ]] && cpu_percent="0"
    
    # メモリ使用率（vm_stat活用）
    local memory_stats=$(vm_stat)
    local pages_free=$(echo "$memory_stats" | grep "Pages free" | awk '{print $3}' | sed 's/\.//')
    local pages_active=$(echo "$memory_stats" | grep "Pages active" | awk '{print $3}' | sed 's/\.//')
    local pages_inactive=$(echo "$memory_stats" | grep "Pages inactive" | awk '{print $3}' | sed 's/\.//')
    local pages_wired=$(echo "$memory_stats" | grep "Pages wired down" | awk '{print $4}' | sed 's/\.//')
    
    # ページサイズの取得（通常4KB）
    local page_size=$(vm_stat | head -1 | grep -o '[0-9]\+')
    [[ -z "$page_size" ]] && page_size=4096
    
    # メモリ使用率計算
    local total_pages=$((pages_free + pages_active + pages_inactive + pages_wired))
    local used_pages=$((pages_active + pages_inactive + pages_wired))
    local memory_percent=0
    if [[ $total_pages -gt 0 ]]; then
        memory_percent=$((used_pages * 100 / total_pages))
    fi
    
    # tmux固有プロセス監視
    local tmux_processes=$(pgrep -c tmux || echo "0")
    local claude_processes=$(pgrep -c -f "(claude-voice|tmux.*claude)" || echo "0")
    
    # 負荷平均
    local load_avg=$(uptime | awk -F'load averages:' '{print $2}' | awk '{print $1}' | sed 's/,//')
    
    # ディスク使用率（tmuxディレクトリ）
    local disk_usage=$(df -h "$HOME/.tmux" 2>/dev/null | tail -1 | awk '{print $5}' | sed 's/%//')
    [[ -z "$disk_usage" ]] && disk_usage="0"
    
    # Homebrewの高度なメトリクス（利用可能な場合）
    local advanced_stats=""
    if command -v tmux-mem-cpu-load >/dev/null 2>&1; then
        advanced_stats=$(tmux-mem-cpu-load --colors --interval 1 --mem-mode 2 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g')
    fi
    
    # JSONライン形式でログ出力
    cat >> "$metrics_file" <<EOF
{"timestamp":"$timestamp","cpu_percent":$cpu_percent,"memory_percent":$memory_percent,"tmux_processes":$tmux_processes,"claude_processes":$claude_processes,"load_avg":"$load_avg","disk_usage":$disk_usage,"advanced_stats":"$advanced_stats"}
EOF
}

# パフォーマンス分析
analyze_performance() {
    local metrics_file="${1:-$HOME/.tmux/claude/logs/performance.log}"
    local window_minutes="${2:-60}"
    
    if [[ ! -f "$metrics_file" ]]; then
        echo "No performance data available"
        return 1
    fi
    
    # 過去N分間のデータを分析
    local cutoff_time=$(date -v-${window_minutes}M '+%Y-%m-%d %H:%M:%S')
    
    echo "=== Performance Analysis (Last ${window_minutes} minutes) ==="
    
    # CPU使用率の統計
    local cpu_stats=$(tail -100 "$metrics_file" | \
        jq -r "select(.timestamp >= \"$cutoff_time\") | .cpu_percent" 2>/dev/null | \
        awk '{sum+=$1; count++; if($1>max) max=$1} END {
            avg = (count > 0) ? sum/count : 0;
            printf "Avg: %.1f%%, Max: %.1f%%", avg, max
        }')
    echo "CPU Usage: $cpu_stats"
    
    # メモリ使用率の統計
    local memory_stats=$(tail -100 "$metrics_file" | \
        jq -r "select(.timestamp >= \"$cutoff_time\") | .memory_percent" 2>/dev/null | \
        awk '{sum+=$1; count++; if($1>max) max=$1} END {
            avg = (count > 0) ? sum/count : 0;
            printf "Avg: %.1f%%, Max: %.1f%%", avg, max
        }')
    echo "Memory Usage: $memory_stats"
    
    # tmux関連プロセス数
    local process_count=$(tail -10 "$metrics_file" | \
        jq -r '.tmux_processes + .claude_processes' 2>/dev/null | tail -1)
    echo "Active Processes: tmux+claude = $process_count"
    
    # システム負荷
    local current_load=$(tail -1 "$metrics_file" | jq -r '.load_avg' 2>/dev/null)
    echo "System Load: $current_load"
}

# アラート機能
check_performance_alerts() {
    local metrics_file="${1:-$HOME/.tmux/claude/logs/performance.log}"
    local cpu_threshold="${2:-80}"
    local memory_threshold="${3:-85}"
    
    if [[ ! -f "$metrics_file" ]]; then
        return 0
    fi
    
    # 最新のメトリクスを取得
    local latest_cpu=$(tail -1 "$metrics_file" | jq -r '.cpu_percent' 2>/dev/null || echo "0")
    local latest_memory=$(tail -1 "$metrics_file" | jq -r '.memory_percent' 2>/dev/null || echo "0")
    
    # CPU使用率チェック
    if [[ $(echo "$latest_cpu >= $cpu_threshold" | bc 2>/dev/null || echo "0") -eq 1 ]]; then
        echo "🔥 HIGH CPU USAGE: ${latest_cpu}% (threshold: ${cpu_threshold}%)"
        
        # 高CPU使用時の対策提案
        echo "   Recommendations:"
        echo "   - Check for excessive tmux sessions: tmux list-sessions"
        echo "   - Monitor Claude Voice processes: ps aux | grep claude"
    fi
    
    # メモリ使用率チェック
    if [[ $(echo "$latest_memory >= $memory_threshold" | bc 2>/dev/null || echo "0") -eq 1 ]]; then
        echo "💾 HIGH MEMORY USAGE: ${latest_memory}% (threshold: ${memory_threshold}%)"
        
        # 高メモリ使用時の対策提案
        echo "   Recommendations:"
        echo "   - Clear tmux history: tmux clear-history -a"
        echo "   - Check for memory leaks in Claude modules"
    fi
}

# パフォーマンス最適化の自動提案
suggest_optimizations() {
    local metrics_file="${1:-$HOME/.tmux/claude/logs/performance.log}"
    
    echo "=== Performance Optimization Suggestions ==="
    
    # tmux設定の最適化提案
    local current_interval=$(tmux show-options -g status-interval 2>/dev/null | cut -d' ' -f2)
    if [[ "$current_interval" -lt 5 ]]; then
        echo "🔧 Consider increasing status-interval to 5+ seconds for better performance"
    fi
    
    # ヒストリ設定の確認
    local history_limit=$(tmux show-options -g history-limit 2>/dev/null | cut -d' ' -f2)
    if [[ "$history_limit" -gt 10000 ]]; then
        echo "🔧 Consider reducing history-limit from $history_limit to 10000 or less"
    fi
    
    # Claude Voiceプロセスの最適化
    local claude_process_count=$(pgrep -c -E "(claude-voice|say)" || echo "0")
    if [[ "$claude_process_count" -gt 2 ]]; then
        echo "🔧 Multiple voice processes detected ($claude_process_count). Consider enabling process limiting"
    fi
    
    # macOS固有の最適化
    echo ""
    echo "macOS-specific optimizations:"
    echo "✅ Use Activity Monitor to check for high CPU processes"
    echo "✅ Enable 'Reduce motion' in Accessibility settings for better performance"
    echo "✅ Consider using tmux-mem-cpu-load for advanced monitoring"
    
    if ! command -v tmux-mem-cpu-load >/dev/null 2>&1; then
        echo "💡 Install tmux-mem-cpu-load: brew install tmux-mem-cpu-load"
    fi
}

# 設定値の動的最適化
auto_optimize_settings() {
    local metrics_file="${1:-$HOME/.tmux/claude/logs/performance.log}"
    local dry_run="${2:-false}"
    
    if [[ ! -f "$metrics_file" ]]; then
        echo "No performance data for optimization"
        return 1
    fi
    
    # 過去10分間の平均CPU使用率を計算
    local avg_cpu=$(tail -20 "$metrics_file" | \
        jq -r '.cpu_percent' 2>/dev/null | \
        awk '{sum+=$1; count++} END {print (count > 0) ? sum/count : 0}')
    
    echo "Average CPU usage (last 10 min): ${avg_cpu}%"
    
    # CPU使用率に基づく動的調整
    if [[ $(echo "$avg_cpu >= 70" | bc 2>/dev/null || echo "0") -eq 1 ]]; then
        echo "🔧 High CPU detected - Suggesting conservative settings"
        
        if [[ "$dry_run" == "false" ]]; then
            # status-intervalを増加
            tmux set-option -g status-interval 10
            echo "   ✅ Increased status-interval to 10 seconds"
            
            # ヒストリ制限を縮小
            tmux set-option -g history-limit 5000
            echo "   ✅ Reduced history-limit to 5000"
        else
            echo "   [DRY RUN] Would increase status-interval to 10 seconds"
            echo "   [DRY RUN] Would reduce history-limit to 5000"
        fi
    elif [[ $(echo "$avg_cpu <= 20" | bc 2>/dev/null || echo "0") -eq 1 ]]; then
        echo "⚡ Low CPU detected - Enabling responsive settings"
        
        if [[ "$dry_run" == "false" ]]; then
            # status-intervalを減少
            tmux set-option -g status-interval 5
            echo "   ✅ Set status-interval to 5 seconds"
            
            # ヒストリ制限を標準化
            tmux set-option -g history-limit 10000
            echo "   ✅ Set history-limit to 10000"
        else
            echo "   [DRY RUN] Would set status-interval to 5 seconds"
            echo "   [DRY RUN] Would set history-limit to 10000"
        fi
    fi
}

# ログ管理
manage_performance_logs() {
    local log_dir="$HOME/.tmux/claude/logs"
    local metrics_file="$log_dir/performance.log"
    local max_lines="${1:-1000}"
    
    mkdir -p "$log_dir"
    
    # ログローテーション
    if [[ -f "$metrics_file" ]] && [[ $(wc -l < "$metrics_file") -gt $max_lines ]]; then
        # 古いログをアーカイブ
        local archive_file="$log_dir/performance_$(date +%Y%m%d_%H%M%S).log"
        tail -$((max_lines / 2)) "$metrics_file" > "$archive_file.tmp"
        mv "$archive_file.tmp" "$metrics_file"
        
        echo "Performance log rotated - kept last $((max_lines / 2)) entries"
    fi
}

# CLIインターフェース
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-}" in
        "collect")
            collect_macos_metrics "$2"
            ;;
        "analyze")
            analyze_performance "$2" "$3"
            ;;
        "alerts")
            check_performance_alerts "$2" "$3" "$4"
            ;;
        "optimize")
            auto_optimize_settings "$2" "$3"
            ;;
        "suggest")
            suggest_optimizations "$2"
            ;;
        "clean")
            manage_performance_logs "$2"
            ;;
        *)
            echo "Usage: $0 {collect|analyze|alerts|optimize|suggest|clean}"
            echo "  collect [file]           - Collect performance metrics"
            echo "  analyze [file] [minutes] - Analyze performance data"
            echo "  alerts [file] [cpu%] [mem%] - Check performance alerts"
            echo "  optimize [file] [dry_run] - Auto-optimize settings"
            echo "  suggest [file]           - Suggest optimizations"
            echo "  clean [max_lines]        - Manage log files"
            exit 1
            ;;
    esac
fi