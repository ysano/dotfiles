#!/bin/bash
# Status Bar Performance Test Suite
# 動的vs静的ステータスバーのパフォーマンス比較

set -euo pipefail

# テスト設定
ITERATIONS=10
DELAY=0.1

# 結果ファイル
RESULTS_DIR="/tmp/tmux-status-test-$$"
mkdir -p "$RESULTS_DIR"

log_info() {
    echo "[INFO] $*" >&2
}

log_result() {
    echo "[RESULT] $*"
}

# 単一ステータス更新時間測定
measure_status_update() {
    local method="$1"
    local config_cmd="$2"
    local total_time=0
    
    log_info "Testing $method status bar..."
    
    for i in $(seq 1 $ITERATIONS); do
        start_time=$(date +%s.%N)
        
        # ステータスバー設定を適用
        eval "$config_cmd" > /dev/null 2>&1
        
        end_time=$(date +%s.%N)
        iteration_time=$(echo "$end_time - $start_time" | bc -l)
        total_time=$(echo "$total_time + $iteration_time" | bc -l)
        
        sleep $DELAY
    done
    
    local avg_time=$(echo "scale=4; $total_time / $ITERATIONS" | bc -l)
    echo "$avg_time"
}

# 動的ステータスバーテスト
test_dynamic_status() {
    local os="${1:-$(~/.tmux/scripts/generate-status-bar.sh detect)}"
    local template="${2:-full}"
    
    local cmd="~/.tmux/scripts/generate-status-bar.sh generate $os $template"
    measure_status_update "Dynamic ($os-$template)" "$cmd"
}

# 静的ステータスバーテスト（参考）
test_static_status() {
    local os="$1"
    
    case "$os" in
        "wsl")
            local cmd='echo "set -g status-right \"WSL Static Status %H:%M\""'
            ;;
        "macos")
            local cmd='echo "set -g status-right \"macOS Static Status %H:%M\""'
            ;;
        *)
            local cmd='echo "set -g status-right \"Static Status %H:%M\""'
            ;;
    esac
    
    measure_status_update "Static ($os)" "$cmd"
}

# システム負荷テストの実行
test_system_load() {
    local os="$1"
    
    log_info "Testing system load impact for $os..."
    
    # 各OS固有コマンドの実行時間測定
    eval "$(~/.tmux/scripts/generate-status-bar.sh generate | head -1 | sed 's/.*get_os_commands "\([^"]*\)".*/\1/')"
    
    case "$os" in
        "wsl")
            echo "Load command: $(time (cat /proc/loadavg | cut -d ' ' -f 1-3) 2>&1 | grep real)"
            echo "Memory command: $(time (free | awk 'NR==2{printf \"%.0f%%\", $3*100/$2}') 2>&1 | grep real)"
            echo "Disk command: $(time (df -h / | awk 'NR==2{print $5}') 2>&1 | grep real)"
            ;;
        "macos")
            if command -v tmux-mem-cpu-load >/dev/null; then
                echo "tmux-mem-cpu-load: $(time (tmux-mem-cpu-load --colors --interval 2 --graph-lines 0 --mem-mode 2) 2>&1 | grep real)"
            else
                echo "sysctl load: $(time (sysctl -n vm.loadavg | cut -d' ' -f2) 2>&1 | grep real)"
            fi
            ;;
    esac
}

# メイン比較テスト
run_comparison_test() {
    local os="${1:-$(~/.tmux/scripts/generate-status-bar.sh detect)}"
    
    log_info "=== tmux Status Bar Performance Comparison ==="
    log_info "OS: $os, Iterations: $ITERATIONS"
    echo ""
    
    # 動的ステータスバーテスト
    local dynamic_full_time=$(test_dynamic_status "$os" "full")
    local dynamic_minimal_time=$(test_dynamic_status "$os" "minimal")
    
    # 静的ステータスバーテスト
    local static_time=$(test_static_status "$os")
    
    # 結果出力
    log_result "Dynamic Status Bar (Full):    ${dynamic_full_time}s average"
    log_result "Dynamic Status Bar (Minimal): ${dynamic_minimal_time}s average"
    log_result "Static Status Bar:            ${static_time}s average"
    
    # パフォーマンス比較
    local full_vs_static=$(echo "scale=2; $dynamic_full_time / $static_time" | bc -l)
    local minimal_vs_static=$(echo "scale=2; $dynamic_minimal_time / $static_time" | bc -l)
    
    echo ""
    log_result "Performance Ratios (vs Static):"
    log_result "  Dynamic Full:    ${full_vs_static}x"
    log_result "  Dynamic Minimal: ${minimal_vs_static}x"
    
    # システム負荷測定
    echo ""
    test_system_load "$os"
}

# 推奨事項生成
generate_recommendations() {
    local dynamic_full_time="$1"
    local dynamic_minimal_time="$2" 
    local static_time="$3"
    
    echo ""
    log_info "=== Performance Recommendations ==="
    
    if (( $(echo "$dynamic_full_time < 0.1" | bc -l) )); then
        echo "✅ Dynamic full status bar performance is excellent (<0.1s)"
    elif (( $(echo "$dynamic_full_time < 0.5" | bc -l) )); then
        echo "✅ Dynamic full status bar performance is acceptable (<0.5s)"
    else
        echo "⚠️  Dynamic full status bar performance may be slow (>0.5s)"
        echo "    Consider using minimal template or optimizing commands"
    fi
    
    if (( $(echo "$dynamic_minimal_time < 0.05" | bc -l) )); then
        echo "✅ Dynamic minimal status bar performance is excellent (<0.05s)"
    fi
    
    local memory_usage=$(ps aux | grep tmux | awk '{sum+=$6} END {print sum/1024}' 2>/dev/null || echo "unknown")
    echo "📊 Current tmux memory usage: ${memory_usage}MB"
}

# メイン実行
main() {
    local command="${1:-test}"
    local os="${2:-auto}"
    
    case "$command" in
        "test"|"")
            if [[ "$os" == "auto" ]]; then
                os=$(~/.tmux/scripts/generate-status-bar.sh detect)
            fi
            run_comparison_test "$os"
            ;;
        "load")
            if [[ "$os" == "auto" ]]; then
                os=$(~/.tmux/scripts/generate-status-bar.sh detect)
            fi
            test_system_load "$os"
            ;;
        *)
            echo "Usage: $0 [test|load] [os]"
            echo "  test: Run performance comparison (default)"
            echo "  load: Test system load impact only"
            echo "  os:   macos, wsl, linux, freebsd, auto (default)"
            ;;
    esac
}

# 依存関係チェック
if ! command -v bc >/dev/null 2>&1; then
    log_info "Installing bc for calculations..."
    # Ubuntu/Debian
    if command -v apt-get >/dev/null 2>&1; then
        sudo apt-get install -y bc >/dev/null 2>&1 || echo "Warning: Could not install bc"
    fi
fi

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi