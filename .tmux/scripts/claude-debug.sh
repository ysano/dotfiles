#!/bin/bash
# Claude Code status detection debugging and monitoring tool

# Configuration
TMUX_DEBUG_DIR="$HOME/.tmux/debug"
TMUX_LOG_FILE="$TMUX_DEBUG_DIR/detection.log"
TMUX_METRICS_FILE="$TMUX_DEBUG_DIR/metrics.json"

# Create debug directory if it doesn't exist
mkdir -p "$TMUX_DEBUG_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Logging functions
log_detection_event() {
    local timestamp=$(date -Iseconds)
    local window_id="$1"
    local pane_id="$2"
    local detected_state="$3"
    local confidence="$4"
    local detection_time_ms="$5"
    
    local log_entry=$(cat <<EOF
{
  "timestamp": "$timestamp",
  "window_id": "$window_id",
  "pane_id": "$pane_id",
  "detected_state": "$detected_state",
  "confidence": "$confidence",
  "detection_time_ms": $detection_time_ms,
  "type": "detection_event"
}
EOF
)
    
    echo "$log_entry" >> "$TMUX_LOG_FILE"
}

log_error_event() {
    local timestamp=$(date -Iseconds)
    local error_type="$1"
    local error_message="$2"
    local context="$3"
    
    local log_entry=$(cat <<EOF
{
  "timestamp": "$timestamp",
  "error_type": "$error_type",
  "error_message": "$error_message",
  "context": "$context",
  "type": "error_event"
}
EOF
)
    
    echo "$log_entry" >> "$TMUX_LOG_FILE"
}

# Real-time monitoring
real_time_monitor() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local monitor_duration="${2:-60}"
    
    echo -e "${CYAN}=== Real-time Claude Code Status Monitor ===${NC}"
    echo -e "Window: $window_id, Duration: ${monitor_duration}s"
    echo -e "Press ${YELLOW}Ctrl+C${NC} to stop\n"
    
    local start_time=$(date +%s)
    local end_time=$((start_time + monitor_duration))
    local last_status=""
    local detection_count=0
    local state_changes=0
    
    while [ $(date +%s) -lt $end_time ]; do
        local current_time=$(date +%s%N)
        
        # Run enhanced detection with timing
        local status_output=$(CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" 2>/dev/null)
        local detection_time=$(( ($(date +%s%N) - current_time) / 1000000 ))
        
        ((detection_count++))
        
        # Check for state changes
        if [ "$status_output" != "$last_status" ]; then
            local timestamp=$(date '+%H:%M:%S')
            echo -e "${timestamp} Status change: ${YELLOW}$last_status${NC} → ${GREEN}$status_output${NC} (${detection_time}ms)"
            ((state_changes++))
            last_status="$status_output"
        fi
        
        # Brief pause to avoid excessive CPU usage
        sleep 0.5
    done
    
    echo -e "\n${CYAN}=== Monitor Summary ===${NC}"
    echo -e "Total detections: $detection_count"
    echo -e "State changes: $state_changes"
    echo -e "Average rate: $(( detection_count * 60 / monitor_duration )) detections/minute"
}

# Detailed debugging session
debug_current_state() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local pane_id="${2:-$(tmux display-message -p '#P')}"
    
    echo -e "${CYAN}=== Claude Code Status Debug Session ===${NC}"
    echo -e "Window: $window_id, Pane: $pane_id\n"
    
    # Capture terminal output
    echo -e "${YELLOW}--- Terminal Output (last 20 lines) ---${NC}"
    local terminal_output=$(tmux capture-pane -p -S -20 -t "${window_id}.${pane_id}" 2>/dev/null)
    echo "$terminal_output"
    echo ""
    
    # Run enhanced detection with full debug output
    echo -e "${YELLOW}--- Enhanced Detection Analysis ---${NC}"
    local start_time=$(date +%s%N)
    local status_output=$(CLAUDE_STATUS_DEBUG=1 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" "$pane_id" 2>&1)
    local detection_time=$(( ($(date +%s%N) - start_time) / 1000000 ))
    
    echo -e "${GREEN}Final Status: $status_output${NC}"
    echo -e "${BLUE}Detection Time: ${detection_time}ms${NC}"
    echo ""
    
    # Process analysis
    echo -e "${YELLOW}--- Process Analysis ---${NC}"
    local pane_pid=$(tmux display-message -t "${window_id}.${pane_id}" -p '#{pane_pid}' 2>/dev/null)
    echo -e "Pane PID: $pane_pid"
    
    if [ -n "$pane_pid" ]; then
        echo -e "Process tree:"
        pstree -p "$pane_pid" 2>/dev/null | head -10
        echo ""
        
        echo -e "Claude processes:"
        pgrep -f claude | head -5
    fi
    
    # tmux environment
    echo -e "${YELLOW}--- Tmux Environment ---${NC}"
    echo -e "Session: $(tmux display-message -p '#S')"
    echo -e "Window: $(tmux display-message -p '#W')"
    echo -e "Pane: $(tmux display-message -p '#P')"
    echo -e "PWD: $(tmux display-message -p '#{pane_current_path}')"
}

# Performance analysis
analyze_performance() {
    local iterations="${1:-100}"
    local window_id="${2:-$(tmux display-message -p '#I')}"
    
    echo -e "${CYAN}=== Performance Analysis ($iterations iterations) ===${NC}"
    
    local total_time=0
    local min_time=999999
    local max_time=0
    local measurements=()
    
    for ((i=1; i<=iterations; i++)); do
        local start_time=$(date +%s%N)
        CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" >/dev/null 2>&1
        local end_time=$(date +%s%N)
        
        local duration=$(( (end_time - start_time) / 1000000 ))
        measurements+=($duration)
        total_time=$((total_time + duration))
        
        if [ $duration -lt $min_time ]; then
            min_time=$duration
        fi
        if [ $duration -gt $max_time ]; then
            max_time=$duration
        fi
        
        # Show progress every 10 iterations
        if [ $((i % 10)) -eq 0 ]; then
            echo -ne "\rProgress: $i/$iterations"
        fi
    done
    
    echo -e "\n"
    
    # Calculate statistics
    local avg_time=$((total_time / iterations))
    
    # Calculate median (approximate)
    local sorted_measurements=($(printf '%s\n' "${measurements[@]}" | sort -n))
    local median_index=$((iterations / 2))
    local median_time=${sorted_measurements[$median_index]}
    
    # Calculate 95th percentile
    local p95_index=$((iterations * 95 / 100))
    local p95_time=${sorted_measurements[$p95_index]}
    
    echo -e "${YELLOW}--- Performance Results ---${NC}"
    echo -e "Average time: ${avg_time}ms"
    echo -e "Median time: ${median_time}ms"
    echo -e "Min time: ${min_time}ms"
    echo -e "Max time: ${max_time}ms"
    echo -e "95th percentile: ${p95_time}ms"
    echo ""
    
    # Performance assessment
    if [ $avg_time -lt 50 ]; then
        echo -e "${GREEN}✅ Performance: Excellent (<50ms)${NC}"
    elif [ $avg_time -lt 100 ]; then
        echo -e "${YELLOW}⚠️  Performance: Good (50-100ms)${NC}"
    elif [ $avg_time -lt 200 ]; then
        echo -e "${YELLOW}⚠️  Performance: Acceptable (100-200ms)${NC}"
    else
        echo -e "${RED}❌ Performance: Poor (>200ms)${NC}"
    fi
    
    # Save metrics
    local metrics_entry=$(cat <<EOF
{
  "timestamp": "$(date -Iseconds)",
  "iterations": $iterations,
  "avg_time_ms": $avg_time,
  "median_time_ms": $median_time,
  "min_time_ms": $min_time,
  "max_time_ms": $max_time,
  "p95_time_ms": $p95_time,
  "type": "performance_analysis"
}
EOF
)
    
    echo "$metrics_entry" >> "$TMUX_METRICS_FILE"
}

# Compare old vs new detection
compare_detection_methods() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local iterations="${2:-20}"
    
    echo -e "${CYAN}=== Detection Method Comparison ===${NC}"
    echo -e "Comparing old vs enhanced detection methods\n"
    
    local old_results=()
    local new_results=()
    local agreements=0
    local disagreements=0
    
    for ((i=1; i<=iterations; i++)); do
        # Run old detection
        local old_result=$(~/.tmux/scripts/claude-status.sh "$window_id" 2>/dev/null)
        old_results+=("$old_result")
        
        # Run new detection  
        local new_result=$(CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" 2>/dev/null)
        new_results+=("$new_result")
        
        # Compare results
        if [ "$old_result" = "$new_result" ]; then
            ((agreements++))
            echo -e "[$i] ${GREEN}✓${NC} Both: $old_result"
        else
            ((disagreements++))
            echo -e "[$i] ${YELLOW}!${NC} Old: $old_result, New: $new_result"
        fi
        
        sleep 1
    done
    
    echo -e "\n${YELLOW}--- Comparison Summary ---${NC}"
    echo -e "Agreements: ${agreements}/${iterations} ($(( agreements * 100 / iterations ))%)"
    echo -e "Disagreements: ${disagreements}/${iterations} ($(( disagreements * 100 / iterations ))%)"
    
    if [ $disagreements -eq 0 ]; then
        echo -e "${GREEN}✅ Perfect agreement between methods${NC}"
    elif [ $((disagreements * 100 / iterations)) -lt 10 ]; then
        echo -e "${YELLOW}⚠️  Minor differences (<10%)${NC}"
    else
        echo -e "${RED}❌ Significant differences (>10%)${NC}"
    fi
}

# Generate status report
generate_report() {
    local report_file="$TMUX_DEBUG_DIR/status_report_$(date +%Y%m%d_%H%M%S).txt"
    
    echo -e "${CYAN}=== Generating Status Report ===${NC}"
    
    {
        echo "Claude Code Status Detection Report"
        echo "Generated: $(date)"
        echo "=================================="
        echo ""
        
        echo "System Information:"
        echo "- OS: $(uname -s) $(uname -r)"
        echo "- Tmux version: $(tmux -V)"
        echo "- Shell: $SHELL"
        echo ""
        
        echo "Current Status:"
        debug_current_state
        echo ""
        
        echo "Performance Analysis:"
        analyze_performance 50
        echo ""
        
        echo "Detection Comparison:"
        compare_detection_methods
        
    } > "$report_file"
    
    echo -e "${GREEN}Report saved to: $report_file${NC}"
}

# Help function
show_help() {
    echo -e "${CYAN}Claude Code Status Detection Debug Tool${NC}"
    echo ""
    echo -e "${YELLOW}Usage:${NC}"
    echo "  $0 [command] [options]"
    echo ""
    echo -e "${YELLOW}Commands:${NC}"
    echo "  debug [window_id] [pane_id]    - Debug current state (default: current window/pane)"
    echo "  monitor [window_id] [duration] - Real-time monitoring (default: current window, 60s)"
    echo "  perf [iterations] [window_id]  - Performance analysis (default: 100 iterations)"
    echo "  compare [window_id] [iters]    - Compare old vs new detection"
    echo "  report                         - Generate comprehensive report"
    echo "  help                           - Show this help"
    echo ""
    echo -e "${YELLOW}Examples:${NC}"
    echo "  $0 debug 1 0                  - Debug window 1, pane 0"
    echo "  $0 monitor 2 30                - Monitor window 2 for 30 seconds"
    echo "  $0 perf 200                    - Run 200 performance iterations"
    echo "  $0 compare 1 10                - Compare methods 10 times on window 1"
}

# Main execution
main() {
    local command="${1:-debug}"
    
    case "$command" in
        "debug")
            debug_current_state "$2" "$3"
            ;;
        "monitor")
            real_time_monitor "$2" "$3"
            ;;
        "perf"|"performance")
            analyze_performance "$2" "$3"
            ;;
        "compare")
            compare_detection_methods "$2" "$3"
            ;;
        "report")
            generate_report
            ;;
        "help"|"-h"|"--help")
            show_help
            ;;
        *)
            echo -e "${RED}Unknown command: $command${NC}"
            show_help
            exit 1
            ;;
    esac
}

# Run if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi