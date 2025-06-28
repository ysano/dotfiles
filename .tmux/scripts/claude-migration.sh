#!/bin/bash
# Claude Code status detection migration and A/B testing system

# Configuration
TMUX_CONFIG_DIR="$HOME/.tmux"
MIGRATION_CONFIG="$TMUX_CONFIG_DIR/migration.conf"
MIGRATION_LOG="$TMUX_CONFIG_DIR/migration.log"

# Default configuration
DEFAULT_DETECTION_METHOD="legacy"    # legacy, enhanced, hybrid
DEFAULT_ROLLBACK_THRESHOLD=5         # Number of errors before auto-rollback
DEFAULT_TEST_DURATION=300           # Test duration in seconds (5 minutes)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Logging function
log_migration_event() {
    local timestamp=$(date -Iseconds)
    local event_type="$1"
    local details="$2"
    
    echo "[$timestamp] $event_type: $details" >> "$MIGRATION_LOG"
    echo -e "${BLUE}[$timestamp]${NC} $event_type: $details"
}

# Load migration configuration
load_migration_config() {
    if [ -f "$MIGRATION_CONFIG" ]; then
        source "$MIGRATION_CONFIG"
    else
        # Create default configuration
        cat > "$MIGRATION_CONFIG" << EOF
# Tmux Claude Code detection migration configuration
DETECTION_METHOD="$DEFAULT_DETECTION_METHOD"
ROLLBACK_THRESHOLD=$DEFAULT_ROLLBACK_THRESHOLD
TEST_DURATION=$DEFAULT_TEST_DURATION
A_B_TEST_ENABLED=false
ERROR_COUNT=0
LAST_MIGRATION_TIME=""
PERFORMANCE_BASELINE=""
EOF
    fi
    
    # Set defaults if variables are empty
    DETECTION_METHOD="${DETECTION_METHOD:-$DEFAULT_DETECTION_METHOD}"
    ROLLBACK_THRESHOLD="${ROLLBACK_THRESHOLD:-$DEFAULT_ROLLBACK_THRESHOLD}"
    TEST_DURATION="${TEST_DURATION:-$DEFAULT_TEST_DURATION}"
    A_B_TEST_ENABLED="${A_B_TEST_ENABLED:-false}"
    ERROR_COUNT="${ERROR_COUNT:-0}"
}

# Save migration configuration
save_migration_config() {
    cat > "$MIGRATION_CONFIG" << EOF
# Tmux Claude Code detection migration configuration
DETECTION_METHOD="$DETECTION_METHOD"
ROLLBACK_THRESHOLD=$ROLLBACK_THRESHOLD
TEST_DURATION=$TEST_DURATION
A_B_TEST_ENABLED=$A_B_TEST_ENABLED
ERROR_COUNT=$ERROR_COUNT
LAST_MIGRATION_TIME="$(date -Iseconds)"
PERFORMANCE_BASELINE="$PERFORMANCE_BASELINE"
EOF
}

# Get current status using specified method
get_status_with_method() {
    local method="$1"
    local window_id="$2"
    local pane_id="$3"
    
    case "$method" in
        "legacy")
            ~/.tmux/scripts/claude-status.sh "$window_id" "$pane_id" 2>/dev/null
            ;;
        "enhanced")
            CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" "$pane_id" 2>/dev/null
            ;;
        "hybrid")
            # Run both and compare, use enhanced if they agree, legacy if they disagree
            local legacy_result=$(~/.tmux/scripts/claude-status.sh "$window_id" "$pane_id" 2>/dev/null)
            local enhanced_result=$(CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" "$pane_id" 2>/dev/null)
            
            if [ "$legacy_result" = "$enhanced_result" ]; then
                echo "$enhanced_result"
            else
                # Log disagreement
                log_migration_event "HYBRID_DISAGREEMENT" "Legacy: $legacy_result, Enhanced: $enhanced_result"
                echo "$legacy_result"  # Default to legacy for safety
            fi
            ;;
        *)
            echo ""
            ;;
    esac
}

# Check system health
check_system_health() {
    local issues=0
    local health_report=""
    
    # Check if tmux is running
    if ! tmux list-sessions >/dev/null 2>&1; then
        health_report+="\n- Tmux not running or accessible"
        ((issues++))
    fi
    
    # Check if Claude processes exist
    if ! pgrep -f claude >/dev/null 2>&1; then
        health_report+="\n- No Claude processes detected"
        ((issues++))
    fi
    
    # Check script accessibility
    if [ ! -x ~/.tmux/scripts/claude-status.sh ]; then
        health_report+="\n- Legacy script not executable"
        ((issues++))
    fi
    
    if [ ! -x ~/.tmux/scripts/claude-status-enhanced.sh ]; then
        health_report+="\n- Enhanced script not executable"
        ((issues++))
    fi
    
    # Check performance
    local perf_test_start=$(date +%s%N)
    get_status_with_method "$DETECTION_METHOD" >/dev/null 2>&1
    local perf_test_time=$(( ($(date +%s%N) - perf_test_start) / 1000000 ))
    
    if [ $perf_test_time -gt 500 ]; then
        health_report+="\n- Detection performance degraded (${perf_test_time}ms > 500ms)"
        ((issues++))
    fi
    
    echo "$issues:$health_report"
}

# Enable enhanced detection
enable_enhanced() {
    log_migration_event "MIGRATION_START" "Switching to enhanced detection"
    
    # Health check before migration
    local health_result=$(check_system_health)
    local health_issues=$(echo "$health_result" | cut -d: -f1)
    
    if [ "$health_issues" -gt 0 ]; then
        local health_details=$(echo "$health_result" | cut -d: -f2-)
        echo -e "${RED}❌ System health check failed:${NC}$health_details"
        return 1
    fi
    
    # Backup current configuration
    if [ -f ~/.tmux/status.conf ]; then
        cp ~/.tmux/status.conf ~/.tmux/status.conf.backup
    fi
    
    # Update detection method
    DETECTION_METHOD="enhanced"
    ERROR_COUNT=0
    save_migration_config
    
    # Update tmux configuration to use enhanced script
    if grep -q "claude-status.sh" ~/.tmux/status.conf; then
        sed -i 's/claude-status\.sh/claude-status-enhanced.sh/g' ~/.tmux/status.conf
        tmux source-file ~/.tmux/status.conf
    fi
    
    log_migration_event "MIGRATION_SUCCESS" "Enhanced detection enabled"
    echo -e "${GREEN}✅ Enhanced detection enabled${NC}"
    
    return 0
}

# Rollback to legacy detection
rollback_to_legacy() {
    local reason="$1"
    
    log_migration_event "ROLLBACK_START" "Rolling back to legacy detection: $reason"
    
    # Restore backup configuration
    if [ -f ~/.tmux/status.conf.backup ]; then
        cp ~/.tmux/status.conf.backup ~/.tmux/status.conf
        tmux source-file ~/.tmux/status.conf
    fi
    
    # Update detection method
    DETECTION_METHOD="legacy"
    ERROR_COUNT=0
    save_migration_config
    
    log_migration_event "ROLLBACK_SUCCESS" "Legacy detection restored"
    echo -e "${YELLOW}⚠️  Rolled back to legacy detection: $reason${NC}"
    
    return 0
}

# Enable hybrid mode (A/B testing)
enable_hybrid_mode() {
    log_migration_event "HYBRID_START" "Enabling hybrid mode for A/B testing"
    
    DETECTION_METHOD="hybrid"
    A_B_TEST_ENABLED=true
    ERROR_COUNT=0
    save_migration_config
    
    echo -e "${CYAN}🔬 Hybrid mode enabled for A/B testing${NC}"
    echo -e "Duration: ${TEST_DURATION}s"
    echo -e "Both methods will run in parallel with comparison logging"
    
    return 0
}

# Run A/B test
run_ab_test() {
    local duration="${1:-$TEST_DURATION}"
    local window_id="${2:-$(tmux display-message -p '#I')}"
    
    echo -e "${CYAN}=== Running A/B Test ===${NC}"
    echo -e "Duration: ${duration}s"
    echo -e "Window: $window_id"
    echo ""
    
    local start_time=$(date +%s)
    local end_time=$((start_time + duration))
    local test_count=0
    local agreement_count=0
    local disagreement_count=0
    local legacy_time_total=0
    local enhanced_time_total=0
    
    while [ $(date +%s) -lt $end_time ]; do
        ((test_count++))
        
        # Test legacy method
        local legacy_start=$(date +%s%N)
        local legacy_result=$(~/.tmux/scripts/claude-status.sh "$window_id" 2>/dev/null)
        local legacy_time=$(( ($(date +%s%N) - legacy_start) / 1000000 ))
        legacy_time_total=$((legacy_time_total + legacy_time))
        
        # Test enhanced method
        local enhanced_start=$(date +%s%N)
        local enhanced_result=$(CLAUDE_STATUS_DEBUG=0 ~/.tmux/scripts/claude-status-enhanced.sh "$window_id" 2>/dev/null)
        local enhanced_time=$(( ($(date +%s%N) - enhanced_start) / 1000000 ))
        enhanced_time_total=$((enhanced_time_total + enhanced_time))
        
        # Compare results
        if [ "$legacy_result" = "$enhanced_result" ]; then
            ((agreement_count++))
            echo -ne "\r[$test_count] ${GREEN}✓${NC} Agreement: $legacy_result"
        else
            ((disagreement_count++))
            echo -ne "\r[$test_count] ${YELLOW}!${NC} Disagreement: L:$legacy_result E:$enhanced_result"
            log_migration_event "AB_DISAGREEMENT" "Test $test_count - Legacy: $legacy_result, Enhanced: $enhanced_result"
        fi
        
        sleep 2
    done
    
    echo -e "\n\n${CYAN}=== A/B Test Results ===${NC}"
    
    # Calculate statistics
    local agreement_rate=$(( agreement_count * 100 / test_count ))
    local legacy_avg_time=$(( legacy_time_total / test_count ))
    local enhanced_avg_time=$(( enhanced_time_total / test_count ))
    
    echo -e "Total tests: $test_count"
    echo -e "Agreements: $agreement_count (${agreement_rate}%)"
    echo -e "Disagreements: $disagreement_count ($(( 100 - agreement_rate ))%)"
    echo ""
    echo -e "${YELLOW}Performance Comparison:${NC}"
    echo -e "Legacy average: ${legacy_avg_time}ms"
    echo -e "Enhanced average: ${enhanced_avg_time}ms"
    
    if [ $enhanced_avg_time -lt $legacy_avg_time ]; then
        local improvement=$(( (legacy_avg_time - enhanced_avg_time) * 100 / legacy_avg_time ))
        echo -e "${GREEN}Enhanced is ${improvement}% faster${NC}"
    else
        local degradation=$(( (enhanced_avg_time - legacy_avg_time) * 100 / legacy_avg_time ))
        echo -e "${RED}Enhanced is ${degradation}% slower${NC}"
    fi
    
    # Recommendation
    echo -e "\n${YELLOW}Recommendation:${NC}"
    if [ $agreement_rate -ge 95 ] && [ $enhanced_avg_time -le $((legacy_avg_time + 50)) ]; then
        echo -e "${GREEN}✅ Enhanced detection ready for production${NC}"
        echo -e "High agreement rate and acceptable performance"
        return 0
    elif [ $agreement_rate -ge 90 ]; then
        echo -e "${YELLOW}⚠️  Enhanced detection needs review${NC}"
        echo -e "Good agreement but check disagreement patterns"
        return 1
    else
        echo -e "${RED}❌ Enhanced detection not ready${NC}"
        echo -e "Too many disagreements - needs improvement"
        return 2
    fi
}

# Monitor for errors and auto-rollback
monitor_errors() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local check_interval=30
    
    echo -e "${CYAN}=== Error Monitoring (Auto-rollback enabled) ===${NC}"
    echo -e "Threshold: $ROLLBACK_THRESHOLD errors"
    echo -e "Check interval: ${check_interval}s"
    echo -e "Press Ctrl+C to stop\n"
    
    while true; do
        # Test current detection method
        local status_result=$(get_status_with_method "$DETECTION_METHOD" "$window_id" 2>&1)
        local exit_code=$?
        
        if [ $exit_code -ne 0 ] || [ -z "$status_result" ]; then
            ((ERROR_COUNT++))
            log_migration_event "ERROR_DETECTED" "Count: $ERROR_COUNT/$ROLLBACK_THRESHOLD"
            echo -e "${RED}❌ Error detected (${ERROR_COUNT}/${ROLLBACK_THRESHOLD})${NC}"
            
            if [ $ERROR_COUNT -ge $ROLLBACK_THRESHOLD ]; then
                rollback_to_legacy "Error threshold exceeded ($ERROR_COUNT errors)"
                break
            fi
        else
            # Reset error count on successful detection
            if [ $ERROR_COUNT -gt 0 ]; then
                ERROR_COUNT=0
                save_migration_config
                echo -e "${GREEN}✅ Error count reset${NC}"
            fi
        fi
        
        sleep $check_interval
    done
}

# Show current status
show_status() {
    load_migration_config
    
    echo -e "${CYAN}=== Claude Code Detection Status ===${NC}"
    echo -e "Detection method: ${YELLOW}$DETECTION_METHOD${NC}"
    echo -e "A/B testing: $A_B_TEST_ENABLED"
    echo -e "Error count: $ERROR_COUNT/$ROLLBACK_THRESHOLD"
    echo -e "Last migration: $LAST_MIGRATION_TIME"
    echo ""
    
    # Show current detection result
    local current_window=$(tmux display-message -p '#I' 2>/dev/null)
    if [ -n "$current_window" ]; then
        echo -e "${YELLOW}Current detection result:${NC}"
        local result=$(get_status_with_method "$DETECTION_METHOD" "$current_window")
        echo -e "Window $current_window: $result"
    fi
    
    # System health
    local health_result=$(check_system_health)
    local health_issues=$(echo "$health_result" | cut -d: -f1)
    
    if [ "$health_issues" -eq 0 ]; then
        echo -e "${GREEN}✅ System health: OK${NC}"
    else
        local health_details=$(echo "$health_result" | cut -d: -f2-)
        echo -e "${RED}❌ System health issues ($health_issues):${NC}$health_details"
    fi
}

# Help function
show_help() {
    echo -e "${CYAN}Claude Code Detection Migration Tool${NC}"
    echo ""
    echo -e "${YELLOW}Usage:${NC}"
    echo "  $0 [command] [options]"
    echo ""
    echo -e "${YELLOW}Commands:${NC}"
    echo "  status                         - Show current migration status"
    echo "  enable-enhanced               - Switch to enhanced detection"
    echo "  enable-hybrid                 - Enable hybrid mode (A/B testing)"
    echo "  rollback [reason]             - Rollback to legacy detection"
    echo "  test [duration] [window_id]   - Run A/B test (default: 300s, current window)"
    echo "  monitor [window_id]           - Monitor for errors with auto-rollback"
    echo "  help                          - Show this help"
    echo ""
    echo -e "${YELLOW}Examples:${NC}"
    echo "  $0 enable-enhanced            - Switch to enhanced detection"
    echo "  $0 test 600 1                 - Run 10-minute A/B test on window 1"
    echo "  $0 monitor 2                  - Monitor window 2 for errors"
}

# Main execution
main() {
    load_migration_config
    
    local command="${1:-status}"
    
    case "$command" in
        "status")
            show_status
            ;;
        "enable-enhanced"|"enhanced")
            enable_enhanced
            ;;
        "enable-hybrid"|"hybrid")
            enable_hybrid_mode
            ;;
        "rollback")
            rollback_to_legacy "${2:-Manual rollback}"
            ;;
        "test"|"ab-test")
            run_ab_test "$2" "$3"
            ;;
        "monitor")
            monitor_errors "$2"
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