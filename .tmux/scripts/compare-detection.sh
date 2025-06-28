#!/bin/bash
# Comparison tool for old vs new Claude Code detection systems

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

WINDOW_ID="${1:-$(tmux display-message -p '#I')}"
ITERATIONS="${2:-10}"

echo -e "${CYAN}=== Claude Code Detection Comparison ===${NC}"
echo -e "Window: $WINDOW_ID"
echo -e "Iterations: $ITERATIONS"
echo -e "Comparing: legacy vs enhanced (3-state)\n"

# Counters
agreements=0
disagreements=0
total_old_time=0
total_new_time=0

echo -e "${YELLOW}Running comparison tests...${NC}\n"

for ((i=1; i<=ITERATIONS; i++)); do
    echo -ne "\r[Test $i/$ITERATIONS] "
    
    # Test old detection
    old_start=$(date +%s%N)
    old_result=$(~/.tmux/scripts/claude-status.sh "$WINDOW_ID" 2>/dev/null || echo "")
    old_time=$(( ($(date +%s%N) - old_start) / 1000000 ))
    total_old_time=$((total_old_time + old_time))
    
    # Test new detection
    new_start=$(date +%s%N)
    new_result=$(~/.tmux/scripts/claude-status-enhanced.sh "$WINDOW_ID" 2>/dev/null || echo "")
    new_time=$(( ($(date +%s%N) - new_start) / 1000000 ))
    total_new_time=$((total_new_time + new_time))
    
    # Compare results
    if [ "$old_result" = "$new_result" ]; then
        ((agreements++))
        echo -ne "${GREEN}✓${NC}"
    else
        ((disagreements++))
        echo -ne "${YELLOW}!${NC}"
        
        # Log disagreement details
        echo "" # New line
        echo -e "  ${YELLOW}Disagreement $disagreements:${NC}"
        echo -e "    Legacy: '$old_result' (${old_time}ms)"
        echo -e "    Enhanced: '$new_result' (${new_time}ms)"
    fi
    
    # Brief pause
    sleep 0.5
done

echo -e "\n\n${CYAN}=== Comparison Results ===${NC}"

# Agreement statistics
agreement_rate=$(( agreements * 100 / ITERATIONS ))
echo -e "Agreements: ${GREEN}$agreements${NC}/$ITERATIONS (${agreement_rate}%)"
echo -e "Disagreements: ${YELLOW}$disagreements${NC}/$ITERATIONS ($(( 100 - agreement_rate ))%)"

# Performance comparison
avg_old_time=$(( total_old_time / ITERATIONS ))
avg_new_time=$(( total_new_time / ITERATIONS ))

echo -e "\n${YELLOW}Performance Comparison:${NC}"
echo -e "Legacy average: ${avg_old_time}ms"
echo -e "Enhanced average: ${avg_new_time}ms"

if [ $avg_new_time -lt $avg_old_time ]; then
    improvement=$(( (avg_old_time - avg_new_time) * 100 / avg_old_time ))
    echo -e "${GREEN}Enhanced is ${improvement}% faster${NC}"
elif [ $avg_new_time -gt $avg_old_time ]; then
    degradation=$(( (avg_new_time - avg_old_time) * 100 / avg_old_time ))
    echo -e "${RED}Enhanced is ${degradation}% slower${NC}"
else
    echo -e "${BLUE}Performance is equivalent${NC}"
fi

# Overall assessment
echo -e "\n${YELLOW}Assessment:${NC}"
if [ $agreement_rate -ge 95 ]; then
    echo -e "${GREEN}✅ Excellent agreement (≥95%)${NC}"
elif [ $agreement_rate -ge 90 ]; then
    echo -e "${YELLOW}⚠️  Good agreement (≥90%)${NC}"
else
    echo -e "${RED}❌ Poor agreement (<90%)${NC}"
fi

if [ $avg_new_time -le $((avg_old_time + 20)) ]; then
    echo -e "${GREEN}✅ Performance acceptable (≤20ms overhead)${NC}"
else
    echo -e "${RED}❌ Performance degraded (>20ms overhead)${NC}"
fi

# Recommendation
echo -e "\n${CYAN}Recommendation:${NC}"
if [ $agreement_rate -ge 90 ] && [ $avg_new_time -le $((avg_old_time + 50)) ]; then
    echo -e "${GREEN}Enhanced detection ready for production${NC}"
else
    echo -e "${YELLOW}Enhanced detection needs review${NC}"
fi