#!/bin/bash
# Performance benchmark script for status detection

echo "=== Claude Status Detection Performance Benchmark ==="
echo ""

# Test function
benchmark_script() {
    local script="$1"
    local name="$2"
    local iterations=10
    
    if [[ ! -x "$script" ]]; then
        echo "❌ $name: Script not found or not executable"
        return
    fi
    
    echo "Testing: $name"
    echo -n "  Running $iterations iterations... "
    
    local start_time=$(date +%s%N)
    
    for ((i=1; i<=iterations; i++)); do
        "$script" 1 >/dev/null 2>&1
    done
    
    local end_time=$(date +%s%N)
    local elapsed=$((end_time - start_time))
    local avg_ms=$((elapsed / iterations / 1000000))
    
    echo "Done"
    echo "  Average time: ${avg_ms}ms per execution"
    echo ""
}

# Run benchmarks
echo "1. Original Implementation"
benchmark_script "$HOME/.tmux/scripts/claude-status-enhanced.sh" "claude-status-enhanced.sh"

echo "2. Refactored Implementation"
benchmark_script "$HOME/.tmux/scripts/claude-status-refactored.sh" "claude-status-refactored.sh"

# Memory usage comparison
echo "=== Memory Usage Comparison ==="
echo ""

echo "1. Original Implementation"
if [[ -x "$HOME/.tmux/scripts/claude-status-enhanced.sh" ]]; then
    /usr/bin/time -l "$HOME/.tmux/scripts/claude-status-enhanced.sh" 1 2>&1 | grep "maximum resident" || echo "  Memory profiling not available"
fi

echo ""
echo "2. Refactored Implementation"
if [[ -x "$HOME/.tmux/scripts/claude-status-refactored.sh" ]]; then
    /usr/bin/time -l "$HOME/.tmux/scripts/claude-status-refactored.sh" 1 2>&1 | grep "maximum resident" || echo "  Memory profiling not available"
fi

echo ""
echo "=== Benchmark Complete ==="