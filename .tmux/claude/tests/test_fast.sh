#!/bin/bash
# Fast and simple test for CI compatibility

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_VOICE_HOME="$(dirname "$SCRIPT_DIR")"
CORE_DIR="$CLAUDE_VOICE_HOME/core"

echo "=== Fast Test Suite ==="
echo "Testing: $(date)"

passed=0
failed=0

# Basic syntax check for core modules
modules=(
    "user_interface.sh"
    "stats_monitor.sh"
    "config_manager.sh"
    "health_diagnostics.sh"
    "execution_engine.sh"
    "base.sh"
)

for module in "${modules[@]}"; do
    echo "Processing: $module"
    if [[ -f "$CORE_DIR/$module" ]]; then
        echo "File exists: $CORE_DIR/$module"
        if bash -n "$CORE_DIR/$module" 2>/dev/null; then
            echo "✅ $module - syntax OK"
            passed=$((passed + 1))
            echo "Passed count: $passed"
        else
            echo "❌ $module - syntax error"
            failed=$((failed + 1))
            echo "Failed count: $failed"
        fi
    else
        echo "⚠️ $module - not found"
    fi
    echo "Completed: $module"
done

# Module loading test (with timeout)
echo ""
echo "=== Module Loading Test ==="
if timeout 10 bash -c "source '$CORE_DIR/base.sh' && echo 'base.sh loaded'" 2>/dev/null; then
    echo "✅ base.sh loads successfully"
    ((passed++))
else
    echo "❌ base.sh loading failed"
    ((failed++))
fi

# Function definition test
echo ""
echo "=== Function Definition Test ==="
if timeout 10 bash -c "
source '$CORE_DIR/base.sh' 2>/dev/null &&
source '$CORE_DIR/user_interface.sh' 2>/dev/null &&
declare -f show_usage >/dev/null 2>&1 &&
echo 'Functions defined'
" 2>/dev/null; then
    echo "✅ Functions defined correctly"
    ((passed++))
else
    echo "❌ Function definition failed"
    ((failed++))
fi

# Summary
echo ""
echo "=== Test Summary ==="
echo "Passed: $passed"
echo "Failed: $failed"
echo "Total: $((passed + failed))"

if [[ $failed -eq 0 ]]; then
    echo "🎉 All fast tests passed!"
    exit 0
else
    echo "❌ $failed test(s) failed"
    exit 1
fi
