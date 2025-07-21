#!/bin/bash
# Debug test to identify where the hang occurs

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_VOICE_HOME="$(dirname "$SCRIPT_DIR")"
CORE_DIR="$CLAUDE_VOICE_HOME/core"
MODULE_PATH="$CORE_DIR/user_interface.sh"

echo "=== Debug Test Start ==="
echo "CLAUDE_VOICE_HOME: $CLAUDE_VOICE_HOME"

# Step 1: Load dependencies
echo "Step 1: Loading base.sh..."
if timeout 10 bash -c "source '$CORE_DIR/base.sh'" 2>/dev/null; then
    echo "✅ base.sh loaded"
else
    echo "❌ base.sh failed"
    exit 1
fi

# Step 2: Load user_interface module
echo "Step 2: Loading user_interface.sh..."
if timeout 10 bash -c "source '$CORE_DIR/base.sh' && source '$MODULE_PATH'" 2>/dev/null; then
    echo "✅ user_interface.sh loaded"
else
    echo "❌ user_interface.sh failed"
    exit 1
fi

# Step 3: Test individual functions
echo "Step 3: Testing function definitions..."
if timeout 10 bash -c "
source '$CORE_DIR/base.sh' && 
source '$MODULE_PATH' && 
declare -f show_usage >/dev/null 2>&1 && 
echo 'Functions defined'
" 2>/dev/null; then
    echo "✅ Functions defined correctly"
else
    echo "❌ Function definition failed"
    exit 1
fi

# Step 4: Test show_usage function call
echo "Step 4: Testing show_usage function call..."
if timeout 10 bash -c "
source '$CORE_DIR/base.sh' && 
source '$MODULE_PATH' && 
show_usage >/dev/null 2>&1 && 
echo 'show_usage executed'
" 2>/dev/null; then
    echo "✅ show_usage executed successfully"
else
    echo "❌ show_usage execution failed or timed out"
    exit 1
fi

echo "=== Debug Test Completed Successfully ==="
