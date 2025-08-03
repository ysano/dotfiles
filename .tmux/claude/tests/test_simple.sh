#!/bin/bash
# Simple test to identify the hanging issue

set -euo pipefail

echo "=== Simple Test Start ==="

# Debug paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_VOICE_HOME="$(dirname "$SCRIPT_DIR")"
echo "SCRIPT_DIR: $SCRIPT_DIR"
echo "CLAUDE_VOICE_HOME: $CLAUDE_VOICE_HOME"

# Test base.sh loading with timeout
echo "Testing base.sh loading..."
if timeout 5 bash -c "source '$CLAUDE_VOICE_HOME/core/foundation.sh' && echo 'base.sh loaded successfully'" 2>/dev/null; then
    echo "✅ base.sh loads without hanging"
else
    echo "❌ base.sh loading failed or timed out"
fi

# Test user_interface.sh loading with timeout
echo "Testing user_interface.sh loading..."
if timeout 5 bash -c "source '$CLAUDE_VOICE_HOME/core/foundation.sh' && source '$CLAUDE_VOICE_HOME/core/user_interface.sh' && echo 'user_interface.sh loaded successfully'" 2>/dev/null; then
    echo "✅ user_interface.sh loads without hanging"
else
    echo "❌ user_interface.sh loading failed or timed out"
fi

echo "=== Simple Test End ==="
