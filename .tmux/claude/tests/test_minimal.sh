#!/bin/bash
# Minimal test to identify hanging point

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_VOICE_HOME="$(dirname "$SCRIPT_DIR")"
CORE_DIR="$CLAUDE_VOICE_HOME/core"

echo "=== Minimal Test ==="

echo "Step 1: Test user_interface.sh"
if bash -n "$CORE_DIR/user_interface.sh" 2>/dev/null; then
    echo "✅ user_interface.sh syntax OK"
else
    echo "❌ user_interface.sh syntax error"
    exit 1
fi

echo "Step 2: Test stats_monitor.sh"
if bash -n "$CORE_DIR/stats_monitor.sh" 2>/dev/null; then
    echo "✅ stats_monitor.sh syntax OK"
else
    echo "❌ stats_monitor.sh syntax error"
    exit 1
fi

echo "Step 3: Test config_manager.sh"
if bash -n "$CORE_DIR/config_manager.sh" 2>/dev/null; then
    echo "✅ config_manager.sh syntax OK"
else
    echo "❌ config_manager.sh syntax error"
    exit 1
fi

echo "Step 4: Test health_diagnostics.sh"
if bash -n "$CORE_DIR/health_diagnostics.sh" 2>/dev/null; then
    echo "✅ health_diagnostics.sh syntax OK"
else
    echo "❌ health_diagnostics.sh syntax error"
    exit 1
fi

echo "Step 5: Test execution_engine.sh"
if bash -n "$CORE_DIR/execution_engine.sh" 2>/dev/null; then
    echo "✅ execution_engine.sh syntax OK"
else
    echo "❌ execution_engine.sh syntax error"
    exit 1
fi

echo "Step 6: Test base.sh"
if bash -n "$CORE_DIR/base.sh" 2>/dev/null; then
    echo "✅ base.sh syntax OK"
else
    echo "❌ base.sh syntax error"
    exit 1
fi

echo "All steps completed successfully!"
