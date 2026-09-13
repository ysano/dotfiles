#!/bin/bash
# Merge synchronous state hooks, keeping user hooks and notification preferences.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec python3 "$SCRIPT_DIR/setup_hooks.py" "$@"
