#!/bin/bash
# Shared hook entrypoint. Python owns atomic pane-local state and deduplication.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../agents" && pwd)"
exec python3 "$SCRIPT_DIR/claude_status.py" hook
