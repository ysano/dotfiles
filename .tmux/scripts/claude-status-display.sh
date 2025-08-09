#!/bin/bash
# Claude Status Display - DEPRECATED
# This script is deprecated. Please use claude-status-unified.sh instead.
#
# Usage migration:
#   OLD: claude-status-display.sh
#   NEW: claude-status-unified.sh --mode display

# Redirect to unified script
exec "$(dirname "$0")/claude-status-unified.sh" --mode display "$@"