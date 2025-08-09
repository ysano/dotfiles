#!/bin/bash
# Claude Status Debug Detection - DEPRECATED
# This script is deprecated. Please use claude-status-unified.sh instead.
#
# Usage migration:
#   OLD: claude-status-debug.sh
#   NEW: claude-status-unified.sh --mode debug

# Redirect to unified script
exec "$(dirname "$0")/claude-status-unified.sh" --mode debug "$@"