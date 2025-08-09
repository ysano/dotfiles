#!/bin/bash
# Claude Status Enhanced Detection - DEPRECATED
# This script is deprecated. Please use claude-status-unified.sh instead.
#
# Usage migration:
#   OLD: claude-status-enhanced.sh
#   NEW: claude-status-unified.sh --mode enhanced

# Redirect to unified script
exec "$(dirname "$0")/claude-status-unified.sh" --mode enhanced "$@"