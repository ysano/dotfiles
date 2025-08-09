#!/bin/bash
# Claude Status Precision Detection - DEPRECATED
# This script is deprecated. Please use claude-status-unified.sh instead.
#
# Usage migration:
#   OLD: claude-status-precision.sh
#   NEW: claude-status-unified.sh --mode precision

# Redirect to unified script
exec "$(dirname "$0")/claude-status-unified.sh" --mode precision "$@"