#!/bin/bash
# Claude Status Smart Detection - DEPRECATED
# This script is deprecated. Please use claude-status-unified.sh instead.
#
# Usage migration:
#   OLD: claude-status-smart.sh
#   NEW: claude-status-unified.sh --mode smart

# Redirect to unified script
exec "$(dirname "$0")/claude-status-unified.sh" --mode smart "$@"