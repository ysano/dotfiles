#!/bin/sh
# Git interactive diffFilter: delta with color-only mode
# Graceful degradation: falls back to cat if delta is not installed
if command -v delta >/dev/null 2>&1; then
    delta --color-only "$@"
else
    cat
fi
