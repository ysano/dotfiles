#!/bin/sh
# Git pager: delta with dynamic side-by-side based on terminal width
# Graceful degradation: falls back to less if delta is not installed
if command -v delta >/dev/null 2>&1; then
    if [ "$(tput cols)" -ge 120 ]; then
        delta --side-by-side "$@"
    else
        delta "$@"
    fi
else
    less "$@"
fi
