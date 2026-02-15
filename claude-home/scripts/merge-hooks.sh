#!/usr/bin/env bash
# merge-hooks.sh — Merge AI-DLC hooks config into ~/.claude/settings.json
#
# Usage:
#   ./merge-hooks.sh settings-ai-dlc.json              # merge into settings.json
#   ./merge-hooks.sh settings-ai-dlc.json --dry-run     # preview changes
#   ./merge-hooks.sh settings-ai-dlc.json --target /path/to/settings.json
#
# Requirements: jq
# Idempotent: running twice produces the same result (dedup by command field)

set -euo pipefail

# --- Defaults ---
TARGET="${HOME}/.claude/settings.json"
DRY_RUN=false
SOURCE=""

# --- Parse args ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --target)
            TARGET="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: merge-hooks.sh <source.json> [--dry-run] [--target <path>]"
            echo ""
            echo "Merges hooks from <source.json> into ~/.claude/settings.json."
            echo "Idempotent: deduplicates by 'command' field."
            echo ""
            echo "Options:"
            echo "  --dry-run    Preview merged output without writing"
            echo "  --target     Target settings file (default: ~/.claude/settings.json)"
            exit 0
            ;;
        *)
            if [[ -z "$SOURCE" ]]; then
                SOURCE="$1"
            else
                echo "Error: unexpected argument '$1'" >&2
                exit 1
            fi
            shift
            ;;
    esac
done

# --- Validate ---
if [[ -z "$SOURCE" ]]; then
    echo "Error: source JSON file required" >&2
    echo "Usage: merge-hooks.sh <source.json> [--dry-run] [--target <path>]" >&2
    exit 1
fi

if ! command -v jq &>/dev/null; then
    echo "Error: jq is required but not installed" >&2
    exit 1
fi

if [[ ! -f "$SOURCE" ]]; then
    echo "Error: source file not found: $SOURCE" >&2
    exit 1
fi

# Validate source JSON
if ! jq empty "$SOURCE" 2>/dev/null; then
    echo "Error: source file is not valid JSON: $SOURCE" >&2
    exit 1
fi

# --- Initialize target if missing ---
if [[ ! -f "$TARGET" ]]; then
    if [[ "$DRY_RUN" == "true" ]]; then
        echo "[dry-run] Would create $TARGET"
    else
        mkdir -p "$(dirname "$TARGET")"
        echo '{}' > "$TARGET"
    fi
fi

# Validate target JSON (if exists)
if [[ -f "$TARGET" ]] && ! jq empty "$TARGET" 2>/dev/null; then
    echo "Error: target file is not valid JSON: $TARGET" >&2
    exit 1
fi

# --- Merge logic ---
# Strategy: for each hook event (PreToolUse, PostToolUse, Stop, etc.),
# merge entries from source into target. Dedup by "command" field within
# each matcher group's hooks array.

MERGED=$(jq -s '
def dedup_hooks:
    # Deduplicate hooks array by .command field
    reduce .[] as $h ([];
        if any(.[]; .command == $h.command) then .
        else . + [$h]
        end
    );

def merge_event($existing; $new_entries):
    # For each new entry, merge with existing by matcher (dedup by command)
    reduce ($new_entries[]) as $new_entry (
        $existing;
        (map(select(
            (.matcher // null) == ($new_entry.matcher // null)
        )) | length) as $match_count |
        if $match_count > 0 then
            map(
                if (.matcher // null) == ($new_entry.matcher // null) then
                    .hooks = ([.hooks[], $new_entry.hooks[]] | dedup_hooks)
                else .
                end
            )
        else
            . + [$new_entry]
        end
    );

# $target = .[0], $source = .[1]
.[0] as $target | .[1] as $source |

# Start with target, merge hooks from source
if $source.hooks then
    $target | .hooks = (
        ($target.hooks // {}) as $existing_hooks |
        reduce ($source.hooks | keys[]) as $event (
            $existing_hooks;
            .[$event] = merge_event($existing_hooks[$event] // []; $source.hooks[$event])
        )
    )
else
    $target
end
' "$TARGET" "$SOURCE" 2>/dev/null)

if [[ $? -ne 0 ]] || [[ -z "$MERGED" ]]; then
    echo "Error: merge failed" >&2
    exit 1
fi

# --- Check for changes ---
if [[ -f "$TARGET" ]]; then
    CURRENT=$(jq -S . "$TARGET")
    NEW=$(echo "$MERGED" | jq -S .)
    if [[ "$CURRENT" == "$NEW" ]]; then
        echo "No changes needed — hooks already merged in $TARGET"
        exit 0
    fi
fi

# --- Output ---
if [[ "$DRY_RUN" == "true" ]]; then
    echo "[dry-run] Would write to $TARGET:"
    echo "$MERGED" | jq .
    echo ""
    echo "[dry-run] No changes written."
else
    echo "$MERGED" | jq . > "$TARGET"
    echo "Merged hooks from $SOURCE into $TARGET"
fi
