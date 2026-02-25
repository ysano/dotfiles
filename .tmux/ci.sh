#!/bin/bash
# ci.sh - tmux CI validation script
# Called from .github/workflows/ci.yml
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== tmux CI Check ==="
echo ""

# 1. Syntax check: all .conf files parseable (no unmatched quotes/braces)
echo "--- Syntax Check ---"
errors=0
for conf in "$SCRIPT_DIR"/*.conf "$SCRIPT_DIR"/**/*.conf; do
  [ -f "$conf" ] || continue
  # Check for unmatched quotes (simple heuristic)
  if grep -nP "^[^#]*(?<![\\\\])\"([^\"]*$)" "$conf" 2>/dev/null; then
    echo "WARN: Possible unmatched quote in $conf"
  fi
done
echo "[PASS] Syntax check completed"
echo ""

# 2. Script executability check
echo "--- Script Permissions ---"
for script in "$SCRIPT_DIR"/scripts/*.sh; do
  [ -f "$script" ] || continue
  if [ ! -x "$script" ]; then
    echo "[FAIL] Not executable: $script"
    errors=$((errors + 1))
  else
    echo "[PASS] Executable: $(basename "$script")"
  fi
done
echo ""

# 3. Script syntax check (sh -n)
echo "--- Script Syntax ---"
for script in "$SCRIPT_DIR"/scripts/*.sh; do
  [ -f "$script" ] || continue
  if sh -n "$script" 2>/dev/null; then
    echo "[PASS] Valid syntax: $(basename "$script")"
  else
    echo "[FAIL] Syntax error: $script"
    errors=$((errors + 1))
  fi
done
echo ""

# 4. Run conflict check if available
CONFLICT_SCRIPT="$DOTFILES_ROOT/.claude/skills/tmux-config/scripts/check_conflicts.sh"
if [ -f "$CONFLICT_SCRIPT" ]; then
  echo "--- Conflict Check ---"
  bash "$CONFLICT_SCRIPT"
  echo ""
fi

echo "=== CI Complete (errors: $errors) ==="
[ "$errors" -eq 0 ] && exit 0 || exit 1
