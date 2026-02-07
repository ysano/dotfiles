#!/usr/bin/env bash
# =============================================================================
# Emacs キーバインド重複検出スクリプト
# Usage: check_keybindings.sh
# Output: 重複キーバインドを WARN/FAIL で報告
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
INITS_DIR="$DOTFILES_ROOT/.emacs.d/inits"

# --- Colors ---
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BOLD=''; NC=''
fi

pass=0; warn=0; fail=0

result() {
    local level="$1" msg="$2"
    case "$level" in
        PASS) echo -e "[${GREEN}PASS${NC}] $msg"; ((pass++)) || true ;;
        WARN) echo -e "[${YELLOW}WARN${NC}] $msg"; ((warn++)) || true ;;
        FAIL) echo -e "[${RED}FAIL${NC}] $msg"; ((fail++)) || true ;;
    esac
}

echo -e "${BOLD}=== Emacs Keybinding Conflict Check ===${NC}"
echo ""

# =============================================================================
# Extract: global-set-key, define-key, :bind patterns
# =============================================================================
declare -A global_bindings  # key -> "file:line command"
declare -a conflicts=()

# --- global-set-key / define-key (global-map) ---
while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    lineno=$(echo "$match" | cut -d: -f2)
    line=$(echo "$match" | cut -d: -f3-)

    # キーシーケンスを抽出
    key=""
    if echo "$line" | grep -q 'global-set-key'; then
        key=$(echo "$line" | sed -n 's/.*kbd "\([^"]*\)".*/\1/p')
    elif echo "$line" | grep -q 'define-key.*ai-tools-map\|define-key.*global-map'; then
        # define-key で特定の map は分離して扱う
        key=$(echo "$line" | sed -n 's/.*kbd "\([^"]*\)".*/\1/p')
        local_map=$(echo "$line" | sed -n 's/.*(define-key \([^ ]*\) .*/\1/p')
        [[ -n "$key" ]] && key="${local_map}:${key}"
    fi

    if [[ -z "$key" ]]; then
        continue
    fi

    basename_file=$(basename "$file")
    entry="${basename_file}:${lineno}"

    if [[ -n "${global_bindings[$key]:-}" ]]; then
        existing="${global_bindings[$key]}"
        conflicts+=("$key|$existing|$entry")
    else
        global_bindings[$key]="$entry"
    fi
done < <(grep -rn 'global-set-key\|define-key' "$INITS_DIR" --include='*.el' | grep -v '^\s*;' | grep -v '^\s*;;')

# --- :bind in use-package (global bindings only) ---
while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    lineno=$(echo "$match" | cut -d: -f2)
    line=$(echo "$match" | cut -d: -f3-)

    # :bind ("KEY" . function) パターンからキーを抽出
    while IFS= read -r key; do
        [[ -z "$key" ]] && continue
        basename_file=$(basename "$file")
        entry="${basename_file}:${lineno}"

        if [[ -n "${global_bindings[$key]:-}" ]]; then
            existing="${global_bindings[$key]}"
            conflicts+=("$key|$existing|$entry")
        else
            global_bindings[$key]="$entry"
        fi
    done < <(echo "$line" | grep -oP '"\K[^"]+(?="\s*\.)' || true)
done < <(grep -rn ':bind\s*(("' "$INITS_DIR" --include='*.el' | grep -v ':map ')

# =============================================================================
# Report
# =============================================================================
echo -e "${BOLD}Global Bindings: ${#global_bindings[@]} detected${NC}"
echo ""

if [[ ${#conflicts[@]} -eq 0 ]]; then
    result PASS "No global keybinding conflicts detected"
else
    echo -e "${BOLD}Conflicts:${NC}"
    for conflict in "${conflicts[@]}"; do
        IFS='|' read -r key first second <<< "$conflict"
        # C-c a の再設定は意図的（nil でクリア → map で再設定）
        if [[ "$key" == "C-c a" ]]; then
            result WARN "\"$key\" defined in $first and $second (intentional: org-agenda → ai-tools-map)"
        else
            result FAIL "\"$key\" conflict: $first vs $second"
        fi
    done
fi

echo ""

# =============================================================================
# Known intentional overrides (suppress false positives)
# =============================================================================
echo -e "${BOLD}Known Intentional Overrides:${NC}"
echo "  C-c a: org-agenda (init-org-simple) → ai-tools-map (init-ai) — AI keybindings take priority"
echo "  C-c C-r: sudo-edit (init-editor) vs verb-command-map (init-dev-web) — different major modes"
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
