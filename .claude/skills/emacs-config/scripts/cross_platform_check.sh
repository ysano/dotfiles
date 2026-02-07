#!/usr/bin/env bash
# =============================================================================
# Emacs クロスプラットフォーム検証スクリプト
# Usage: cross_platform_check.sh
# Checks: system-type 分岐の網羅性、WSL検出、OS固有パッケージ
# Output: PASS/WARN/FAIL per check with file:line
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
EMACS_DIR="$DOTFILES_ROOT/.emacs.d"
INITS_DIR="$EMACS_DIR/inits"

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

echo -e "${BOLD}=== Emacs Cross-Platform Check ===${NC}"
echo ""

# =============================================================================
# Check 1: init-platform.el の OS分岐カバレッジ
# =============================================================================
echo -e "${BOLD}--- init-platform.el OS Branch Coverage ---${NC}"

platform_file="$INITS_DIR/init-platform.el"
if [[ ! -f "$platform_file" ]]; then
    result FAIL "init-platform.el not found"
else
    # 各OS分岐の存在確認
    has_windows=false
    has_darwin=false
    has_linux=false

    grep -q "system-type 'windows-nt" "$platform_file" && has_windows=true
    grep -q "system-type 'darwin" "$platform_file" && has_darwin=true
    # Linux は「windows でも darwin でもない」パターンも含む
    if grep -q "system-type 'gnu/linux" "$platform_file" || \
       grep -q "not (memq system-type" "$platform_file"; then
        has_linux=true
    fi

    $has_windows && result PASS "Windows/Cygwin branch exists" \
                 || result WARN "Windows branch missing (may not be needed)"
    $has_darwin  && result PASS "macOS (darwin) branch exists" \
                 || result FAIL "macOS branch missing in init-platform.el"
    $has_linux   && result PASS "Linux/WSL branch exists" \
                 || result FAIL "Linux branch missing in init-platform.el"

    # WSL 固有の検出があるか
    if grep -q 'microsoft\|wsl\|WSL' "$platform_file" 2>/dev/null; then
        result PASS "WSL-specific detection exists in init-platform.el"
    else
        result WARN "No WSL-specific detection in init-platform.el"
    fi
fi
echo ""

# =============================================================================
# Check 2: 全 init-*.el での system-type 使用箇所
# =============================================================================
echo -e "${BOLD}--- system-type Usage Across Modules ---${NC}"

# init-platform.el 以外で system-type を使っているファイルを検出
other_platform_refs=()
while IFS= read -r match; do
    file=$(echo "$match" | cut -d: -f1)
    lineno=$(echo "$match" | cut -d: -f2)
    line=$(echo "$match" | cut -d: -f3-)

    # コメント行を除外
    if echo "$line" | grep -q '^\s*;'; then
        continue
    fi

    basename_file=$(basename "$file")
    # init-platform.el 自体はスキップ
    [[ "$basename_file" == "init-platform.el" ]] && continue

    other_platform_refs+=("${basename_file}:${lineno}")
done < <(grep -rn 'system-type' "$INITS_DIR" --include='*.el' 2>/dev/null || true)

if [[ ${#other_platform_refs[@]} -eq 0 ]]; then
    result PASS "No system-type usage outside init-platform.el (centralized)"
else
    result WARN "system-type used outside init-platform.el (consider centralizing):"
    for ref in "${other_platform_refs[@]}"; do
        echo "       $ref"
    done
fi
echo ""

# =============================================================================
# Check 3: OS固有パッケージの条件付き読み込み
# =============================================================================
echo -e "${BOLD}--- OS-Specific Package Guards ---${NC}"

# macOS 固有パッケージ
macos_packages=(osx-trash ns-auto-titlebar exec-path-from-shell)
for pkg in "${macos_packages[@]}"; do
    if grep -rq "(use-package $pkg" "$INITS_DIR" --include='*.el' 2>/dev/null; then
        file=$(grep -rl "(use-package $pkg" "$INITS_DIR" --include='*.el' | head -1)
        lineno=$(grep -n "(use-package $pkg" "$file" | head -1 | cut -d: -f1)
        basename_file=$(basename "$file")

        # when ガードまたは :if 条件があるか確認（前後5行）
        block=$(sed -n "$((lineno > 5 ? lineno - 5 : 1)),${lineno}p" "$file")
        if echo "$block" | grep -q "system-type.*darwin\|:if.*darwin\|when.*darwin"; then
            result PASS "$pkg: macOS-guarded ($basename_file:$lineno)"
        else
            result WARN "$pkg: may lack macOS guard ($basename_file:$lineno)"
        fi
    fi
done

# Linux/WSL 固有パッケージ
linux_packages=(mozc mozc-im mozc-popup)
for pkg in "${linux_packages[@]}"; do
    if grep -rq "(use-package $pkg" "$INITS_DIR" --include='*.el' 2>/dev/null; then
        file=$(grep -rl "(use-package $pkg" "$INITS_DIR" --include='*.el' | head -1)
        lineno=$(grep -n "(use-package $pkg" "$file" | head -1 | cut -d: -f1)
        basename_file=$(basename "$file")

        # Linux/非Windows ガードがあるか確認（前後10行）
        start=$((lineno > 10 ? lineno - 10 : 1))
        block=$(sed -n "${start},${lineno}p" "$file")
        if echo "$block" | grep -q "system-type.*gnu/linux\|not.*windows-nt\|not.*darwin\|not.*memq"; then
            result PASS "$pkg: Linux/non-Windows guarded ($basename_file:$lineno)"
        else
            result WARN "$pkg: may lack Linux guard ($basename_file:$lineno)"
        fi
    fi
done
echo ""

# =============================================================================
# Check 4: フォント設定のOS分岐
# =============================================================================
echo -e "${BOLD}--- Font Configuration ---${NC}"

font_refs=$(grep -rn 'set-face-attribute\|set-frame-font\|font-family\|default-frame-alist.*font' "$INITS_DIR" --include='*.el' 2>/dev/null | grep -v '^\s*;' | wc -l || true)

if [[ "$font_refs" -gt 0 ]]; then
    # フォント設定が OS 条件分岐内にあるか
    font_in_platform=$(grep -c 'set-face-attribute\|set-frame-font\|font-family' "$INITS_DIR/init-platform.el" 2>/dev/null || true)
    font_in_platform=${font_in_platform:-0}

    if [[ "$font_in_platform" -gt 0 ]]; then
        result PASS "Font configuration in init-platform.el ($font_in_platform references)"
    else
        result WARN "Font settings found ($font_refs refs) but not in init-platform.el"
    fi

    # smart-font-scaling.el の存在確認
    if [[ -f "$EMACS_DIR/elisp/smart-font-scaling.el" ]]; then
        result PASS "smart-font-scaling.el exists (cross-platform font handling)"
    fi
else
    result PASS "No direct font configuration found (may use smart-font-scaling)"
fi
echo ""

# =============================================================================
# Check 5: クリップボード / コピー&ペースト統合
# =============================================================================
echo -e "${BOLD}--- Clipboard Integration ---${NC}"

clipboard_refs=$(grep -rn 'interprogram-paste\|interprogram-cut\|xclip\|xsel\|pbcopy\|pbpaste\|clip\.exe\|wl-copy\|wl-paste' "$INITS_DIR" --include='*.el' 2>/dev/null || true)

if [[ -n "$clipboard_refs" ]]; then
    # OS 条件内にあるか
    echo "$clipboard_refs" | while IFS= read -r match; do
        file=$(echo "$match" | cut -d: -f1)
        lineno=$(echo "$match" | cut -d: -f2)
        basename_file=$(basename "$file")
        result PASS "Clipboard config: $basename_file:$lineno"
    done
else
    # Emacs はデフォルトのクリップボード統合がある
    result PASS "No explicit clipboard config (using Emacs defaults)"
fi
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
