#!/usr/bin/env bash
# =============================================================================
# Emacs init-*.el 規約検証スクリプト
# Usage: validate.sh [module-name]
#   validate.sh              全モジュールを検証
#   validate.sh init-ai      特定モジュールのみ検証
# Output: PASS/WARN/FAIL per check with file:line
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
EMACS_DIR="$DOTFILES_ROOT/.emacs.d"
INITS_DIR="$EMACS_DIR/inits"

# --- Colors (tty 判定) ---
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

# =============================================================================
# Check 1: (provide 'init-xxx) 終端の確認
# =============================================================================
check_provide() {
    local file="$1"
    local base
    base="$(basename "$file" .el)"

    if grep -q "(provide '${base})" "$file"; then
        result PASS "$base: (provide '${base}) found"
    else
        result FAIL "$base: missing (provide '${base}) at end of file"
    fi
}

# =============================================================================
# Check 2: 括弧バランスの確認
# =============================================================================
check_parens() {
    local file="$1"
    local base
    base="$(basename "$file" .el)"

    # コメント行とクォート文字列内の括弧を除外した簡易チェック
    local opens closes
    opens=$(grep -v '^\s*;' "$file" | tr -cd '(' | wc -c)
    closes=$(grep -v '^\s*;' "$file" | tr -cd ')' | wc -c)

    if [[ "$opens" -eq "$closes" ]]; then
        result PASS "$base: parentheses balanced ($opens pairs)"
    else
        result FAIL "$base: parentheses unbalanced (open=$opens, close=$closes)"
    fi
}

# =============================================================================
# Check 3: use-package 規約 (:ensure t, :defer t)
# =============================================================================
check_use_package() {
    local file="$1"
    local base
    base="$(basename "$file" .el)"

    # use-package 宣言を抽出
    local pkg_count
    pkg_count=$(grep -c '(use-package ' "$file" 2>/dev/null || echo 0)

    if [[ "$pkg_count" -eq 0 ]]; then
        return  # use-package 未使用のモジュール
    fi

    # :ensure t チェック（built-in パッケージは除外）
    local missing_ensure=()
    while IFS= read -r line; do
        local pkg
        pkg=$(echo "$line" | sed 's/.*(\s*use-package \([^ )]*\).*/\1/')
        local lineno
        lineno=$(grep -n "(use-package ${pkg}" "$file" | head -1 | cut -d: -f1)
        [[ -z "$lineno" ]] && continue

        # use-package ブロック内に :ensure があるか簡易チェック
        # （ブロック終端まで見る完全な解析は困難なので、前後20行で探す）
        local block
        block=$(sed -n "${lineno},$((lineno + 20))p" "$file")
        if ! echo "$block" | grep -q ':ensure'; then
            # :ensure-system-package や built-in チェック
            if ! echo "$block" | grep -q ':ensure-system-package\|;.*built-in\|:elpaca nil'; then
                missing_ensure+=("${pkg}:${lineno}")
            fi
        fi
    done < <(grep '(use-package ' "$file")

    if [[ ${#missing_ensure[@]} -eq 0 ]]; then
        result PASS "$base: all use-package declarations have :ensure ($pkg_count packages)"
    else
        for item in "${missing_ensure[@]}"; do
            result WARN "$base: :ensure missing — ${item%%:*} (${base}.el:${item##*:})"
        done
    fi

    # :defer t / :hook / :mode / :commands チェック（遅延読み込み）
    local no_defer=()
    while IFS= read -r line; do
        local pkg
        pkg=$(echo "$line" | sed 's/.*(\s*use-package \([^ )]*\).*/\1/')
        local lineno
        lineno=$(grep -n "(use-package ${pkg}" "$file" | head -1 | cut -d: -f1)
        [[ -z "$lineno" ]] && continue

        local block
        block=$(sed -n "${lineno},$((lineno + 20))p" "$file")
        if ! echo "$block" | grep -q ':defer\|:hook\|:mode\|:commands\|:bind\|:after'; then
            no_defer+=("${pkg}:${lineno}")
        fi
    done < <(grep '(use-package ' "$file")

    if [[ ${#no_defer[@]} -eq 0 ]]; then
        result PASS "$base: all packages have lazy-loading (:defer/:hook/:mode/:commands/:bind)"
    else
        for item in "${no_defer[@]}"; do
            result WARN "$base: no lazy-loading — ${item%%:*} (${base}.el:${item##*:})"
        done
    fi
}

# =============================================================================
# Main
# =============================================================================
echo -e "${BOLD}=== Emacs Config Validation ===${NC}"
echo ""

# 対象ファイルの決定
target_files=()
if [[ $# -ge 1 ]]; then
    target="$1"
    # .el 拡張子がなければ追加
    [[ "$target" == *.el ]] || target="${target}.el"
    if [[ -f "$INITS_DIR/$target" ]]; then
        target_files+=("$INITS_DIR/$target")
    else
        echo -e "${RED}Error: $INITS_DIR/$target not found${NC}" >&2
        exit 1
    fi
else
    for f in "$INITS_DIR"/init-*.el; do
        [[ -f "$f" ]] && target_files+=("$f")
    done
fi

if [[ ${#target_files[@]} -eq 0 ]]; then
    echo -e "${RED}Error: No init-*.el files found in $INITS_DIR${NC}" >&2
    exit 1
fi

echo "Checking ${#target_files[@]} module(s)..."
echo ""

for file in "${target_files[@]}"; do
    echo -e "${BOLD}--- $(basename "$file") ---${NC}"
    check_provide "$file"
    check_parens "$file"
    check_use_package "$file"
    echo ""
done

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

if [[ $fail -gt 0 ]]; then
    exit 1
elif [[ $warn -gt 0 ]]; then
    exit 0
else
    exit 0
fi
