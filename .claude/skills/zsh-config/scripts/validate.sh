#!/usr/bin/env bash
# =============================================================================
# Zsh 設定検証スクリプト
# Usage: validate.sh [module-name]
#   validate.sh                全モジュール検証
#   validate.sh aliases.zsh    特定モジュールのみ
# Checks: source チェーン、構文、has_command ガード、safe_path_prepend 使用
# Output: PASS/WARN/FAIL per check with file:line
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
ZSH_DIR="$DOTFILES_ROOT/.zsh"
ZSHRC="$DOTFILES_ROOT/.zshrc"
ZPROFILE="$DOTFILES_ROOT/.zprofile"

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

echo -e "${BOLD}=== Zsh Config Validation ===${NC}"
echo ""

# =============================================================================
# Check 1: Source チェーン — .zshrc から参照されるファイルが存在するか
# =============================================================================
echo -e "${BOLD}--- Source Chain (.zshrc) ---${NC}"

while IFS= read -r match; do
    lineno=$(echo "$match" | cut -d: -f1)
    line=$(echo "$match" | cut -d: -f2-)

    # source パスを抽出
    path=$(echo "$line" | sed -n 's/.*source "\{0,1\}\([^"]*\)"\{0,1\}.*/\1/p')
    [[ -z "$path" ]] && continue

    # 変数展開を解決
    check_path="$path"
    check_path="${check_path/\$HOME\/\.zsh/$ZSH_DIR}"
    check_path="${check_path/\~\/\.zsh/$ZSH_DIR}"
    check_path="${check_path/\$HOME/$DOTFILES_ROOT}"

    # 条件付き source ([[ -f ... ]] &&) は存在しなくてもOK
    is_conditional=false
    if echo "$line" | grep -q '\[\[.*-f\|test.*-f\|&&.*source'; then
        is_conditional=true
    fi

    # XDG_CACHE_HOME 等の動的パスはスキップ
    if echo "$check_path" | grep -q 'XDG_CACHE_HOME\|\.cache\|\.fzf\.zsh\|\.p10k\.zsh\|\.zshrc\.local'; then
        result PASS "source: $path (conditional/dynamic, line $lineno)"
        continue
    fi

    if [[ -f "$check_path" ]]; then
        result PASS "source: $(basename "$check_path") exists (line $lineno)"
    elif $is_conditional; then
        result PASS "source: $path (conditional, line $lineno)"
    else
        result FAIL "source: $path NOT FOUND (line $lineno)"
    fi
done < <(grep -n 'source ' "$ZSHRC" 2>/dev/null || true)
echo ""

# =============================================================================
# Check 2: .zprofile の source チェーン
# =============================================================================
echo -e "${BOLD}--- Source Chain (.zprofile) ---${NC}"

while IFS= read -r match; do
    lineno=$(echo "$match" | cut -d: -f1)
    line=$(echo "$match" | cut -d: -f2-)

    path=$(echo "$line" | sed -n 's/.*source "\{0,1\}\([^"]*\)"\{0,1\}.*/\1/p')
    [[ -z "$path" ]] && continue

    check_path="$path"
    check_path="${check_path/\$HOME\/\.zsh/$ZSH_DIR}"
    check_path="${check_path/\$DOTFILES_ZSH_DIR/$ZSH_DIR}"

    # 変数パスはベストエフォート
    if echo "$check_path" | grep -q '^\$'; then
        result PASS "source: $path (variable path, line $lineno)"
        continue
    fi

    if [[ -f "$check_path" ]]; then
        result PASS "source: $(basename "$check_path") exists (line $lineno)"
    else
        result WARN "source: $path not resolvable (line $lineno)"
    fi
done < <(grep -n 'source ' "$ZPROFILE" 2>/dev/null || true)
echo ""

# =============================================================================
# Check 3: Zsh 構文チェック (zsh -n)
# =============================================================================
echo -e "${BOLD}--- Syntax Check (zsh -n) ---${NC}"

if ! command -v zsh &>/dev/null; then
    result WARN "zsh not found — skipping syntax check"
else
    # 個別モジュールの構文チェック
    target_files=()
    if [[ $# -ge 1 ]]; then
        target="$1"
        [[ "$target" == *.zsh ]] || target="${target}.zsh"
        if [[ -f "$ZSH_DIR/$target" ]]; then
            target_files+=("$ZSH_DIR/$target")
        else
            echo -e "${RED}Error: $ZSH_DIR/$target not found${NC}" >&2
        fi
    else
        for f in "$ZSH_DIR"/*.zsh; do
            [[ -f "$f" ]] && target_files+=("$f")
        done
        # .zprofile と .zshrc も追加
        target_files+=("$ZPROFILE" "$ZSHRC")
    fi

    for file in "${target_files[@]}"; do
        basename_file=$(basename "$file")

        # git-worktree.zsh は大きいのでタイムアウト付き
        if [[ "$basename_file" == "git-worktree.zsh" ]]; then
            if timeout 10 zsh -n "$file" 2>/dev/null; then
                result PASS "$basename_file: syntax OK"
            else
                result FAIL "$basename_file: syntax error"
            fi
        else
            if zsh -n "$file" 2>/dev/null; then
                result PASS "$basename_file: syntax OK"
            else
                # zsh -n は source 先が見つからないとエラーになることがある
                # エラー内容を確認
                local_err=$(zsh -n "$file" 2>&1 || true)
                if echo "$local_err" | grep -q 'no such file\|not found'; then
                    result WARN "$basename_file: syntax check skipped (unresolvable source)"
                else
                    result FAIL "$basename_file: syntax error — $local_err"
                fi
            fi
        fi
    done
fi
echo ""

# =============================================================================
# Check 4: has_command ガードの使用チェック
# =============================================================================
echo -e "${BOLD}--- has_command Guard Usage ---${NC}"

# aliases.zsh で外部コマンドに依存するエイリアスが has_command で保護されているか
if [[ -f "$ZSH_DIR/aliases.zsh" ]]; then
    # 知られている外部コマンド依存のエイリアス
    modern_tools=(eza exa bat fd rg delta duf dust procs btop)
    for tool in "${modern_tools[@]}"; do
        if grep -q "$tool" "$ZSH_DIR/aliases.zsh" 2>/dev/null; then
            if grep -B2 "$tool" "$ZSH_DIR/aliases.zsh" | grep -q 'has_command'; then
                result PASS "aliases.zsh: $tool guarded by has_command"
            else
                lineno=$(grep -n "$tool" "$ZSH_DIR/aliases.zsh" | head -1 | cut -d: -f1)
                result WARN "aliases.zsh: $tool may lack has_command guard (line $lineno)"
            fi
        fi
    done
fi
echo ""

# =============================================================================
# Check 5: PATH 管理 — safe_path_prepend/append の使用
# =============================================================================
echo -e "${BOLD}--- PATH Management ---${NC}"

if [[ -f "$ZPROFILE" ]]; then
    # export PATH= や PATH= の直接設定がないか
    raw_path_count=$(grep -c 'export PATH=\|^PATH=' "$ZPROFILE" 2>/dev/null || true)
    raw_path_count=${raw_path_count:-0}
    safe_path_count=$(grep -c 'safe_path_prepend\|safe_path_append' "$ZPROFILE" 2>/dev/null || true)
    safe_path_count=${safe_path_count:-0}

    if [[ "$raw_path_count" -eq 0 ]]; then
        result PASS ".zprofile: no raw PATH assignments (using safe_path_* exclusively)"
    else
        result WARN ".zprofile: $raw_path_count raw PATH assignment(s) found (prefer safe_path_prepend/append)"
    fi
    result PASS ".zprofile: $safe_path_count safe_path_* calls"
fi
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
