#!/bin/bash
# Claude-Command-Suite統合の検証スクリプト
# 使用方法: ./validate_integration.sh [category]

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CATEGORY="${1:-}"

# カラー出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 結果カウンター
PASSED=0
FAILED=0
WARNINGS=0

check_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASSED++))
}

check_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAILED++))
}

check_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ((WARNINGS++))
}

# 1. ファイル数検証
echo "=== ファイル数検証 ==="

if [[ -z "$CATEGORY" ]]; then
    # 全体検証
    COMMANDS_COUNT=$(find "$REPO_ROOT/.claude/commands" -name "*.md" -type f | wc -l | tr -d ' ')
    AGENTS_COUNT=$(find "$REPO_ROOT/.claude/agents" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' ')
    HOOKS_COUNT=$(find "$REPO_ROOT/.claude/hooks" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' ')
    SKILLS_COUNT=$(find "$REPO_ROOT/.claude/skills" -type d -mindepth 1 -maxdepth 1 2>/dev/null | wc -l | tr -d ' ')

    echo "Commands: $COMMANDS_COUNT"
    echo "Agents: $AGENTS_COUNT"
    echo "Hooks: $HOOKS_COUNT"
    echo "Skills: $SKILLS_COUNT"

    # 期待値チェック（柔軟に）
    if [[ "$COMMANDS_COUNT" -ge 200 ]]; then
        check_pass "Commands数が妥当 ($COMMANDS_COUNT >= 200)"
    else
        check_fail "Commands数が不足 ($COMMANDS_COUNT < 200)"
    fi

    if [[ "$AGENTS_COUNT" -ge 100 ]]; then
        check_pass "Agents数が妥当 ($AGENTS_COUNT >= 100)"
    else
        check_warn "Agents数が少ない可能性 ($AGENTS_COUNT < 100)"
    fi
else
    # カテゴリー別検証
    CATEGORY_COUNT=$(find "$REPO_ROOT/.claude/commands/$CATEGORY" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' ')
    echo "$CATEGORY: $CATEGORY_COUNT files"
    if [[ "$CATEGORY_COUNT" -gt 0 ]]; then
        check_pass "$CATEGORY カテゴリーにファイルが存在"
    else
        check_fail "$CATEGORY カテゴリーにファイルが存在しない"
    fi
fi

# 2. 日本語化状態検証
echo ""
echo "=== 日本語化状態検証 ==="

SEARCH_PATH="$REPO_ROOT/.claude/commands"
if [[ -n "$CATEGORY" ]]; then
    SEARCH_PATH="$REPO_ROOT/.claude/commands/$CATEGORY"
fi

if [[ -d "$SEARCH_PATH" ]]; then
    JA_HEADERS=$(grep -r "## 実行手順\|## 注意点\|## 関連コマンド\|## 前提条件" "$SEARCH_PATH" --include="*.md" 2>/dev/null | wc -l | tr -d ' ')
    TOTAL_FILES=$(find "$SEARCH_PATH" -name "*.md" -type f | wc -l | tr -d ' ')

    echo "日本語化されたセクション: $JA_HEADERS"
    echo "総ファイル数: $TOTAL_FILES"

    if [[ "$JA_HEADERS" -gt 0 ]]; then
        check_pass "日本語化されたセクションが存在"
    else
        check_warn "日本語化が未実施の可能性"
    fi
fi

# 3. YAMLフロントマター検証
echo ""
echo "=== YAMLフロントマター検証 ==="

YAML_ERRORS=0
if [[ -d "$SEARCH_PATH" ]]; then
    while IFS= read -r file; do
        # 最初の行が---で始まるかチェック
        if head -1 "$file" | grep -q "^---$"; then
            # YAMLフロントマターの終了を探す
            if ! sed -n '2,/^---$/p' "$file" | grep -q "^---$"; then
                check_fail "YAMLフロントマターが正しく閉じられていない: $file"
                ((YAML_ERRORS++))
            fi
        fi
    done < <(find "$SEARCH_PATH" -name "*.md" -type f)

    if [[ "$YAML_ERRORS" -eq 0 ]]; then
        check_pass "YAMLフロントマターの構文エラーなし"
    fi
fi

# 4. .gitattributes検証
echo ""
echo "=== .gitattributes検証 ==="

if [[ -f "$REPO_ROOT/.gitattributes" ]]; then
    ATTR_COUNT=$(grep -c "source=Claude-Command-Suite" "$REPO_ROOT/.gitattributes" 2>/dev/null || echo "0")
    echo ".gitattributes登録数: $ATTR_COUNT"

    if [[ "$ATTR_COUNT" -gt 0 ]]; then
        check_pass ".gitattributesにClaude-Command-Suite出典が登録されている"
    else
        check_warn ".gitattributesに出典登録がない"
    fi
else
    check_warn ".gitattributesファイルが存在しない"
fi

# サマリー
echo ""
echo "=== 検証サマリー ==="
echo -e "${GREEN}合格: $PASSED${NC}"
echo -e "${RED}失敗: $FAILED${NC}"
echo -e "${YELLOW}警告: $WARNINGS${NC}"

if [[ "$FAILED" -gt 0 ]]; then
    exit 1
fi

exit 0
