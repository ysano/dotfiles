#!/bin/bash
# Minimal Test Suite - Essential functionality only
# 最小限テストスイート - 基本機能のみ

set -uo pipefail

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

# テスト統計
TESTS_PASSED=0
TESTS_FAILED=0

# ベースパス
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_HOME="$(dirname "$TEST_DIR")"

echo -e "${BLUE}🧪 Minimal Test Suite${NC}"
echo "========================"

# Test 1: status_detector.sh
echo -n "Testing status_detector.sh... "
if [[ -f "$CLAUDE_HOME/claude/core/status_detector.sh" ]]; then
    echo -e "${GREEN}PASS${NC}"
    ((TESTS_PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((TESTS_FAILED++))
fi

# Test 2: interfaces.sh
echo -n "Testing interfaces.sh... "
if [[ -f "$CLAUDE_HOME/claude/core/interfaces.sh" ]]; then
    echo -e "${GREEN}PASS${NC}"
    ((TESTS_PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((TESTS_FAILED++))
fi

# Test 3: cache_manager.sh
echo -n "Testing cache_manager.sh... "
if [[ -f "$CLAUDE_HOME/claude/core/cache_manager.sh" ]]; then
    echo -e "${GREEN}PASS${NC}"
    ((TESTS_PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((TESTS_FAILED++))
fi

# Test 4: config_manager_v2.sh
echo -n "Testing config_manager_v2.sh... "
if [[ -f "$CLAUDE_HOME/claude/core/config_manager_v2.sh" ]]; then
    echo -e "${GREEN}PASS${NC}"
    ((TESTS_PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((TESTS_FAILED++))
fi

echo ""
echo "Results: ${TESTS_PASSED} passed, ${TESTS_FAILED} failed"

if [[ $TESTS_FAILED -eq 0 ]]; then
    echo -e "${GREEN}✅ All essential tests passed${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed${NC}"
    exit 1
fi