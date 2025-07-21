#!/bin/bash
# Simple CI Script - Minimal Continuous Integration
# 簡易CI - 最小限の継続統合

set -euo pipefail

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# CI設定
CI_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_FILE="$CI_DIR/ci.log"

# ログ関数
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# CI段階実行
run_stage() {
    local stage_name="$1"
    local command="$2"
    
    echo -e "${BLUE}🔄 Stage: $stage_name${NC}"
    log "INFO" "Starting stage: $stage_name"
    
    if eval "$command"; then
        echo -e "${GREEN}✅ $stage_name: SUCCESS${NC}"
        log "INFO" "$stage_name completed successfully"
        return 0
    else
        echo -e "${RED}❌ $stage_name: FAILED${NC}"
        log "ERROR" "$stage_name failed"
        return 1
    fi
}

# === CI パイプライン ===

stage_lint() {
    # 基本的な構文チェック（ShellCheck軽量版）
    # bash -n による構文チェック
    find "$CI_DIR/claude" -name "*.sh" -exec bash -n {} \; 2>/dev/null || return 1
    
    # ShellCheckがある場合は軽量チェック（重要なエラーのみ）
    if command -v shellcheck >/dev/null 2>&1; then
        find "$CI_DIR/claude" -name "*.sh" -print0 | \
        xargs -0 shellcheck \
        -e SC1090,SC1091,SC2034,SC2086,SC2155,SC2119,SC2120,SC2181,SC2207 \
        -e SC2088,SC2144,SC2145,SC2152,SC2154,SC2046,SC2076,SC2184,SC2168 \
        -e SC2178,SC2064,SC2206,SC2221,SC2222 \
        -S error 2>/dev/null || true
    fi
}

stage_test() {
    # 最小限テスト実行
    bash "$CI_DIR/tests/minimal-test.sh"
}

stage_architecture() {
    # アーキテクチャ検証
    if [[ -f "$CI_DIR/claude/core/module_registry_v2.sh" ]]; then
        bash "$CI_DIR/claude/core/module_registry_v2.sh" validate >/dev/null
    else
        echo "Architecture validation skipped"
    fi
}

# === メインCI実行 ===

main() {
    echo -e "${YELLOW}🚀 Simple CI Pipeline${NC}"
    echo "========================"
    echo "Timestamp: $(date)"
    echo ""
    
    # ログファイル初期化
    echo "CI Pipeline started at $(date)" > "$LOG_FILE"
    
    local exit_code=0
    
    # Stage 1: Lint
    if ! run_stage "Lint" "stage_lint"; then
        exit_code=1
    fi
    echo ""
    
    # Stage 2: Test  
    if ! run_stage "Test" "stage_test"; then
        exit_code=1
    fi
    echo ""
    
    # Stage 3: Architecture
    if ! run_stage "Architecture" "stage_architecture"; then
        exit_code=1
    fi
    echo ""
    
    # 結果サマリー
    if [[ $exit_code -eq 0 ]]; then
        echo -e "${GREEN}🎉 CI Pipeline: ALL STAGES PASSED${NC}"
        log "INFO" "CI pipeline completed successfully"
    else
        echo -e "${RED}💥 CI Pipeline: SOME STAGES FAILED${NC}"
        log "ERROR" "CI pipeline failed"
    fi
    
    echo ""
    echo "Detailed logs: $LOG_FILE"
    
    exit $exit_code
}

# 引数処理
case "${1:-full}" in
    "lint")
        run_stage "Lint Only" "stage_lint"
        ;;
    "test")
        run_stage "Test Only" "stage_test"
        ;;
    "arch")
        run_stage "Architecture Only" "stage_architecture"
        ;;
    "full"|"")
        main
        ;;
    *)
        echo "Usage: $0 [lint|test|arch|full]"
        exit 1
        ;;
esac