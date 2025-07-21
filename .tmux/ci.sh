#!/bin/bash
# Minimal CI - Essential checks only
# 最小限CI - 必須チェックのみ

set -euo pipefail

# 基本設定
CI_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 基本チェック
check() {
    echo "🔍 Running basic checks..."
    
    # 構文チェック
    find "$CI_DIR/claude" -name "*.sh" -exec bash -n {} \; || exit 1
    
    # テスト実行
    bash "$CI_DIR/tests/minimal-test.sh" || exit 1
    
    echo "✅ All checks passed"
}

# 引数処理
case "${1:-check}" in
    "check"|"")
        check
        ;;
    *)
        echo "Usage: $0 [check]"
        exit 1
        ;;
esac