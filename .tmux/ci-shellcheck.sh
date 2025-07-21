#!/bin/bash
# CI compatible ShellCheck script
# CI互換ShellCheckスクリプト

set -euo pipefail

# ベースディレクトリ
CI_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ShellCheck CI version - make lintと同等の軽量チェック
echo "🔍 Running CI ShellCheck..."

# 基本的な構文チェック
echo "Step 1: Basic syntax check..."
find "$CI_DIR/claude" -name "*.sh" -exec bash -n {} \; || exit 1

# ShellCheck軽量版（重要なエラーのみ）
echo "Step 2: ShellCheck (error level only)..."
if command -v shellcheck >/dev/null 2>&1; then
    find "$CI_DIR/claude" -name "*.sh" -print0 | \
    xargs -0 shellcheck \
    -e SC1090,SC1091,SC2034,SC2086,SC2155,SC2119,SC2120,SC2181,SC2207 \
    -e SC2088,SC2144,SC2145,SC2152,SC2154,SC2046,SC2076,SC2184,SC2168 \
    -e SC2178,SC2064,SC2206,SC2221,SC2222 \
    -S error || exit 1
else
    echo "ShellCheck not available, skipping advanced checks"
fi

echo "✅ CI ShellCheck completed successfully"
exit 0