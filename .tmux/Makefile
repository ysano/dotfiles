# Simple Makefile for Claude Voice tmux integration
# 簡易Makefile - 新しい軽量テストシステム用

.PHONY: test lint ci clean help

# デフォルトターゲット
all: test

# 最小限テスト実行
test:
	@echo "🧪 Running minimal test suite..."
	@bash tests/minimal-test.sh

# 構文チェック
lint:
	@echo "🔍 Running syntax check..."
	@bash ci.sh lint

# 完全CI実行
ci:
	@echo "🚀 Running full CI pipeline..."
	@bash ci.sh full

# アーキテクチャ検証のみ
arch:
	@echo "🏗️  Running architecture validation..."
	@bash ci.sh arch

# クリーンアップ
clean:
	@echo "🧹 Cleaning up..."
	@rm -f ci.log
	@find . -name "*.log" -type f -delete 2>/dev/null || true
	@echo "✅ Cleanup completed"

# ヘルプ
help:
	@echo "Available targets:"
	@echo "  test    - Run minimal test suite (default)"
	@echo "  lint    - Run syntax check only"
	@echo "  ci      - Run full CI pipeline"
	@echo "  arch    - Run architecture validation"
	@echo "  clean   - Clean up log files"
	@echo "  help    - Show this help"