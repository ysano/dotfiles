# Minimal Makefile
# 最小限Makefile

.PHONY: test check clean

# デフォルト: テスト実行
test:
	@bash tests/minimal-test.sh

# 基本チェック（構文+テスト）
check:
	@bash ci.sh

# クリーンアップ
clean:
	@rm -f ci.log 2>/dev/null || true