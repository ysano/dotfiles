# Claude Voice Integration - Development Makefile
# ローカル開発用の品質チェックツール

.PHONY: all check lint format test security clean install-tools help
.DEFAULT_GOAL := help

# 変数定義
CLAUDE_DIR := .tmux/claude
CORE_DIR := $(CLAUDE_DIR)/core
OS_DIR := $(CLAUDE_DIR)/os
TEST_DIR := $(CLAUDE_DIR)/tests
EXCLUDE_SC := SC1090,SC1091,SC2034,SC2086,SC2155,SC2016,SC2001,SC2154,SC2005,SC2120,SC2046,SC2162,SC2119,SC2207,SC2184,SC2152,SC2064,SC2168,SC2178,SC2221,SC2222,SC2076,SC2181,SC2094,SC2088,SC2004,SC2126,SC2144,SC2206,SC2317

# カラー出力
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
BLUE := \033[0;34m
CYAN := \033[0;36m
NC := \033[0m # No Color

# ============================================================================
# メインターゲット
# ============================================================================

## 🚀 すべてのチェックを実行（CI相当）
all: check

## ✅ 包括的品質チェック（CI相当の高速版）
check: lint test security
	@echo "$(GREEN)✅ すべてのチェックが完了しました！$(NC)"

## 🔍 コード品質チェック（ShellCheck + フォーマット）
lint: shellcheck format-check

## 🧪 テスト実行
test: unit-test integration-test

## 🔒 セキュリティチェック
security: secret-scan vuln-scan

## 🧹 一時ファイルクリーンアップ
clean:
	@echo "$(CYAN)🧹 一時ファイルをクリーンアップ中...$(NC)"
	@find $(CLAUDE_DIR) -name "*.tmp" -delete 2>/dev/null || true
	@find $(CLAUDE_DIR) -name "*.log" -delete 2>/dev/null || true
	@rm -f quality-report.md security-report.md 2>/dev/null || true
	@echo "$(GREEN)✅ クリーンアップ完了$(NC)"

# ============================================================================
# 依存関係インストール
# ============================================================================

## 🔧 必要なツールをインストール
install-tools:
	@echo "$(CYAN)🔧 開発ツールをインストール中...$(NC)"
	@if ! command -v shellcheck >/dev/null 2>&1; then \
		echo "$(YELLOW)⚙️ ShellCheckをダウンロード中...$(NC)"; \
		mkdir -p ~/.local/bin; \
		if curl -sL "https://github.com/koalaman/shellcheck/releases/download/v0.9.0/shellcheck-v0.9.0.linux.x86_64.tar.xz" | tar -xJv; then \
			cp shellcheck-v0.9.0/shellcheck ~/.local/bin/ && chmod +x ~/.local/bin/shellcheck; \
			rm -rf shellcheck-v0.9.0; \
			echo "$(GREEN)✅ ShellCheckインストール完了$(NC)"; \
		else \
			echo "$(RED)❌ ShellCheckのダウンロードに失敗しました$(NC)"; \
		fi; \
	fi
	@if ! command -v shfmt >/dev/null 2>&1; then \
		echo "$(YELLOW)⚙️ shfmtをインストール中...$(NC)"; \
		if command -v go >/dev/null 2>&1; then \
			go install mvdan.cc/sh/v3/cmd/shfmt@latest; \
		else \
			echo "$(YELLOW)⚠️ shfmtのインストールにはGoが必要です$(NC)"; \
		fi; \
	fi
	@if ! command -v yamllint >/dev/null 2>&1; then \
		echo "$(YELLOW)⚙️ yamllintをインストール中...$(NC)"; \
		pip3 install --user yamllint 2>/dev/null || pip install --user yamllint 2>/dev/null || true; \
	fi
	@echo "$(GREEN)✅ ツールインストール完了$(NC)"

# ============================================================================
# ShellCheckとフォーマット
# ============================================================================

## 📋 ShellCheck - Bashスクリプト静的解析
shellcheck:
	@echo "$(CYAN)📋 ShellCheck解析を実行中...$(NC)"
	@SHELLCHECK_CMD=""; \
	if command -v /opt/homebrew/bin/shellcheck >/dev/null 2>&1; then \
		SHELLCHECK_CMD="/opt/homebrew/bin/shellcheck"; \
	elif command -v shellcheck >/dev/null 2>&1; then \
		SHELLCHECK_CMD="shellcheck"; \
	elif [ -x ~/.local/bin/shellcheck ]; then \
		SHELLCHECK_CMD="$$HOME/.local/bin/shellcheck"; \
	else \
		echo "$(RED)❌ ShellCheckが見つかりません。make install-tools を実行してください$(NC)"; \
		exit 1; \
	fi; \
	error_count=0; \
	total_files=0; \
	for script in $$(find $(CLAUDE_DIR) -name "*.sh" -type f | sort); do \
		total_files=$$((total_files + 1)); \
		echo "  🔍 $$script"; \
		if ! $$SHELLCHECK_CMD --exclude=$(EXCLUDE_SC) --format=gcc "$$script"; then \
			error_count=$$((error_count + 1)); \
		fi; \
	done; \
	echo "$(BLUE)📊 解析結果: $$total_files ファイル中 $$error_count エラー$(NC)"; \
	if [ $$error_count -eq 0 ]; then \
		echo "$(GREEN)✅ ShellCheck解析成功$(NC)"; \
	else \
		echo "$(RED)❌ ShellCheck解析失敗: $$error_count エラー$(NC)"; \
		exit 1; \
	fi

## 📝 フォーマットチェック
format-check:
	@echo "$(CYAN)📝 コードフォーマットをチェック中...$(NC)"
	@if command -v shfmt >/dev/null 2>&1; then \
		format_issues=0; \
		for script in $$(find $(CLAUDE_DIR) -name "*.sh" -type f | sort); do \
			if ! shfmt -d -i 4 -ci "$$script" >/dev/null 2>&1; then \
				echo "$(YELLOW)⚠️ フォーマット問題: $$script$(NC)"; \
				format_issues=$$((format_issues + 1)); \
			fi; \
		done; \
		if [ $$format_issues -eq 0 ]; then \
			echo "$(GREEN)✅ フォーマットチェック成功$(NC)"; \
		else \
			echo "$(YELLOW)⚠️ $$format_issues ファイルにフォーマット問題があります$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)⚠️ shfmtが見つかりません。フォーマットチェックをスキップします$(NC)"; \
	fi

## 🔧 コードフォーマット自動修正
format-fix:
	@echo "$(CYAN)🔧 コードフォーマットを自動修正中...$(NC)"
	@if command -v shfmt >/dev/null 2>&1; then \
		for script in $$(find $(CLAUDE_DIR) -name "*.sh" -type f | sort); do \
			echo "  🔧 $$script"; \
			shfmt -w -i 4 -ci "$$script"; \
		done; \
		echo "$(GREEN)✅ フォーマット修正完了$(NC)"; \
	else \
		echo "$(RED)❌ shfmtがインストールされていません$(NC)"; \
		exit 1; \
	fi

# ============================================================================
# テスト実行
# ============================================================================

## 🧪 単体テスト実行
unit-test:
	@echo "$(CYAN)🧪 単体テストを実行中...$(NC)"
	@if [ -f "$(TEST_DIR)/test_runner.sh" ]; then \
		cd $(TEST_DIR) && chmod +x test_runner.sh && ./test_runner.sh unit; \
		echo "$(GREEN)✅ 単体テスト完了$(NC)"; \
	else \
		echo "$(YELLOW)⚠️ テストランナーが見つかりません: $(TEST_DIR)/test_runner.sh$(NC)"; \
		exit 1; \
	fi

## 🔗 統合テスト実行
integration-test:
	@echo "$(CYAN)🔗 統合テストを実行中...$(NC)"
	@if [ -f "$(TEST_DIR)/test_runner.sh" ]; then \
		cd $(TEST_DIR) && chmod +x test_runner.sh && ./test_runner.sh integration; \
		echo "$(GREEN)✅ 統合テスト完了$(NC)"; \
	else \
		echo "$(YELLOW)⚠️ テストランナーが見つかりません$(NC)"; \
		exit 1; \
	fi

## ⚡ 高速テスト（基本チェックのみ）
test-quick:
	@echo "$(CYAN)⚡ 高速テストを実行中...$(NC)"
	@if [ -f "$(TEST_DIR)/test_runner.sh" ]; then \
		cd $(TEST_DIR) && chmod +x test_runner.sh && timeout 60 ./test_runner.sh unit --quick; \
		echo "$(GREEN)✅ 高速テスト完了$(NC)"; \
	else \
		echo "$(YELLOW)⚠️ テストランナーが見つかりません$(NC)"; \
	fi

# ============================================================================
# セキュリティチェック
# ============================================================================

## 🔍 シークレット検出
secret-scan:
	@echo "$(CYAN)🔍 シークレット検出を実行中...$(NC)"
	@secrets_found=0; \
	patterns="password['\"]?\\s*[:=]\\s*['\"][^'\"]*['\"] api[_-]?key['\"]?\\s*[:=]\\s*['\"][^'\"]*['\"] secret['\"]?\\s*[:=]\\s*['\"][^'\"]*['\"] token['\"]?\\s*[:=]\\s*['\"][^'\"]*['\"] [A-Za-z0-9]{32,}"; \
	for pattern in $$patterns; do \
		if grep -r -E "$$pattern" $(CLAUDE_DIR)/ --include="*.sh" --include="*.conf" | grep -v -E "(example|test|mock|dummy)" >/dev/null 2>&1; then \
			echo "$(RED)❌ 潜在的シークレット検出: $$pattern$(NC)"; \
			secrets_found=1; \
		fi; \
	done; \
	if [ $$secrets_found -eq 0 ]; then \
		echo "$(GREEN)✅ シークレット検出: 問題なし$(NC)"; \
	else \
		echo "$(RED)❌ セキュリティリスク検出$(NC)"; \
		exit 1; \
	fi

## 🛡️ 脆弱性パターン検出
vuln-scan:
	@echo "$(CYAN)🛡️ 脆弱性パターン検出を実行中...$(NC)"
	@vuln_found=0; \
	echo "  🔍 パストラバーサルパターンチェック..."; \
	if grep -r -E '\.\./|\.\.\\' $(CLAUDE_DIR)/ --include="*.sh" --exclude-dir="tests" | grep -v -E "(# Safe:|sed.*\.{3}|\.\.\.)"; then \
		echo "$(YELLOW)⚠️ パストラバーサルパターン検出$(NC)"; \
		vuln_found=1; \
	fi; \
	echo "  🔍 危険なeval使用チェック..."; \
	if grep -r -E '\beval\s+' $(CLAUDE_DIR)/ --include="*.sh" --exclude-dir="tests" | grep -v -E "(# Safe:|yq eval|test_|mock_|errors_var|check_function|engine_function)"; then \
		echo "$(YELLOW)⚠️ 危険なeval使用検出$(NC)"; \
		vuln_found=1; \
	fi; \
	if [ $$vuln_found -eq 0 ]; then \
		echo "$(GREEN)✅ 脆弱性スキャン: 問題なし$(NC)"; \
	else \
		echo "$(YELLOW)⚠️ 潜在的セキュリティリスクが検出されました$(NC)"; \
	fi

# ============================================================================
# パフォーマンスと品質
# ============================================================================

## ⚡ パフォーマンステスト
perf-test:
	@echo "$(CYAN)⚡ パフォーマンステストを実行中...$(NC)"
	@total_load_time=0; \
	module_count=0; \
	for module in $(CORE_DIR)/*.sh; do \
		if [ -f "$$module" ]; then \
			echo "  ⚡ $$(basename $$module)"; \
			start_time=$$(date +%s%3N); \
			bash -n "$$module" 2>/dev/null; \
			end_time=$$(date +%s%3N); \
			load_time=$$((end_time - start_time)); \
			total_load_time=$$((total_load_time + load_time)); \
			module_count=$$((module_count + 1)); \
			if [ $$load_time -gt 500 ]; then \
				echo "$(YELLOW)    ⚠️ 遅いロード時間: $${load_time}ms$(NC)"; \
			fi; \
		fi; \
	done; \
	if [ $$module_count -gt 0 ]; then \
		avg_load_time=$$((total_load_time / module_count)); \
		echo "$(BLUE)📊 平均ロード時間: $${avg_load_time}ms$(NC)"; \
		echo "$(BLUE)📊 総推定ロード時間: $${total_load_time}ms$(NC)"; \
		if [ $$total_load_time -gt 5000 ]; then \
			echo "$(YELLOW)⚠️ 総ロード時間が5秒を超えています$(NC)"; \
		else \
			echo "$(GREEN)✅ パフォーマンス: 良好$(NC)"; \
		fi; \
	fi

## 📊 品質メトリクス生成
quality-report:
	@echo "$(CYAN)📊 品質メトリクスを生成中...$(NC)"
	@{	echo "# Claude Voice Integration - 品質レポート"; \
		echo ""; \
		echo "生成日時: $$(date)"; \
		echo ""; \
		echo "## コード統計"; \
		echo "| モジュール | 行数 | 関数数 | 平均関数長 |"; \
		echo "|-----------|------|-------|-----------|"; \
		total_lines=0; \
		total_functions=0; \
		total_files=0; \
		for script in $$(find $(CORE_DIR) -name "*.sh" -type f | sort); do \
			module_name=$$(basename "$$script" .sh); \
			lines=$$(wc -l < "$$script"); \
			functions=$$(grep -c "^[a-zA-Z_][a-zA-Z0-9_]*() *{" "$$script" 2>/dev/null || echo 0); \
			if [ $$functions -gt 0 ]; then \
				avg_func_length=$$((lines / functions)); \
			else \
				avg_func_length=0; \
			fi; \
			echo "| $$module_name | $$lines | $$functions | $$avg_func_length |"; \
			total_lines=$$((total_lines + lines)); \
			total_functions=$$((total_functions + functions)); \
			total_files=$$((total_files + 1)); \
		done; \
		echo ""; \
		echo "## サマリー"; \
		echo "- **総ファイル数**: $$total_files"; \
		echo "- **総行数**: $$total_lines"; \
		echo "- **総関数数**: $$total_functions"; \
		if [ $$total_files -gt 0 ]; then \
			echo "- **ファイル当たり平均行数**: $$((total_lines / total_files))"; \
			echo "- **ファイル当たり平均関数数**: $$((total_functions / total_files))"; \
		fi; \
	} > quality-report.md
	@echo "$(GREEN)✅ 品質レポート生成完了: quality-report.md$(NC)"

# ============================================================================
# 開発支援
# ============================================================================

## 🔄 開発用フルチェック（高速）
dev-check: shellcheck test-quick
	@echo "$(GREEN)🔄 開発用チェック完了$(NC)"

## 👀 ファイル変更監視（要: inotify-tools）
watch:
	@echo "$(CYAN)👀 ファイル変更を監視中... (Ctrl+C で停止)$(NC)"
	@if command -v inotifywait >/dev/null 2>&1; then \
		while inotifywait -e modify -r $(CLAUDE_DIR) >/dev/null 2>&1; do \
			echo "$(YELLOW)🔄 変更検出 - チェック実行中...$(NC)"; \
			make dev-check; \
		done; \
	else \
		echo "$(RED)❌ inotify-toolsがインストールされていません$(NC)"; \
		echo "$(BLUE)Ubuntu/Debian: sudo apt-get install inotify-tools$(NC)"; \
		echo "$(BLUE)macOS: brew install fswatch$(NC)"; \
	fi

## 📈 開発統計表示
stats:
	@echo "$(CYAN)📈 プロジェクト統計$(NC)"
	@echo "$(BLUE)📁 ディレクトリ構成:$(NC)"
	@find $(CLAUDE_DIR) -type d | sort | sed 's/^/  /'
	@echo ""
	@echo "$(BLUE)📄 ファイル統計:$(NC)"
	@echo "  Shell scripts: $$(find $(CLAUDE_DIR) -name "*.sh" | wc -l)"
	@echo "  Test files: $$(find $(TEST_DIR) -name "*.sh" 2>/dev/null | wc -l)"
	@echo "  Core modules: $$(find $(CORE_DIR) -name "*.sh" 2>/dev/null | wc -l)"
	@echo "  OS modules: $$(find $(OS_DIR) -name "*.sh" 2>/dev/null | wc -l)"
	@echo ""
	@echo "$(BLUE)📊 コード統計:$(NC)"
	@total_lines=$$(find $(CLAUDE_DIR) -name "*.sh" -exec wc -l {} + | tail -1 | awk '{print $$1}'); \
	echo "  総行数: $$total_lines"
	@total_functions=$$(find $(CLAUDE_DIR) -name "*.sh" -exec grep -c "^[a-zA-Z_][a-zA-Z0-9_]*() *{" {} + 2>/dev/null | awk -F: '{sum+=$$2} END {print sum}'); \
	echo "  総関数数: $$total_functions"

# ============================================================================
# ヘルプ
# ============================================================================

## 📚 このヘルプを表示
help:
	@echo "$(CYAN)🛠️  Claude Voice Integration - 開発用Makefile$(NC)"
	@echo ""
	@echo "$(GREEN)メインコマンド:$(NC)"
	@grep -E '^## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = "## "}; {printf "  $(BLUE)%-20s$(NC) %s\n", $$2, $$2}' | sed 's/## //'
	@echo ""
	@echo "$(GREEN)使用例:$(NC)"
	@echo "  $(BLUE)make check$(NC)          # CI相当のフルチェック"
	@echo "  $(BLUE)make dev-check$(NC)      # 開発用高速チェック"
	@echo "  $(BLUE)make lint$(NC)           # コード品質チェックのみ"
	@echo "  $(BLUE)make test-quick$(NC)     # 高速テストのみ"
	@echo "  $(BLUE)make install-tools$(NC)  # 必要ツールをインストール"
	@echo "  $(BLUE)make watch$(NC)          # ファイル変更監視"
	@echo ""
	@echo "$(GREEN)品質チェック項目:$(NC)"
	@echo "  ✅ ShellCheck静的解析"
	@echo "  📝 コードフォーマット"
	@echo "  🧪 単体・統合テスト"
	@echo "  🔒 セキュリティスキャン"
	@echo "  ⚡ パフォーマンステスト"
	@echo "  📊 品質メトリクス"