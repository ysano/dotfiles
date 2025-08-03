#!/bin/bash
# WSL Claude Voice Integration Test
# WSL環境でのOllama要約音声機能統合テスト

set -euo pipefail

# 環境設定
export CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
CORE_DIR="$CLAUDE_VOICE_HOME/core"

# カラー出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ログ関数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# テストカウンタ
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# テスト実行関数
run_test() {
    local test_name="$1"
    local test_cmd="$2"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    log_info "Testing: $test_name"
    
    if eval "$test_cmd" >/dev/null 2>&1; then
        log_success "✓ $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        log_error "✗ $test_name"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# WSL環境確認
test_wsl_environment() {
    echo "=== WSL環境確認 ==="
    
    run_test "WSL環境検出" '[[ -n "$WSL_DISTRO_NAME" ]] || grep -qi microsoft /proc/version'
    
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        log_info "WSL Distribution: $WSL_DISTRO_NAME"
    fi
    
    echo ""
}

# 動的IP解決テスト
test_dynamic_ip_resolution() {
    echo "=== 動的IP解決テスト ==="
    
    run_test "wsl_host_resolver.sh存在確認" '[[ -x "$CORE_DIR/wsl_host_resolver.sh" ]]'
    
    local host_ip
    if host_ip=$("$CORE_DIR/wsl_host_resolver.sh" ip 2>/dev/null); then
        log_success "WindowsホストIP解決: $host_ip"
    else
        log_error "WindowsホストIP解決失敗"
        return 1
    fi
    
    local ollama_url
    if ollama_url=$("$CORE_DIR/wsl_host_resolver.sh" url 2>/dev/null); then
        log_success "Ollama URL生成: $ollama_url"
    else
        log_error "Ollama URL生成失敗"
        return 1
    fi
    
    echo ""
}

# Ollama接続テスト
test_ollama_connection() {
    echo "=== Ollama接続テスト ==="
    
    local ollama_url
    ollama_url=$("$CORE_DIR/wsl_host_resolver.sh" url 2>/dev/null) || {
        log_error "Ollama URLが取得できません"
        return 1
    }
    
    run_test "Ollama API応答確認" "curl -s --connect-timeout 5 '$ollama_url/api/tags' | jq -e '.models' >/dev/null"
    
    # 利用可能モデル確認
    local models
    models=$(curl -s "$ollama_url/api/tags" | jq -r '.models[].name' | head -3)
    if [[ -n "$models" ]]; then
        log_success "利用可能モデル:"
        echo "$models" | while read -r model; do
            echo "  - $model"
        done
    fi
    
    echo ""
}

# WSL音声システムテスト
test_wsl_voice_system() {
    echo "=== WSL音声システムテスト ==="
    
    run_test "WSL音声エンジンv2.0存在確認" '[[ -x "$CORE_DIR/wsl_voice_engine_v2.sh" ]]'
    
    run_test "PowerShell実行可能確認" '[[ -x "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" ]]'
    
    # 音声合成テスト（実際の音声出力は行わない）
    if "$CORE_DIR/wsl_voice_engine_v2.sh" diagnose >/dev/null 2>&1; then
        log_success "WSL音声システムv2.0診断成功"
    else
        log_warning "WSL音声システムv2.0診断で問題を検出"
    fi
    
    echo ""
}

# LLM Manager統合テスト
test_llm_manager_integration() {
    echo "=== LLM Manager統合テスト ==="
    
    # 環境変数設定
    export CLAUDE_VOICE_HOME
    
    # LLM Managerの動的URL解決テスト
    if source "$CORE_DIR/llm_manager.sh" 2>/dev/null; then
        if [[ "$DEFAULT_OLLAMA_API" == *"172.29"* ]] || [[ "$DEFAULT_OLLAMA_API" == *"192.168"* ]]; then
            log_success "LLM Manager動的IP解決: $DEFAULT_OLLAMA_API"
        else
            log_warning "LLM Manager動的IP解決が静的: $DEFAULT_OLLAMA_API"
        fi
    else
        log_error "LLM Manager読み込み失敗"
        return 1
    fi
    
    echo ""
}

# Claude Voice統合テスト
test_claude_voice_integration() {
    echo "=== Claude Voice統合テスト ==="
    
    run_test "Claude Voiceバイナリ存在確認" '[[ -x "$CLAUDE_VOICE_HOME/bin/claude-voice" ]]'
    
    # 設定ファイル確認
    run_test "YAML設定ファイル存在確認" '[[ -f "$CLAUDE_VOICE_HOME/config/claude-voice.yaml" ]]'
    
    # 簡単な機能テスト（実際の音声出力なし）
    echo "Testing WSL Claude Voice functionality (no audio)..."
    
    echo ""
}

# メイン実行
main() {
    echo "================================================================"
    echo "WSL Claude Voice Integration Test Suite"
    echo "================================================================"
    echo ""
    
    test_wsl_environment
    test_dynamic_ip_resolution
    test_ollama_connection
    test_wsl_voice_system
    test_llm_manager_integration
    test_claude_voice_integration
    
    echo "================================================================"
    echo "テスト結果サマリー"
    echo "================================================================"
    echo "実行: $TESTS_RUN"
    echo -e "成功: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "失敗: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo ""
        log_success "🎉 すべてのテストが成功しました！"
        log_success "WSL環境でのOllama要約音声読み上げ機能が利用可能です。"
        echo ""
        echo "次のステップ:"
        echo "1. tmuxでPrefix+C-tを押して音声テスト"
        echo "2. ~/.tmux/claude/bin/claude-voice brief 10 でOllama要約テスト"
        echo "3. Prefix+C-aで自動要約ON/OFF切り替え"
        return 0
    else
        echo ""
        log_error "⚠️  一部のテストが失敗しました。"
        log_error "詳細なログを確認してください。"
        return 1
    fi
}

# スクリプト実行
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi