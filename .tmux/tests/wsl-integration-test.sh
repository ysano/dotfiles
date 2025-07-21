#!/bin/bash
# WSL統合テストスクリプト
# Claude Voice tmux統合システムのWSL環境での動作確認

set -euo pipefail

# テスト設定
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLAUDE_HOME="$HOME/.tmux/claude"
LOG_FILE="$CLAUDE_HOME/logs/wsl-integration-test.log"

# ログ関数
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# テスト結果
declare -a test_results=()
test_count=0
pass_count=0

# テスト実行関数
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    ((test_count++))
    log "INFO" "Running test: $test_name"
    
    if eval "$test_command" >/dev/null 2>&1; then
        log "PASS" "$test_name"
        test_results+=("✅ $test_name")
        ((pass_count++))
        return 0
    else
        log "FAIL" "$test_name"
        test_results+=("❌ $test_name")
        return 1
    fi
}

# WSL環境検出テスト
test_wsl_detection() {
    echo "=== WSL環境検出テスト ==="
    
    run_test "WSL_DISTRO_NAME環境変数の確認" '[[ -n "${WSL_DISTRO_NAME:-}" ]]'
    run_test "Microsoftカーネルの検出" 'grep -qi microsoft /proc/version 2>/dev/null'
    run_test "WSL2の検出" 'grep -qi "WSL2" /proc/version 2>/dev/null'
    run_test "Windowsファイルシステムのマウント確認" '[[ -d "/mnt/c" ]]'
}

# PowerShell接続テスト
test_powershell_integration() {
    echo "=== PowerShell統合テスト ==="
    
    run_test "PowerShell実行ファイルの検出" 'command -v powershell.exe >/dev/null || command -v pwsh.exe >/dev/null'
    run_test "PowerShellコマンド実行テスト" 'powershell.exe -Command "echo \"test\"" 2>/dev/null | grep -q "test"'
    run_test "PowerShellバージョン確認" 'powershell.exe -Command "$PSVersionTable.PSVersion" 2>/dev/null | grep -q "Major"'
}

# クリップボード統合テスト
test_clipboard_integration() {
    echo "=== クリップボード統合テスト ==="
    
    run_test "clip.exe の検出" 'command -v clip.exe >/dev/null'
    
    if command -v clip.exe >/dev/null 2>&1; then
        run_test "クリップボードへの書き込みテスト" 'echo "WSL-test" | clip.exe'
    fi
    
    run_test "PowerShellクリップボード機能テスト" 'powershell.exe -Command "Set-Clipboard -Value \"test\"; Get-Clipboard" 2>/dev/null | grep -q "test"'
}

# 音声システムテスト
test_audio_system() {
    echo "=== 音声システムテスト ==="
    
    # Claude Voice モジュールのロード
    if [[ -f "$CLAUDE_HOME/core/base.sh" ]]; then
        source "$CLAUDE_HOME/core/base.sh"
        claude_voice_init false
        
        if [[ -f "$CLAUDE_HOME/os/windows.sh" ]]; then
            source "$CLAUDE_HOME/os/windows.sh"
            
            run_test "Windows依存関係チェック" 'check_windows_dependencies'
            run_test "PowerShellパスの検出" '[[ -n "$(find_powershell_path)" ]]'
            run_test "Windows TTS音声の検出" '[[ -n "$(detect_windows_tts_voices)" ]]'
            run_test "日本語音声の選択" '[[ -n "$(select_japanese_voice)" ]]'
        fi
    else
        log "WARN" "Claude Voice modules not found, skipping audio tests"
    fi
}

# tmux統合テスト
test_tmux_integration() {
    echo "=== tmux統合テスト ==="
    
    run_test "tmuxの実行確認" 'command -v tmux >/dev/null'
    run_test "WSL設定ファイルの存在確認" '[[ -f "$HOME/.tmux/os/wsl.conf" ]]'
    run_test "メイン設定ファイルの存在確認" '[[ -f "$HOME/.tmux.conf" ]]'
    
    if command -v tmux >/dev/null 2>&1; then
        run_test "tmux設定の構文チェック" 'tmux -f "$HOME/.tmux.conf" list-keys >/dev/null'
    fi
}

# システム情報収集テスト
test_system_info() {
    echo "=== システム情報収集テスト ==="
    
    if [[ -f "$CLAUDE_HOME/os/windows.sh" ]]; then
        source "$CLAUDE_HOME/os/windows.sh"
        
        run_test "WSL情報の取得" '[[ -n "$(get_wsl_info version)" ]]'
        run_test "Windows Build情報の取得" '[[ -n "$(get_wsl_info windows_build)" ]]'
        run_test "メモリ情報の取得" '[[ -n "$(get_wsl_info memory)" ]]'
    fi
}

# Claude Voice統合テスト
test_claude_voice_integration() {
    echo "=== Claude Voice統合テスト ==="
    
    if [[ -f "$CLAUDE_HOME/bin/claude-voice" ]]; then
        run_test "Claude Voiceバイナリの実行権限確認" '[[ -x "$CLAUDE_HOME/bin/claude-voice" ]]'
        run_test "Claude Voice設定ファイルの確認" '[[ -f "$CLAUDE_HOME/config/claude-voice.yaml" || -f "$CLAUDE_HOME/config/claude-voice.conf" ]]'
        
        # ヘルスチェックの実行
        if [[ -x "$CLAUDE_HOME/bin/claude-voice" ]]; then
            run_test "Claude Voiceヘルスチェック" '$CLAUDE_HOME/bin/claude-voice --health-check'
        fi
    else
        log "WARN" "Claude Voice binary not found, skipping integration tests"
    fi
}

# パフォーマンステスト
test_performance() {
    echo "=== パフォーマンステスト ==="
    
    # tmux起動時間の測定
    local start_time=$(date +%s.%N)
    tmux new-session -d -s wsl-test 'sleep 1' 2>/dev/null || true
    tmux kill-session -t wsl-test 2>/dev/null || true
    local end_time=$(date +%s.%N)
    local duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "0.5")
    
    run_test "tmux起動時間 (<2秒)" '[[ $(echo "$duration < 2.0" | bc 2>/dev/null || echo "1") -eq 1 ]]'
    
    # PowerShell実行時間の測定
    if command -v powershell.exe >/dev/null 2>&1; then
        local ps_start=$(date +%s.%N)
        powershell.exe -Command "echo 'test'" >/dev/null 2>&1
        local ps_end=$(date +%s.%N)
        local ps_duration=$(echo "$ps_end - $ps_start" | bc 2>/dev/null || echo "1.0")
        
        run_test "PowerShell実行時間 (<3秒)" '[[ $(echo "$ps_duration < 3.0" | bc 2>/dev/null || echo "1") -eq 1 ]]'
    fi
}

# メインテスト実行
main() {
    echo "🧪 WSL統合テスト開始"
    echo "====================="
    
    # ログディレクトリの作成
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # 環境情報の表示
    echo "WSL Distribution: ${WSL_DISTRO_NAME:-Unknown}"
    echo "Kernel Version: $(uname -r)"
    echo "PowerShell Available: $(command -v powershell.exe >/dev/null && echo "Yes" || echo "No")"
    echo "clip.exe Available: $(command -v clip.exe >/dev/null && echo "Yes" || echo "No")"
    echo ""
    
    # テスト実行
    test_wsl_detection
    test_powershell_integration
    test_clipboard_integration
    test_audio_system
    test_tmux_integration
    test_system_info
    test_claude_voice_integration
    test_performance
    
    echo ""
    echo "=== テスト結果 ==="
    for result in "${test_results[@]}"; do
        echo "$result"
    done
    
    echo ""
    echo "📊 テスト統計"
    echo "全体: $test_count tests"
    echo "成功: $pass_count tests"
    echo "失敗: $((test_count - pass_count)) tests"
    echo "成功率: $(( (pass_count * 100) / test_count ))%"
    
    if [[ $pass_count -eq $test_count ]]; then
        echo "🎉 すべてのテストが成功しました！"
        log "INFO" "All WSL integration tests passed"
        exit 0
    else
        echo "⚠️  いくつかのテストが失敗しました。ログを確認してください: $LOG_FILE"
        log "WARN" "Some WSL integration tests failed"
        exit 1
    fi
}

# スクリプト実行
main "$@"