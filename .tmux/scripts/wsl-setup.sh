#!/bin/bash
# WSL環境セットアップスクリプト
# Claude Voice tmux統合システムのWSL環境初期化

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TMUX_HOME="$(dirname "$SCRIPT_DIR")"
LOG_FILE="$HOME/.tmux/claude/logs/wsl-setup.log"

# ログ関数
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# エラーハンドリング
error_exit() {
    log "ERROR" "$1"
    exit 1
}

# WSL環境の確認
check_wsl_environment() {
    log "INFO" "WSL環境の確認を開始"
    
    # WSL環境の検証
    if [[ -z "${WSL_DISTRO_NAME:-}" ]] && ! grep -qi microsoft /proc/version 2>/dev/null; then
        error_exit "WSL環境ではありません。このスクリプトはWSL環境でのみ実行してください。"
    fi
    
    # WSLバージョンの確認
    if grep -qi "WSL2" /proc/version 2>/dev/null; then
        log "INFO" "WSL2環境を検出しました"
        export WSL_VERSION="2"
    else
        log "INFO" "WSL1環境を検出しました"
        export WSL_VERSION="1"
    fi
    
    # ディストリビューション情報
    if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
        log "INFO" "ディストリビューション: $WSL_DISTRO_NAME"
    fi
    
    log "INFO" "WSL環境の確認完了"
}

# Windows側アプリケーションの確認
check_windows_applications() {
    log "INFO" "Windows側アプリケーションの確認を開始"
    
    local missing_apps=()
    local optional_apps=()
    
    # PowerShellの確認
    if ! command -v powershell.exe >/dev/null 2>&1 && ! command -v pwsh.exe >/dev/null 2>&1; then
        missing_apps+=("PowerShell")
    else
        log "INFO" "PowerShell検出済み"
    fi
    
    # clip.exeの確認
    if ! command -v clip.exe >/dev/null 2>&1; then
        optional_apps+=("clip.exe (クリップボード統合)")
    else
        log "INFO" "clip.exe検出済み"
    fi
    
    # Windows System32の確認
    if [[ ! -d "/mnt/c/Windows/System32" ]]; then
        missing_apps+=("Windows System32 mount (/mnt/c/Windows/System32)")
    else
        log "INFO" "Windows System32マウント確認済み"
    fi
    
    # 結果報告
    if [[ ${#missing_apps[@]} -gt 0 ]]; then
        log "ERROR" "必要なアプリケーションが見つかりません: ${missing_apps[*]}"
        return 1
    fi
    
    if [[ ${#optional_apps[@]} -gt 0 ]]; then
        log "WARN" "オプション機能が利用できません: ${optional_apps[*]}"
    fi
    
    log "INFO" "Windows側アプリケーションの確認完了"
    return 0
}

# 必要なディレクトリの作成
create_directories() {
    log "INFO" "必要なディレクトリの作成を開始"
    
    local directories=(
        "$HOME/.tmux/claude/logs"
        "$HOME/.tmux/claude/config"
        "$HOME/.tmux/claude/bin"
        "$HOME/.tmux/claude/core"
        "$HOME/.tmux/claude/os"
        "$HOME/.tmux/status"
        "$HOME/.tmux/tests"
    )
    
    for dir in "${directories[@]}"; do
        if [[ ! -d "$dir" ]]; then
            mkdir -p "$dir"
            log "INFO" "ディレクトリ作成: $dir"
        fi
    done
    
    log "INFO" "ディレクトリ作成完了"
}

# WSL固有設定の適用
apply_wsl_configuration() {
    log "INFO" "WSL固有設定の適用を開始"
    
    # 環境変数の設定
    export CLAUDE_WSL_MODE="true"
    export CLAUDE_AUDIO_BACKEND="windows"
    
    # 設定ファイルの存在確認
    local config_files=(
        "$HOME/.tmux/os/wsl.conf"
        "$HOME/.tmux.conf"
    )
    
    for config in "${config_files[@]}"; do
        if [[ ! -f "$config" ]]; then
            log "WARN" "設定ファイルが見つかりません: $config"
        else
            log "INFO" "設定ファイル確認済み: $config"
        fi
    done
    
    # PowerShellパスのキャッシュ
    if command -v powershell.exe >/dev/null 2>&1; then
        export CLAUDE_POWERSHELL_PATH="$(command -v powershell.exe)"
        log "INFO" "PowerShellパスをキャッシュしました: $CLAUDE_POWERSHELL_PATH"
    elif command -v pwsh.exe >/dev/null 2>&1; then
        export CLAUDE_POWERSHELL_PATH="$(command -v pwsh.exe)"
        log "INFO" "PowerShell Coreパスをキャッシュしました: $CLAUDE_POWERSHELL_PATH"
    fi
    
    log "INFO" "WSL固有設定の適用完了"
}

# パフォーマンス最適化
optimize_performance() {
    log "INFO" "パフォーマンス最適化を開始"
    
    # WSL固有のtmux設定最適化
    local tmux_optimizations=(
        "set -g status-interval 5"  # ステータス更新頻度を下げる
        "set -g escape-time 0"      # ESCキーの遅延を削除
        "set -g repeat-time 600"    # リピート時間を調整
    )
    
    # tmux環境での確認
    if command -v tmux >/dev/null 2>&1; then
        log "INFO" "tmux設定の最適化を適用"
        
        # 現在のtmux設定で起動テスト
        if tmux new-session -d -s wsl-setup-test 'sleep 1' 2>/dev/null; then
            tmux kill-session -t wsl-setup-test 2>/dev/null
            log "INFO" "tmux起動テスト成功"
        else
            log "WARN" "tmux起動テストに失敗しました"
        fi
    fi
    
    log "INFO" "パフォーマンス最適化完了"
}

# システム情報の収集と表示
collect_system_info() {
    log "INFO" "システム情報の収集"
    
    echo ""
    echo "=== WSL環境情報 ==="
    echo "WSL Distribution: ${WSL_DISTRO_NAME:-Unknown}"
    echo "WSL Version: $WSL_VERSION"
    echo "Kernel: $(uname -r)"
    echo "Architecture: $(uname -m)"
    echo "Ubuntu Version: $(lsb_release -d 2>/dev/null | cut -f2 || echo "Unknown")"
    
    echo ""
    echo "=== Windows統合情報 ==="
    echo "PowerShell: $(command -v powershell.exe >/dev/null && echo "Available" || echo "Not found")"
    echo "PowerShell Core: $(command -v pwsh.exe >/dev/null && echo "Available" || echo "Not found")"
    echo "clip.exe: $(command -v clip.exe >/dev/null && echo "Available" || echo "Not found")"
    echo "Windows Mount: $([[ -d "/mnt/c" ]] && echo "Available (/mnt/c)" || echo "Not found")"
    
    if command -v powershell.exe >/dev/null 2>&1; then
        echo ""
        echo "=== Windows情報 ==="
        echo "Windows Build: $(powershell.exe -Command "(Get-ComputerInfo).WindowsVersion" 2>/dev/null || echo "Unknown")"
        echo "Windows Edition: $(powershell.exe -Command "(Get-ComputerInfo).WindowsEditionId" 2>/dev/null || echo "Unknown")"
    fi
    
    echo ""
    echo "=== リソース情報 ==="
    echo "Memory: $(free -h | awk 'NR==2{printf "Used: %s / Total: %s (%.1f%%)", $3, $2, ($3/$2)*100}')"
    echo "Disk (WSL): $(df -h / | awk 'NR==2{printf "Used: %s / Total: %s (%s)", $3, $2, $5}')"
    if [[ -d "/mnt/c" ]]; then
        echo "Disk (Windows C:): $(df -h /mnt/c | awk 'NR==2{printf "Used: %s / Total: %s (%s)", $3, $2, $5}' 2>/dev/null || echo "Unable to access")"
    fi
    echo ""
}

# 統合テストの実行
run_integration_test() {
    log "INFO" "統合テストの実行"
    
    local test_script="$TMUX_HOME/tests/wsl-integration-test.sh"
    
    if [[ -f "$test_script" ]] && [[ -x "$test_script" ]]; then
        echo "🧪 WSL統合テストを実行中..."
        if "$test_script"; then
            log "INFO" "統合テスト成功"
            echo "✅ WSL統合テストが成功しました"
        else
            log "WARN" "統合テストで問題が検出されました"
            echo "⚠️ 統合テストで問題が検出されました。詳細はログを確認してください。"
        fi
    else
        log "WARN" "統合テストスクリプトが見つかりません: $test_script"
        echo "⚠️ 統合テストスクリプトが見つかりません"
    fi
}

# セットアップ完了の確認
verify_setup() {
    log "INFO" "セットアップ完了の確認"
    
    local verification_items=(
        "WSL環境検出"
        "PowerShell接続"
        "tmux設定"
        "ディレクトリ構造"
    )
    
    local success_count=0
    local total_count=${#verification_items[@]}
    
    # WSL環境検出
    if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
        ((success_count++))
        echo "✅ WSL環境検出"
    else
        echo "❌ WSL環境検出"
    fi
    
    # PowerShell接続
    if command -v powershell.exe >/dev/null 2>&1 || command -v pwsh.exe >/dev/null 2>&1; then
        ((success_count++))
        echo "✅ PowerShell接続"
    else
        echo "❌ PowerShell接続"
    fi
    
    # tmux設定
    if [[ -f "$HOME/.tmux.conf" ]] && [[ -f "$HOME/.tmux/os/wsl.conf" ]]; then
        ((success_count++))
        echo "✅ tmux設定"
    else
        echo "❌ tmux設定"
    fi
    
    # ディレクトリ構造
    if [[ -d "$HOME/.tmux/claude" ]]; then
        ((success_count++))
        echo "✅ ディレクトリ構造"
    else
        echo "❌ ディレクトリ構造"
    fi
    
    echo ""
    echo "📊 セットアップ成功率: $success_count/$total_count ($(( (success_count * 100) / total_count ))%)"
    
    if [[ $success_count -eq $total_count ]]; then
        echo "🎉 WSLセットアップが正常に完了しました！"
        log "INFO" "WSL setup completed successfully"
        return 0
    else
        echo "⚠️ セットアップに問題があります。ログを確認してください: $LOG_FILE"
        log "WARN" "WSL setup completed with issues"
        return 1
    fi
}

# メイン処理
main() {
    echo "🚀 WSL環境セットアップを開始します"
    echo "================================="
    
    # ログディレクトリの作成
    mkdir -p "$(dirname "$LOG_FILE")"
    
    # セットアップ手順の実行
    check_wsl_environment
    check_windows_applications || {
        echo "⚠️ Windows側アプリケーションの問題があります。続行しますか? (y/N)"
        read -r response
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            exit 1
        fi
    }
    
    create_directories
    apply_wsl_configuration
    optimize_performance
    collect_system_info
    
    echo ""
    echo "🧪 統合テスト実行"
    run_integration_test
    
    echo ""
    echo "✅ セットアップ確認"
    verify_setup
    
    echo ""
    echo "📝 次のステップ:"
    echo "1. tmux を再起動してください: tmux kill-server && tmux"
    echo "2. Claude Voice をテストしてください: ~/.tmux/claude/bin/claude-voice --health-check"
    echo "3. 音声テストを実行してください: ~/.tmux/claude/bin/claude-voice brief 10"
    echo "4. 問題がある場合はログを確認してください: $LOG_FILE"
    
    log "INFO" "WSL setup script completed"
}

# スクリプト実行
main "$@"