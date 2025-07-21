#!/bin/bash
# User Interface Module - ユーザーインターフェース機能
# ヘルプ表示、バージョン情報、使用方法の表示を担当

# === ユーザーインターフェース機能 ===

# ヘルプとコマンド使用方法の表示
show_usage() {
    cat <<EOF
Claude Voice v${CLAUDE_VOICE_VERSION:-"未定義"} - クロスプラットフォーム音声通知システム

使用法: $(basename "$0") [OPTIONS] [SUMMARY_TYPE] [LINES] [VOICE] [MODEL] [DEVICE]

SUMMARY_TYPE:
  brief         簡潔な要約（デフォルト）
  detailed      詳細な要約  
  technical     技術的な要約

LINES:          取得する画面の行数（デフォルト: ${DEFAULT_LINES:-50}）
VOICE:          使用する音声（デフォルト: auto）
MODEL:          使用するLLMモデル（デフォルト: auto）
DEVICE:         オーディオデバイス（デフォルト: system_default）
                - system_default: システムデフォルト出力
                - alert_device: アラート音デバイス
                - sound_effects: サウンドエフェクトデバイス

OPTIONS:
  -h, --help    このヘルプを表示
  -v, --version バージョン情報を表示
  -d, --debug   デバッグモードで実行
  --stats       使用統計を表示
  --test        システムテストを実行
  --config      設定ファイルを表示/編集
  --health      ヘルスチェックを実行
  --repair      設定の修復を実行

例:
  $(basename "$0")                          # デフォルト設定で実行
  $(basename "$0") brief 30                 # 簡潔要約、30行
  $(basename "$0") detailed 50 Kyoko        # 詳細要約、50行、Kyoko音声
  $(basename "$0") technical 40 auto phi4-mini:latest  # 技術的要約、指定モデル

環境変数:
  CLAUDE_VOICE_HOME    ホームディレクトリ（デフォルト: ~/.tmux/claude）
  DEBUG_MODE          デバッグモード（true/false）

対応OS: macOS, Linux, Windows/WSL

詳細なドキュメント: $CLAUDE_VOICE_HOME/WSL-INTEGRATION-GUIDE.md
EOF
}

# バージョン情報とシステム状態の表示
show_version() {
    echo "Claude Voice ${CLAUDE_VOICE_VERSION:-"未定義"}"
    echo "OS: $(detect_os 2>/dev/null || echo "未検出")"
    echo "Home: ${CLAUDE_VOICE_HOME:-"未設定"}"
    echo ""

    echo "Core modules:"
    check_module_status "base" "screen_capture" "llm_manager" "summary_engine" "voice_engine_registry" "universal_voice"

    echo ""
    echo "OS-specific module:"
    check_os_module_status

    echo ""
    echo "Voice engines:"
    check_voice_engine_status

    echo ""
    echo "Configuration:"
    check_configuration_status
}

# コアモジュールの状態確認
check_module_status() {
    local modules=("$@")
    for module in "${modules[@]}"; do
        local module_path="$CLAUDE_VOICE_HOME/core/${module}.sh"
        if [[ -f "$module_path" ]]; then
            echo "  ✅ $module"
        else
            echo "  ❌ $module (missing)"
        fi
    done
}

# OS固有モジュールの状態確認
check_os_module_status() {
    local os_type=$(detect_os 2>/dev/null || echo "unknown")
    local os_module="$CLAUDE_VOICE_HOME/os/${os_type}.sh"

    if [[ -f "$os_module" ]]; then
        echo "  ✅ $os_type"
    else
        echo "  ❌ $os_type (missing)"
    fi
}

# 音声エンジンの状態確認
check_voice_engine_status() {
    # voice_engine_registryが利用可能な場合は詳細チェック
    if [[ -f "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" 2>/dev/null; then
            local engines=("wsl_powershell" "osascript" "espeak" "festival" "simple_fallback")
            for engine in "${engines[@]}"; do
                if is_engine_available "$engine" 2>/dev/null; then
                    echo "  ✅ $engine"
                else
                    echo "  ❌ $engine"
                fi
            done
        else
            echo "  ❌ Voice engine registry not accessible"
        fi
    else
        echo "  ❌ Voice engine registry not found"
    fi
}

# 設定ファイルの状態確認
check_configuration_status() {
    local config_files=(
        "config/claude-voice.yaml"
        "config/integration.conf"
        ".config_cache"
    )

    for config_file in "${config_files[@]}"; do
        local config_path="$CLAUDE_VOICE_HOME/$config_file"
        if [[ -f "$config_path" ]]; then
            echo "  ✅ $config_file"
        else
            echo "  ❌ $config_file (missing)"
        fi
    done
}

# システム情報の詳細表示
show_system_info() {
    echo "=== System Information ==="
    echo "OS: $(uname -s) $(uname -r)"
    echo "Architecture: $(uname -m)"
    echo "User: $(whoami)"
    echo "PWD: $(pwd)"
    echo "Shell: $SHELL"

    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        echo "Environment: WSL"
        if [[ -n "${WSL_DISTRO_NAME:-}" ]]; then
            echo "WSL Distribution: $WSL_DISTRO_NAME"
        fi
    fi

    echo ""
    echo "Claude Voice Environment:"
    echo "CLAUDE_VOICE_HOME: ${CLAUDE_VOICE_HOME:-"未設定"}"
    echo "DEBUG_MODE: ${DEBUG_MODE:-"false"}"
    echo "PATH contains claude-voice: $(command -v claude-voice >/dev/null 2>&1 && echo "Yes" || echo "No")"
}

# 簡潔なシステム情報
show_brief_info() {
    local os_type=$(detect_os 2>/dev/null || echo "unknown")
    local voice_engines_count=0

    # 利用可能な音声エンジン数をカウント
    if [[ -f "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" 2>/dev/null; then
            local engines=("wsl_powershell" "osascript" "espeak" "festival" "simple_fallback")
            for engine in "${engines[@]}"; do
                if is_engine_available "$engine" 2>/dev/null; then
                    ((voice_engines_count++))
                fi
            done
        fi
    fi

    echo "Claude Voice ${CLAUDE_VOICE_VERSION:-"未定義"} | OS: $os_type | Voice Engines: $voice_engines_count/5 available"
}

# インタラクティブヘルプ（コマンド例付き）
show_interactive_help() {
    echo "Claude Voice - インタラクティブヘルプ"
    echo "======================================"
    echo ""

    echo "🚀 クイックスタート:"
    echo "  claude-voice                    # 基本実行"
    echo "  claude-voice brief 30           # 30行で簡潔要約"
    echo "  claude-voice --test             # システムテスト"
    echo ""

    echo "🔧 診断・修復:"
    echo "  claude-voice --health           # ヘルスチェック"
    echo "  claude-voice --repair           # 設定修復"
    echo "  claude-voice --stats            # 使用統計"
    echo ""

    echo "⚙️ 設定管理:"
    echo "  claude-voice --config           # 設定表示・編集"
    echo "  claude-voice --version          # システム状態確認"
    echo ""

    echo "❓ 困ったときは:"
    echo "  1. --health でシステム状態を確認"
    echo "  2. --repair で設定を修復"
    echo "  3. --test でテスト実行"
    echo "  4. デバッグが必要な場合: DEBUG_MODE=true claude-voice"
    echo ""

    echo "📁 重要なファイル:"
    echo "  設定: $CLAUDE_VOICE_HOME/config/"
    echo "  ログ: $CLAUDE_VOICE_HOME/logs/"
    echo "  ドキュメント: $CLAUDE_VOICE_HOME/WSL-INTEGRATION-GUIDE.md"
}

# エラー時のトラブルシューティングガイド
show_troubleshooting_guide() {
    cat <<'EOF'
Claude Voice - トラブルシューティングガイド
==========================================

🔍 よくある問題と解決方法:

1. 音声が出力されない
   → claude-voice --health で音声エンジンの状態確認
   → WSLの場合: PowerShellの利用可能性を確認

2. LLMへの接続エラー
   → Ollamaの起動状態を確認: ollama list
   → ネットワーク設定とモデルの存在確認

3. 設定ファイルエラー
   → claude-voice --repair で自動修復
   → 手動確認: ~/.tmux/claude/config/

4. 権限エラー
   → ファイル権限の確認: ls -la ~/.tmux/claude/
   → 実行権限の確認: chmod +x ~/.tmux/claude/bin/claude-voice

5. 依存関係の問題
   → 必要なコマンドの確認: tmux, curl, PowerShell (WSL)
   → パッケージマネージャーでの追加インストール

🔧 診断コマンド:
  claude-voice --health     # 包括的チェック
  claude-voice --test       # 統合テスト
  claude-voice --version    # システム状態

📞 サポート:
  GitHub Issues: https://github.com/anthropics/claude-code/issues
  ドキュメント: ~/.tmux/claude/WSL-INTEGRATION-GUIDE.md
EOF
}

# このモジュールが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # テスト用の環境変数設定
    CLAUDE_VOICE_VERSION="1.0.0"
    CLAUDE_VOICE_HOME="${HOME}/.tmux/claude"
    DEFAULT_LINES=50

    # detect_os関数の簡易版
    detect_os() {
        local os_type=$(uname)
        case "$os_type" in
            "Darwin") echo "darwin" ;;
            "Linux")
                if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
                    echo "windows"
                else
                    echo "linux"
                fi
                ;;
            *) echo "unknown" ;;
        esac
    }

    echo "User Interface Module Test"
    echo "=========================="
    echo ""

    case "${1:-version}" in
        "usage")
            show_usage
            ;;
        "version")
            show_version
            ;;
        "system")
            show_system_info
            ;;
        "brief")
            show_brief_info
            ;;
        "interactive")
            show_interactive_help
            ;;
        "troubleshooting")
            show_troubleshooting_guide
            ;;
        *)
            echo "Available tests: usage, version, system, brief, interactive, troubleshooting"
            ;;
    esac
fi
