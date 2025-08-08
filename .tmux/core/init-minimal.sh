#!/bin/bash
# TMux Minimal Initialization - Phase 2 Performance Optimized
# 最小限初期化スクリプト - パフォーマンス特化版

set -euo pipefail

readonly INIT_VERSION="2.0.0"
readonly INIT_START_TIME=$(date +%s%3N)

# === 環境変数設定 ===
export TMUX_MINIMAL_INIT="${TMUX_MINIMAL_INIT:-true}"
export TMUX_INIT_DEBUG="${TMUX_INIT_DEBUG:-false}"

# === パス設定 ===
readonly SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly CONFIG_FILE="$HOME/.tmux/tmux-minimal.yaml"
readonly LOG_FILE="$HOME/.tmux/claude/logs/global.log"

# === 高速ログ関数 ===
log_init() {
    if [[ "${TMUX_INIT_DEBUG:-false}" == "true" ]]; then
        echo "[$(date '+%H:%M:%S')] [INIT] $*" >&2
    fi
}

# === 最小限設定読み込み ===
load_minimal_config() {
    # YAML設定の最小限読み込み（Python依存なし）
    if [[ -f "$CONFIG_FILE" ]]; then
        # 音声モード設定のみ抽出
        local audio_mode=$(grep "audio_mode:" "$CONFIG_FILE" | cut -d'"' -f2 2>/dev/null || echo "auto")
        export TMUX_AUDIO_MODE="$audio_mode"
        log_init "Audio mode loaded: $audio_mode"
    else
        export TMUX_AUDIO_MODE="auto"
        log_init "Default audio mode: auto"
    fi
}

# === 必要最小限のサービス初期化 ===
initialize_core_services() {
    log_init "Initializing core services"
    
    # グローバルログディレクトリ作成
    mkdir -p "$(dirname "$LOG_FILE")" 2>/dev/null || true
    
    # 初期化ログ
    echo "$(date '+%Y-%m-%d %H:%M:%S') - TMux minimal initialization started" >> "$LOG_FILE" 2>/dev/null || true
    
    # オーディオモード設定初期化（高速版）
    if [[ -x "$SCRIPT_DIR/audio-mode-switcher.sh" ]]; then
        "$SCRIPT_DIR/audio-mode-switcher.sh" get >/dev/null 2>&1 || true
        log_init "Audio mode switcher initialized"
    fi
}

# === プラットフォーム検出（キャッシュ利用） ===
detect_platform_cached() {
    local cache_file="/tmp/.tmux_platform_$$"
    
    # キャッシュチェック（60秒有効）
    if [[ -f "$cache_file" ]]; then
        local file_age=$(( $(date +%s) - $(stat -c %Y "$cache_file" 2>/dev/null || echo 0) ))
        if [[ $file_age -lt 60 ]]; then
            cat "$cache_file"
            return 0
        fi
    fi
    
    # 高速プラットフォーム検出
    local platform
    case "$(uname)" in
        "Darwin") platform="macos" ;;
        "Linux")
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                platform="wsl"
            else
                platform="linux"
            fi
            ;;
        *) platform="unknown" ;;
    esac
    
    # キャッシュ保存
    echo "$platform" > "$cache_file"
    echo "$platform"
}

# === 条件付きサービス初期化 ===
initialize_conditional_services() {
    local platform=$(detect_platform_cached)
    log_init "Platform detected: $platform"
    
    # プラットフォーム固有の最小限初期化
    case "$platform" in
        "wsl")
            # WSL環境での最小限音声確認
            if command -v powershell.exe >/dev/null 2>&1; then
                export TMUX_VOICE_AVAILABLE="true"
            else
                export TMUX_VOICE_AVAILABLE="false"
            fi
            ;;
        "macos")
            # macOS環境での最小限音声確認
            if command -v say >/dev/null 2>&1; then
                export TMUX_VOICE_AVAILABLE="true"
            else
                export TMUX_VOICE_AVAILABLE="false"
            fi
            ;;
        "linux")
            # Linux環境での最小限音声確認
            if command -v espeak >/dev/null 2>&1 || command -v paplay >/dev/null 2>&1; then
                export TMUX_VOICE_AVAILABLE="true"
            else
                export TMUX_VOICE_AVAILABLE="false"
            fi
            ;;
        *)
            export TMUX_VOICE_AVAILABLE="false"
            ;;
    esac
    
    log_init "Voice availability: $TMUX_VOICE_AVAILABLE"
}

# === パフォーマンス測定 ===
measure_performance() {
    local end_time=$(date +%s%3N)
    local duration=$((end_time - INIT_START_TIME))
    
    echo "$(date '+%Y-%m-%d %H:%M:%S') - TMux minimal initialization completed in ${duration}ms" >> "$LOG_FILE" 2>/dev/null || true
    
    if [[ "${TMUX_INIT_DEBUG:-false}" == "true" ]]; then
        echo "TMux Minimal Init Performance: ${duration}ms" >&2
    fi
    
    # パフォーマンス目標チェック
    if [[ $duration -lt 100 ]]; then
        log_init "Performance target achieved: ${duration}ms < 100ms"
    else
        log_init "Performance target missed: ${duration}ms >= 100ms"
    fi
}

# === エラーハンドリング ===
handle_error() {
    local error_msg="$1"
    echo "$(date '+%Y-%m-%d %H:%M:%S') - ERROR: $error_msg" >> "$LOG_FILE" 2>/dev/null || true
    log_init "ERROR: $error_msg"
}

# === メイン初期化処理 ===
main() {
    local operation="${1:-init}"
    
    case "$operation" in
        "init")
            log_init "Starting minimal initialization"
            
            # 段階的初期化（エラー時も継続）
            load_minimal_config || handle_error "Config loading failed"
            initialize_core_services || handle_error "Core services initialization failed"
            initialize_conditional_services || handle_error "Conditional services initialization failed"
            
            measure_performance
            log_init "Minimal initialization completed"
            ;;
        "test")
            echo "=== TMux Minimal Initialization Test ==="
            TMUX_INIT_DEBUG=true main init
            echo "Test completed"
            ;;
        "benchmark")
            echo "=== Performance Benchmark ==="
            for i in {1..5}; do
                echo -n "Run $i: "
                start_time=$(date +%s%3N)
                main init >/dev/null 2>&1
                end_time=$(date +%s%3N)
                echo "$((end_time - start_time))ms"
            done
            ;;
        "status")
            echo "TMux Minimal Init Status:"
            echo "  Audio Mode: ${TMUX_AUDIO_MODE:-unset}"
            echo "  Voice Available: ${TMUX_VOICE_AVAILABLE:-unset}"
            echo "  Platform: $(detect_platform_cached)"
            ;;
        *)
            echo "Usage: $0 {init|test|benchmark|status}"
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi