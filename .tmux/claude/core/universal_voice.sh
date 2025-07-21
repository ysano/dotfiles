#!/bin/bash
# Universal Voice System - クロスプラットフォーム音声出力統合
# WSL, macOS, Linux対応の統一音声インターフェース

# === 音声エンジンレジストリの初期化 ===
init_universal_voice() {
    # レジストリが初期化されていない場合は初期化
    if [[ ${#VOICE_ENGINES[@]} -eq 0 ]]; then
        source "$(dirname "${BASH_SOURCE[0]}")/voice_engine_registry.sh"
        init_voice_engine_registry
    fi
}

# === 音声エンジンの検出と選択 ===
detect_voice_engine() {
    init_universal_voice
    select_best_engine
}

# === 統合音声出力関数 ===
universal_speak() {
    local text="$1"
    local voice_setting="${2:-auto}"
    local engine="${3:-$(detect_voice_engine)}"

    log "DEBUG" "Universal speak: engine=$engine, voice=$voice_setting"

    init_universal_voice
    execute_voice_engine "$engine" "$text" "$voice_setting"
}

# === 非同期音声出力 ===
universal_speak_async() {
    local text="$1"
    local voice_setting="${2:-auto}"
    local timeout="${3:-30}"
    local max_concurrent="${4:-1}"

    log "DEBUG" "Starting async universal speech synthesis"

    # 並行音声出力の制限
    limit_concurrent_voices "$max_concurrent"

    # バックグラウンドで音声出力
    (
        timeout "$timeout" universal_speak "$text" "$voice_setting" 2>/dev/null
        local exit_code=$?
        if [[ $exit_code -eq 124 ]]; then
            log "WARN" "Universal speech synthesis timed out after ${timeout}s"
        elif [[ $exit_code -ne 0 ]]; then
            log "WARN" "Universal speech synthesis failed with exit code $exit_code"
        fi
    ) &

    local bg_pid=$!
    log "DEBUG" "Universal speech synthesis started in background (PID: $bg_pid)"

    return 0
}

# === 並行音声プロセス制限 ===
limit_concurrent_voices() {
    local max_processes="${1:-1}"

    # 様々な音声プロセスの検出パターン
    local voice_patterns=(
        "powershell.*Speech"
        "osascript.*say"
        "espeak"
        "festival.*tts"
    )

    local total_count=0
    local pids_to_kill=()

    # 各パターンでプロセスを検索
    for pattern in "${voice_patterns[@]}"; do
        local pids=($(pgrep -f "$pattern" 2>/dev/null))
        total_count=$((total_count + ${#pids[@]}))

        # 制限を超えた場合のPIDを収集
        if [[ $total_count -gt $max_processes ]]; then
            local excess=$((total_count - max_processes))
            for ((i = 0; i < excess && i < ${#pids[@]}; i++)); do
                pids_to_kill+=("${pids[i]}")
            done
        fi
    done

    # 余分なプロセスを終了
    for pid in "${pids_to_kill[@]}"; do
        kill "$pid" 2>/dev/null
        log "DEBUG" "Terminated voice process: $pid"
    done

    if [[ ${#pids_to_kill[@]} -gt 0 ]]; then
        sleep 0.5
    fi
}

# === 音声システム診断 ===
diagnose_universal_voice() {
    echo "=== Universal Voice System Diagnostics ==="
    echo ""

    # 環境検出
    local os_type=$(uname)
    echo "Operating System: $os_type"

    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        echo "Environment: WSL"
    fi

    echo ""
    init_universal_voice
    diagnose_engine_registry
}

# === テスト関数 ===
test_universal_voice() {
    echo "Testing Universal Voice System..."
    echo ""

    # 診断実行
    diagnose_universal_voice
    echo ""

    # 音声テスト
    local engine=$(detect_voice_engine)
    echo "Testing with engine: $engine"
    echo ""

    echo "Testing Japanese speech..."
    if universal_speak "ユニバーサル音声システムのテストです。日本語の読み上げが動作しています。"; then
        echo "✅ Japanese speech test: PASSED"
    else
        echo "❌ Japanese speech test: FAILED"
    fi

    echo ""
    echo "Testing English speech..."
    if universal_speak "This is a test of the universal voice system. Cross-platform speech synthesis is working."; then
        echo "✅ English speech test: PASSED"
    else
        echo "❌ English speech test: FAILED"
    fi

    echo ""
    echo "Testing async speech..."
    universal_speak_async "非同期音声テストです。バックグラウンドで再生されています。"
    echo "✅ Async speech test: INITIATED"

    echo ""
    echo "Universal Voice System test completed"
}

# === このスクリプトが直接実行された場合 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # 基本モジュールの読み込み
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

    if [[ -f "$SCRIPT_DIR/base.sh" ]]; then
        source "$SCRIPT_DIR/base.sh"
        claude_voice_init true
    else
        # スタンドアロン実行時の簡易ログ関数
        log() {
            local level="$1"
            local message="$2"
            echo "[$level] $message" >&2
        }
    fi

    # コマンドライン引数の処理
    case "${1:-test}" in
        "test")
            test_universal_voice
            ;;
        "diagnose")
            diagnose_universal_voice
            ;;
        "speak")
            if [[ -n "$2" ]]; then
                universal_speak "$2" "${3:-auto}"
            else
                echo "Usage: $0 speak <text> [voice]"
                exit 1
            fi
            ;;
        "async")
            if [[ -n "$2" ]]; then
                universal_speak_async "$2" "${3:-auto}"
                echo "Async speech initiated"
            else
                echo "Usage: $0 async <text> [voice]"
                exit 1
            fi
            ;;
        *)
            echo "Usage: $0 {test|diagnose|speak|async}"
            echo "  test     - Run comprehensive tests"
            echo "  diagnose - Show system diagnostics"
            echo "  speak    - Test speech synthesis"
            echo "  async    - Test async speech synthesis"
            exit 1
            ;;
    esac
fi
