#!/bin/bash
# Voice Unified Engine - Phase 2 Cross-Platform Integration
# 統一音声エンジン - クロスプラットフォーム対応

set -euo pipefail

readonly VOICE_SCRIPT_VERSION="2.0.0"
readonly VOICE_SCRIPT_NAME="Voice Unified Engine"

# === 環境変数 ===
export VOICE_UNIFIED_DEBUG="${VOICE_UNIFIED_DEBUG:-false}"

# === 依存スクリプト読み込み ===
readonly SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly OLLAMA_CROSS_SCRIPT="$SCRIPT_DIR/ollama-cross.sh"

if [[ -f "$OLLAMA_CROSS_SCRIPT" ]]; then
    source "$OLLAMA_CROSS_SCRIPT"
else
    echo "ERROR: Ollama cross-platform script not found: $OLLAMA_CROSS_SCRIPT" >&2
    exit 1
fi

# === ログ関数 ===
log_voice() {
    local level="$1"
    shift
    if [[ "${VOICE_UNIFIED_DEBUG:-false}" == "true" ]] || [[ "$level" == "ERROR" ]]; then
        echo "[$(date '+%H:%M:%S')] [VOICE-UNIFIED] [$level] $*" >&2
    fi
}

# === プラットフォーム別音声エンジン ===
get_voice_engine() {
    local platform=$(detect_platform)
    
    case "$platform" in
        "macos")
            echo "say"
            ;;
        "linux")
            if command -v espeak >/dev/null 2>&1; then
                echo "espeak"
            elif command -v festival >/dev/null 2>&1; then
                echo "festival"
            else
                echo "none"
            fi
            ;;
        "wsl")
            # WSL環境ではWindowsの音声合成を使用
            echo "powershell"
            ;;
        *)
            echo "none"
            ;;
    esac
}

# === 音声合成実行 ===
synthesize_speech() {
    local text="$1"
    local status_icon="${2:-✅}"
    local window_id="${3:-1}"
    local voice_engine="${4:-auto}"
    
    log_voice "DEBUG" "Speech synthesis: text='${text:0:30}...', status=$status_icon, window=$window_id"
    
    # 音声エンジンの自動選択
    if [[ "$voice_engine" == "auto" ]]; then
        voice_engine=$(get_voice_engine)
    fi
    
    case "$voice_engine" in
        "say")
            # macOS音声合成
            echo "$text" | say -v "Kyoko" -r 180 &
            log_voice "INFO" "macOS speech synthesis executed"
            ;;
        "espeak")
            # Linux espeak
            echo "$text" | espeak -v ja -s 150 2>/dev/null &
            log_voice "INFO" "Linux espeak synthesis executed"
            ;;
        "festival")
            # Linux festival
            echo "$text" | festival --tts 2>/dev/null &
            log_voice "INFO" "Linux festival synthesis executed"
            ;;
        "powershell")
            # WSL PowerShell音声合成
            local platform_engine="/home/user/dotfiles/.tmux/claude/platforms/wsl/wsl_voice_engine.sh"
            if [[ -x "$platform_engine" ]]; then
                "$platform_engine" speak "$text" "auto" "0" "80" "$window_id" "$status_icon"
                log_voice "INFO" "WSL PowerShell synthesis executed"
            else
                log_voice "ERROR" "WSL voice engine not found"
                return 1
            fi
            ;;
        "none")
            log_voice "WARN" "No voice engine available, skipping speech synthesis"
            return 1
            ;;
        *)
            log_voice "ERROR" "Unknown voice engine: $voice_engine"
            return 1
            ;;
    esac
}

# === ステータス効果音再生 ===
play_status_sound() {
    local status_icon="$1"
    local platform=$(detect_platform)
    
    log_voice "DEBUG" "Playing status sound: $status_icon on $platform"
    
    case "$platform" in
        "macos")
            case "$status_icon" in
                "⚡") afplay "/System/Library/Sounds/Funk.aiff" 2>/dev/null & ;;
                "⌛") afplay "/System/Library/Sounds/Tink.aiff" 2>/dev/null & ;;
                "✅") afplay "/System/Library/Sounds/Glass.aiff" 2>/dev/null & ;;
            esac
            ;;
        "linux")
            # Linuxでのシステム音再生
            case "$status_icon" in
                "⚡") paplay /usr/share/sounds/alsa/Front_Left.wav 2>/dev/null & ;;
                "⌛") paplay /usr/share/sounds/alsa/Rear_Left.wav 2>/dev/null & ;;
                "✅") paplay /usr/share/sounds/alsa/Front_Right.wav 2>/dev/null & ;;
            esac
            ;;
        "wsl")
            # WSL環境ではWindows効果音を使用
            local wsl_engine="/home/user/dotfiles/.tmux/claude/platforms/wsl/wsl_voice_engine.sh"
            if [[ -x "$wsl_engine" ]]; then
                "$wsl_engine" sound "$status_icon"
            else
                log_voice "ERROR" "WSL voice engine not found for sound"
                return 1
            fi
            ;;
    esac
    
    log_voice "INFO" "Status sound played: $status_icon"
}

# === AI要約生成 (Ollama統合) ===
generate_ai_summary() {
    local input_text="$1"
    local summary_type="${2:-brief}"
    local max_length="${3:-100}"
    
    log_voice "DEBUG" "Generating AI summary: type=$summary_type, length=$max_length"
    
    # Ollama健康状態チェック
    if ! check_ollama_health >/dev/null 2>&1; then
        log_voice "WARN" "Ollama not available, skipping AI summary"
        return 1
    fi
    
    # 最適なモデルを選択
    local model
    if ! model=$(select_best_model 2>/dev/null); then
        log_voice "ERROR" "No suitable Ollama model available"
        return 1
    fi
    
    # プロンプト生成
    local prompt
    case "$summary_type" in
        "brief")
            prompt="次のテキストを日本語で簡潔に要約してください（最大${max_length}文字）: $input_text"
            ;;
        "detailed")
            prompt="次のテキストを日本語で詳細に分析・要約してください: $input_text"
            ;;
        "technical")
            prompt="次の技術的な内容を専門用語を含めて日本語で要約してください: $input_text"
            ;;
        *)
            prompt="次のテキストを日本語で要約してください: $input_text"
            ;;
    esac
    
    # Ollama実行
    local summary
    if summary=$(execute_ollama_request "$model" "$prompt" "" 30 2>/dev/null); then
        echo "$summary"
        log_voice "INFO" "AI summary generated successfully with model: $model"
        return 0
    else
        log_voice "ERROR" "AI summary generation failed"
        return 1
    fi
}

# === 統合通知機能 ===
unified_notify() {
    local text="$1"
    local status_icon="${2:-✅}"
    local window_id="${3:-1}"
    local mode="${4:-both}"  # sound, speech, ai, both, full
    
    log_voice "DEBUG" "Unified notification: text='${text:0:30}...', status=$status_icon, mode=$mode"
    
    case "$mode" in
        "sound")
            play_status_sound "$status_icon"
            ;;
        "speech")
            synthesize_speech "$text" "$status_icon" "$window_id"
            ;;
        "ai")
            local summary
            if summary=$(generate_ai_summary "$text" "brief" 50); then
                synthesize_speech "$summary" "$status_icon" "$window_id"
            else
                synthesize_speech "$text" "$status_icon" "$window_id"
            fi
            ;;
        "both")
            play_status_sound "$status_icon" &
            sleep 0.3
            synthesize_speech "$text" "$status_icon" "$window_id"
            wait
            ;;
        "full")
            play_status_sound "$status_icon" &
            sleep 0.3
            local summary
            if summary=$(generate_ai_summary "$text" "brief" 50); then
                synthesize_speech "$summary" "$status_icon" "$window_id"
            else
                synthesize_speech "$text" "$status_icon" "$window_id"
            fi
            wait
            ;;
        *)
            log_voice "ERROR" "Unknown notification mode: $mode"
            return 1
            ;;
    esac
}

# === 診断機能 ===
diagnose_voice() {
    echo "=== $VOICE_SCRIPT_NAME Diagnostics ==="
    echo "Version: $VOICE_SCRIPT_VERSION"
    echo
    
    local platform=$(detect_platform)
    echo "Platform: $platform"
    
    local voice_engine=$(get_voice_engine)
    echo "Voice Engine: $voice_engine"
    
    echo
    echo "Ollama Integration:"
    if check_ollama_health >/dev/null 2>&1; then
        echo "  ✅ Ollama connection: OK"
        local model
        if model=$(select_best_model 2>/dev/null); then
            echo "  ✅ Best model: $model"
        else
            echo "  ❌ No suitable model found"
        fi
    else
        echo "  ❌ Ollama connection: Failed"
    fi
    
    echo
    echo "Voice System Status:"
    case "$voice_engine" in
        "say"|"espeak"|"festival"|"powershell")
            echo "  ✅ Voice synthesis: Available ($voice_engine)"
            ;;
        "none")
            echo "  ❌ Voice synthesis: Not available"
            ;;
    esac
    
    echo
    echo "=== End Diagnostics ==="
}

# === テスト機能 ===
test_voice_unified() {
    echo "=== $VOICE_SCRIPT_NAME Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: プラットフォーム検出
    echo "Test 1: Platform detection"
    ((tests_total++))
    local platform=$(detect_platform)
    if [[ -n "$platform" && "$platform" != "unknown" ]]; then
        echo "✅ PASS - Platform: $platform"
        ((tests_passed++))
    else
        echo "❌ FAIL - Platform detection"
    fi
    
    # Test 2: 音声エンジン検出
    echo "Test 2: Voice engine detection"
    ((tests_total++))
    local voice_engine=$(get_voice_engine)
    if [[ -n "$voice_engine" ]]; then
        echo "✅ PASS - Voice engine: $voice_engine"
        ((tests_passed++))
    else
        echo "❌ FAIL - Voice engine detection"
    fi
    
    # Test 3: 効果音テスト
    echo "Test 3: Status sound test"
    ((tests_total++))
    if play_status_sound "✅" >/dev/null 2>&1; then
        echo "✅ PASS - Status sound"
        ((tests_passed++))
    else
        echo "❌ FAIL - Status sound"
    fi
    
    # Test 4: 音声合成テスト (音声エンジンがある場合のみ)
    if [[ "$voice_engine" != "none" ]]; then
        echo "Test 4: Speech synthesis test"
        ((tests_total++))
        if synthesize_speech "統一音声エンジンのテストです" "✅" 1 >/dev/null 2>&1; then
            echo "✅ PASS - Speech synthesis"
            ((tests_passed++))
        else
            echo "❌ FAIL - Speech synthesis"
        fi
    fi
    
    # Test 5: AI要約テスト (Ollamaが利用可能な場合のみ)
    if check_ollama_health >/dev/null 2>&1; then
        echo "Test 5: AI summary generation"
        ((tests_total++))
        if generate_ai_summary "This is a test message for summarization" "brief" 50 >/dev/null 2>&1; then
            echo "✅ PASS - AI summary"
            ((tests_passed++))
        else
            echo "❌ FAIL - AI summary"
        fi
    fi
    
    echo
    echo "Test Results: $tests_passed/$tests_total passed"
    
    if [[ $tests_passed -eq $tests_total ]]; then
        echo "🎉 All tests passed!"
        return 0
    else
        echo "⚠️ Some tests failed"
        return 1
    fi
}

# === メイン処理 ===
main() {
    case "${1:-help}" in
        "speak")
            shift
            synthesize_speech "$@"
            ;;
        "sound")
            shift
            play_status_sound "$@"
            ;;
        "summary")
            shift
            generate_ai_summary "$@"
            ;;
        "notify")
            shift
            unified_notify "$@"
            ;;
        "engine")
            get_voice_engine
            ;;
        "diagnose")
            diagnose_voice
            ;;
        "test")
            test_voice_unified
            ;;
        "help"|"-h"|"--help")
            cat << EOF
$VOICE_SCRIPT_NAME - Usage Guide

COMMANDS:
  speak <text> [status] [window] [engine]     - 音声合成実行
  sound <status>                              - ステータス音再生
  summary <text> [type] [length]              - AI要約生成
  notify <text> [status] [window] [mode]      - 統合通知
  engine                                      - 利用可能音声エンジン表示
  diagnose                                    - システム診断
  test                                        - テスト実行
  help                                        - このヘルプ

EXAMPLES:
  $0 speak "こんにちは" "✅" 1
  $0 sound "⚡"
  $0 summary "長いテキスト" "brief" 100
  $0 notify "完了しました" "✅" 1 "full"
  $0 diagnose
  $0 test

STATUS ICONS:
  ⚡ - Busy/Alert
  ⌛ - Waiting/Processing  
  ✅ - Complete/Success

NOTIFICATION MODES:
  sound  - 効果音のみ
  speech - 音声合成のみ
  ai     - AI要約+音声合成
  both   - 効果音+音声合成
  full   - 効果音+AI要約+音声合成

EOF
            ;;
        *)
            echo "Unknown command: $1" >&2
            main "help"
            exit 1
            ;;
    esac
}

# === スクリプト実行 ===
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi