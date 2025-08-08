#!/bin/bash
# Audio Mode Switcher - TMux Prefix C-n Integration
# 音声・通知モード切り替えシステム

set -euo pipefail

readonly SWITCHER_VERSION="2.0.0"
readonly SWITCHER_NAME="Audio Mode Switcher"

# === 環境変数 ===
export AUDIO_MODE_SWITCHER_DEBUG="${AUDIO_MODE_SWITCHER_DEBUG:-false}"

# === 設定ファイル ===
readonly CONFIG_DIR="$HOME/.tmux/claude/config"
readonly MODE_CONFIG_FILE="$CONFIG_DIR/audio_mode.conf"

# === 依存スクリプト ===
readonly SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
readonly AUDIO_FALLBACK_SCRIPT="$SCRIPT_DIR/audio-fallback.sh"

# === ログ関数 ===
log_switcher() {
    local level="$1"
    shift
    if [[ "${AUDIO_MODE_SWITCHER_DEBUG:-false}" == "true" ]] || [[ "$level" == "ERROR" ]]; then
        echo "[$(date '+%H:%M:%S')] [AUDIO-SWITCHER] [$level] $*" >&2
    fi
}

# === 設定ディレクトリ初期化 ===
initialize_config() {
    mkdir -p "$CONFIG_DIR"
    
    # デフォルト設定ファイル作成
    if [[ ! -f "$MODE_CONFIG_FILE" ]]; then
        cat > "$MODE_CONFIG_FILE" << EOF
# Audio Mode Configuration
# Current mode: auto, audio, notification, beep, silent
CURRENT_MODE=auto
LAST_UPDATED=$(date '+%Y-%m-%d %H:%M:%S')
EOF
        log_switcher "INFO" "Default audio mode config created"
    fi
}

# === 現在のモード取得 ===
get_current_mode() {
    initialize_config
    
    if [[ -f "$MODE_CONFIG_FILE" ]]; then
        grep "^CURRENT_MODE=" "$MODE_CONFIG_FILE" | cut -d'=' -f2 2>/dev/null || echo "auto"
    else
        echo "auto"
    fi
}

# === モード設定 ===
set_audio_mode() {
    local new_mode="$1"
    
    # モード検証
    case "$new_mode" in
        "auto"|"audio"|"notification"|"beep"|"silent")
            ;;
        *)
            log_switcher "ERROR" "Invalid mode: $new_mode"
            return 1
            ;;
    esac
    
    initialize_config
    
    # 設定ファイル更新
    cat > "$MODE_CONFIG_FILE" << EOF
# Audio Mode Configuration
# Current mode: auto, audio, notification, beep, silent
CURRENT_MODE=$new_mode
LAST_UPDATED=$(date '+%Y-%m-%d %H:%M:%S')
EOF
    
    log_switcher "INFO" "Audio mode set to: $new_mode"
    return 0
}

# === モード切り替え（循環） ===
cycle_audio_mode() {
    local current_mode=$(get_current_mode)
    local next_mode
    
    case "$current_mode" in
        "auto")
            next_mode="audio"
            ;;
        "audio")
            next_mode="notification"
            ;;
        "notification")
            next_mode="beep"
            ;;
        "beep")
            next_mode="silent"
            ;;
        "silent")
            next_mode="auto"
            ;;
        *)
            next_mode="auto"
            ;;
    esac
    
    set_audio_mode "$next_mode"
    echo "$next_mode"
}

# === モード表示 ===
display_current_mode() {
    local current_mode=$(get_current_mode)
    local mode_description
    
    case "$current_mode" in
        "auto")
            mode_description="自動選択 (音声→通知→フォールバック)"
            ;;
        "audio")
            mode_description="音声デバイスのみ"
            ;;
        "notification")
            mode_description="システム通知のみ"
            ;;
        "beep")
            mode_description="ビープ音のみ"
            ;;
        "silent")
            mode_description="無音"
            ;;
        *)
            mode_description="不明なモード"
            ;;
    esac
    
    echo "🔊 Audio Mode: $current_mode ($mode_description)"
}

# === モード付きステータス音再生 ===
play_with_current_mode() {
    local status_icon="$1"
    local message="${2:-Mode switched}"
    local current_mode=$(get_current_mode)
    
    log_switcher "DEBUG" "Playing with mode: $current_mode"
    
    if [[ -x "$AUDIO_FALLBACK_SCRIPT" ]]; then
        "$AUDIO_FALLBACK_SCRIPT" play "$status_icon" "$current_mode" "$message"
    else
        log_switcher "ERROR" "Audio fallback script not found: $AUDIO_FALLBACK_SCRIPT"
        # フォールバック: 基本ビープ
        printf '\a'
        return 1
    fi
}

# === TMux統合: モード切り替え + 通知 ===
tmux_cycle_mode() {
    local new_mode=$(cycle_audio_mode)
    local mode_description
    
    case "$new_mode" in
        "auto") mode_description="自動選択" ;;
        "audio") mode_description="音声デバイス" ;;
        "notification") mode_description="システム通知" ;;
        "beep") mode_description="ビープ音" ;;
        "silent") mode_description="無音" ;;
        *) mode_description="不明" ;;
    esac
    
    # モード切り替え通知
    play_with_current_mode "🔊" "Audio mode: $mode_description"
    
    # TMuxステータスライン更新 (オプション)
    if command -v tmux >/dev/null 2>&1 && [[ -n "${TMUX:-}" ]]; then
        tmux display-message "🔊 Audio Mode: $mode_description"
    fi
    
    echo "$new_mode"
}

# === TMuxキーバインド生成 ===
generate_tmux_keybinding() {
    local script_path="$0"
    cat << EOF
# Audio Mode Switcher - TMux Key Binding
# Add this to your tmux.conf:

# Prefix + C-n: Audio mode cycle
bind-key C-n run-shell "$script_path tmux-cycle"

# Optional: Prefix + n (without Ctrl)
bind-key n run-shell "$script_path tmux-cycle"

# Display current mode
bind-key M-n run-shell "$script_path display && $script_path tmux-cycle"

EOF
}

# === 診断機能 ===
diagnose_switcher() {
    echo "=== $SWITCHER_NAME Diagnostics ==="
    echo "Version: $SWITCHER_VERSION"
    echo
    
    echo "Configuration:"
    echo "  Config file: $MODE_CONFIG_FILE"
    if [[ -f "$MODE_CONFIG_FILE" ]]; then
        echo "  ✅ Config exists"
        echo "  Current mode: $(get_current_mode)"
        echo "  Last updated: $(grep "LAST_UPDATED=" "$MODE_CONFIG_FILE" | cut -d'=' -f2 2>/dev/null || echo "Unknown")"
    else
        echo "  ❌ Config not found"
    fi
    
    echo
    echo "Dependencies:"
    if [[ -x "$AUDIO_FALLBACK_SCRIPT" ]]; then
        echo "  ✅ Audio fallback script: Available"
    else
        echo "  ❌ Audio fallback script: Not found"
    fi
    
    if command -v tmux >/dev/null 2>&1; then
        echo "  ✅ TMux: Available"
        if [[ -n "${TMUX:-}" ]]; then
            echo "  ✅ TMux session: Active"
        else
            echo "  ⚠️  TMux session: Not active"
        fi
    else
        echo "  ❌ TMux: Not available"
    fi
    
    echo
    display_current_mode
    
    echo
    echo "=== End Diagnostics ==="
}

# === テスト機能 ===
test_switcher() {
    echo "=== $SWITCHER_NAME Test Suite ==="
    
    local tests_passed=0
    local tests_total=0
    
    # Test 1: 設定初期化
    echo "Test 1: Configuration initialization"
    ((tests_total++))
    if initialize_config; then
        echo "✅ PASS - Config initialization"
        ((tests_passed++))
    else
        echo "❌ FAIL - Config initialization"
    fi
    
    # Test 2: モード取得
    echo "Test 2: Mode retrieval"
    ((tests_total++))
    local current_mode=$(get_current_mode)
    if [[ -n "$current_mode" ]]; then
        echo "✅ PASS - Current mode: $current_mode"
        ((tests_passed++))
    else
        echo "❌ FAIL - Mode retrieval"
    fi
    
    # Test 3: モード設定
    echo "Test 3: Mode setting"
    ((tests_total++))
    if set_audio_mode "notification"; then
        local new_mode=$(get_current_mode)
        if [[ "$new_mode" == "notification" ]]; then
            echo "✅ PASS - Mode setting"
            ((tests_passed++))
        else
            echo "❌ FAIL - Mode not set correctly"
        fi
    else
        echo "❌ FAIL - Mode setting"
    fi
    
    # Test 4: モード循環
    echo "Test 4: Mode cycling"
    ((tests_total++))
    local original_mode=$(get_current_mode)
    local cycled_mode=$(cycle_audio_mode)
    if [[ "$cycled_mode" != "$original_mode" ]]; then
        echo "✅ PASS - Mode cycling: $original_mode → $cycled_mode"
        ((tests_passed++))
    else
        echo "❌ FAIL - Mode cycling"
    fi
    
    # Test 5: 音声テスト (現在のモードで)
    echo "Test 5: Audio test with current mode"
    ((tests_total++))
    if play_with_current_mode "🔊" "Test notification" >/dev/null 2>&1; then
        echo "✅ PASS - Audio test"
        ((tests_passed++))
    else
        echo "❌ FAIL - Audio test"
    fi
    
    # 元のモードに戻す
    set_audio_mode "$original_mode" >/dev/null 2>&1
    
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
        "get")
            get_current_mode
            ;;
        "set")
            shift
            set_audio_mode "$@"
            ;;
        "cycle")
            cycle_audio_mode
            ;;
        "display")
            display_current_mode
            ;;
        "play")
            shift
            play_with_current_mode "$@"
            ;;
        "tmux-cycle")
            tmux_cycle_mode
            ;;
        "tmux-keybinding")
            generate_tmux_keybinding
            ;;
        "diagnose")
            diagnose_switcher
            ;;
        "test")
            test_switcher
            ;;
        "help"|"-h"|"--help")
            cat << EOF
$SWITCHER_NAME - Usage Guide

COMMANDS:
  get                                 - 現在のモード表示
  set <mode>                         - モード設定
  cycle                              - モード循環切り替え
  display                            - 現在のモード詳細表示
  play <status> [message]            - 現在のモードで音声再生
  tmux-cycle                         - TMux統合モード切り替え
  tmux-keybinding                    - TMuxキーバインド設定表示
  diagnose                           - システム診断
  test                               - テスト実行
  help                               - このヘルプ

MODES:
  auto         - 自動選択 (音声→通知→フォールバック)
  audio        - 音声デバイスのみ
  notification - システム通知のみ
  beep         - ビープ音のみ
  silent       - 無音

TMUX INTEGRATION:
  Add to your tmux.conf:
    bind-key C-n run-shell "$0 tmux-cycle"
    bind-key n run-shell "$0 tmux-cycle"

EXAMPLES:
  $0 get                              # 現在のモード確認
  $0 set audio                        # 音声デバイスモードに設定
  $0 cycle                            # 次のモードに切り替え
  $0 play "✅" "Test notification"     # 現在のモードでテスト再生
  $0 tmux-cycle                       # TMux統合切り替え

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