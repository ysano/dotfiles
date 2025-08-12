#!/bin/bash
# ファイル名: toggle_notify_mode.sh
# 説明: Claude Voice通知モード切り替えスクリプト
# 用途: Prefix + n で3つの通知モードを循環切り替え

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 設定とヘルパー関数を読み込み
source "$SCRIPT_DIR/functions.sh"

# 通知モードの定義
NOTIFY_MODE_SYSTEM_ONLY="system_only"      # システム通知のみ
NOTIFY_MODE_SOUND_ONLY="sound_only"        # ステータスごとの通知音声
NOTIFY_MODE_SOUND_SUMMARY="sound_summary"  # ステータスごとの通知音声＋要約読み上げ

# 現在の通知モードを取得
get_current_notify_mode() {
    local current_mode=$(tmux show-option -gqv @claude_voice_notify_mode 2>/dev/null)
    echo "${current_mode:-$NOTIFY_MODE_SOUND_SUMMARY}"
}

# 次の通知モードを取得
get_next_notify_mode() {
    local current_mode="$1"
    
    case "$current_mode" in
        "$NOTIFY_MODE_SYSTEM_ONLY")
            echo "$NOTIFY_MODE_SOUND_ONLY"
            ;;
        "$NOTIFY_MODE_SOUND_ONLY")
            echo "$NOTIFY_MODE_SOUND_SUMMARY"
            ;;
        "$NOTIFY_MODE_SOUND_SUMMARY")
            echo "$NOTIFY_MODE_SYSTEM_ONLY"
            ;;
        *)
            echo "$NOTIFY_MODE_SOUND_SUMMARY"
            ;;
    esac
}

# 通知モード名を日本語で取得
get_notify_mode_display_name() {
    local mode="$1"
    
    case "$mode" in
        "$NOTIFY_MODE_SYSTEM_ONLY")
            echo "システム通知のみ"
            ;;
        "$NOTIFY_MODE_SOUND_ONLY")
            echo "通知音声のみ"
            ;;
        "$NOTIFY_MODE_SOUND_SUMMARY")
            echo "通知音声＋要約読み上げ"
            ;;
        *)
            echo "不明なモード"
            ;;
    esac
}

# 通知モードを設定
set_notify_mode() {
    local mode="$1"
    
    # 通知モードを設定
    tmux set-option -g @claude_voice_notify_mode "$mode"
    
    # モードに応じて関連設定を調整
    case "$mode" in
        "$NOTIFY_MODE_SYSTEM_ONLY")
            # システム通知のみ: 音声と要約を無効化
            tmux set-option -g @claude_voice_sound_enabled "false"
            tmux set-option -g @claude_voice_summary_enabled "false"
            ;;
        "$NOTIFY_MODE_SOUND_ONLY")
            # 通知音声のみ: 音声を有効化、要約を無効化
            tmux set-option -g @claude_voice_sound_enabled "true"
            tmux set-option -g @claude_voice_summary_enabled "false"
            ;;
        "$NOTIFY_MODE_SOUND_SUMMARY")
            # 通知音声＋要約読み上げ: 両方を有効化
            tmux set-option -g @claude_voice_sound_enabled "true"
            tmux set-option -g @claude_voice_summary_enabled "true"
            ;;
    esac
    
    # 設定変更をログ出力
    local display_name=$(get_notify_mode_display_name "$mode")
    log_info "通知モードを変更: $display_name"
    
    # tmuxにメッセージを表示
    tmux display-message "通知モード: $display_name"
}

# 通知モード切り替えのメイン処理
toggle_notify_mode() {
    # 現在のモードを取得
    local current_mode=$(get_current_notify_mode)
    
    # 次のモードを計算
    local next_mode=$(get_next_notify_mode "$current_mode")
    
    # モードを設定
    set_notify_mode "$next_mode"
    
    return 0
}

# 通知モードの状態を表示
show_notify_mode() {
    local current_mode=$(get_current_notify_mode)
    local display_name=$(get_notify_mode_display_name "$current_mode")
    
    echo "現在の通知モード: $display_name"
    echo "モード値: $current_mode"
    
    # 関連設定の状態も表示
    local sound_enabled=$(tmux show-option -gqv @claude_voice_sound_enabled 2>/dev/null)
    local summary_enabled=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
    
    echo "音声有効: ${sound_enabled:-false}"
    echo "要約有効: ${summary_enabled:-false}"
}

# メイン処理
main() {
    # tmuxセッション内で実行されているかチェック
    if [[ -z "$TMUX" ]]; then
        log_error "tmuxセッション内で実行してください"
        exit 1
    fi
    
    # 引数に応じて処理を分岐
    case "${1:-}" in
        "show"|"status")
            show_notify_mode
            ;;
        "system_only")
            set_notify_mode "$NOTIFY_MODE_SYSTEM_ONLY"
            ;;
        "sound_only")
            set_notify_mode "$NOTIFY_MODE_SOUND_ONLY"
            ;;
        "sound_summary")
            set_notify_mode "$NOTIFY_MODE_SOUND_SUMMARY"
            ;;
        "toggle"|"")
            toggle_notify_mode
            ;;
        "help"|"-h"|"--help")
            echo "使用方法: $0 [コマンド]"
            echo ""
            echo "コマンド:"
            echo "  toggle         - 通知モードを次のモードに切り替え"
            echo "  show           - 現在の通知モードを表示"
            echo "  system_only    - システム通知のみモードに設定"
            echo "  sound_only     - 通知音声のみモードに設定"
            echo "  sound_summary  - 通知音声＋要約読み上げモードに設定"
            echo "  help           - このヘルプを表示"
            echo ""
            echo "通知モード:"
            echo "  1. システム通知のみ"
            echo "  2. ステータスごとの通知音声"
            echo "  3. ステータスごとの通知音声＋要約読み上げ"
            ;;
        *)
            log_error "不明なコマンド: $1"
            echo "使用方法: $0 help"
            exit 1
            ;;
    esac
}

# スクリプトが直接実行された場合のみmainを呼び出し
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
