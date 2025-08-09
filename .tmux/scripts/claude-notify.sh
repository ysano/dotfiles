#!/bin/bash
# Claude Code Notification Script - Refactored Version
# Claude Voice統合アーキテクチャに準拠した通知スクリプト

# ライブラリのロード
CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-$HOME/.tmux/claude}"
source "$CLAUDE_VOICE_HOME/core/lib/status_common.sh"
source "$CLAUDE_VOICE_HOME/core/lib/notification_common.sh"

# 引数処理
WINDOW_ID=${1:-$(tmux display-message -p '#I')}
OLD_STATUS="$2"
NEW_STATUS="$3"

# メイン処理開始
main() {
    # 初期化
    [[ ! -d "$STATUS_DIR" ]] && mkdir -p "$STATUS_DIR"
    
    # 前回のステータスを取得（引数で渡されなかった場合）
    if [[ -z "$OLD_STATUS" ]]; then
        OLD_STATUS=$(load_window_status "$WINDOW_ID")
    fi
    
    # 現在のステータスを取得（引数で渡されなかった場合）
    if [[ -z "$NEW_STATUS" ]]; then
        NEW_STATUS=$("$(dirname "$0")/claude-status-refactored.sh" "$WINDOW_ID")
    fi
    
    # 通知が必要かチェック
    if ! should_notify "$WINDOW_ID" "$OLD_STATUS" "$NEW_STATUS"; then
        exit 0
    fi
    
    # ステータスを保存
    if [[ -n "$NEW_STATUS" ]]; then
        save_window_status "$WINDOW_ID" "$NEW_STATUS"
    fi
    
    # 通知モードの確認
    local notification_mode=$(get_notification_mode)
    
    if [[ "$notification_mode" == "beep" ]]; then
        # ビープモード
        play_beep
    else
        # サウンドモード
        notify_with_sound "$NEW_STATUS"
    fi
    
    # 最終通知時刻を更新
    update_last_notification "$WINDOW_ID"
    
    # Claude Voice自動サマリーチェック（オプション）
    # 注: trigger_voice_notificationで処理するため、ここではコメントアウト
    # check_auto_summary "$NEW_STATUS"
}

# サウンド付き通知
notify_with_sound() {
    local status="$1"
    local os_type=$(detect_os_with_cache)
    
    case "$status" in
        "$STATUS_IDLE")
            play_complete_sound "$os_type"
            send_notification "Claude Code" "✅ Complete" "処理が完了しました"
            # 音声読み上げを追加
            trigger_voice_notification "complete" "$WINDOW_ID"
            ;;
        "$STATUS_WAITING")
            play_waiting_sound "$os_type"
            send_notification "Claude Code" "⌛ Waiting" "入力待ちです"
            # 音声読み上げを追加
            trigger_voice_notification "waiting" "$WINDOW_ID"
            ;;
        "$STATUS_BUSY")
            play_busy_sound "$os_type"
            send_notification "Claude Code" "⚡ Busy" "処理中です"
            # 音声読み上げを追加（通常はBusyでは読み上げない）
            # trigger_voice_notification "busy" "$WINDOW_ID"
            ;;
    esac
}

# 完了音再生
play_complete_sound() {
    local os_type="$1"
    
    case "$os_type" in
        "macos")
            play_macos_sound "/System/Library/Sounds/Glass.aiff"  # Glass音で完了を示す
            ;;
        "wsl")
            play_wsl_sound "complete"
            ;;
        "linux")
            play_linux_sound "complete"
            ;;
        *)
            play_beep
            ;;
    esac
}

# 待機音再生
play_waiting_sound() {
    local os_type="$1"
    
    case "$os_type" in
        "macos")
            play_macos_sound "/System/Library/Sounds/Tink.aiff"  # Tink音で待機を示す
            ;;
        "wsl")
            play_wsl_sound "waiting"
            ;;
        "linux")
            play_linux_sound "waiting"
            ;;
        *)
            play_beep
            ;;
    esac
}

# Busy音再生
play_busy_sound() {
    local os_type="$1"
    
    case "$os_type" in
        "macos")
            play_macos_sound "/System/Library/Sounds/Basso.aiff"  # Basso音で忙しい状態を示す
            ;;
        "wsl")
            play_wsl_sound "busy"
            ;;
        "linux")
            play_linux_sound "busy"
            ;;
        *)
            play_beep
            ;;
    esac
}

# macOSサウンド再生
play_macos_sound() {
    local sound_file="$1"
    
    # パンニング対応再生を試みる
    if [[ -f "$CLAUDE_VOICE_HOME/core/base.sh" ]] && [[ -f "$CLAUDE_VOICE_HOME/os/darwin.sh" ]]; then
        (source "$CLAUDE_VOICE_HOME/core/base.sh" && \
         source "$CLAUDE_VOICE_HOME/os/darwin.sh" && \
         play_sound_file_with_panning "$sound_file" "1.0" "$WINDOW_ID") &
    elif command -v afplay >/dev/null 2>&1; then
        afplay -v 1.0 "$sound_file" 2>/dev/null &
    else
        play_beep
    fi
}

# WSLサウンド再生
play_wsl_sound() {
    local sound_type="$1"
    local powershell_path="/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
    
    if [[ ! -f "$powershell_path" ]]; then
        play_beep
        return
    fi
    
    case "$sound_type" in
        "complete")
            "$powershell_path" -Command "[console]::beep(523,80);[console]::beep(659,80);[console]::beep(783,80);[console]::beep(1046,120)" 2>/dev/null &
            ;;
        "waiting")
            "$powershell_path" -Command "[console]::beep(659,100);Start-Sleep -Milliseconds 50;[console]::beep(880,150);Start-Sleep -Milliseconds 50;[console]::beep(1175,100)" 2>/dev/null &
            ;;
        "busy")
            "$powershell_path" -Command "[console]::beep(800,80);Start-Sleep -Milliseconds 30;[console]::beep(800,80);Start-Sleep -Milliseconds 30;[console]::beep(600,100)" 2>/dev/null &
            ;;
    esac
}

# Linuxサウンド再生
play_linux_sound() {
    local sound_type="$1"
    
    if ! command -v paplay >/dev/null 2>&1; then
        play_beep
        return
    fi
    
    # 適切なシステムサウンドファイルを探す
    local sound_files=()
    case "$sound_type" in
        "complete")
            sound_files=("/usr/share/sounds/freedesktop/stereo/complete.oga")
            ;;
        "waiting")
            sound_files=("/usr/share/sounds/freedesktop/stereo/dialog-information.oga")
            ;;
        "busy")
            sound_files=("/usr/share/sounds/freedesktop/stereo/dialog-warning.oga")
            ;;
    esac
    
    for sound_file in "${sound_files[@]}"; do
        if [[ -f "$sound_file" ]]; then
            paplay "$sound_file" 2>/dev/null &
            return
        fi
    done
    
    play_beep
}

# デスクトップ通知送信
send_notification() {
    local title="$1"
    local subtitle="$2"
    local message="$3"
    local os_type=$(detect_os_with_cache)
    
    case "$os_type" in
        "macos")
            if command -v terminal-notifier >/dev/null 2>&1; then
                cleanup_macos_notifications
                terminal-notifier \
                    -title "$title" \
                    -subtitle "$subtitle" \
                    -message "$message" \
                    -group "claude-code" \
                    -sender "com.apple.Terminal" 2>/dev/null &
            elif command -v osascript >/dev/null 2>&1; then
                osascript -e "display notification \"$message\" with title \"$title\" subtitle \"$subtitle\"" 2>/dev/null &
            fi
            ;;
        "linux"|"wsl")
            if command -v notify-send >/dev/null 2>&1; then
                notify-send "$title" "$subtitle - $message" 2>/dev/null &
            fi
            ;;
    esac
}

# Claude Voice自動サマリーチェック
check_auto_summary() {
    local status="$1"
    
    # 環境変数から設定を取得
    local auto_summary=$(tmux show-environment -g CLAUDE_VOICE_AUTO_SUMMARY 2>/dev/null | cut -d= -f2 || echo "false")
    
    if [[ "$auto_summary" != "true" ]]; then
        return
    fi
    
    case "$status" in
        "$STATUS_IDLE")
            local on_complete=$(tmux show-environment -g CLAUDE_VOICE_ON_COMPLETE 2>/dev/null | cut -d= -f2 || echo "true")
            if [[ "$on_complete" == "true" ]]; then
                trigger_voice_summary "complete"
            fi
            ;;
        "$STATUS_WAITING")
            local on_waiting=$(tmux show-environment -g CLAUDE_VOICE_ON_WAITING 2>/dev/null | cut -d= -f2 || echo "true")
            if [[ "$on_waiting" == "true" ]]; then
                trigger_voice_summary "waiting"
            fi
            ;;
        "$STATUS_BUSY")
            local on_busy=$(tmux show-environment -g CLAUDE_VOICE_ON_BUSY 2>/dev/null | cut -d= -f2 || echo "false")
            if [[ "$on_busy" == "true" ]]; then
                trigger_voice_summary "busy"
            fi
            ;;
    esac
}

# Claude Voice サマリーをトリガー
trigger_voice_summary() {
    local context="$1"
    local voice_script="$CLAUDE_VOICE_HOME/bin/claude-voice"
    
    if [[ -x "$voice_script" ]]; then
        case "$context" in
            "complete")
                "$voice_script" brief 20 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
                ;;
            "waiting")
                "$voice_script" brief 15 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
                ;;
            "busy")
                "$voice_script" brief 10 "Kyoko (Enhanced)" "auto" "auto" "${WINDOW_ID}.1" >/dev/null 2>&1 &
                ;;
        esac
    fi
}

# 音声通知をトリガー（通知音の後に音声読み上げ）
trigger_voice_notification() {
    local context="$1"
    local window_id="$2"
    
    # 空間音響用の音声割り当て
    source "$CLAUDE_VOICE_HOME/core/spatial_audio.sh" 2>/dev/null || return
    
    # ウィンドウインデックスから音声を割り当て
    local voice=$(assign_voice_to_window "$window_id")
    
    # claude-voiceコマンドを使って画面を要約
    local voice_script="$CLAUDE_VOICE_HOME/bin/claude-voice"
    
    if [[ -x "$voice_script" ]]; then
        # 0.5秒待ってから要約を生成・読み上げ（通知音の後）
        case "$context" in
            "complete")
                # 完了時は最後の20行を要約（パンニング用にウィンドウIDを渡す）
                (sleep 0.5 && TMUX_PANE="%${window_id}" CLAUDE_VOICE_WINDOW_ID="$window_id" "$voice_script" brief 20 "$voice") &
                ;;
            "waiting")
                # 待機時は枠全体を取得してから要約
                (sleep 0.5 && {
                    # frame_detector.shを使って枠全体を取得
                    source "$CLAUDE_VOICE_HOME/core/frame_detector.sh" 2>/dev/null
                    
                    # 枠全体を取得（最初15行、必要に応じて100行まで拡張）
                    local frame_content=$(get_waiting_frame "$window_id")
                    
                    if [[ -n "$frame_content" ]]; then
                        # 枠が取得できた場合は、その内容を一時ファイルに保存
                        local temp_file="/tmp/claude_waiting_frame_${window_id}.txt"
                        echo "$frame_content" > "$temp_file"
                        
                        # claude-voiceに一時ファイルから読み込ませる（パンニング用にウィンドウIDを渡す）
                        TMUX_PANE="%${window_id}" CLAUDE_VOICE_CAPTURE_OVERRIDE="$temp_file" CLAUDE_VOICE_WINDOW_ID="$window_id" "$voice_script" brief 50 "$voice"
                        
                        # 一時ファイルを削除
                        rm -f "$temp_file"
                    else
                        # 枠が取得できない場合は通常の15行要約（パンニング用にウィンドウIDを渡す）
                        TMUX_PANE="%${window_id}" CLAUDE_VOICE_WINDOW_ID="$window_id" "$voice_script" brief 15 "$voice"
                    fi
                }) &
                ;;
            "busy")
                # Busyの場合は通常読み上げない
                ;;
        esac
    else
        # フォールバック: claude-voiceが無い場合は簡単なメッセージ
        local message=""
        case "$context" in
            "complete")
                message="ウィンドウ${window_id}の処理が完了しました"
                ;;
            "waiting")
                message="ウィンドウ${window_id}が入力を待っています"
                ;;
            "busy")
                message="ウィンドウ${window_id}が処理中です"
                ;;
        esac
        
        if [[ -n "$message" ]]; then
            (sleep 0.5 && say -v "$voice" -r 200 "$message" 2>/dev/null) &
        fi
    fi
}

# メイン処理実行
main