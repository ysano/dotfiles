#!/bin/bash
# Claude Voice Notification Common Library
# 通知に関する共通関数

# ソース必要なライブラリ
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../foundation.sh"

# 通知設定
readonly NOTIFICATION_RATE_LIMIT=2      # レート制限（秒）
readonly PERIODIC_NOTIFY_INTERVAL=300   # 定期通知間隔（5分）

# 通知モード取得
get_notification_mode() {
    local mode_file="$CLAUDE_VOICE_HOME/notification_mode"
    if [[ -f "$mode_file" ]]; then
        cat "$mode_file" 2>/dev/null || echo "sound"
    else
        echo "sound"
    fi
}

# 通知モード設定
set_notification_mode() {
    local mode="$1"
    local mode_file="$CLAUDE_VOICE_HOME/notification_mode"
    echo "$mode" > "$mode_file"
}

# 通知が無効化されているかチェック
are_notifications_disabled() {
    [[ -f "$HOME/.tmux/notifications_disabled" ]]
}

# レート制限チェック
check_rate_limit() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local rate_file="$STATUS_DIR/.rate_limit_${window_id}"
    local now=$(date +%s)
    local last_notify=0
    
    [[ -f "$rate_file" ]] && last_notify=$(cat "$rate_file" 2>/dev/null || echo 0)
    
    if [[ $((now - last_notify)) -lt $NOTIFICATION_RATE_LIMIT ]]; then
        return 0  # レート制限中
    fi
    
    echo "$now" > "$rate_file"
    return 1  # 通知可能
}

# 最終通知時刻を更新
update_last_notification() {
    local window_id="${1:-$(tmux display-message -p '#I')}"
    local last_file="$STATUS_DIR/.last_notify_${window_id}"
    echo "$(date +%s)" > "$last_file"
}

# 定期通知が必要かチェック
needs_periodic_notification() {
    local window_id="$1"
    local status="$2"
    local last_file="$STATUS_DIR/.last_notify_${window_id}"
    local now=$(date +%s)
    local last_notify=0
    
    # アクティブな状態のみ定期通知
    if [[ "$status" != "$STATUS_BUSY" && "$status" != "$STATUS_WAITING" ]]; then
        return 1
    fi
    
    [[ -f "$last_file" ]] && last_notify=$(cat "$last_file" 2>/dev/null || echo 0)
    
    if [[ $((now - last_notify)) -gt $PERIODIC_NOTIFY_INTERVAL ]]; then
        return 0  # 定期通知必要
    fi
    
    return 1  # 不要
}

# 通知が必要かチェック
should_notify() {
    local window_id="$1"
    local old_status="$2"
    local new_status="$3"
    
    # 通知が無効化されている場合
    if are_notifications_disabled; then
        return 1
    fi
    
    # レート制限チェック
    if check_rate_limit "$window_id"; then
        return 1
    fi
    
    # ステータス変更時は常に通知
    if [[ "$old_status" != "$new_status" ]]; then
        return 0
    fi
    
    # 定期通知チェック
    if needs_periodic_notification "$window_id" "$new_status"; then
        return 0
    fi
    
    return 1
}

# OS検出（キャッシュ付き）
detect_os_with_cache() {
    local cache_file="/tmp/.claude_os_type_cache"
    local cache_ttl=60
    
    # キャッシュチェック
    if [[ -f "$cache_file" ]]; then
        local file_age=$(($(date +%s) - $(stat -f %m "$cache_file" 2>/dev/null || stat -c %Y "$cache_file" 2>/dev/null || echo 0)))
        if [[ $file_age -lt $cache_ttl ]]; then
            cat "$cache_file"
            return
        fi
    fi
    
    # OS検出
    local os_type
    case "$OSTYPE" in
        darwin*)
            os_type="macos"
            ;;
        linux*)
            if [[ -n "${WSL_DISTRO_NAME:-}" ]] || grep -qi microsoft /proc/version 2>/dev/null; then
                os_type="wsl"
            else
                os_type="linux"
            fi
            ;;
        freebsd*)
            os_type="freebsd"
            ;;
        *)
            os_type="other"
            ;;
    esac
    
    # キャッシュ保存
    echo "$os_type" > "$cache_file" 2>/dev/null
    echo "$os_type"
}

# 簡易サウンド再生（フォールバック用）
play_beep() {
    echo -e "\a"
}

# macOS通知のクリーンアップ
cleanup_macos_notifications() {
    if command -v terminal-notifier >/dev/null 2>&1; then
        terminal-notifier -remove "claude-code-waiting" 2>/dev/null
        terminal-notifier -remove "claude-code-complete" 2>/dev/null
        terminal-notifier -remove "claude-code-busy" 2>/dev/null
    fi
}