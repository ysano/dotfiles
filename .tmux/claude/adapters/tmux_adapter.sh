#!/bin/bash
# Tmux Adapter - Infrastructure Layer
# tmux システムとの統合アダプター

# 依存関係のインポート
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../core/interfaces.sh"

# === Tmux Adapter Configuration ===

declare -A TMUX_ADAPTER_CONFIG=(
    ["command_timeout"]="5"              # tmuxコマンドタイムアウト（秒）
    ["retry_attempts"]="3"               # リトライ回数
    ["session_detection"]="auto"         # セッション検出モード
    ["fallback_mode"]="false"            # フォールバックモード
)

# === Tmux Session Management ===

# tmux セッション存在確認
is_tmux_session_active() {
    if ! command -v tmux >/dev/null 2>&1; then
        return 1
    fi
    
    # TMUX環境変数またはアクティブセッションの確認
    if [[ -n "${TMUX:-}" ]] || tmux list-sessions >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# 現在のtmux環境情報取得
get_tmux_environment() {
    local env_info=()
    
    if is_tmux_session_active; then
        # セッション情報
        local session_name=$(tmux display-message -p '#S' 2>/dev/null || echo "unknown")
        local session_id=$(tmux display-message -p '#{session_id}' 2>/dev/null || echo "unknown")
        
        # ウィンドウ情報
        local window_name=$(tmux display-message -p '#W' 2>/dev/null || echo "unknown")
        local window_id=$(tmux display-message -p '#I' 2>/dev/null || echo "unknown")
        
        # pane情報
        local pane_id=$(tmux display-message -p '#P' 2>/dev/null || echo "unknown")
        local pane_pid=$(tmux display-message -p '#{pane_pid}' 2>/dev/null || echo "unknown")
        
        env_info=(
            "session_name:$session_name"
            "session_id:$session_id"
            "window_name:$window_name"
            "window_id:$window_id"
            "pane_id:$pane_id"
            "pane_pid:$pane_pid"
            "tmux_version:$(tmux -V 2>/dev/null || echo 'unknown')"
        )
    else
        env_info=("status:not_in_tmux")
    fi
    
    printf '%s\n' "${env_info[@]}"
}

# === Enhanced Tmux Operations ===

# 改良されたpane情報取得
get_pane_info() {
    local window_id="${1:-current}"
    local pane_id="${2:-current}"
    local timeout="${TMUX_ADAPTER_CONFIG[command_timeout]}"
    
    local pane_info=()
    
    # ターゲット指定
    local target=""
    if [[ "$window_id" == "current" && "$pane_id" == "current" ]]; then
        target=""  # 現在のpane
    elif [[ "$pane_id" == "current" ]]; then
        target=":$window_id"
    else
        target=":$window_id.$pane_id"
    fi
    
    # タイムアウト付きでpane情報を取得
    if timeout "${timeout}s" tmux display-message -t "$target" -p \
        "pid:#{pane_pid};title:#{pane_title};current_path:#{pane_current_path};active:#{pane_active};width:#{pane_width};height:#{pane_height}" 2>/dev/null; then
        return 0
    else
        echo "Failed to get pane info for $target"
        return 1
    fi
}

# 高性能なpane出力キャプチャ
capture_pane_optimized() {
    local window_id="${1:-current}"
    local pane_id="${2:-current}"
    local lines="${3:-30}"
    local mode="${4:-recent}"  # recent, all, visible
    
    # ターゲット指定
    local target=""
    if [[ "$window_id" == "current" && "$pane_id" == "current" ]]; then
        target=""
    elif [[ "$pane_id" == "current" ]]; then
        target=":$window_id"
    else
        target=":$window_id.$pane_id"
    fi
    
    # キャプチャモード別の引数設定
    local capture_args=()
    case "$mode" in
        "recent")
            capture_args=("-p" "-S" "-$lines")
            ;;
        "all")
            capture_args=("-p" "-S" "-")
            ;;
        "visible")
            capture_args=("-p")
            ;;
        *)
            capture_args=("-p" "-S" "-$lines")
            ;;
    esac
    
    # リトライ機能付きキャプチャ
    local retry_count="${TMUX_ADAPTER_CONFIG[retry_attempts]}"
    local timeout="${TMUX_ADAPTER_CONFIG[command_timeout]}"
    
    for ((attempt=1; attempt<=retry_count; attempt++)); do
        if timeout "${timeout}s" tmux capture-pane "${capture_args[@]}" -t "$target" 2>/dev/null; then
            return 0
        fi
        
        if [[ $attempt -lt $retry_count ]]; then
            sleep 0.1
        fi
    done
    
    return 1
}

# === Tmux Pane Discovery ===

# 全paneの詳細情報取得
discover_all_panes() {
    local include_inactive="${1:-false}"
    local pane_details=()
    
    if ! is_tmux_session_active; then
        echo "No active tmux session"
        return 1
    fi
    
    # 全paneのリストを取得
    local pane_format="#{session_name}:#{window_id}:#{pane_id}:#{pane_pid}:#{pane_active}:#{pane_current_command}"
    
    while IFS=':' read -r session_name window_id pane_id pane_pid pane_active pane_command; do
        # 非アクティブpaneをスキップするかどうか
        if [[ "$include_inactive" == "false" && "$pane_active" != "1" ]]; then
            continue
        fi
        
        pane_details+=("$session_name:$window_id:$pane_id:$pane_pid:$pane_active:$pane_command")
    done < <(tmux list-panes -a -F "$pane_format" 2>/dev/null)
    
    printf '%s\n' "${pane_details[@]}"
}

# Claude Code paneの検出
find_claude_panes() {
    local claude_panes=()
    
    while IFS=':' read -r session_name window_id pane_id pane_pid pane_active pane_command; do
        # プロセス名でClaude Codeを検出
        if echo "$pane_command" | grep -qi "claude"; then
            claude_panes+=("$session_name:$window_id:$pane_id:$pane_pid:$pane_active")
        elif [[ -n "$pane_pid" ]]; then
            # プロセスツリーでClaude Codeを検出
            if pstree -p "$pane_pid" 2>/dev/null | grep -q "claude"; then
                claude_panes+=("$session_name:$window_id:$pane_id:$pane_pid:$pane_active")
            fi
        fi
    done < <(discover_all_panes "true")
    
    printf '%s\n' "${claude_panes[@]}"
}

# === Tmux Status Integration ===

# ステータスバーのフォーマット設定
configure_status_bar() {
    local position="${1:-right}"    # left, right
    local format="${2:-auto}"       # auto, minimal, detailed
    local refresh_interval="${3:-1}" # 秒
    
    if ! is_tmux_session_active; then
        echo "Not in tmux session"
        return 1
    fi
    
    # フォーマット文字列の構築
    local status_format=""
    case "$format" in
        "minimal")
            status_format="#{?pane_active,#[fg=green]●,#[fg=red]○}"
            ;;
        "detailed")
            status_format="#[fg=cyan]#{session_name}#[default]:#[fg=yellow]#{window_index}#[default].#{pane_index} #{?pane_active,#[fg=green]●,#[fg=red]○}"
            ;;
        "auto"|*)
            status_format="#{?pane_active,#[fg=green],#[fg=yellow]}Claude#[default]"
            ;;
    esac
    
    # tmux設定の適用
    local tmux_commands=(
        "set-option -g status-interval $refresh_interval"
        "set-option -g status-$position-length 50"
        "set-option -g status-$position '$status_format'"
    )
    
    for cmd in "${tmux_commands[@]}"; do
        if ! tmux $cmd 2>/dev/null; then
            echo "Failed to execute: tmux $cmd"
            return 1
        fi
    done
    
    echo "Status bar configured: $position, $format, ${refresh_interval}s"
}

# === Window/Pane Management ===

# 新しいウィンドウでClaude Codeを起動
create_claude_window() {
    local window_name="${1:-claude}"
    local session_name="${2:-}"
    
    if ! is_tmux_session_active; then
        echo "Not in tmux session"
        return 1
    fi
    
    # セッション指定
    local target_session=""
    if [[ -n "$session_name" ]]; then
        target_session="-t $session_name"
    fi
    
    # 新しいウィンドウを作成
    if tmux new-window $target_session -n "$window_name" "claude" 2>/dev/null; then
        echo "Created Claude window: $window_name"
        return 0
    else
        echo "Failed to create Claude window"
        return 1
    fi
}

# paneの分割とClaude起動
split_pane_for_claude() {
    local split_direction="${1:-horizontal}"  # horizontal, vertical
    local size="${2:-50}"                     # percentage
    
    if ! is_tmux_session_active; then
        echo "Not in tmux session"
        return 1
    fi
    
    # 分割方向の引数設定
    local split_args=()
    case "$split_direction" in
        "horizontal")
            split_args=("-h" "-p" "$size")
            ;;
        "vertical")
            split_args=("-v" "-p" "$size")
            ;;
        *)
            echo "Invalid split direction: $split_direction"
            return 1
            ;;
    esac
    
    # paneを分割してClaude起動
    if tmux split-window "${split_args[@]}" "claude" 2>/dev/null; then
        echo "Created Claude pane: $split_direction split, ${size}%"
        return 0
    else
        echo "Failed to split pane for Claude"
        return 1
    fi
}

# === Configuration Management ===

# アダプター設定の更新
configure_tmux_adapter() {
    local key="$1"
    local value="$2"
    
    if [[ -n "${TMUX_ADAPTER_CONFIG[$key]}" ]]; then
        TMUX_ADAPTER_CONFIG["$key"]="$value"
        echo "Updated $key = $value"
    else
        echo "Unknown configuration key: $key"
        echo "Available keys: ${!TMUX_ADAPTER_CONFIG[*]}"
        return 1
    fi
}

# 現在の設定表示
show_tmux_adapter_config() {
    echo "=== Tmux Adapter Configuration ==="
    for key in "${!TMUX_ADAPTER_CONFIG[@]}"; do
        echo "  $key = ${TMUX_ADAPTER_CONFIG[$key]}"
    done
}

# === Test & Debug Functions ===

# tmux統合テスト
test_tmux_adapter() {
    echo "=== Tmux Adapter Test ==="
    echo ""
    
    # tmuxセッション確認
    echo "1. Session Check:"
    if is_tmux_session_active; then
        echo "  ✅ Active tmux session detected"
    else
        echo "  ❌ No active tmux session"
        return 1
    fi
    echo ""
    
    # 環境情報取得
    echo "2. Environment Information:"
    get_tmux_environment | sed 's/^/  /'
    echo ""
    
    # pane情報取得
    echo "3. Current Pane Info:"
    get_pane_info | sed 's/^/  /'
    echo ""
    
    # pane発見
    echo "4. Claude Pane Discovery:"
    local claude_panes=($(find_claude_panes))
    if [[ ${#claude_panes[@]} -gt 0 ]]; then
        echo "  Found ${#claude_panes[@]} Claude pane(s):"
        printf '    %s\n' "${claude_panes[@]}"
    else
        echo "  No Claude panes found"
    fi
    echo ""
    
    # キャプチャテスト
    echo "5. Capture Test:"
    local capture_result=$(capture_pane_optimized "current" "current" "5" "recent")
    if [[ $? -eq 0 ]]; then
        echo "  ✅ Pane capture successful (5 lines)"
        echo "  Sample: $(echo "$capture_result" | head -1 | cut -c1-50)..."
    else
        echo "  ❌ Pane capture failed"
    fi
}

# === Main Execution ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_tmux_adapter
            ;;
        "env")
            get_tmux_environment
            ;;
        "panes")
            discover_all_panes "${2:-false}"
            ;;
        "claude-panes")
            find_claude_panes
            ;;
        "config")
            show_tmux_adapter_config
            ;;
        "status")
            configure_status_bar "${2:-right}" "${3:-auto}" "${4:-1}"
            ;;
        *)
            echo "Usage: $0 [test|env|panes|claude-panes|config|status]"
            ;;
    esac
fi