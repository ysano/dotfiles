#!/bin/bash
# Interface Abstraction Layer for Claude Voice System
# テスタブルアーキテクチャのための抽象化インターフェース

# === システムコマンド抽象化 ===

# tmux操作インターフェース
declare -A TMUX_INTERFACE=(
    ["get_window_id"]="tmux_get_window_id_impl"
    ["get_pane_id"]="tmux_get_pane_id_impl"
    ["get_pane_pid"]="tmux_get_pane_pid_impl"
    ["capture_pane"]="tmux_capture_pane_impl"
)

# プロセス管理インターフェース
declare -A PROCESS_INTERFACE=(
    ["find_process"]="process_find_impl"
    ["get_process_tree"]="process_tree_impl"
)

# ファイルシステムインターフェース
declare -A FILESYSTEM_INTERFACE=(
    ["has_command"]="filesystem_has_command_impl"
    ["read_file"]="filesystem_read_file_impl"
    ["write_file"]="filesystem_write_file_impl"
)

# === デフォルト実装 ===

# tmux操作の実装
tmux_get_window_id_impl() {
    tmux display-message -p '#I' 2>/dev/null
}

tmux_get_pane_id_impl() {
    tmux display-message -p '#P' 2>/dev/null
}

tmux_get_pane_pid_impl() {
    local window_id="$1"
    local pane_id="$2"
    
    if [[ -n "$window_id" && -n "$pane_id" ]]; then
        tmux display-message -t "${window_id}.${pane_id}" -p '#{pane_pid}' 2>/dev/null
    else
        tmux display-message -p '#{pane_pid}' 2>/dev/null
    fi
}

tmux_capture_pane_impl() {
    local window_id="$1"
    local pane_id="$2"
    local lines="${3:-30}"
    
    tmux capture-pane -p -S "-$lines" -t "${window_id}.${pane_id}" 2>/dev/null
}

# プロセス管理の実装
process_find_impl() {
    local pattern="$1"
    pgrep -f "$pattern" 2>/dev/null
}

process_tree_impl() {
    local pid="$1"
    pstree -p "$pid" 2>/dev/null || echo ""
}

# ファイルシステム操作の実装
filesystem_has_command_impl() {
    local cmd="$1"
    command -v "$cmd" >/dev/null 2>&1
}

filesystem_read_file_impl() {
    local file_path="$1"
    [[ -f "$file_path" ]] && cat "$file_path"
}

filesystem_write_file_impl() {
    local file_path="$1"
    local content="$2"
    echo "$content" > "$file_path"
}

# === インターフェース呼び出し関数 ===

# tmux操作
call_tmux() {
    local operation="$1"
    shift
    
    local impl_func="${TMUX_INTERFACE[$operation]}"
    if [[ -n "$impl_func" ]] && declare -f "$impl_func" >/dev/null; then
        "$impl_func" "$@"
    else
        echo "ERROR: tmux operation '$operation' not implemented" >&2
        return 1
    fi
}

# プロセス管理
call_process() {
    local operation="$1"
    shift
    
    local impl_func="${PROCESS_INTERFACE[$operation]}"
    if [[ -n "$impl_func" ]] && declare -f "$impl_func" >/dev/null; then
        "$impl_func" "$@"
    else
        echo "ERROR: process operation '$operation' not implemented" >&2
        return 1
    fi
}

# ファイルシステム
call_filesystem() {
    local operation="$1"
    shift
    
    local impl_func="${FILESYSTEM_INTERFACE[$operation]}"
    if [[ -n "$impl_func" ]] && declare -f "$impl_func" >/dev/null; then
        "$impl_func" "$@"
    else
        echo "ERROR: filesystem operation '$operation' not implemented" >&2
        return 1
    fi
}

# === モック/テスト用インターフェース登録 ===

# テスト時のモック実装を登録する関数
register_mock_interface() {
    local interface_type="$1"
    local operation="$2"
    local mock_function="$3"
    
    case "$interface_type" in
        "tmux")
            TMUX_INTERFACE["$operation"]="$mock_function"
            ;;
        "process")
            PROCESS_INTERFACE["$operation"]="$mock_function"
            ;;
        "filesystem")
            FILESYSTEM_INTERFACE["$operation"]="$mock_function"
            ;;
        *)
            echo "ERROR: Unknown interface type '$interface_type'" >&2
            return 1
            ;;
    esac
}

# インターフェースをデフォルトに戻す
reset_interface() {
    local interface_type="$1"
    
    case "$interface_type" in
        "tmux")
            TMUX_INTERFACE=(
                ["get_window_id"]="tmux_get_window_id_impl"
                ["get_pane_id"]="tmux_get_pane_id_impl"
                ["get_pane_pid"]="tmux_get_pane_pid_impl"
                ["capture_pane"]="tmux_capture_pane_impl"
            )
            ;;
        "process")
            PROCESS_INTERFACE=(
                ["find_process"]="process_find_impl"
                ["get_process_tree"]="process_tree_impl"
            )
            ;;
        "filesystem")
            FILESYSTEM_INTERFACE=(
                ["has_command"]="filesystem_has_command_impl"
                ["read_file"]="filesystem_read_file_impl"
                ["write_file"]="filesystem_write_file_impl"
            )
            ;;
        "all")
            reset_interface "tmux"
            reset_interface "process"
            reset_interface "filesystem"
            ;;
    esac
}

# === デバッグ・検証用関数 ===

# 現在のインターフェース設定を表示
show_interface_config() {
    echo "=== Current Interface Configuration ==="
    echo "TMUX Interface:"
    for op in "${!TMUX_INTERFACE[@]}"; do
        echo "  $op -> ${TMUX_INTERFACE[$op]}"
    done
    
    echo "Process Interface:"
    for op in "${!PROCESS_INTERFACE[@]}"; do
        echo "  $op -> ${PROCESS_INTERFACE[$op]}"
    done
    
    echo "Filesystem Interface:"
    for op in "${!FILESYSTEM_INTERFACE[@]}"; do
        echo "  $op -> ${FILESYSTEM_INTERFACE[$op]}"
    done
}

# インターフェース機能のテスト
test_interface() {
    local interface_type="$1"
    local operation="$2"
    shift 2
    
    echo "Testing ${interface_type}::${operation} with args: $@"
    call_"$interface_type" "$operation" "$@"
}