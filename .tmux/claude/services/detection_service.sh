#!/bin/bash
# Detection Service - Application Layer
# ステータス検出のアプリケーションサービス

# 依存関係のインポート
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../core/interfaces.sh"
source "$SCRIPT_DIR/../core/status_detector.sh"
source "$SCRIPT_DIR/../core/config_manager_v2.sh"
source "$SCRIPT_DIR/../core/cache_manager.sh"

# === Detection Service Interface ===

# サービス設定
declare -A DETECTION_CONFIG=(
    ["cache_ttl"]="5"                    # キャッシュ有効期間（秒）
    ["retry_count"]="3"                  # リトライ回数
    ["timeout"]="2"                      # タイムアウト（秒）
    ["debug_mode"]="false"               # デバッグモード
)

# キャッシュストレージ
declare -A DETECTION_CACHE=()
declare -A CACHE_TIMESTAMPS=()

# === コア検出サービス ===

# 統合ステータス検出（高度なキャッシュ・リトライ機能付き）
detect_status_with_cache() {
    local window_id="${1:-auto}"
    local pane_id="${2:-auto}"
    local use_cache="${3:-true}"
    
    # キャッシュキーの生成
    local cache_key="detection_${window_id}_${pane_id}"
    
    # キャッシュチェック
    if [[ "$use_cache" == "true" ]]; then
        local cached_result=$(cache_get "$cache_key" 2>/dev/null)
        if [[ $? -eq 0 && -n "$cached_result" ]]; then
            [[ "${DETECTION_CONFIG[debug_mode]}" == "true" ]] && 
                echo "[DEBUG] Cache hit for $cache_key: $cached_result" >&2
            echo "$cached_result"
            return 0
        fi
    fi
    
    # 実際の検出実行（リトライ機能付き）
    local result=""
    local retry_count="${DETECTION_CONFIG[retry_count]}"
    
    for ((attempt=1; attempt<=retry_count; attempt++)); do
        # タイムアウト機能付き検出
        local timeout_duration="${DETECTION_CONFIG[timeout]}"
        
        if result=$(timeout "${timeout_duration}s" detect_claude_status_with_deps "$window_id" "$pane_id" 2>/dev/null); then
            # 成功した場合は高度なキャッシュに保存
            local cache_ttl="${DETECTION_CONFIG[cache_ttl]}"
            cache_set "$cache_key" "$result" "$cache_ttl"
            
            [[ "${DETECTION_CONFIG[debug_mode]}" == "true" ]] && 
                echo "[DEBUG] Detection succeeded on attempt $attempt: $result" >&2
            
            echo "$result"
            return 0
        fi
        
        [[ "${DETECTION_CONFIG[debug_mode]}" == "true" ]] && 
            echo "[DEBUG] Detection attempt $attempt failed" >&2
        
        # 最後の試行でない場合は短時間待機
        if [[ $attempt -lt $retry_count ]]; then
            sleep 0.1
        fi
    done
    
    # 全ての試行が失敗した場合
    [[ "${DETECTION_CONFIG[debug_mode]}" == "true" ]] && 
        echo "[DEBUG] All detection attempts failed" >&2
    
    echo ""
    return 1
}

# バッチ検出（複数paneの同時処理）
detect_multiple_panes() {
    local pane_specs=("$@")  # "window_id:pane_id" format
    local results=()
    
    for spec in "${pane_specs[@]}"; do
        IFS=':' read -ra parts <<< "$spec"
        local window_id="${parts[0]}"
        local pane_id="${parts[1]}"
        
        local result=$(detect_status_with_cache "$window_id" "$pane_id")
        results+=("$spec:$result")
    done
    
    printf '%s\n' "${results[@]}"
}

# === 高水準検出API ===

# 現在のpaneのステータス検出
detect_current_pane() {
    detect_status_with_cache "auto" "auto" "true"
}

# 指定paneのステータス検出（キャッシュなし）
detect_pane_direct() {
    local window_id="$1"
    local pane_id="$2"
    detect_status_with_cache "$window_id" "$pane_id" "false"
}

# 全tmux paneのステータス検出
detect_all_panes() {
    local pane_list=()
    
    # tmux経由で全pane情報を取得
    if command -v tmux >/dev/null 2>&1; then
        while IFS= read -r line; do
            # Format: window_id:pane_id
            pane_list+=("$line")
        done < <(tmux list-panes -a -F "#{window_id}:#{pane_id}" 2>/dev/null)
    fi
    
    if [[ ${#pane_list[@]} -gt 0 ]]; then
        detect_multiple_panes "${pane_list[@]}"
    else
        echo "No tmux panes found"
        return 1
    fi
}

# === ステータス変更検出 ===

# ステータス変更の監視
monitor_status_changes() {
    local window_id="${1:-auto}"
    local pane_id="${2:-auto}"
    local interval="${3:-1}"
    local callback_function="${4:-on_status_change}"
    
    local previous_status=""
    local current_status=""
    
    echo "[INFO] Starting status monitoring for ${window_id}:${pane_id} (interval: ${interval}s)"
    
    while true; do
        current_status=$(detect_status_with_cache "$window_id" "$pane_id" "false")
        
        if [[ "$current_status" != "$previous_status" ]] && [[ -n "$previous_status" ]]; then
            # ステータス変更を検出
            if declare -f "$callback_function" >/dev/null; then
                "$callback_function" "$previous_status" "$current_status" "$window_id" "$pane_id"
            else
                echo "[CHANGE] Status changed from '$previous_status' to '$current_status'"
            fi
        fi
        
        previous_status="$current_status"
        sleep "$interval"
    done
}

# デフォルトのステータス変更コールバック
on_status_change() {
    local old_status="$1"
    local new_status="$2"
    local window_id="$3"
    local pane_id="$4"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    echo "[$timestamp] Status change detected (${window_id}:${pane_id}): $old_status → $new_status"
}

# === 設定管理 ===

# サービス設定の更新
configure_detection_service() {
    local key="$1"
    local value="$2"
    
    if [[ -n "${DETECTION_CONFIG[$key]}" ]]; then
        DETECTION_CONFIG["$key"]="$value"
        echo "Updated $key = $value"
    else
        echo "Unknown configuration key: $key"
        return 1
    fi
}

# 現在の設定表示
show_detection_config() {
    echo "=== Detection Service Configuration ==="
    for key in "${!DETECTION_CONFIG[@]}"; do
        echo "  $key = ${DETECTION_CONFIG[$key]}"
    done
}

# === キャッシュ管理 ===

# キャッシュクリア（高度なキャッシュマネージャー使用）
clear_detection_cache() {
    cache_clear_all
    # 従来のキャッシュもクリア（後方互換性）
    DETECTION_CACHE=()
    CACHE_TIMESTAMPS=()
    echo "Detection cache cleared"
}

# キャッシュ統計表示（高度なキャッシュマネージャー統合）
show_cache_stats() {
    echo "=== Detection Service Cache Statistics ==="
    
    # 高度なキャッシュマネージャーの統計
    show_cache_statistics
    
    # 従来のキャッシュ統計（後方互換性）
    local legacy_cache_count=${#DETECTION_CACHE[@]}
    if [[ $legacy_cache_count -gt 0 ]]; then
        echo ""
        echo "Legacy Cache:"
        echo "  Entries: $legacy_cache_count"
        echo "  TTL: ${DETECTION_CONFIG[cache_ttl]}s"
    fi
}

# === テスト・デバッグ機能 ===

# サービステスト
test_detection_service() {
    echo "=== Detection Service Test ==="
    
    # 設定テスト
    echo "1. Configuration Test:"
    show_detection_config
    echo ""
    
    # 基本検出テスト
    echo "2. Basic Detection Test:"
    local result=$(detect_current_pane)
    echo "  Current pane status: $result"
    echo ""
    
    # キャッシュテスト（高度なキャッシュマネージャー）
    echo "3. Enhanced Cache Test:"
    local start_time=$(date +%s%N)
    detect_current_pane >/dev/null
    local end_time=$(date +%s%N)
    local first_duration=$(( (end_time - start_time) / 1000000 ))
    
    start_time=$(date +%s%N)
    detect_current_pane >/dev/null
    end_time=$(date +%s%N)
    local cached_duration=$(( (end_time - start_time) / 1000000 ))
    
    echo "  First call: ${first_duration}ms"
    echo "  Cached call: ${cached_duration}ms"
    echo "  Cache speedup: $((first_duration - cached_duration))ms"
    
    # キャッシュマネージャーのテスト
    echo "  Testing cache manager directly..."
    cache_set "test_detection" "Busy" "10"
    local test_result=$(cache_get "test_detection")
    if [[ "$test_result" == "Busy" ]]; then
        echo "  ✅ Cache manager integration working"
    else
        echo "  ❌ Cache manager integration failed"
    fi
    echo ""
    
    # キャッシュ統計
    echo "4. Cache Statistics:"
    show_cache_stats
}

# デバッグモード切り替え
toggle_debug_mode() {
    if [[ "${DETECTION_CONFIG[debug_mode]}" == "true" ]]; then
        DETECTION_CONFIG["debug_mode"]="false"
        echo "Debug mode disabled"
    else
        DETECTION_CONFIG["debug_mode"]="true"
        echo "Debug mode enabled"
    fi
}

# === メイン実行 ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_detection_service
            ;;
        "monitor")
            monitor_status_changes "${2:-auto}" "${3:-auto}" "${4:-1}"
            ;;
        "config")
            show_detection_config
            ;;
        "cache")
            show_cache_stats
            ;;
        "clear-cache")
            clear_detection_cache
            ;;
        "debug")
            toggle_debug_mode
            ;;
        *)
            echo "Usage: $0 [test|monitor|config|cache|clear-cache|debug]"
            ;;
    esac
fi