#!/bin/bash
# Cache Manager - Performance Optimization Layer
# パフォーマンス最適化のためのキャッシュ管理システム

# === Cache Configuration ===

declare -A CACHE_CONFIG=(
    ["default_ttl"]="5"              # デフォルトTTL（秒）
    ["max_entries"]="1000"           # 最大エントリ数
    ["cleanup_interval"]="60"        # クリーンアップ間隔（秒）
    ["enable_compression"]="false"   # 圧縮有効化
    ["enable_persistence"]="false"   # 永続化有効化
    ["cache_directory"]="/tmp/claude_cache"
)

# === In-Memory Cache Storage ===

declare -A MEMORY_CACHE=()          # キー => 値
declare -A CACHE_METADATA=()        # キー => メタデータ（timestamp|ttl|size）
declare -A CACHE_STATISTICS=()      # 統計情報

# 統計初期化
CACHE_STATISTICS=(
    ["hits"]="0"
    ["misses"]="0"
    ["evictions"]="0"
    ["writes"]="0"
    ["last_cleanup"]="0"
)

# === Core Cache Functions ===

# キャッシュエントリの設定
cache_set() {
    local key="$1"
    local value="$2"
    local ttl="${3:-${CACHE_CONFIG[default_ttl]}}"
    local compress="${4:-${CACHE_CONFIG[enable_compression]}}"
    
    local timestamp=$(date +%s)
    local size=${#value}
    
    # 圧縮処理（必要に応じて）
    local stored_value="$value"
    if [[ "$compress" == "true" ]]; then
        stored_value=$(echo "$value" | gzip -c | base64 -w 0)
        size=${#stored_value}
    fi
    
    # メモリ使用量チェック
    if ! check_memory_limits "$size"; then
        perform_cache_eviction
    fi
    
    # キャッシュエントリの保存
    MEMORY_CACHE["$key"]="$stored_value"
    CACHE_METADATA["$key"]="$timestamp|$ttl|$size|$compress"
    
    # 統計更新
    CACHE_STATISTICS["writes"]=$((CACHE_STATISTICS["writes"] + 1))
    
    # 永続化（設定に応じて）
    if [[ "${CACHE_CONFIG[enable_persistence]}" == "true" ]]; then
        persist_cache_entry "$key" "$stored_value" "$timestamp" "$ttl" "$size"
    fi
    
    return 0
}

# キャッシュエントリの取得
cache_get() {
    local key="$1"
    local current_time=$(date +%s)
    
    # エントリ存在確認
    if [[ -z "${MEMORY_CACHE[$key]:-}" ]]; then
        # 永続化されたエントリのチェック
        if [[ "${CACHE_CONFIG[enable_persistence]}" == "true" ]]; then
            if restore_cache_entry "$key"; then
                return 0  # 復元成功、再帰呼び出し回避のため関数を抜ける
            fi
        fi
        
        CACHE_STATISTICS["misses"]=$((CACHE_STATISTICS["misses"] + 1))
        return 1
    fi
    
    # TTL確認
    local metadata="${CACHE_METADATA[$key]:-}"
    if [[ -z "$metadata" ]]; then
        CACHE_STATISTICS["misses"]=$((CACHE_STATISTICS["misses"] + 1))
        return 1
    fi
    
    IFS='|' read -ra meta_parts <<< "$metadata"
    local stored_time="${meta_parts[0]}"
    local ttl="${meta_parts[1]}"
    local size="${meta_parts[2]}"
    local is_compressed="${meta_parts[3]:-false}"
    
    if [[ $((current_time - stored_time)) -gt $ttl ]]; then
        # 期限切れエントリの削除
        cache_delete "$key"
        CACHE_STATISTICS["misses"]=$((CACHE_STATISTICS["misses"] + 1))
        return 1
    fi
    
    # 値の取得と展開
    local value="${MEMORY_CACHE[$key]}"
    if [[ "$is_compressed" == "true" ]]; then
        value=$(echo "$value" | base64 -d | gunzip)
    fi
    
    # ヒット統計更新
    CACHE_STATISTICS["hits"]=$((CACHE_STATISTICS["hits"] + 1))
    
    echo "$value"
    return 0
}

# キャッシュエントリの削除
cache_delete() {
    local key="$1"
    
    if [[ -n "${MEMORY_CACHE[$key]:-}" ]]; then
        unset MEMORY_CACHE["$key"]
        unset CACHE_METADATA["$key"]
        
        # 永続化ファイルも削除
        if [[ "${CACHE_CONFIG[enable_persistence]}" == "true" ]]; then
            rm -f "${CACHE_CONFIG[cache_directory]}/${key}.cache"
        fi
        
        return 0
    fi
    
    return 1
}

# キャッシュの存在確認
cache_exists() {
    local key="$1"
    local current_time=$(date +%s)
    
    if [[ -z "${MEMORY_CACHE[$key]:-}" ]]; then
        return 1
    fi
    
    # TTL確認
    local metadata="${CACHE_METADATA[$key]:-}"
    if [[ -z "$metadata" ]]; then
        return 1
    fi
    
    IFS='|' read -ra meta_parts <<< "$metadata"
    local stored_time="${meta_parts[0]}"
    local ttl="${meta_parts[1]}"
    
    if [[ $((current_time - stored_time)) -gt $ttl ]]; then
        return 1
    fi
    
    return 0
}

# === Memory Management ===

# メモリ制限チェック
check_memory_limits() {
    local new_entry_size="$1"
    local current_entries=${#MEMORY_CACHE[@]}
    local max_entries="${CACHE_CONFIG[max_entries]}"
    
    # エントリ数制限
    if [[ $current_entries -ge $max_entries ]]; then
        return 1
    fi
    
    # メモリ使用量チェック（簡易版）
    local total_size=0
    for key in "${!CACHE_METADATA[@]}"; do
        local metadata="${CACHE_METADATA[$key]:-}"
        if [[ -n "$metadata" ]]; then
            IFS='|' read -ra meta_parts <<< "$metadata"
            local size="${meta_parts[2]:-0}"
            if [[ "$size" =~ ^[0-9]+$ ]]; then
                total_size=$((total_size + size))
            fi
        fi
    done
    
    # 100MB制限（簡易）
    local max_memory_kb=102400
    local total_kb=$((total_size / 1024))
    
    if [[ $total_kb -gt $max_memory_kb ]]; then
        return 1
    fi
    
    return 0
}

# キャッシュエビクション（LRU風）
perform_cache_eviction() {
    local eviction_count=0
    local target_evictions=$((${#MEMORY_CACHE[@]} / 4))  # 25%を削除
    
    # タイムスタンプでソートして古いものから削除
    local sorted_keys=()
    for key in "${!CACHE_METADATA[@]}"; do
        local metadata="${CACHE_METADATA[$key]}"
        IFS='|' read -ra meta_parts <<< "$metadata"
        local timestamp="${meta_parts[0]}"
        sorted_keys+=("$timestamp:$key")
    done
    
    # ソート
    IFS=$'\n' sorted_keys=($(sort <<< "${sorted_keys[*]}"))
    unset IFS
    
    # 古いエントリから削除
    for entry in "${sorted_keys[@]}"; do
        if [[ $eviction_count -ge $target_evictions ]]; then
            break
        fi
        
        local key="${entry#*:}"
        cache_delete "$key"
        ((eviction_count++))
    done
    
    CACHE_STATISTICS["evictions"]=$((CACHE_STATISTICS["evictions"] + eviction_count))
}

# === Persistence Support ===

# キャッシュエントリの永続化
persist_cache_entry() {
    local key="$1"
    local value="$2"
    local timestamp="$3"
    local ttl="$4"
    local size="$5"
    
    local cache_dir="${CACHE_CONFIG[cache_directory]}"
    mkdir -p "$cache_dir"
    
    local cache_file="$cache_dir/${key}.cache"
    
    {
        echo "timestamp=$timestamp"
        echo "ttl=$ttl"
        echo "size=$size"
        echo "---"
        echo "$value"
    } > "$cache_file"
}

# 永続化されたエントリの復元
restore_cache_entry() {
    local key="$1"
    local cache_file="${CACHE_CONFIG[cache_directory]}/${key}.cache"
    
    if [[ ! -f "$cache_file" ]]; then
        return 1
    fi
    
    local timestamp=""
    local ttl=""
    local size=""
    local value=""
    local reading_value=false
    
    while IFS= read -r line; do
        if [[ "$line" == "---" ]]; then
            reading_value=true
            continue
        fi
        
        if [[ "$reading_value" == "true" ]]; then
            if [[ -n "$value" ]]; then
                value+=$'\n'
            fi
            value+="$line"
        else
            case "$line" in
                timestamp=*)
                    timestamp="${line#timestamp=}"
                    ;;
                ttl=*)
                    ttl="${line#ttl=}"
                    ;;
                size=*)
                    size="${line#size=}"
                    ;;
            esac
        fi
    done < "$cache_file"
    
    # TTL確認
    local current_time=$(date +%s)
    if [[ $((current_time - timestamp)) -gt $ttl ]]; then
        rm -f "$cache_file"
        return 1
    fi
    
    # メモリキャッシュに復元
    MEMORY_CACHE["$key"]="$value"
    CACHE_METADATA["$key"]="$timestamp|$ttl|$size|false"
    
    return 0
}

# === Maintenance and Cleanup ===

# 期限切れエントリのクリーンアップ
cleanup_expired_entries() {
    local current_time=$(date +%s)
    local cleaned_count=0
    local keys_to_delete=()
    
    # 期限切れエントリを特定
    for key in "${!CACHE_METADATA[@]}"; do
        local metadata="${CACHE_METADATA[$key]}"
        IFS='|' read -ra meta_parts <<< "$metadata"
        local stored_time="${meta_parts[0]}"
        local ttl="${meta_parts[1]}"
        
        if [[ $((current_time - stored_time)) -gt $ttl ]]; then
            keys_to_delete+=("$key")
        fi
    done
    
    # 削除実行
    for key in "${keys_to_delete[@]}"; do
        cache_delete "$key"
        ((cleaned_count++))
    done
    
    CACHE_STATISTICS["last_cleanup"]="$current_time"
    
    if [[ $cleaned_count -gt 0 ]]; then
        echo "Cleaned up $cleaned_count expired entries"
    fi
    
    return 0
}

# 全キャッシュクリア
cache_clear_all() {
    MEMORY_CACHE=()
    CACHE_METADATA=()
    
    # 永続化ファイルも削除
    if [[ "${CACHE_CONFIG[enable_persistence]}" == "true" ]]; then
        rm -rf "${CACHE_CONFIG[cache_directory]}"
    fi
    
    # 統計リセット（一部保持）
    local total_operations=$((CACHE_STATISTICS["hits"] + CACHE_STATISTICS["misses"]))
    CACHE_STATISTICS=(
        ["hits"]="0"
        ["misses"]="0"
        ["evictions"]="0"
        ["writes"]="0"
        ["last_cleanup"]="$(date +%s)"
        ["total_operations_before_clear"]="$total_operations"
    )
    
    echo "Cache cleared successfully"
}

# === Configuration Management ===

# キャッシュ設定の更新
configure_cache() {
    local key="$1"
    local value="$2"
    
    if [[ -n "${CACHE_CONFIG[$key]}" ]]; then
        CACHE_CONFIG["$key"]="$value"
        echo "Updated cache config: $key = $value"
        
        # 設定変更に応じた処理
        case "$key" in
            "enable_persistence")
                if [[ "$value" == "true" ]]; then
                    mkdir -p "${CACHE_CONFIG[cache_directory]}"
                fi
                ;;
            "max_entries")
                # エントリ数チェック
                if [[ ${#MEMORY_CACHE[@]} -gt $value ]]; then
                    perform_cache_eviction
                fi
                ;;
        esac
    else
        echo "Unknown cache configuration key: $key"
        return 1
    fi
}

# === Statistics and Monitoring ===

# キャッシュ統計の表示
show_cache_statistics() {
    local current_entries=${#MEMORY_CACHE[@]}
    local total_operations=$((CACHE_STATISTICS["hits"] + CACHE_STATISTICS["misses"]))
    local hit_rate=0
    
    if [[ $total_operations -gt 0 ]]; then
        hit_rate=$(( (CACHE_STATISTICS["hits"] * 100) / total_operations ))
    fi
    
    echo "=== Cache Statistics ==="
    echo "Entries: $current_entries / ${CACHE_CONFIG[max_entries]}"
    echo "Hits: ${CACHE_STATISTICS[hits]}"
    echo "Misses: ${CACHE_STATISTICS[misses]}"
    echo "Hit Rate: ${hit_rate}%"
    echo "Writes: ${CACHE_STATISTICS[writes]}"
    echo "Evictions: ${CACHE_STATISTICS[evictions]}"
    echo "Last Cleanup: $(date -d "@${CACHE_STATISTICS[last_cleanup]}" 2>/dev/null || echo 'Never')"
    
    # メモリ使用量概算
    local total_size=0
    for key in "${!CACHE_METADATA[@]}"; do
        local metadata="${CACHE_METADATA[$key]}"
        IFS='|' read -ra meta_parts <<< "$metadata"
        local size="${meta_parts[2]:-0}"
        total_size=$((total_size + size))
    done
    
    echo "Memory Usage: $((total_size / 1024)) KB"
}

# アクティブエントリ一覧
list_cache_entries() {
    local filter_pattern="${1:-.*}"
    local current_time=$(date +%s)
    
    echo "=== Active Cache Entries ==="
    printf "%-30s %-10s %-8s %-10s\n" "Key" "Age(s)" "TTL(s)" "Size(B)"
    echo "--------------------------------------------------------"
    
    for key in "${!CACHE_METADATA[@]}"; do
        if [[ "$key" =~ $filter_pattern ]]; then
            local metadata="${CACHE_METADATA[$key]}"
            IFS='|' read -ra meta_parts <<< "$metadata"
            local timestamp="${meta_parts[0]}"
            local ttl="${meta_parts[1]}"
            local size="${meta_parts[2]}"
            local age=$((current_time - timestamp))
            
            printf "%-30s %-10s %-8s %-10s\n" "$key" "$age" "$ttl" "$size"
        fi
    done
}

# === Test Functions ===

# キャッシュ機能テスト
test_cache_manager() {
    echo "=== Cache Manager Test ==="
    echo ""
    
    # 基本操作テスト
    echo "1. Basic Operations Test:"
    cache_set "test_key1" "test_value1" "10"
    
    if cache_exists "test_key1"; then
        echo "  ✅ Cache entry exists"
    else
        echo "  ❌ Cache entry not found"
    fi
    
    local value=$(cache_get "test_key1")
    if [[ "$value" == "test_value1" ]]; then
        echo "  ✅ Value retrieval successful"
    else
        echo "  ❌ Value retrieval failed: $value"
    fi
    echo ""
    
    # TTL テスト
    echo "2. TTL Test:"
    cache_set "ttl_test" "short_lived" "1"
    echo "  Waiting 2 seconds..."
    sleep 2
    
    if ! cache_exists "ttl_test"; then
        echo "  ✅ TTL expiration working"
    else
        echo "  ❌ TTL expiration failed"
    fi
    echo ""
    
    # パフォーマンステスト
    echo "3. Performance Test:"
    local iterations=1000
    local start_time=$(date +%s%N)
    
    for ((i=1; i<=iterations; i++)); do
        cache_set "perf_$i" "value_$i" "60" >/dev/null
    done
    
    local end_time=$(date +%s%N)
    local write_duration=$(( (end_time - start_time) / 1000000 ))
    
    start_time=$(date +%s%N)
    for ((i=1; i<=iterations; i++)); do
        cache_get "perf_$i" >/dev/null
    done
    end_time=$(date +%s%N)
    local read_duration=$(( (end_time - start_time) / 1000000 ))
    
    echo "  Write performance: ${write_duration}ms for $iterations entries"
    echo "  Read performance: ${read_duration}ms for $iterations entries"
    echo ""
    
    # 統計表示
    echo "4. Statistics:"
    show_cache_statistics
    
    # クリーンアップ
    cache_clear_all >/dev/null
}

# === Main Execution ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-test}" in
        "test")
            test_cache_manager
            ;;
        "stats")
            show_cache_statistics
            ;;
        "list")
            list_cache_entries "${2:-.*}"
            ;;
        "cleanup")
            cleanup_expired_entries
            ;;
        "clear")
            cache_clear_all
            ;;
        "config")
            if [[ -n "$2" && -n "$3" ]]; then
                configure_cache "$2" "$3"
            else
                echo "Current cache configuration:"
                for key in "${!CACHE_CONFIG[@]}"; do
                    echo "  $key = ${CACHE_CONFIG[$key]}"
                done
            fi
            ;;
        *)
            echo "Usage: $0 [test|stats|list|cleanup|clear|config]"
            ;;
    esac
fi