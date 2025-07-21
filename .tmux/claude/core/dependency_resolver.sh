#!/bin/bash
# Dependency Resolver - モジュール依存関係解決システム
# 動的依存関係解決とロード順序最適化

# グローバル変数
declare -g -A MODULE_DEPENDENCIES=()
declare -g -A MODULE_STATUS=() # loaded, loading, failed, not_found
declare -g DEPENDENCY_RESOLVER_INITIALIZED="false"

# 依存関係解決システムの初期化
init_dependency_resolver() {
    if [[ "$DEPENDENCY_RESOLVER_INITIALIZED" == "true" ]]; then
        return 0
    fi

    # モジュール依存関係の定義
    define_module_dependencies

    DEPENDENCY_RESOLVER_INITIALIZED="true"
    log "DEBUG" "Dependency resolver initialized"
}

# モジュール依存関係の定義
define_module_dependencies() {
    # 基本モジュール（依存関係なし）
    MODULE_DEPENDENCIES["base"]=""
    MODULE_DEPENDENCIES["powershell_engine"]="base"
    MODULE_DEPENDENCIES["voice_engine_registry"]="base"

    # Windows特化モジュール（PowerShellエンジンに依存）
    MODULE_DEPENDENCIES["windows_tts_engine"]="base powershell_engine"
    MODULE_DEPENDENCIES["windows_audio_system"]="base powershell_engine"
    MODULE_DEPENDENCIES["windows_notification_system"]="base powershell_engine"
    MODULE_DEPENDENCIES["wsl_integration"]="base powershell_engine"

    # 音声統合モジュール
    MODULE_DEPENDENCIES["universal_voice"]="base voice_engine_registry"
    MODULE_DEPENDENCIES["wsl_voice_engine"]="base"

    # OS統合レイヤー（複数モジュールに依存）
    MODULE_DEPENDENCIES["windows"]="base powershell_engine windows_tts_engine windows_audio_system windows_notification_system wsl_integration"

    log "DEBUG" "Module dependencies defined"
}

# 依存関係グラフの検証
validate_dependency_graph() {
    local visited=()
    local recursion_stack=()

    for module in "${!MODULE_DEPENDENCIES[@]}"; do
        if ! check_circular_dependency "$module" visited recursion_stack; then
            log "ERROR" "Circular dependency detected involving module: $module"
            return 1
        fi
    done

    log "DEBUG" "Dependency graph validation passed"
    return 0
}

# 循環依存関係チェック（深度優先探索）
check_circular_dependency() {
    local module="$1"
    local -n visited_ref=$2
    local -n stack_ref=$3

    # 既に処理済みの場合はスキップ
    local module_visited=false
    for v in "${visited_ref[@]}"; do
        if [[ "$v" == "$module" ]]; then
            module_visited=true
            break
        fi
    done

    if [[ "$module_visited" == "true" ]]; then
        return 0
    fi

    # 再帰スタックに追加
    stack_ref+=("$module")

    # 依存関係を探索
    local deps="${MODULE_DEPENDENCIES[$module]}"
    if [[ -n "$deps" ]]; then
        for dep in $deps; do
            # スタック内に既に存在する場合は循環依存
            for s in "${stack_ref[@]}"; do
                if [[ "$s" == "$dep" ]]; then
                    log "ERROR" "Circular dependency: $module -> $dep"
                    return 1
                fi
            done

            # 再帰的にチェック
            if ! check_circular_dependency "$dep" visited_ref stack_ref; then
                return 1
            fi
        done
    fi

    # スタックから削除
    local new_stack=()
    for s in "${stack_ref[@]}"; do
        if [[ "$s" != "$module" ]]; then
            new_stack+=("$s")
        fi
    done
    stack_ref=("${new_stack[@]}")

    # 訪問済みに追加
    visited_ref+=("$module")

    return 0
}

# トポロジカルソート（依存関係順序でのロード順序決定）
get_load_order() {
    local target_modules=("$@")
    local load_order=()
    local in_degree=()
    local queue=()

    # 入次数の計算
    for module in "${target_modules[@]}"; do
        in_degree["$module"]=0
    done

    for module in "${target_modules[@]}"; do
        local deps="${MODULE_DEPENDENCIES[$module]}"
        if [[ -n "$deps" ]]; then
            for dep in $deps; do
                # 対象モジュール内での依存関係のみカウント
                for target in "${target_modules[@]}"; do
                    if [[ "$target" == "$dep" ]]; then
                        ((in_degree["$module"]++))
                        break
                    fi
                done
            done
        fi
    done

    # 入次数0のモジュールをキューに追加
    for module in "${target_modules[@]}"; do
        if [[ ${in_degree["$module"]} -eq 0 ]]; then
            queue+=("$module")
        fi
    done

    # トポロジカルソート実行
    while [[ ${#queue[@]} -gt 0 ]]; do
        local current="${queue[0]}"
        queue=("${queue[@]:1}")
        load_order+=("$current")

        # 依存先の入次数を減らす
        for module in "${target_modules[@]}"; do
            local deps="${MODULE_DEPENDENCIES[$module]}"
            if [[ -n "$deps" ]]; then
                for dep in $deps; do
                    if [[ "$dep" == "$current" ]]; then
                        ((in_degree["$module"]--))
                        if [[ ${in_degree["$module"]} -eq 0 ]]; then
                            queue+=("$module")
                        fi
                    fi
                done
            fi
        done
    done

    # 結果出力
    printf '%s\n' "${load_order[@]}"
}

# 依存関係を満たした順序でのモジュール読み込み
load_modules_with_dependencies() {
    local target_modules=("$@")
    local failed_modules=()

    log "DEBUG" "Loading modules with dependency resolution: ${target_modules[*]}"

    # 依存関係の検証
    if ! validate_dependency_graph; then
        log "ERROR" "Dependency graph validation failed"
        return 1
    fi

    # ロード順序の決定
    local load_order
    load_order=($(get_load_order "${target_modules[@]}"))

    log "DEBUG" "Determined load order: ${load_order[*]}"

    # 順序に従ってモジュールを読み込み
    for module in "${load_order[@]}"; do
        if ! load_module_with_status "$module"; then
            failed_modules+=("$module")
            log "ERROR" "Failed to load module: $module"
        fi
    done

    if [[ ${#failed_modules[@]} -gt 0 ]]; then
        log "ERROR" "Failed to load modules: ${failed_modules[*]}"
        return 1
    fi

    log "DEBUG" "All modules loaded successfully"
    return 0
}

# ステータス管理付きモジュール読み込み
load_module_with_status() {
    local module_name="$1"

    # 既に読み込み済みかチェック
    if [[ "${MODULE_STATUS[$module_name]}" == "loaded" ]]; then
        log "DEBUG" "Module $module_name already loaded"
        return 0
    fi

    # 読み込み中フラグ設定（循環依存検出用）
    if [[ "${MODULE_STATUS[$module_name]}" == "loading" ]]; then
        log "ERROR" "Circular dependency detected during loading: $module_name"
        return 1
    fi

    MODULE_STATUS["$module_name"]="loading"

    # 依存関係の事前読み込み
    local deps="${MODULE_DEPENDENCIES[$module_name]}"
    if [[ -n "$deps" ]]; then
        for dep in $deps; do
            if ! load_module_with_status "$dep"; then
                MODULE_STATUS["$module_name"]="failed"
                log "ERROR" "Failed to load dependency $dep for module $module_name"
                return 1
            fi
        done
    fi

    # 実際のモジュール読み込み
    if load_module "$module_name" false; then
        MODULE_STATUS["$module_name"]="loaded"
        log "DEBUG" "Module $module_name loaded successfully"
        return 0
    else
        MODULE_STATUS["$module_name"]="failed"
        log "ERROR" "Failed to load module $module_name"
        return 1
    fi
}

# 依存関係情報取得
get_dependency_info() {
    local module_name="$1"
    local format="${2:-text}"

    local deps="${MODULE_DEPENDENCIES[$module_name]}"
    local status="${MODULE_STATUS[$module_name]:-not_loaded}"

    case "$format" in
        "json")
            echo "{"
            echo "  \"module\": \"$module_name\","
            echo "  \"status\": \"$status\","
            echo "  \"dependencies\": [$(echo "$deps" | sed 's/ /", "/g' | sed 's/^/"/' | sed 's/$/"/' | sed 's/""//')]"
            echo "}"
            ;;
        "text")
            echo "Module: $module_name"
            echo "  Status: $status"
            echo "  Dependencies: ${deps:-none}"
            ;;
        *)
            log "ERROR" "Unknown format: $format"
            return 1
            ;;
    esac
}

# 全依存関係情報取得
get_all_dependencies() {
    local format="${1:-text}"

    case "$format" in
        "text")
            echo "=== Module Dependencies ==="
            for module in "${!MODULE_DEPENDENCIES[@]}"; do
                get_dependency_info "$module" "text"
                echo ""
            done
            ;;
        "json")
            echo "{"
            local first=true
            for module in "${!MODULE_DEPENDENCIES[@]}"; do
                if [[ "$first" == "true" ]]; then
                    first=false
                else
                    echo ","
                fi
                echo "  \"$module\": $(get_dependency_info "$module" "json" | tr -d '\n')"
            done
            echo ""
            echo "}"
            ;;
    esac
}

# 依存関係解決システムのテスト
test_dependency_resolver() {
    echo "=== Dependency Resolver Test ==="

    # 初期化テスト
    if init_dependency_resolver; then
        echo "✅ Dependency resolver initialization successful"
    else
        echo "❌ Dependency resolver initialization failed"
        return 1
    fi

    # 依存関係グラフ検証テスト
    if validate_dependency_graph; then
        echo "✅ Dependency graph validation passed"
    else
        echo "❌ Dependency graph validation failed"
        return 1
    fi

    # ロード順序テスト
    echo ""
    echo "Testing load order for Windows modules:"
    local windows_modules=("powershell_engine" "windows_tts_engine" "windows_audio_system")
    local load_order
    load_order=($(get_load_order "${windows_modules[@]}"))
    echo "Load order: ${load_order[*]}"

    # 依存関係情報表示
    echo ""
    echo "Module dependency information:"
    get_dependency_info "windows_tts_engine" "text"

    echo ""
    echo "Dependency resolver test completed"
    return 0
}

# 依存関係解決システムの自動初期化
if [[ -z "$DEPENDENCY_RESOLVER_INITIALIZED" ]] || [[ "$DEPENDENCY_RESOLVER_INITIALIZED" != "true" ]]; then
    init_dependency_resolver
fi
