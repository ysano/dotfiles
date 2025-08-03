#!/bin/bash
# Module Registry V2 - Clean Architecture Implementation
# クリーンアーキテクチャ実装のための改良されたモジュールレジストリ

# === アーキテクチャレイヤー定義 ===

# レイヤー1: Core/Domain Layer (最内層 - ビジネスロジック)
declare -A CORE_MODULES=(
    ["status_detector"]="core/status_detector.sh"
    ["interfaces"]="core/interfaces.sh"
    ["config_manager"]="core/config_manager.sh"
    ["error_handler"]="core/error_handler.sh"
)

# レイヤー2: Application Layer (アプリケーションサービス)
declare -A APPLICATION_MODULES=(
    ["voice_service"]="services/voice_service.sh"
    ["detection_service"]="services/detection_service.sh"
    ["notification_service"]="services/notification_service.sh"
    ["health_service"]="services/health_service.sh"
)

# レイヤー3: Infrastructure Layer (外部システム統合)
declare -A INFRASTRUCTURE_MODULES=(
    ["tmux_adapter"]="adapters/tmux_adapter.sh"
    ["os_adapter"]="adapters/os_adapter.sh"
    ["audio_adapter"]="adapters/audio_adapter.sh"
    ["process_adapter"]="adapters/process_adapter.sh"
)

# レイヤー4: Presentation Layer (UI/外部インターフェース)
declare -A PRESENTATION_MODULES=(
    ["cli_interface"]="presentation/cli_interface.sh"
    ["tmux_status"]="presentation/tmux_status.sh"
    ["debug_interface"]="presentation/debug_interface.sh"
)

# === モジュール依存関係マップ ===
declare -A MODULE_DEPENDENCIES=(
    # Core modules - no dependencies
    ["status_detector"]=""
    ["interfaces"]=""
    ["config_manager"]=""
    ["error_handler"]=""
    
    # Application modules - depend on core only
    ["voice_service"]="status_detector,interfaces,config_manager"
    ["detection_service"]="status_detector,interfaces,config_manager"
    ["notification_service"]="interfaces,config_manager"
    ["health_service"]="interfaces,config_manager,error_handler"
    
    # Infrastructure modules - depend on core and application
    ["tmux_adapter"]="interfaces"
    ["os_adapter"]="interfaces,config_manager"
    ["audio_adapter"]="interfaces,config_manager"
    ["process_adapter"]="interfaces"
    
    # Presentation modules - can depend on any layer but not each other
    ["cli_interface"]="voice_service,detection_service,health_service"
    ["tmux_status"]="detection_service,tmux_adapter"
    ["debug_interface"]="health_service,detection_service"
)

# === モジュールメタデータ ===
declare -A MODULE_METADATA=(
    ["status_detector"]="Pure functions for Claude status detection|core|high"
    ["interfaces"]="Dependency injection interfaces|core|high"
    ["config_manager"]="Configuration management|core|high"
    ["error_handler"]="Error handling utilities|core|medium"
    ["voice_service"]="Voice synthesis coordination|application|high"
    ["detection_service"]="Status detection coordination|application|high"
    ["notification_service"]="Notification dispatch service|application|medium"
    ["health_service"]="System health monitoring|application|medium"
    ["tmux_adapter"]="tmux integration adapter|infrastructure|high"
    ["os_adapter"]="Operating system adapter|infrastructure|high"
    ["audio_adapter"]="Audio system adapter|infrastructure|medium"
    ["process_adapter"]="Process management adapter|infrastructure|medium"
    ["cli_interface"]="Command-line interface|presentation|medium"
    ["tmux_status"]="tmux status bar integration|presentation|high"
    ["debug_interface"]="Debug and diagnostics interface|presentation|low"
)

# === レジストリ管理関数 ===

# モジュールレイヤーの取得
get_module_layer() {
    local module_name="$1"
    
    if [[ -n "${CORE_MODULES[$module_name]}" ]]; then
        echo "core"
    elif [[ -n "${APPLICATION_MODULES[$module_name]}" ]]; then
        echo "application"
    elif [[ -n "${INFRASTRUCTURE_MODULES[$module_name]}" ]]; then
        echo "infrastructure"
    elif [[ -n "${PRESENTATION_MODULES[$module_name]}" ]]; then
        echo "presentation"
    else
        echo "unknown"
    fi
}

# モジュールパスの取得
get_module_path() {
    local module_name="$1"
    local layer=$(get_module_layer "$module_name")
    
    case "$layer" in
        "core")
            echo "${CORE_MODULES[$module_name]}"
            ;;
        "application")
            echo "${APPLICATION_MODULES[$module_name]}"
            ;;
        "infrastructure")
            echo "${INFRASTRUCTURE_MODULES[$module_name]}"
            ;;
        "presentation")
            echo "${PRESENTATION_MODULES[$module_name]}"
            ;;
        *)
            return 1
            ;;
    esac
}

# モジュール依存関係の取得
get_module_dependencies() {
    local module_name="$1"
    echo "${MODULE_DEPENDENCIES[$module_name]}"
}

# モジュール読み込み順序の計算
calculate_load_order() {
    local target_modules=("$@")
    local load_order=()
    local visited=()
    
    # 依存関係解決（トポロジカルソート簡易版）
    resolve_dependencies() {
        local module="$1"
        
        # 既に訪問済みの場合はスキップ
        if [[ " ${visited[*]} " =~ " $module " ]]; then
            return 0
        fi
        
        # 依存関係を先に解決
        local deps=$(get_module_dependencies "$module")
        if [[ -n "$deps" ]]; then
            IFS=',' read -ra dep_array <<< "$deps"
            for dep in "${dep_array[@]}"; do
                resolve_dependencies "$dep"
            done
        fi
        
        # このモジュールを訪問済みに追加
        visited+=("$module")
        load_order+=("$module")
    }
    
    # 全ターゲットモジュールについて依存関係を解決
    for module in "${target_modules[@]}"; do
        resolve_dependencies "$module"
    done
    
    echo "${load_order[@]}"
}

# レイヤー別モジュール一覧の取得
get_modules_by_layer() {
    local layer="$1"
    local modules=()
    
    case "$layer" in
        "core")
            modules=("${!CORE_MODULES[@]}")
            ;;
        "application")
            modules=("${!APPLICATION_MODULES[@]}")
            ;;
        "infrastructure")
            modules=("${!INFRASTRUCTURE_MODULES[@]}")
            ;;
        "presentation")
            modules=("${!PRESENTATION_MODULES[@]}")
            ;;
        "all")
            modules=(
                "${!CORE_MODULES[@]}"
                "${!APPLICATION_MODULES[@]}"
                "${!INFRASTRUCTURE_MODULES[@]}"
                "${!PRESENTATION_MODULES[@]}"
            )
            ;;
    esac
    
    echo "${modules[@]}"
}

# === アーキテクチャ検証関数 ===

# 依存関係の方向性チェック（内層から外層への依存は禁止）
validate_dependency_direction() {
    local errors=()
    
    # レイヤー順序定義
    declare -A layer_order=(["core"]=1 ["application"]=2 ["infrastructure"]=3 ["presentation"]=4)
    
    for module in $(get_modules_by_layer "all"); do
        local module_layer=$(get_module_layer "$module")
        local module_order=${layer_order[$module_layer]}
        
        local deps=$(get_module_dependencies "$module")
        if [[ -n "$deps" ]]; then
            IFS=',' read -ra dep_array <<< "$deps"
            for dep in "${dep_array[@]}"; do
                local dep_layer=$(get_module_layer "$dep")
                local dep_order=${layer_order[$dep_layer]}
                
                # 内層から外層への依存をチェック
                if [[ $dep_order -gt $module_order ]]; then
                    errors+=("VIOLATION: $module ($module_layer) depends on $dep ($dep_layer)")
                fi
            done
        fi
    done
    
    if [[ ${#errors[@]} -gt 0 ]]; then
        echo "Architecture violations found:"
        printf '%s\n' "${errors[@]}"
        return 1
    else
        echo "Architecture validation passed"
        return 0
    fi
}

# 循環依存チェック
detect_circular_dependencies() {
    local errors=()
    local visiting=()
    local visited=()
    
    check_circular() {
        local module="$1"
        
        # 現在訪問中の場合は循環依存
        if [[ " ${visiting[*]} " =~ " $module " ]]; then
            errors+=("CIRCULAR: ${visiting[*]} -> $module")
            return 1
        fi
        
        # 既に完了している場合はスキップ
        if [[ " ${visited[*]} " =~ " $module " ]]; then
            return 0
        fi
        
        # 訪問開始
        visiting+=("$module")
        
        # 依存関係をチェック
        local deps=$(get_module_dependencies "$module")
        if [[ -n "$deps" ]]; then
            IFS=',' read -ra dep_array <<< "$deps"
            for dep in "${dep_array[@]}"; do
                check_circular "$dep"
            done
        fi
        
        # 訪問完了
        visited+=("$module")
        visiting=("${visiting[@]/$module}")
    }
    
    for module in $(get_modules_by_layer "all"); do
        check_circular "$module"
    done
    
    if [[ ${#errors[@]} -gt 0 ]]; then
        echo "Circular dependencies found:"
        printf '%s\n' "${errors[@]}"
        return 1
    else
        echo "No circular dependencies detected"
        return 0
    fi
}

# === レポート生成関数 ===

# アーキテクチャサマリーの表示
show_architecture_summary() {
    echo "=== Claude Voice Architecture Summary ==="
    echo ""
    
    for layer in "core" "application" "infrastructure" "presentation"; do
        echo "Layer: $layer"
        local modules=($(get_modules_by_layer "$layer"))
        for module in "${modules[@]}"; do
            local metadata="${MODULE_METADATA[$module]}"
            IFS='|' read -ra meta_parts <<< "$metadata"
            local description="${meta_parts[0]}"
            local priority="${meta_parts[2]}"
            printf "  %-20s %s [%s]\n" "$module" "$description" "$priority"
        done
        echo ""
    done
}

# 依存関係グラフの表示
show_dependency_graph() {
    echo "=== Dependency Graph ==="
    echo ""
    
    for module in $(get_modules_by_layer "all"); do
        local deps=$(get_module_dependencies "$module")
        local layer=$(get_module_layer "$module")
        
        if [[ -n "$deps" ]]; then
            echo "$module [$layer] -> $deps"
        else
            echo "$module [$layer] -> (no dependencies)"
        fi
    done
}

# === テスト関数 ===

run_architecture_tests() {
    echo "=== Architecture Validation ==="
    echo ""
    
    echo "1. Dependency Direction Validation:"
    validate_dependency_direction
    echo ""
    
    echo "2. Circular Dependency Detection:"
    detect_circular_dependencies
    echo ""
    
    echo "3. Load Order Calculation Test:"
    local test_modules=("tmux_status" "cli_interface")
    local order=($(calculate_load_order "${test_modules[@]}"))
    echo "Load order for ${test_modules[*]}: ${order[*]}"
    echo ""
}

# === メイン実行 ===

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-summary}" in
        "summary")
            show_architecture_summary
            ;;
        "dependencies")
            show_dependency_graph
            ;;
        "validate")
            run_architecture_tests
            ;;
        "test")
            run_architecture_tests
            ;;
        *)
            echo "Usage: $0 [summary|dependencies|validate|test]"
            ;;
    esac
fi