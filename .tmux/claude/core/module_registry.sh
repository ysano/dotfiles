#!/bin/bash
# Module Registry - モジュール登録・管理システム
# モジュールのメタデータ管理と動的ロード制御

# グローバル変数
declare -g -A MODULE_REGISTRY=()
declare -g -A MODULE_METADATA=()
declare -g MODULE_REGISTRY_INITIALIZED="false"

# モジュールレジストリの初期化
init_module_registry() {
    if [[ "$MODULE_REGISTRY_INITIALIZED" == "true" ]]; then
        return 0
    fi

    # コアモジュールの登録
    register_core_modules

    MODULE_REGISTRY_INITIALIZED="true"
    log "DEBUG" "Module registry initialized"
}

# コアモジュールの登録
register_core_modules() {
    # 基盤モジュール
    register_module "base" \
        "category:foundation" \
        "description:Basic utilities and logging" \
        "version:1.0" \
        "required:true"

    register_module "module_loader" \
        "category:foundation" \
        "description:Module loading and dependency management" \
        "version:1.0" \
        "required:false"

    register_module "dependency_resolver" \
        "category:foundation" \
        "description:Dependency resolution and load ordering" \
        "version:1.0" \
        "required:false"

    # PowerShell統合モジュール
    register_module "powershell_engine" \
        "category:engine" \
        "description:PowerShell detection and execution" \
        "version:1.0" \
        "required:false" \
        "platform:windows,wsl"

    # Windows特化モジュール
    register_module "windows_tts_engine" \
        "category:voice" \
        "description:Windows Text-to-Speech engine" \
        "version:1.0" \
        "required:false" \
        "platform:windows,wsl" \
        "dependencies:powershell_engine"

    register_module "windows_audio_system" \
        "category:audio" \
        "description:Windows audio and sound management" \
        "version:1.0" \
        "required:false" \
        "platform:windows,wsl" \
        "dependencies:powershell_engine"

    register_module "windows_notification_system" \
        "category:notification" \
        "description:Windows notification and toast messages" \
        "version:1.0" \
        "required:false" \
        "platform:windows,wsl" \
        "dependencies:powershell_engine"

    register_module "wsl_integration" \
        "category:integration" \
        "description:WSL environment integration" \
        "version:1.0" \
        "required:false" \
        "platform:wsl" \
        "dependencies:powershell_engine"

    # 音声統合モジュール
    register_module "voice_engine_registry" \
        "category:voice" \
        "description:Voice engine registration and management" \
        "version:1.0" \
        "required:false"

    register_module "universal_voice" \
        "category:voice" \
        "description:Universal voice interface" \
        "version:1.0" \
        "required:false" \
        "dependencies:voice_engine_registry"

    register_module "wsl_voice_engine" \
        "category:voice" \
        "description:WSL-specific voice engine" \
        "version:1.0" \
        "required:false" \
        "platform:wsl"

    log "DEBUG" "Core modules registered"
}

# モジュールの登録
register_module() {
    local module_name="$1"
    shift

    if [[ -z "$module_name" ]]; then
        log "ERROR" "Module name required for registration"
        return 1
    fi

    # メタデータのパース
    local metadata=()
    for attr in "$@"; do
        if [[ "$attr" == *":"* ]]; then
            local key="${attr%%:*}"
            local value="${attr#*:}"
            metadata["${module_name}_${key}"]="$value"
        fi
    done

    # レジストリに登録
    MODULE_REGISTRY["$module_name"]="registered"

    # メタデータを保存
    for key in "${!metadata[@]}"; do
        MODULE_METADATA["$key"]="${metadata[$key]}"
    done

    log "DEBUG" "Module registered: $module_name"
}

# モジュール情報取得
get_module_info() {
    local module_name="$1"
    local attribute="${2:-all}"

    if [[ -z "${MODULE_REGISTRY[$module_name]}" ]]; then
        log "WARN" "Module not registered: $module_name"
        return 1
    fi

    case "$attribute" in
        "all")
            echo "Module: $module_name"
            echo "  Status: ${MODULE_REGISTRY[$module_name]}"
            echo "  Category: ${MODULE_METADATA[${module_name}_category]:-unknown}"
            echo "  Description: ${MODULE_METADATA[${module_name}_description]:-none}"
            echo "  Version: ${MODULE_METADATA[${module_name}_version]:-unknown}"
            echo "  Required: ${MODULE_METADATA[${module_name}_required]:-false}"
            echo "  Platform: ${MODULE_METADATA[${module_name}_platform]:-any}"
            echo "  Dependencies: ${MODULE_METADATA[${module_name}_dependencies]:-none}"
            ;;
        *)
            echo "${MODULE_METADATA[${module_name}_${attribute}]:-}"
            ;;
    esac
}

# カテゴリ別モジュール一覧取得
get_modules_by_category() {
    local category="$1"
    local modules=()

    for module in "${!MODULE_REGISTRY[@]}"; do
        local module_category="${MODULE_METADATA[${module}_category]}"
        if [[ "$module_category" == "$category" ]]; then
            modules+=("$module")
        fi
    done

    printf '%s\n' "${modules[@]}"
}

# プラットフォーム対応モジュール一覧取得
get_modules_for_platform() {
    local platform="$1"
    local modules=()

    for module in "${!MODULE_REGISTRY[@]}"; do
        local module_platforms="${MODULE_METADATA[${module}_platform]:-any}"
        if [[ "$module_platforms" == "any" ]] || [[ "$module_platforms" == *"$platform"* ]]; then
            modules+=("$module")
        fi
    done

    printf '%s\n' "${modules[@]}"
}

# 必須モジュール一覧取得
get_required_modules() {
    local modules=()

    for module in "${!MODULE_REGISTRY[@]}"; do
        local is_required="${MODULE_METADATA[${module}_required]}"
        if [[ "$is_required" == "true" ]]; then
            modules+=("$module")
        fi
    done

    printf '%s\n' "${modules[@]}"
}

# モジュール検索
search_modules() {
    local search_term="$1"
    local search_field="${2:-all}"
    local matches=()

    for module in "${!MODULE_REGISTRY[@]}"; do
        local match=false

        case "$search_field" in
            "name")
                if [[ "$module" == *"$search_term"* ]]; then
                    match=true
                fi
                ;;
            "description")
                local desc="${MODULE_METADATA[${module}_description]}"
                if [[ "$desc" == *"$search_term"* ]]; then
                    match=true
                fi
                ;;
            "category")
                local cat="${MODULE_METADATA[${module}_category]}"
                if [[ "$cat" == *"$search_term"* ]]; then
                    match=true
                fi
                ;;
            "all")
                local desc="${MODULE_METADATA[${module}_description]}"
                local cat="${MODULE_METADATA[${module}_category]}"
                if [[ "$module" == *"$search_term"* ]] ||
                    [[ "$desc" == *"$search_term"* ]] ||
                    [[ "$cat" == *"$search_term"* ]]; then
                    match=true
                fi
                ;;
        esac

        if [[ "$match" == "true" ]]; then
            matches+=("$module")
        fi
    done

    printf '%s\n' "${matches[@]}"
}

# 動的モジュール登録
register_external_module() {
    local module_path="$1"
    local module_name="$2"
    shift 2

    if [[ ! -f "$module_path" ]]; then
        log "ERROR" "Module file not found: $module_path"
        return 1
    fi

    if [[ -z "$module_name" ]]; then
        # ファイル名からモジュール名を推測
        module_name=$(basename "$module_path" .sh)
    fi

    # メタデータを抽出（ファイルのコメントから）
    local description
    description=$(grep -m1 "^# .*- " "$module_path" | sed 's/^# [^-]* - //' || echo "External module")

    register_module "$module_name" \
        "category:external" \
        "description:$description" \
        "version:unknown" \
        "required:false" \
        "path:$module_path" \
        "$@"

    log "INFO" "External module registered: $module_name from $module_path"
}

# モジュールレジストリ統計
get_registry_stats() {
    local format="${1:-text}"

    local total_modules=${#MODULE_REGISTRY[@]}
    local categories=()
    local platforms=()

    # カテゴリ集計
    for module in "${!MODULE_REGISTRY[@]}"; do
        local cat="${MODULE_METADATA[${module}_category]}"
        if [[ -n "$cat" ]] && [[ ! " ${categories[*]} " =~ " $cat " ]]; then
            categories+=("$cat")
        fi
    done

    # プラットフォーム集計
    for module in "${!MODULE_REGISTRY[@]}"; do
        local plat="${MODULE_METADATA[${module}_platform]:-any}"
        IFS=',' read -ra plat_array <<<"$plat"
        for p in "${plat_array[@]}"; do
            if [[ ! " ${platforms[*]} " =~ " $p " ]]; then
                platforms+=("$p")
            fi
        done
    done

    case "$format" in
        "json")
            echo "{"
            echo "  \"total_modules\": $total_modules,"
            echo "  \"categories\": [$(printf '"%s",' "${categories[@]}" | sed 's/,$//')],"
            echo "  \"platforms\": [$(printf '"%s",' "${platforms[@]}" | sed 's/,$//')]"
            echo "}"
            ;;
        "text")
            echo "Module Registry Statistics:"
            echo "  Total modules: $total_modules"
            echo "  Categories: ${#categories[@]} (${categories[*]})"
            echo "  Platforms: ${#platforms[@]} (${platforms[*]})"
            ;;
    esac
}

# 全モジュール情報取得
get_all_modules() {
    local format="${1:-text}"

    case "$format" in
        "text")
            echo "=== Module Registry ==="
            for module in "${!MODULE_REGISTRY[@]}"; do
                get_module_info "$module" "all"
                echo ""
            done
            ;;
        "json")
            echo "{"
            local first=true
            for module in "${!MODULE_REGISTRY[@]}"; do
                if [[ "$first" == "true" ]]; then
                    first=false
                else
                    echo ","
                fi
                echo "  \"$module\": {"
                echo "    \"status\": \"${MODULE_REGISTRY[$module]}\","
                echo "    \"category\": \"${MODULE_METADATA[${module}_category]:-unknown}\","
                echo "    \"description\": \"${MODULE_METADATA[${module}_description]:-none}\","
                echo "    \"version\": \"${MODULE_METADATA[${module}_version]:-unknown}\","
                echo "    \"required\": \"${MODULE_METADATA[${module}_required]:-false}\","
                echo "    \"platform\": \"${MODULE_METADATA[${module}_platform]:-any}\","
                echo "    \"dependencies\": \"${MODULE_METADATA[${module}_dependencies]:-none}\""
                echo -n "  }"
            done
            echo ""
            echo "}"
            ;;
        "list")
            printf '%s\n' "${!MODULE_REGISTRY[@]}"
            ;;
    esac
}

# モジュールレジストリテスト
test_module_registry() {
    echo "=== Module Registry Test ==="

    # 初期化テスト
    if init_module_registry; then
        echo "✅ Module registry initialization successful"
    else
        echo "❌ Module registry initialization failed"
        return 1
    fi

    # モジュール情報取得テスト
    echo ""
    echo "Testing module info retrieval:"
    get_module_info "powershell_engine" "all"

    # カテゴリ検索テスト
    echo ""
    echo "Voice modules:"
    get_modules_by_category "voice"

    # プラットフォーム検索テスト
    echo ""
    echo "WSL modules:"
    get_modules_for_platform "wsl"

    # 統計表示
    echo ""
    get_registry_stats "text"

    echo ""
    echo "Module registry test completed"
    return 0
}

# モジュールレジストリの自動初期化
if [[ -z "$MODULE_REGISTRY_INITIALIZED" ]] || [[ "$MODULE_REGISTRY_INITIALIZED" != "true" ]]; then
    init_module_registry
fi
