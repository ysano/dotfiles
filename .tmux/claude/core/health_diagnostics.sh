#!/bin/bash
# Health Diagnostics Module - ヘルスチェック・診断機能
# システムの健全性確認、統合テスト、システムテストを担当

# === ヘルスチェック機能 ===

# 包括的なシステムヘルスチェック
run_health_check() {
    echo "=== Claude Voice Health Check ==="
    echo ""

    local health_score=0
    local total_checks=0
    local issues=()

    # 設定マネージャーの読み込み（オプション）
    # テストモードでは外部モジュール読み込みをスキップ
    if [[ -z "${CLAUDE_VOICE_TEST_MODE:-}" ]] && [[ -f "$CLAUDE_VOICE_HOME/core/integration.sh" ]]; then
        source "$CLAUDE_VOICE_HOME/core/integration.sh" >/dev/null 2>&1 || true
    fi

    # 1. 設定ファイルのヘルスチェック
    check_configuration_health issues
    local config_score=$?
    health_score=$((health_score + config_score))
    ((total_checks++))

    # 2. 統合レイヤーのヘルスチェック
    check_integration_health issues
    local integration_score=$?
    health_score=$((health_score + integration_score))
    ((total_checks++))

    # 3. 音声システムのヘルスチェック
    check_audio_health issues
    local audio_score=$?
    health_score=$((health_score + audio_score))
    ((total_checks++))

    # 4. LLM統合のヘルスチェック
    check_llm_health issues
    local llm_score=$?
    health_score=$((health_score + llm_score))
    ((total_checks++))

    # 5. ファイルシステムのヘルスチェック
    check_filesystem_health issues
    local fs_score=$?
    health_score=$((health_score + fs_score))
    ((total_checks++))

    # 6. 依存関係のヘルスチェック
    check_dependencies_health issues
    local deps_score=$?
    health_score=$((health_score + deps_score))
    ((total_checks++))

    # 結果の表示
    display_health_results "$health_score" "$total_checks" issues
}

# 設定ファイルのヘルスチェック
check_configuration_health() {
    local issues_array_name=$1
    local score=0

    echo "1. Configuration Health..."

    # YAML設定ファイルのチェック
    if [[ -f "$CLAUDE_VOICE_HOME/config/claude-voice.yaml" ]]; then
        echo "   ✅ YAML configuration file exists"

        # YAML構文チェック
        if command -v yq >/dev/null 2>&1; then
            if yq eval '.' "$CLAUDE_VOICE_HOME/config/claude-voice.yaml" >/dev/null 2>&1; then
                echo "   ✅ YAML syntax is valid"
                score=1
            else
                echo "   ❌ YAML syntax errors detected"
                eval "${issues_array_name}+=(\"yaml_syntax\")"
                score=0
            fi
        else
            echo "   ⚠️  yq not available for YAML validation"
            score=0
        fi
    else
        echo "   ❌ Configuration file missing"
        eval "${issues_array_name}+=(\"config_missing\")"
        score=0
    fi

    # 従来設定ファイルのチェック
    if [[ -f "$CLAUDE_VOICE_HOME/config/claude-voice.conf" ]]; then
        echo "   ✅ Legacy configuration file exists"
    fi

    # 統合設定のチェック
    if [[ -f "$CLAUDE_VOICE_HOME/config/integration.conf" ]]; then
        echo "   ✅ Integration configuration exists"
    fi

    echo ""
    return $score
}

# 統合レイヤーのヘルスチェック
check_integration_health() {
    local issues_array_name=$1
    local score=0

    echo "2. Integration Layer Health..."

    # 統合機能の確認
    if declare -f get_integration_status >/dev/null 2>&1; then
        local integration_status=$(get_integration_status 2>/dev/null || echo "unknown")
        case "$integration_status" in
            "fully_functional")
                echo "   ✅ Integration layer: Fully functional"
                score=1
                ;;
            "degraded")
                echo "   ⚠️  Integration layer: Degraded functionality"
                eval "${issues_array_name}+=(\"integration_degraded\")"
                score=0
                ;;
            *)
                echo "   ❌ Integration layer: Disabled or non-functional"
                eval "${issues_array_name}+=(\"integration_failed\")"
                score=0
                ;;
        esac
    else
        echo "   ❌ Integration layer not accessible"
        eval "${issues_array_name}+=(\"integration_missing\")"
        score=0
    fi

    # tmux統合のチェック
    if command -v tmux >/dev/null 2>&1; then
        echo "   ✅ tmux command available"
        if [[ -n "${TMUX:-}" ]]; then
            echo "   ✅ Running inside tmux session"
        else
            echo "   ⚠️  Not running inside tmux session"
        fi
    else
        echo "   ❌ tmux command not available"
        eval "${issues_array_name}+=(\"tmux_missing\")"
        score=0
    fi

    echo ""
    return $score
}

# 音声システムのヘルスチェック
check_audio_health() {
    local issues_array_name=$1
    local score=0
    local audio_engines_available=0
    local total_engines=0

    echo "3. Audio System Health..."

    # OS固有の音声システムチェック
    local os_type=$(detect_os 2>/dev/null || echo "unknown")
    case "$os_type" in
        "darwin")
            ((total_engines++))
            if command -v osascript >/dev/null 2>&1; then
                echo "   ✅ macOS osascript available"
                ((audio_engines_available++))

                if osascript -e 'get volume settings' >/dev/null 2>&1; then
                    echo "   ✅ Audio session accessible"
                else
                    echo "   ❌ Audio session not accessible"
                    eval "${issues_array_name}+=(\"audio_session_failed\")"
                fi
            else
                echo "   ❌ macOS osascript not available"
                eval "${issues_array_name}+=(\"osascript_missing\")"
            fi
            ;;
        "windows" | "linux")
            # WSL/Linux音声エンジンのチェック
            if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
                echo "   ℹ️  WSL environment detected"

                # WSL PowerShell音声エンジン
                ((total_engines++))
                if [[ -f "$CLAUDE_VOICE_HOME/core/wsl_voice_engine.sh" ]]; then
                    if source "$CLAUDE_VOICE_HOME/core/wsl_voice_engine.sh" 2>/dev/null; then
                        local speech_status=$(check_windows_speech 2>/dev/null || echo "unavailable")
                        if [[ "$speech_status" == "available" ]]; then
                            echo "   ✅ WSL PowerShell Speech available"
                            ((audio_engines_available++))
                        else
                            echo "   ❌ WSL PowerShell Speech not available"
                            eval "${issues_array_name}+=(\"wsl_speech_failed\")"
                        fi
                    fi
                else
                    echo "   ❌ WSL voice engine not found"
                    eval "${issues_array_name}+=(\"wsl_engine_missing\")"
                fi
            fi

            # Linux espeak
            ((total_engines++))
            if command -v espeak >/dev/null 2>&1; then
                echo "   ✅ Linux espeak available"
                ((audio_engines_available++))
            else
                echo "   ❌ Linux espeak not available"
            fi

            # Linux festival
            ((total_engines++))
            if command -v festival >/dev/null 2>&1; then
                echo "   ✅ Linux festival available"
                ((audio_engines_available++))
            else
                echo "   ❌ Linux festival not available"
            fi
            ;;
        *)
            echo "   ⚠️  Unknown OS type: $os_type"
            eval "${issues_array_name}+=(\"unknown_os\")"
            ;;
    esac

    # 音声エンジンレジストリのチェック
    if [[ -f "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" ]]; then
        echo "   ✅ Voice engine registry available"

        if source "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" 2>/dev/null; then
            local best_engine=$(select_best_engine 2>/dev/null || echo "none")
            if [[ "$best_engine" != "none" && "$best_engine" != "simple_fallback" ]]; then
                echo "   ✅ Best voice engine: $best_engine"
            else
                echo "   ⚠️  Only fallback voice engine available"
            fi
        fi
    else
        echo "   ❌ Voice engine registry not found"
        eval "${issues_array_name}+=(\"voice_registry_missing\")"
    fi

    # スコア計算
    if [[ $total_engines -gt 0 ]] && [[ $audio_engines_available -gt 0 ]]; then
        score=1
    else
        score=0
    fi

    echo ""
    return $score
}

# LLM統合のヘルスチェック
check_llm_health() {
    local issues_array_name=$1
    local score=0

    echo "4. LLM Integration Health..."

    # Ollama接続チェック
    if command -v check_ollama_health >/dev/null 2>&1; then
        if check_ollama_health >/dev/null 2>&1; then
            echo "   ✅ Ollama connection successful"
            score=1
        else
            echo "   ❌ Ollama connection failed"
            eval "${issues_array_name}+=(\"ollama_failed\")"
            score=0
        fi
    else
        # 直接確認
        if command -v curl >/dev/null 2>&1; then
            local ollama_url="http://localhost:11434"
            if curl -s "$ollama_url/api/tags" >/dev/null 2>&1; then
                echo "   ✅ Ollama API accessible"
                score=1

                # 利用可能なモデルの確認
                local models=$(curl -s "$ollama_url/api/tags" | grep -o '"name":"[^"]*"' | wc -l 2>/dev/null || echo 0)
                echo "   ℹ️  Available models: $models"
            else
                echo "   ❌ Ollama API not accessible"
                eval "${issues_array_name}+=(\"ollama_api_failed\")"
                score=0
            fi
        else
            echo "   ❌ curl command not available for Ollama check"
            eval "${issues_array_name}+=(\"curl_missing\")"
            score=0
        fi
    fi

    # LLMマネージャーのチェック
    if [[ -f "$CLAUDE_VOICE_HOME/core/llm_manager.sh" ]]; then
        echo "   ✅ LLM manager available"
    else
        echo "   ❌ LLM manager not found"
        eval "${issues_array_name}+=(\"llm_manager_missing\")"
        score=0
    fi

    echo ""
    return $score
}

# ファイルシステムのヘルスチェック
check_filesystem_health() {
    local issues_array_name=$1
    local score=0
    local checks_passed=0
    local total_checks=0

    echo "5. File System Health..."

    # 必要なディレクトリの確認
    local required_dirs=(
        "$CLAUDE_VOICE_HOME/core"
        "$CLAUDE_VOICE_HOME/config"
        "$CLAUDE_VOICE_HOME/logs"
        "$CLAUDE_VOICE_HOME/bin"
    )

    local missing_dirs=()
    for dir in "${required_dirs[@]}"; do
        ((total_checks++))
        if [[ -d "$dir" ]]; then
            ((checks_passed++))
        else
            missing_dirs+=("$dir")
        fi
    done

    if [[ ${#missing_dirs[@]} -eq 0 ]]; then
        echo "   ✅ All required directories exist"
    else
        echo "   ❌ Missing directories: ${missing_dirs[*]}"
        eval "${issues_array_name}+=(\"missing_directories\")"
    fi

    # 必要なファイルの確認
    local required_files=(
        "$CLAUDE_VOICE_HOME/bin/claude-voice"
        "$CLAUDE_VOICE_HOME/core/foundation.sh"
        "$CLAUDE_VOICE_HOME/core/universal_voice.sh"
    )

    local missing_files=()
    for file in "${required_files[@]}"; do
        ((total_checks++))
        if [[ -f "$file" ]]; then
            ((checks_passed++))
        else
            missing_files+=("$file")
        fi
    done

    if [[ ${#missing_files[@]} -eq 0 ]]; then
        echo "   ✅ All core files exist"
    else
        echo "   ❌ Missing files: ${missing_files[*]}"
        eval "${issues_array_name}+=(\"missing_files\")"
    fi

    # 権限の確認
    if [[ -f "$CLAUDE_VOICE_HOME/bin/claude-voice" ]]; then
        ((total_checks++))
        if [[ -x "$CLAUDE_VOICE_HOME/bin/claude-voice" ]]; then
            echo "   ✅ Main executable has correct permissions"
            ((checks_passed++))
        else
            echo "   ❌ Main executable lacks execute permission"
            eval "${issues_array_name}+=(\"permission_error\")"
        fi
    fi

    # スコア計算
    if [[ $total_checks -gt 0 ]] && [[ $checks_passed -eq $total_checks ]]; then
        score=1
    else
        score=0
    fi

    echo ""
    return $score
}

# 依存関係のヘルスチェック
check_dependencies_health() {
    local issues_array_name=$1
    local score=0
    local deps_available=0
    local total_deps=0

    echo "6. Dependencies Health..."

    # 必須依存関係
    local required_deps=("bash" "curl")
    for dep in "${required_deps[@]}"; do
        ((total_deps++))
        if command -v "$dep" >/dev/null 2>&1; then
            echo "   ✅ $dep available"
            ((deps_available++))
        else
            echo "   ❌ $dep not available"
            eval "${issues_array_name}+=(\"missing_${dep}\")"
        fi
    done

    # オプション依存関係
    local optional_deps=("tmux" "jq" "yq")
    for dep in "${optional_deps[@]}"; do
        ((total_deps++))
        if command -v "$dep" >/dev/null 2>&1; then
            echo "   ✅ $dep available (optional)"
            ((deps_available++))
        else
            echo "   ⚠️  $dep not available (optional)"
        fi
    done

    # OS固有依存関係
    local os_type=$(detect_os 2>/dev/null || echo "unknown")
    case "$os_type" in
        "darwin")
            ((total_deps++))
            if command -v osascript >/dev/null 2>&1; then
                echo "   ✅ osascript available (macOS)"
                ((deps_available++))
            else
                echo "   ❌ osascript not available (macOS)"
                eval "${issues_array_name}+=(\"missing_osascript\")"
            fi
            ;;
        "windows" | "linux")
            if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
                ((total_deps++))
                if command -v powershell.exe >/dev/null 2>&1; then
                    echo "   ✅ PowerShell available (WSL)"
                    ((deps_available++))
                else
                    echo "   ❌ PowerShell not available (WSL)"
                    eval "${issues_array_name}+=(\"missing_powershell\")"
                fi
            fi
            ;;
    esac

    # スコア計算
    if [[ $total_deps -gt 0 ]] && [[ $deps_available -ge 2 ]]; then
        score=1
    else
        score=0
    fi

    echo ""
    return $score
}

# ヘルスチェック結果の表示
display_health_results() {
    local health_score=$1
    local total_checks=$2
    local issues_array_name=$3
    
    # 配列参照の安全な方法
    eval "local issues_array=(\"\${${issues_array_name}[@]}\")"

    echo "=== Health Check Results ==="
    local health_percentage=$(((health_score * 100) / total_checks))
    echo "Overall Health Score: $health_score/$total_checks ($health_percentage%)"

    if [[ ${#issues_array[@]} -eq 0 ]]; then
        echo "🎉 System is healthy!"
        return 0
    else
        echo ""
        echo "⚠️  Issues detected:"
        for issue in "${issues_array[@]}"; do
            provide_issue_guidance "$issue"
        done

        if [[ $health_percentage -lt 50 ]]; then
            echo ""
            echo "🚨 Critical health issues detected. System may not function properly."
            return 2 # Critical health issues
        else
            echo ""
            echo "⚠️  Minor health issues detected. System should still function."
            return 1 # Minor health issues
        fi
    fi
}

# 問題に対するガイダンス提供
provide_issue_guidance() {
    local issue="$1"

    case "$issue" in
        "yaml_syntax")
            echo "  - YAML configuration has syntax errors"
            echo "    Fix: Run 'claude-voice --repair' or validate with 'yq'"
            ;;
        "config_missing")
            echo "  - Configuration file missing"
            echo "    Fix: Run 'claude-voice --config reset'"
            ;;
        "integration_degraded")
            echo "  - Integration layer has reduced functionality"
            echo "    Fix: Check dependencies and run 'claude-voice --test'"
            ;;
        "audio_session_failed")
            echo "  - Audio session not accessible"
            echo "    Fix: Check system audio settings and permissions"
            ;;
        "ollama_failed" | "ollama_api_failed")
            echo "  - Ollama LLM service not available"
            echo "    Fix: Start Ollama service (ollama serve) or check connection"
            ;;
        "missing_directories")
            echo "  - Required directories missing"
            echo "    Fix: Run 'claude-voice --repair'"
            ;;
        "missing_powershell")
            echo "  - PowerShell not available in WSL"
            echo "    Fix: Ensure Windows PowerShell is accessible from WSL"
            ;;
        *)
            echo "  - $issue"
            echo "    Fix: Run 'claude-voice --repair' for general fixes"
            ;;
    esac
}

# === 統合テスト機能 ===

# 統合テストの実行
run_integration_test() {
    echo "=== Claude Voice Integration Test ==="
    echo ""

    local test_passed=0
    local test_total=0

    echo "Testing complete integration workflow..."
    echo ""

    # テスト1: 設定システム
    ((test_total++))
    if test_configuration_system; then
        ((test_passed++))
    fi

    # テスト2: 音声システム
    ((test_total++))
    if test_voice_system; then
        ((test_passed++))
    fi

    # テスト3: LLM統合
    ((test_total++))
    if test_llm_integration; then
        ((test_passed++))
    fi

    # テスト4: tmux統合
    ((test_total++))
    if test_tmux_integration; then
        ((test_passed++))
    fi

    # テスト結果の表示
    echo ""
    echo "=== Integration Test Results ==="
    echo "Tests passed: $test_passed/$test_total"

    if [[ $test_passed -eq $test_total ]]; then
        echo "✅ All integration tests passed!"
        return 0
    else
        echo "❌ Some integration tests failed"
        return 1
    fi
}

# 設定システムのテスト
test_configuration_system() {
    echo "1. Configuration System Test..."

    if [[ -f "$CLAUDE_VOICE_HOME/core/config_manager.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/config_manager.sh" 2>/dev/null; then
            echo "   ✅ Configuration system working"
            return 0
        else
            echo "   ❌ Configuration system failed to load"
            return 1
        fi
    else
        echo "   ❌ Configuration manager not found"
        return 1
    fi
}

# 音声システムのテスト
test_voice_system() {
    echo "2. Voice System Test..."

    if [[ -f "$CLAUDE_VOICE_HOME/core/universal_voice.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/universal_voice.sh" 2>/dev/null; then
            local engine=$(detect_voice_engine 2>/dev/null || echo "none")
            if [[ "$engine" != "none" ]]; then
                echo "   ✅ Voice system working (engine: $engine)"
                return 0
            else
                echo "   ❌ No voice engine available"
                return 1
            fi
        else
            echo "   ❌ Voice system failed to load"
            return 1
        fi
    else
        echo "   ❌ Universal voice system not found"
        return 1
    fi
}

# LLM統合のテスト
test_llm_integration() {
    echo "3. LLM Integration Test..."

    if [[ -f "$CLAUDE_VOICE_HOME/core/llm_manager.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/llm_manager.sh" 2>/dev/null; then
            # 簡易接続テスト
            if command -v curl >/dev/null 2>&1; then
                if curl -s "http://localhost:11434/api/tags" >/dev/null 2>&1; then
                    echo "   ✅ LLM integration working"
                    return 0
                else
                    echo "   ❌ LLM service not accessible"
                    return 1
                fi
            else
                echo "   ⚠️  Cannot test LLM integration (curl not available)"
                return 0
            fi
        else
            echo "   ❌ LLM manager failed to load"
            return 1
        fi
    else
        echo "   ❌ LLM manager not found"
        return 1
    fi
}

# tmux統合のテスト
test_tmux_integration() {
    echo "4. tmux Integration Test..."

    if command -v tmux >/dev/null 2>&1; then
        echo "   ✅ tmux command available"

        if [[ -n "${TMUX:-}" ]]; then
            echo "   ✅ Running inside tmux session"
            return 0
        else
            echo "   ⚠️  Not running inside tmux (this is okay)"
            return 0
        fi
    else
        echo "   ❌ tmux not available"
        return 1
    fi
}

# === システムテスト機能 ===

# システムテストの実行
run_system_test() {
    echo "=== Claude Voice System Test ==="
    echo ""

    local test_results=()
    local total_tests=0
    local passed_tests=0

    # コアモジュールテスト
    echo "Testing core modules..."
    if test_core_modules; then
        test_results+=("✅ Core modules")
        ((passed_tests++))
    else
        test_results+=("❌ Core modules")
    fi
    ((total_tests++))

    # OS固有機能テスト
    echo "Testing OS-specific functionality..."
    if test_os_specific_functions; then
        test_results+=("✅ OS-specific functions")
        ((passed_tests++))
    else
        test_results+=("❌ OS-specific functions")
    fi
    ((total_tests++))

    # 音声エンジンテスト
    echo "Testing voice engines..."
    if test_voice_engines; then
        test_results+=("✅ Voice engines")
        ((passed_tests++))
    else
        test_results+=("❌ Voice engines")
    fi
    ((total_tests++))

    # ファイルシステムテスト
    echo "Testing file system operations..."
    if test_filesystem_operations; then
        test_results+=("✅ File system operations")
        ((passed_tests++))
    else
        test_results+=("❌ File system operations")
    fi
    ((total_tests++))

    # 結果表示
    echo ""
    echo "=== System Test Results ==="
    for result in "${test_results[@]}"; do
        echo "$result"
    done

    echo ""
    echo "Tests passed: $passed_tests/$total_tests"

    if [[ $passed_tests -eq $total_tests ]]; then
        echo "🎉 All system tests passed!"
        return 0
    else
        echo "❌ Some system tests failed"
        return 1
    fi
}

# コアモジュールのテスト
test_core_modules() {
    local modules=("base.sh" "universal_voice.sh" "voice_engine_registry.sh")
    local failed_modules=()

    for module in "${modules[@]}"; do
        local module_path="$CLAUDE_VOICE_HOME/core/$module"
        if [[ -f "$module_path" ]]; then
            if bash -n "$module_path" 2>/dev/null; then
                echo "   ✅ $module syntax OK"
            else
                echo "   ❌ $module syntax error"
                failed_modules+=("$module")
            fi
        else
            echo "   ❌ $module not found"
            failed_modules+=("$module")
        fi
    done

    [[ ${#failed_modules[@]} -eq 0 ]]
}

# OS固有機能のテスト
test_os_specific_functions() {
    local os_type=$(detect_os 2>/dev/null || echo "unknown")
    local os_module="$CLAUDE_VOICE_HOME/os/${os_type}.sh"

    if [[ -f "$os_module" ]]; then
        if bash -n "$os_module" 2>/dev/null; then
            echo "   ✅ OS module ($os_type) syntax OK"
            return 0
        else
            echo "   ❌ OS module ($os_type) syntax error"
            return 1
        fi
    else
        echo "   ❌ OS module ($os_type) not found"
        return 1
    fi
}

# 音声エンジンのテスト
test_voice_engines() {
    if [[ -f "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" ]]; then
        if source "$CLAUDE_VOICE_HOME/core/voice_engine_registry.sh" 2>/dev/null; then
            echo "   ✅ Voice engine registry loaded"

            local engine=$(select_best_engine 2>/dev/null || echo "none")
            if [[ "$engine" != "none" ]]; then
                echo "   ✅ Best engine selected: $engine"
                return 0
            else
                echo "   ❌ No voice engine available"
                return 1
            fi
        else
            echo "   ❌ Voice engine registry failed to load"
            return 1
        fi
    else
        echo "   ❌ Voice engine registry not found"
        return 1
    fi
}

# ファイルシステム操作のテスト
test_filesystem_operations() {
    local test_dir="$CLAUDE_VOICE_HOME/logs"
    local test_file="$test_dir/test_write.tmp"

    # 書き込みテスト
    if echo "test" >"$test_file" 2>/dev/null; then
        echo "   ✅ File write test passed"

        # 読み込みテスト
        if [[ "$(cat "$test_file" 2>/dev/null)" == "test" ]]; then
            echo "   ✅ File read test passed"

            # クリーンアップ
            rm "$test_file" 2>/dev/null
            return 0
        else
            echo "   ❌ File read test failed"
            rm "$test_file" 2>/dev/null
            return 1
        fi
    else
        echo "   ❌ File write test failed"
        return 1
    fi
}

# === ユーティリティ関数 ===

# OS検出関数（簡易版）
detect_os() {
    local os_type=$(uname -s)
    case "$os_type" in
        "Darwin")
            echo "darwin"
            ;;
        "Linux")
            if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
                echo "windows" # WSL
            else
                echo "linux"
            fi
            ;;
        *)
            echo "unknown"
            ;;
    esac
}

# このモジュールが直接実行された場合のテスト
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # テスト用の環境変数設定
    CLAUDE_VOICE_HOME="${CLAUDE_VOICE_HOME:-${HOME}/.tmux/claude}"

    echo "Health Diagnostics Module Test"
    echo "=============================="
    echo ""

    case "${1:-health}" in
        "health")
            run_health_check
            ;;
        "integration")
            run_integration_test
            ;;
        "system")
            run_system_test
            ;;
        "config")
            issues=()
            score=$(check_configuration_health issues)
            echo "Configuration health score: $score/100"
            ;;
        "audio")
            issues=()
            score=$(check_audio_health issues)
            echo "Audio health score: $score/100"
            ;;
        *)
            echo "Available tests:"
            echo "  health      - Full health check"
            echo "  integration - Integration tests"
            echo "  system      - System tests"
            echo "  config      - Configuration health only"
            echo "  audio       - Audio health only"
            ;;
    esac
fi
