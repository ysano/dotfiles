# Claude Voice 命名規則ガイドライン

## 概要

このドキュメントは、Claude Voiceシステム全体で一貫した命名規則を定義し、コードの可読性、保守性、および拡張性を向上させることを目的としています。

## 1. 基本原則

### 1.1 統一性
- システム全体で一貫した命名パターンを使用する
- 同じ概念には同じ用語を使用する
- プラットフォーム固有の機能は明示的にプレフィックスを付ける

### 1.2 明確性
- 省略形を避け、完全な単語を使用する
- 関数名から目的と動作が理解できる
- 変数名から型と用途が推測できる

### 1.3 一貫性
- snake_case を全体で使用
- 動詞-名詞の順序を維持
- プレフィックス/サフィックスのパターンを統一

## 2. 関数命名規則

### 2.1 基本パターン

```bash
# テンプレート
{action}_{platform}_{component}_{detail}()

# 例
init_windows_audio_system()
get_wsl_system_info()
check_powershell_execution_capability()
```

### 2.2 動作別命名パターン

#### 初期化関数
```bash
init_{component}()                    # 基本初期化
init_{platform}_{component}()        # プラットフォーム固有初期化

# 例
init_module_loader()
init_windows_tts_engine()
init_wsl_integration()
```

#### 取得関数
```bash
get_{platform}_{component}_{detail}()

# 例
get_windows_audio_devices()
get_wsl_system_info()
get_powershell_execution_policy()
```

#### 設定関数
```bash
set_{platform}_{component}_{property}()

# 例
set_windows_system_volume()
set_powershell_execution_policy()
set_wsl_environment_variable()
```

#### 確認関数
```bash
check_{platform}_{component}_{condition}()
is_{condition}()
has_{capability}()
can_{action}()

# 例
check_powershell_execution_capability()
is_wsl_environment()
has_toast_notification_support()
can_access_windows_clipboard()
```

#### 実行関数
```bash
execute_{platform}_{action}()
perform_{action}()
run_{component}_{action}()

# 例
execute_powershell_script()
perform_speech_synthesis()
run_notification_queue_processor()
```

#### ハンドラー関数
```bash
handle_{type}_error()
handle_{platform}_{event}()

# 例
handle_powershell_error()
handle_windows_notification_event()
handle_module_loading_failure()
```

### 2.3 プラットフォーム固有プレフィックス

- **Windows関連**: `windows_`
- **WSL関連**: `wsl_`
- **PowerShell関連**: `powershell_`
- **汎用システム**: `system_`
- **モジュール管理**: `module_`

## 3. 変数命名規則

### 3.1 グローバル変数

```bash
# テンプレート
declare -g {PLATFORM}_{COMPONENT}_{PURPOSE}

# 例
declare -g WINDOWS_AUDIO_CURRENT_VOLUME=""
declare -g WSL_CLIPBOARD_AVAILABLE=""
declare -g POWERSHELL_PATH_CACHE=""
declare -g MODULE_LOADER_INITIALIZED=""
```

### 3.2 ローカル変数

```bash
# 基本パターン
local {purpose}_{type}=""
local {platform}_{component}_path=""

# 例
local powershell_execution_result=""
local windows_audio_device_list=""
local error_recovery_attempts=""
local configuration_file_path=""
```

### 3.3 定数

```bash
# テンプレート
readonly {SCOPE}_{PURPOSE}={value}

# 例
readonly DEFAULT_TTS_TIMEOUT=30
readonly MAX_RETRY_ATTEMPTS=3
readonly WINDOWS_POWERSHELL_CORE_PATH="/mnt/c/Program Files/PowerShell/7/pwsh.exe"
readonly WSL_CLIPBOARD_COMMAND="clip.exe"
```

### 3.4 特殊変数

#### 配列変数
```bash
declare -a {PURPOSE}_LIST=()
declare -A {PURPOSE}_MAP=()

# 例
declare -a AVAILABLE_VOICES_LIST=()
declare -A ERROR_CODE_MAP=()
declare -a WINDOWS_AUDIO_DEVICES_LIST=()
```

#### 状態変数
```bash
{COMPONENT}_INITIALIZED="false"
{COMPONENT}_AVAILABLE="unknown"
{COMPONENT}_STATUS="pending"

# 例
WINDOWS_TTS_INITIALIZED="false"
TOAST_NOTIFICATION_AVAILABLE="unknown"
MODULE_LOADING_STATUS="pending"
```

## 4. ファイル・モジュール命名規則

### 4.1 ファイル命名パターン

```bash
# テンプレート
{platform}_{component}_{type}.sh

# 例
windows_tts_engine.sh
wsl_integration.sh
powershell_engine.sh
module_loader.sh
error_handler.sh
```

### 4.2 モジュールタイプ分類

- **engine**: 特定機能の実行エンジン
- **system**: システム管理機能
- **integration**: 外部システム統合
- **handler**: エラー・イベント処理
- **loader**: モジュール・リソース読み込み
- **registry**: 登録・管理機能

## 5. エラーコード・メッセージ命名

### 5.1 エラーコード命名

```bash
# テンプレート
{COMPONENT}_{ERROR_TYPE}

# 例
POWERSHELL_NOT_FOUND
TTS_ENGINE_FAILED
WSL_INTEGRATION_FAILED
MODULE_DEPENDENCY_MISSING
```

### 5.2 ログメッセージ形式

```bash
# テンプレート
"[{MODULE}:{FUNCTION}] {メッセージ}: {詳細} (Context: {文脈})"

# 例
"[powershell_engine:execute_script] Script execution failed: timeout occurred (Context: TTS synthesis)"
"[module_loader:load_module] Module not found: windows_tts_engine (Context: Windows initialization)"
```

## 6. 非推奨・改善が必要なパターン

### 6.1 避けるべき命名

```bash
# ❌ 避けるべき
result=""                    # 汎用的すぎる
tmp=""                      # 意味不明
deps=""                     # 省略形
exec()                      # 予約語と競合
test()                      # シェル組み込みコマンドと競合

# ✅ 推奨
powershell_execution_result=""
temporary_file_path=""
module_dependencies=""
execute_powershell_script()
validate_configuration()
```

### 6.2 改善が必要な既存関数

```bash
# 現在の命名 → 推奨命名
speak_text() → synthesize_speech_with_platform_engine()
send_notification() → send_platform_notification()
system_beep() → play_system_beep_sound()
get_info() → get_system_information()
check_deps() → check_module_dependencies()
```

## 7. プラットフォーム固有ガイドライン

### 7.1 Windows関連

```bash
# 機能カテゴリ別プレフィックス
windows_tts_*           # Text-to-Speech関連
windows_audio_*         # 音響システム関連
windows_notification_*  # 通知システム関連
powershell_*           # PowerShell関連
```

### 7.2 WSL関連

```bash
# WSL固有機能
wsl_environment_*      # 環境検出・設定
wsl_clipboard_*        # クリップボード統合
wsl_bridge_*          # Windows連携機能
```

### 7.3 汎用モジュール

```bash
# システム共通機能
module_*              # モジュール管理
error_*               # エラー処理
dependency_*          # 依存関係管理
```

## 8. 実装ガイドライン

### 8.1 段階的移行戦略

1. **Phase 1**: 新規関数は新命名規則に従う
2. **Phase 2**: 重要な既存関数にエイリアスを追加
3. **Phase 3**: 非推奨関数の段階的削除

### 8.2 後方互換性

```bash
# 新しい関数名で実装
send_windows_toast_notification() {
    # 実装
}

# 既存関数は新関数を呼び出し（非推奨警告付き）
send_notification() {
    log "WARN" "send_notification() is deprecated, use send_windows_toast_notification()"
    send_windows_toast_notification "$@"
}
```

### 8.3 命名規則チェックリスト

- [ ] 関数名は動詞-名詞の順序
- [ ] プラットフォーム固有機能にはプレフィックス
- [ ] グローバル変数はALL_CAPS
- [ ] ローカル変数はsnake_case
- [ ] 省略形を使用していない
- [ ] 意図が明確に表現されている
- [ ] 既存の命名パターンと一貫している

## 9. ツール・自動化

### 9.1 命名規則検証スクリプト

```bash
# 命名規則違反を検出するスクリプト例
check_naming_conventions() {
    local violations=()
    
    # 関数名チェック
    grep -n "^[a-z][a-zA-Z]*(" *.sh | while read -r line; do
        # CamelCaseの検出
        if echo "$line" | grep -q "[a-z][A-Z]"; then
            violations+=("CamelCase function: $line")
        fi
    done
    
    # 報告
    if [[ ${#violations[@]} -gt 0 ]]; then
        printf '%s\n' "${violations[@]}"
        return 1
    fi
}
```

## 10. 命名規則適用例

### 10.1 リファクタリング前後の比較

```bash
# Before (不統一)
speak_text()
send_notification()
system_beep()
get_info()

# After (統一規則適用)
synthesize_speech_with_platform_engine()
send_platform_notification()
play_system_beep_sound()
get_system_information()
```

### 10.2 新規実装例

```bash
# Windows音声エンジンの新機能実装例
init_windows_tts_advanced_engine() {
    local tts_engine_config_path=""
    local windows_speech_api_available=""
    
    declare -g WINDOWS_TTS_ADVANCED_ENGINE_INITIALIZED="false"
    declare -g WINDOWS_TTS_VOICE_SYNTHESIS_CACHE=""
    
    # 実装...
}

get_windows_tts_available_voices() {
    local voice_detection_script=""
    local powershell_execution_result=""
    
    # 実装...
}
```

この命名規則ガイドラインに従うことで、Claude Voiceシステム全体の保守性、可読性、および拡張性が大幅に向上します。