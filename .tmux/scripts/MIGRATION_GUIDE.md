# TMux Scripts Library 移行ガイド

## 概要

TMux Scriptsの共通ライブラリシステムへの移行ガイドです。このリファクタリングにより、コードの重複を削減し、保守性と拡張性を大幅に向上させました。

## 🚀 新機能

### 階層的ライブラリシステム

```
.tmux/scripts/lib/
├── core.sh           # 基盤機能（ログ、エラー処理、クリーンアップ）
├── platform.sh       # OS検出、プラットフォーム固有操作
├── tmux_ops.sh       # tmux操作ユーティリティ
├── notification.sh   # 通知・音声関連
└── validation.sh     # 検証・テスト関数
```

## 📦 移行前後の比較

### Before (旧実装)

```bash
#!/bin/bash
# 各スクリプトで独自にログ関数を定義

log() {
    echo "[$1] $2"
}

log_info() { log "INFO" "$@"; }
log_error() { log "ERROR" "$@"; }

error_exit() {
    log_error "$1"
    exit 1
}

# OS検出も独自実装
detect_os() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux) echo "linux" ;;
        *) echo "unknown" ;;
    esac
}
```

### After (新実装)

```bash
#!/bin/bash
# 共通ライブラリをインポートするだけ

source "$(dirname "${BASH_SOURCE[0]}")/lib/core.sh"
source "$(dirname "${BASH_SOURCE[0]}")/lib/platform.sh"

# すべての関数が自動的に利用可能
log_info "Starting script..."
platform=$(detect_platform)
```

## 🔄 移行手順

### Step 1: ライブラリのインポート

スクリプトの先頭で必要なライブラリをインポート：

```bash
#!/bin/bash

# 基本的な使用（必須）
source "$(dirname "${BASH_SOURCE[0]}")/lib/core.sh"

# 必要に応じて追加
source "$(dirname "${BASH_SOURCE[0]}")/lib/platform.sh"     # OS検出が必要な場合
source "$(dirname "${BASH_SOURCE[0]}")/lib/tmux_ops.sh"      # tmux操作が必要な場合
source "$(dirname "${BASH_SOURCE[0]}")/lib/notification.sh"  # 通知機能が必要な場合
source "$(dirname "${BASH_SOURCE[0]}")/lib/validation.sh"    # 検証機能が必要な場合
```

### Step 2: 既存の重複コードを削除

以下の関数定義を削除（ライブラリで提供されるため）：

- `log()`, `log_info()`, `log_warn()`, `log_error()`, `log_debug()`
- `error_exit()`
- `detect_os()`, `detect_platform()`
- `cleanup()` 関連

### Step 3: 新しいAPIに移行

#### ログ機能

```bash
# 旧実装
log "INFO" "メッセージ"

# 新実装
log_info "メッセージ"
log_debug "デバッグメッセージ"  # LOG_LEVELがDEBUGの時のみ出力
```

#### エラーハンドリング

```bash
# 旧実装
if [[ ! -f "$file" ]]; then
    echo "Error: File not found"
    exit 1
fi

# 新実装
require_files "$file"  # 自動的にエラーメッセージとexit
# または
[[ -f "$file" ]] || error_exit "File not found: $file"
```

#### クリーンアップ

```bash
# 旧実装
trap "rm -f $temp_file" EXIT

# 新実装
enable_cleanup
register_cleanup "$temp_file"
# 複数ファイルも可能
register_cleanup "$temp_file1" "$temp_file2"
```

#### プラットフォーム検出

```bash
# 旧実装
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
    # macOS処理
fi

# 新実装
if is_macos; then
    # macOS処理
elif is_linux; then
    # Linux処理
elif is_wsl; then
    # WSL処理
fi
```

## 📚 利用可能な関数一覧

### core.sh

- **ログ機能**
  - `log_debug()`, `log_info()`, `log_warn()`, `log_error()`, `log_fatal()`
  - `setup_logging(log_file, log_level)`

- **エラーハンドリング**
  - `error_exit(message, [exit_code])`
  - `set_error_trap()`

- **クリーンアップ**
  - `enable_cleanup()`
  - `register_cleanup(items...)`
  - `register_cleanup_function(function_name)`

- **ユーティリティ**
  - `command_exists(command)`
  - `require_commands(commands...)`
  - `require_files(files...)`
  - `require_dirs(dirs...)`
  - `measure_time(command...)`

### platform.sh

- **OS検出**
  - `detect_platform()`
  - `is_macos()`, `is_linux()`, `is_wsl()`, `is_windows()`, `is_bsd()`

- **WSL固有**
  - `get_wsl_version()`
  - `get_wsl_distro()`
  - `find_powershell()`

- **システム情報**
  - `get_os_info()`
  - `get_cpu_info()`
  - `get_memory_info()`

- **パス管理**
  - `get_home_dir()`
  - `get_config_dir()`
  - `get_cache_dir()`
  - `get_temp_dir()`

- **クリップボード**
  - `copy_to_clipboard(text)`
  - `paste_from_clipboard()`

### tmux_ops.sh

- **tmux環境**
  - `in_tmux()`
  - `tmux_available()`
  - `get_tmux_version()`

- **セッション操作**
  - `get_current_session()`
  - `list_sessions()`
  - `create_session(name, [window], [command])`
  - `attach_session([name])`

- **ウィンドウ/ペイン操作**
  - `get_current_window()`
  - `get_current_pane()`
  - `capture_pane([pane], [lines])`
  - `send_to_pane(text, [pane])`

- **ステータス管理**
  - `get_window_status([window_id])`
  - `set_window_status(status, [window_id])`

### notification.sh

- **通知送信**
  - `send_notification(title, message, [type], [timeout])`
  - `send_notification_smart(title, message, [type])`

- **音声**
  - `play_sound(file, [volume])`
  - `system_beep([frequency], [duration])`

- **レート制限**
  - `check_rate_limit(key, [seconds])`
  - `reset_rate_limit(key)`

- **DND機能**
  - `enable_dnd([duration])`
  - `disable_dnd()`
  - `is_dnd_enabled()`

### validation.sh

- **検証結果管理**
  - `add_validation_result(type, category, message, [details])`
  - `generate_validation_report([output_file])`

- **ファイル検証**
  - `validate_file_exists(file, [category])`
  - `validate_dir_exists(dir, [category])`
  - `validate_file_permissions(file, perms, [category])`

- **設定検証**
  - `validate_yaml_syntax(file, [category])`
  - `validate_tmux_syntax(file, [category])`

- **アサーション**
  - `assert_equals(expected, actual, [message])`
  - `assert_contains(haystack, needle, [message])`
  - `assert_true(condition, [message])`

## 🧪 テスト

ライブラリのテストを実行：

```bash
.tmux/scripts/tests/test_lib.sh
```

## ⚠️ 注意事項

### Bash バージョン

- 最小要件: Bash 3.2以上
- 推奨: Bash 4.0以上（連想配列サポート）

### インポート順序

一部のライブラリは他のライブラリに依存しています：

1. `core.sh` - 常に最初にインポート
2. `platform.sh` - `core.sh`に依存
3. その他 - `core.sh`と`platform.sh`に依存

### グローバル変数

ライブラリは以下のグローバル変数を使用します：

- `TMUX_SCRIPTS_DEBUG` - デバッグモード（true/false）
- `TMUX_SCRIPTS_LOG_FILE` - ログファイルパス
- `TMUX_SCRIPTS_LOG_LEVEL` - ログレベル（DEBUG/INFO/WARN/ERROR）

## 📈 パフォーマンス改善

### キャッシュ機能

プラットフォーム検出などの重い処理は自動的にキャッシュされます：

```bash
# 初回: ~50ms
platform=$(detect_platform)

# 2回目以降: <1ms（キャッシュから）
platform=$(detect_platform)
```

### 遅延読み込み

必要なライブラリのみをインポートすることで起動時間を最小化：

```bash
# 基本機能のみ: ~10ms
source lib/core.sh

# すべて読み込み: ~30ms
source lib/core.sh
source lib/platform.sh
source lib/tmux_ops.sh
source lib/notification.sh
source lib/validation.sh
```

## 🆘 トラブルシューティング

### "command not found" エラー

ライブラリが正しくインポートされていることを確認：

```bash
# デバッグモードで実行
export TMUX_SCRIPTS_DEBUG=true
./your_script.sh
```

### ログが出力されない

ログファイルのパスと権限を確認：

```bash
export TMUX_SCRIPTS_LOG_FILE="/tmp/debug.log"
export TMUX_SCRIPTS_LOG_LEVEL="DEBUG"
```

### プラットフォーム検出が間違っている

キャッシュをクリア：

```bash
rm -f /tmp/.tmux_platform_cache_*
```

## 📝 変更履歴

### Version 1.0.0 (2024-08-09)

- 初回リリース
- 5つの基本ライブラリを提供
- 14個のスクリプトから重複コードを削除
- 約750行のコード削減（50%削減）

## 🤝 貢献

バグ報告や機能要望は、GitHubのIssueトラッカーまでお願いします。

## 📄 ライセンス

MIT License