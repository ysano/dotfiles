# Claude Voice tmux統合システム - アーキテクチャドキュメント

## 概要

Claude Voice tmux統合システムは、AI駆動の音声通知とリアルタイム画面要約機能を提供する高度なターミナルマルチプレクサー統合システムです。クロスプラットフォーム対応のハイブリッド統合アーキテクチャを採用し、プラットフォーム固有の実装を明確に分離しつつ、統一されたAPIを提供します。

## アーキテクチャの核心原則

### 1. プラットフォーム分離アーキテクチャ（Phase 2）

OS固有コードを`core`ディレクトリから完全に分離し、プラットフォーム別のモジュール化を実現。これにより保守性と拡張性を大幅に向上させています。

```
~/.tmux/claude/
├── core/                           # クロスプラットフォーム共通機能
│   ├── foundation.sh               # 基本ユーティリティ・ロギング
│   ├── platform_utils.sh           # 統一プラットフォーム検出・PowerShell管理
│   ├── integration.sh              # 智的統合管理・条件判定システム
│   ├── status_detector.sh          # Claude Code状態検出（DI対応）
│   ├── interfaces.sh               # テスタブルアーキテクチャ抽象化層
│   ├── execution_engine.sh         # 実行エンジン
│   ├── summary_engine.sh           # LLM要約エンジン
│   ├── screen_capture.sh           # 画面キャプチャ機能
│   ├── error_handler.sh            # エラー処理・回復機構
│   ├── health_diagnostics.sh       # ヘルスチェック・自己診断
│   └── user_interface.sh           # ユーザーインターフェース管理
├── platforms/                      # OS固有実装の完全分離
│   ├── windows/                    # Windows/PowerShell統合
│   │   ├── windows_audio_system.sh
│   │   ├── windows_notification_system.sh
│   │   ├── windows_tts_engine.sh
│   │   └── powershell_engine.sh
│   ├── wsl/                        # WSL環境特化（v3.0統一エンジン）
│   │   ├── wsl_voice_engine.sh     # 統一音声エンジン
│   │   ├── wsl_platform.sh         # プラットフォーム設定
│   │   ├── wsl_integration.sh      # WSL統合機能
│   │   ├── wsl_host_resolver.sh    # ホスト解決
│   │   └── modules/                # 機能別モジュール
│   │       ├── wsl_environment.sh
│   │       ├── powershell_interface.sh
│   │       ├── status_sound_engine.sh
│   │       └── voice_detection.sh
│   ├── macos/                      # macOS Audio Unit統合（計画中）
│   ├── linux/                      # Linux PulseAudio/ALSA対応（計画中）
│   └── freebsd/                    # FreeBSD固有実装（計画中）
├── os/                             # レガシーOS検出（後方互換性）
│   ├── darwin.sh
│   ├── linux.sh
│   └── windows.sh
├── adapters/                       # 外部統合アダプター
│   └── tmux_adapter.sh
├── services/                       # サービス層
│   └── detection_service.sh
├── config/                         # 設定ファイル
│   ├── claude-voice.yaml          # YAML階層設定（主設定）
│   ├── integration.conf            # 統合設定
│   └── audio_mode.conf            # 音声モード設定
├── tests/                          # テストスイート
│   ├── test_runner.sh              # テストランナー
│   ├── test_*_*.sh                 # 各種テストファイル
│   └── output/                     # テスト結果
├── logs/                           # ログファイル
├── bin/                            # 実行バイナリ（claude-voice）
└── scripts/                        # ユーティリティスクリプト
```

### 2. ハイブリッド統合戦略

**Progressive Enhancement（段階的機能向上）**: 依存関係の可用性に応じて機能レベルを自動調整
- **完全機能モード**: LLM + 音声合成 + 画面キャプチャ + ステレオ効果音
- **劣化機能モード**: 基本音声通知 + シンプル効果音
- **フォールバックモード**: 視覚通知のみまたは無音動作

**智的統合ポイント**: tmux.confへの最小限の変更による非侵入的統合
```bash
# tmux設定での智的条件分岐
if-shell '[ -f ~/.tmux/claude.conf ]' 'source-file ~/.tmux/claude.conf'
```

### 3. 依存性注入（DI）アーキテクチャ

`interfaces.sh`により、全ての外部依存を抽象化。これによりテスタビリティと保守性を大幅に向上。

```bash
# 抽象化されたインターフェース呼び出し
call_tmux "capture_pane" "$window_id" "$pane_id" 30
call_process "find_process" "claude"
call_filesystem "read_file" "$config_path"

# テスト時のモック注入
register_mock_interface "tmux" "capture_pane" "mock_capture_function"
```

## 主要コンポーネントの詳細

### 1. コア基盤（foundation.sh）
- **バージョン管理**: CLAUDE_VOICE_VERSION="2.1.0"
- **ロギングシステム**: 4段階ログレベル（DEBUG/INFO/WARN/ERROR）
- **OS検出**: OSTYPE/WSL_DISTRO_NAME/procファイルによる多段階検出
- **モジュール動的ロード**: OS固有モジュールの条件付き読み込み

### 2. プラットフォームユーティリティ（platform_utils.sh）
- **統一WSL検出**: 5分間キャッシュによる高速化
- **PowerShell検出**: 7段階の候補パス検索
- **Windows音声システム検証**: .NET System.Speech可用性チェック
- **キャッシュ機構**: /tmp/.claude_*によるパフォーマンス最適化

### 3. 統合層（integration.sh）
- **前提条件チェッカー**: 5カテゴリーの包括的検証
  - Claude Voiceバイナリ
  - 音声システム（macOS/Linux）
  - Ollamaシステム
  - 設定ファイル整合性
  - ファイルシステム権限
- **智的実行戦略**: アクションタイプ別の適応的実行
- **フォールバック管理**: グレースフル劣化による堅牢性

### 4. ステータス検出器（status_detector.sh）
- **Claude Code 3状態検出**:
  - ⚡ Busy: トークン処理中（中断可能）
  - ⌛ Waiting: ユーザー入力待ち
  - ✅ Idle: 入力準備完了
- **純粋関数設計**: 副作用のない検出ロジック
- **DI対応**: モック可能な外部依存

### 5. WSL音声エンジン（wsl_voice_engine.sh v3.0）
- **統一音声API**: speak()、play_sound()、notify()
- **日本語音声優先順位**: Microsoft Haruka Desktop → Sayaka → Ichiro
- **ステータス別効果音**:
  - 忙しい状態: 警告パターン（800Hz×2→600Hz）
  - 待機状態: 上昇メロディー（659Hz→880Hz→1175Hz）
  - 完了状態: 成功パターン（523Hz→659Hz→783Hz→1046Hz）
- **Equal Power Pan Law**: 3dBセンター法によるステレオ配置

## 設計パターンと実装原則

### 1. ファクトリーパターン
プラットフォーム固有実装の動的選択と生成。

### 2. 戦略パターン
実行レベル（完全/劣化/フォールバック）に基づく適応的動作選択。

### 3. 観察者パターン
tmuxフックによるイベント駆動アーキテクチャ:
- `session-created`: 自動初期化
- `after-new-window`: ウィンドウ状態追跡
- `pane-focus-changed`: アクティブ状態監視

### 4. 依存性注入パターン
テスタブルアーキテクチャのための外部依存抽象化。

### 5. キャッシュパターン
パフォーマンス最適化:
- プラットフォーム検出: 300秒キャッシュ
- PowerShellパス: 300秒キャッシュ
- ステータス検出: 2秒キャッシュ
- 設定読み込み: メモリキャッシュ

## 設定管理

### YAML階層設定（claude-voice.yaml）
```yaml
integration:
  enabled: true
  mode: "smart"  # minimal, smart, full
  strict_mode: false

voice:
  manual:
    mode: "brief"
    lines: 25
    model: "auto"
    voice: "Kyoko (Enhanced)"

audio:
  system_integration: "osascript"
  max_concurrent_voices: 1
  speech_rate: 180

llm:
  provider: "ollama"
  ollama:
    url: "http://localhost:11434"
    default_model: "phi4-mini:latest"
```

## テスト戦略

### 多層テストアーキテクチャ
```bash
# 単体テスト（純粋関数）
~/.tmux/claude/tests/test_status_detector.sh

# 統合テスト（モジュール間）
~/.tmux/claude/tests/test_module_integration.sh

# エンドツーエンドテスト
~/.tmux/claude/tests/test_end_to_end.sh

# パフォーマンステスト
~/.tmux/claude/tests/test_fast.sh
```

### テストランナー
```bash
# 全テスト実行
~/.tmux/claude/tests/test_runner.sh

# 特定テストのみ
~/.tmux/claude/tests/test_runner.sh test_minimal
```

## 命名規則（NAMING_CONVENTIONS.md準拠）

### 関数命名パターン
```bash
{action}_{platform}_{component}_{detail}()

# 例
init_windows_audio_system()
get_wsl_system_info()
check_powershell_execution_capability()
execute_voice_action_intelligently()
```

### 変数命名パターン
```bash
# グローバル変数
declare -g WINDOWS_AUDIO_CURRENT_VOLUME=""
declare -g WSL_CLIPBOARD_AVAILABLE=""

# ローカル変数
local powershell_execution_result=""
local windows_audio_device_list=""

# 定数
readonly DEFAULT_TTS_TIMEOUT=30
readonly MAX_RETRY_ATTEMPTS=3
```

## パフォーマンス特性

| 指標 | 目標値 | 現在値 | 測定方法 |
|------|--------|--------|----------|
| tmux起動時間増加 | <200ms | <50ms | 起動時間測定 |
| 音声通知レスポンス | <5秒 | 3-4秒 | エンドツーエンド測定 |
| 設定読み込み時間 | <100ms | <30ms | キャッシュ効果測定 |
| プラットフォーム検出 | <100ms | <10ms (キャッシュ有効時) | 検出時間測定 |

## デバッグとトラブルシューティング

### デバッグモード有効化
```bash
export CLAUDE_VOICE_DEBUG=true
export CLAUDE_STATUS_DEBUG=1
```

### ヘルスチェック
```bash
# 包括的診断
~/.tmux/claude/core/health_diagnostics.sh

# 特定機能テスト
~/.tmux/claude/platforms/wsl/wsl_voice_engine.sh test
```

### 一般的な問題と解決策

1. **音声が出力されない**
   - PowerShell可用性確認: `find_powershell`
   - Windows音声システム確認: `check_windows_speech`
   - 音声リスト確認: `detect_available_voices`

2. **ステータス検出が機能しない**
   - Claudeプロセス確認: `pgrep -f claude`
   - tmuxペイン確認: `tmux capture-pane -p`
   - デバッグログ確認: `tail -f ~/.tmux/claude/logs/claude-voice.log`

3. **設定が反映されない**
   - 設定検証: `~/.tmux/claude/tests/test_config_manager.sh`
   - キャッシュクリア: `rm /tmp/.claude_*`

## 今後の開発計画

### Phase 3: マルチプラットフォーム完全対応
- macOS Audio Unit統合の完成
- Linux PulseAudio/ALSA完全サポート
- FreeBSD音声システム対応

### Phase 4: 高度な機能追加
- WebSocket統合によるリアルタイム通信
- gRPC統合による高性能クロスプラットフォーム通信
- REST APIによるWeb UI統合
- プラグインアーキテクチャの実装

## セキュリティ考慮事項

### 入力検証とサニタイゼーション
- テキスト長制限: 5000文字
- JSONエスケープ処理
- シェルインジェクション対策

### 権限制御
- ファイル権限: 755/644適切な権限設定
- プロセス分離: tmuxプロセス境界による隔離
- ネットワーク制限: localhost:11434のみ許可

## 保守と運用

### 日常的な保守タスク
```bash
# ログローテーション
logrotate ~/.tmux/claude/logs/

# キャッシュクリーンアップ
find /tmp -name ".claude_*" -mtime +1 -delete

# 使用統計確認
tail -n 100 ~/.tmux/claude/logs/usage_stats.jsonl | jq .
```

この包括的なアーキテクチャにより、Claude Voice tmux統合システムは高い保守性、拡張性、信頼性を実現し、多様なプラットフォームで一貫した体験を提供します。