# Tmux Claude Voice: ドキュメント一覧

このディレクトリには、tmux-claude-voiceシステムの実装に必要なドキュメントが含まれています。

## ドキュメント構成

### 1. [システム概要](./01-system-overview.md)
- システムの基本概念と主要機能
- 技術選択の理由（ポーリング方式）
- システム基本設計
- ステータス判定システム
- 実装優先順位

### 2. [実装ガイド](./02-implementation-guide.md)
- 実装テンプレート
- エラーハンドリング例
- 統合テスト例
- ファイル構造
- 最優先事項
- デバッグガイド

### 3. [デシベルパンニングエンジン](./03-panning-engine.md)
- 音像定位の基本概念
- 動的ウィンドウ配置戦略
- Equal Power Pan Law対応
- 実装関数
- 設定パラメータ

### 4. [Ollama連携と要約機能](./04-ollama-integration.md)
- ホスト検出
- モデル選択
- 要約プロンプトの指針
- 実装関数
- 設定パラメータ
- トラブルシューティング

### 5. [音声エンジン](./05-sound-engine.md)
- プラットフォーム依存処理
- 音声合成機能
- 通知音再生機能
- 利用可能音声
- 設定パラメータ
- トラブルシューティング

### 6. [実装チェックリスト](./06-implementation-checklist.md)
- Phase別実装チェックリスト
- デバッグガイド
- 最終確認事項
- ファイル構造
- 依存関係
- 設定ファイル例

### 7. [Hooks ステートマシン](./07-hooks-state-machine.md)
- Claude Code hooks によるリアルタイム状態検出
- ペイン単位の状態遷移図
- 全遷移テーブル（音声・TTS詳細）
- 重複排除ロジック
- ポーリング監視とのフォールバック連携

## 実装の流れ

1. **システム概要**を読んで全体像を把握
2. **実装ガイド**でテンプレートとベストプラクティスを確認
3. **Phase 1-5**の順序で実装を進める
4. **実装チェックリスト**で進捗を管理
5. 各機能の詳細は対応するドキュメントを参照

## 実装状況

### ✅ 完了済み

- **Phase 1**: 基本監視システム（`polling_monitor.sh`, `functions.sh`）
- **Phase 2**: 音声エンジン（`sound_utils.sh`）
- **Phase 3**: デシベルパンニング（`panning_engine.sh`）
- **Phase 4**: Ollama連携（`ollama_utils.sh`）
- **Phase 5**: 統合テスト（`integration_test.sh`）
- **Phase 6**: Hooks 統合（`hooks/status-update.sh`, `hooks/setup-hooks.sh`）

### 📁 実装ファイル

```
.tmux/claude/
├── polling_monitor.sh      # ポーリング監視（hooks フォールバック付き）
├── functions.sh           # 基本機能関数群（ペインレベル検出対応）
├── sound_utils.sh         # 音声エンジン（OS別デフォルト音対応）
├── panning_engine.sh      # デシベルパンニングエンジン
├── ollama_utils.sh        # Ollama連携機能
├── integration_test.sh    # 統合テスト（hooks テスト含む）
├── toggle_notify_mode.sh  # 通知モード切り替え
├── pan_test.sh            # パンニングテスト
└── hooks/                 # Claude Code hooks 統合
    ├── status-update.sh   # hooks イベント共通エントリポイント
    └── setup-hooks.sh     # ~/.claude/settings.json セットアップ
```

### ⚙️ 設定ファイル

- `.tmux/claude.conf` - システム設定
- `.tmux/status.conf` - ステータス表示設定

## ハイブリッド検出アーキテクチャ

本システムは **Hooks 駆動 + ポーリングフォールバック** のハイブリッド方式を採用しています：

### Hooks 駆動（主系統）
- Claude Code の hooks イベント（`UserPromptSubmit`, `Stop`, `Notification` 等）をリアルタイムに受信
- **ペイン単位**の状態管理により、複数 Claude セッションを正確に識別
- 状態遷移に応じた即時の音声・TTS フィードバック
- 詳細は [07-hooks-state-machine.md](./07-hooks-state-machine.md) を参照

### ポーリング監視（副系統・フォールバック）
- `tmux status-right` から5秒間隔で呼び出される1回実行型スクリプト
- hooks タイムスタンプが30秒以上古い場合に `capture-pane` ベースの従来検出にフォールバック
- hooks が未設定の環境でも動作を保証

### セットアップ

hooks を有効化するには:

```bash
~/.tmux/claude/hooks/setup-hooks.sh
```

このスクリプトは `~/.claude/settings.json` に hooks 設定を安全にマージします。

## AI Agent向けの指示

各ドキュメントは独立して参照できるよう分割されています。実装時は以下の順序で進めてください：

1. `01-system-overview.md` - システム全体の理解
2. `02-implementation-guide.md` - 実装方法の把握
3. `06-implementation-checklist.md` - チェックリストの確認
4. 各機能の実装時に対応するドキュメントを参照

各ドキュメントには具体的なコード例とテスト方法が含まれているため、段階的に実装を進めることができます。

## クイックスタート

### 1. 依存関係のインストール

```bash
# 基本依存関係
sudo apt-get install tmux grep awk curl jq

# 音声関連（WSL）
sudo apt-get install ffmpeg

# 音声関連（macOS）
brew install ffmpeg

# Ollama（オプション）
curl -fsSL https://ollama.ai/install.sh | sh
```

### 2. 設定の有効化

```bash
# .tmux.conf または .tmux/claude.conf に以下を追加
source-file ~/.tmux/claude.conf
```

### 3. Hooks の有効化（推奨）

```bash
# Claude Code hooks を設定（~/.claude/settings.json に安全にマージ）
~/.tmux/claude/hooks/setup-hooks.sh

# Claude Code を再起動して hooks を有効化
```

### 4. システムのテスト

```bash
# 統合テストの実行（hooks テスト含む）
~/.tmux/claude/integration_test.sh

# ポーリング監視の手動テスト
~/.tmux/claude/polling_monitor.sh
```

### 5. キーバインド

- `Prefix + v + t` - 統合テスト実行
- `Prefix + v + v` - 要約機能ON/OFF
- `Prefix + v + 1/2/3` - 音声テスト
- `Prefix + v + s` - 音声合成テスト
- `Prefix + v + p` - パンニング機能ON/OFF
