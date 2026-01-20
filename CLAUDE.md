# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 重要な注意事項

- 不具合を修正する前に、不具合を再現するテストコードを書き、失敗することを確認すること。その後、実装を変更すること。
- 日本語で受け答えすること

## リポジトリ概要

このdotfilesリポジトリは、クロスプラットフォーム（Windows、macOS、Linux/WSL）対応の統合開発環境設定です。Emacsを中心とした構成で、AI統合ワークフロー、日本語環境、高度なZsh設定を含んでいます。

## 主要な開発コマンド

### セットアップとリンク作成
```bash
# 設定ファイルのシンボリックリンク作成
./link.sh

# Zsh設定の検証とテスト
./test_zsh_config.zsh
```

### Emacs設定のデバッグ
```bash
# 設定ファイルの構文チェック
emacs --debug-init --batch -l ~/.emacs.d/inits/init-ai.el | cat

# 特定モジュールのテスト
emacs --batch -l ~/.emacs.d/init.el | cat

# パッケージ依存関係の確認
ls -la ~/.emacs.d/elpa/ | grep <パッケージ名>
```

### Zsh設定のテストとパフォーマンス測定
```bash
# デバッグモードで設定検証
ZSH_DEBUG=1 zsh -c "source ~/.zprofile && echo 'Configuration validated'"

# 起動時間の測定
time zsh -i -c exit

# Zinit統計
zinit times

# キャッシュクリア（必要時）
unset ZSH_CMD_CACHE ZSH_OS_TYPE ZSH_IS_WSL
```

### Git Worktree管理（Claude Code複数起動対応）
```bash
# Git Worktree機能のテスト
./test_git_worktree.zsh

# 新しいworktreeを作成
gwt create <branch-name> [base-branch]

# worktree一覧表示
gwt list

# インタラクティブなworktree切り替え（fzf対応）
gwt switch

# worktreeを削除
gwt remove <worktree-name>

# メンテナンス・クリーンアップ
gwt clean

# ヘルプ表示
gwt help

# 短縮エイリアス
gw       # gwt（最短）
gwc      # gwt create
gwl      # gwt list  
gws      # gwt switch
gwr      # gwt remove
```

### Claude Voice統合機能（tmux）

```bash
# Claude Voice音声エンジンのテスト
~/.tmux/claude/core/wsl_voice_engine.sh test

# ステータス別効果音テスト
~/.tmux/claude/core/wsl_voice_engine.sh sound "⚡"  # 忙しい状態
~/.tmux/claude/core/wsl_voice_engine.sh sound "⌛"  # 待機状態  
~/.tmux/claude/core/wsl_voice_engine.sh sound "✅"  # 完了状態

# 複合通知テスト（効果音+音声合成）
~/.tmux/claude/core/wsl_voice_engine.sh notify "処理完了" "✅" 1 "both"
```

#### tmux Claude Voiceキーバインド
```
Prefix + v + t       # Claude Voice統合テスト
Prefix + v + v       # Claude Voice自動要約 ON/OFF
Prefix + v + 1-3     # ステータス別音声 ON/OFF (1=完了, 2=待機, 3=忙しい)
Prefix + v + s + 1-3 # 音声合成テスト (s=speech)
Prefix + v + e + 1-3 # 効果音テスト (e=effects)  
Prefix + v + n + 1-3 # 複合通知テスト (n=notification)
Prefix + v + p       # パンニング機能 ON/OFF
```

#### Claude Voiceステータス音声バリエーション
- **⚡ 忙しい状態**: 警告パターン (800Hz×2→600Hz)
- **⌛ 待機状態**: 上昇メロディー (659Hz→880Hz→1175Hz)  
- **✅ 完了状態**: 成功パターン (523Hz→659Hz→783Hz→1046Hz)
- **Equal Power Pan Law**: 3dBセンター法によるステレオ配置
- **音声合成**: Microsoft Haruka Desktopによる日本語読み上げ

## アーキテクチャ概要

### Emacs設定 (~2,500行)
- **モジュラー構成**: `~/.emacs.d/inits/` 以下に機能別設定
- **AI統合**: `init-ai.el` でOllama、GitHub Copilotを統合
- **ナレッジマネジメント**: `init-org-integrated.el` でOrg-roam + GTD
- **開発環境**: `init-dev.el` で多言語開発支援
- **カスタムLisp**: `~/.emacs.d/elisp/` にユーティリティ関数

### Zsh設定アーキテクチャ
- **ユーティリティライブラリ**: `~/.zsh/utils.zsh` - OS検出、コマンドキャッシュ
- **プラグイン管理**: `~/.zsh/zinit_setup.zsh` - 最適化されたZinit設定
- **エイリアス**: `~/.zsh/aliases.zsh` - モダンツール統合（exa, bat, fd, rg）
- **キーバインド**: `~/.zsh/keybindings.zsh` - Emacsライクナビゲーション

### キーバインド統一システム
- **macOS**: Karabiner-Elements設定 (`karabiner/`)
- **Windows**: Mayu設定 (`mayu/`)
- **クロスプラットフォーム**: Emacsライクキーバインド統一

### WSL最適化
- **フォント**: `wsl/etc_fonts/local.conf`
- **日本語入力**: `wsl/mnt_c_opt_mozc/`
- **X11設定**: `wsl/vcxsrv/config.xlaunch`

## 重要な設計原則

### パフォーマンス最適化
- **キャッシュ機構**: OS検出とコマンド存在チェックをキャッシュ
- **遅延読み込み**: Zinitとuse-packageで段階的ロード
- **PATH管理**: 重複チェックと安全な追加機能

### エラーハンドリング
- **グレースフル劣化**: モダンツール未導入時の自動フォールバック
- **設定検証**: デバッグモードでの包括的チェック
- **バックアップ**: 既存設定の自動保護

### AI統合ワークフロー
- **複数LLMプロバイダー**: Ollama（ローカル）、GitHub Copilot（クラウド）
- **専門化モデル**: コーディング用、翻訳用、汎用の使い分け
- **統合キーバインド**: `C-c a` プレフィックスで統一操作

## 主要ファイルの役割

### 設定の読み込み順序
1. `~/.zprofile` → OS検出とPATH設定
2. `~/.zshrc` → インタラクティブ設定
3. `~/.zsh/utils.zsh` → ユーティリティ関数
4. `~/.zsh/zinit_setup.zsh` → プラグイン読み込み

### Emacs初期化順序
1. `early-init.el` → パフォーマンス最適化
2. `init.el` → パッケージシステム初期化
3. `inits/` モジュール群 → 機能別設定

## トラブルシューティング

### よくある問題のデバッグ手順
1. **Zsh起動エラー**: `ZSH_DEBUG=1` でデバッグモード実行
2. **Emacs起動失敗**: `emacs --debug-init` で初期化デバッグ
3. **パッケージエラー**: `M-x package-refresh-contents` でリフレッシュ
4. **WSL表示問題**: `echo $DISPLAY` でX11設定確認

### パフォーマンス問題
- Emacs起動時間: `M-x emacs-init-time` で測定
- Zsh起動時間: `time zsh -i -c exit` で測定
- キャッシュクリア: 上記コマンド参照

## 開発時の注意点

### コード修正時のワークフロー
1. 該当ファイルのバックアップ作成
2. テストスクリプトで動作確認
3. 段階的デプロイ（新ファイル → リンク切り替え）
4. 設定検証スクリプト実行

### 新機能追加時
- OS固有機能は条件分岐で対応
- フォールバック機構を必ず実装
- デバッグ出力を含める
- パフォーマンス影響を測定

## Claude Commands管理

### コマンド実行
```bash
# フォルダ構造での実行
/dev:git-status
/team:issue-triage
/deploy:prepare-release
```

### 出典確認
```bash
# 出典統計（Commands/Agents/Hooks/Skills）
./scripts/check_command_sources.sh

# 新規追加
./scripts/add_new_command.sh <category> <name> <source>
```

## Claude-Command-Suite統合機能（2026-01-20追加）

### 統合規模
- Commands: 208ファイル（119ファイル追加）
- Agents: 128ファイル（新規）
- Hooks: 24ファイル + スクリプト（新規）
- Skills: 2スキル（新規）

詳細は`.claude/INTEGRATION_REPORT.md`を参照してください。

### 新規カテゴリー（12個追加）

#### アーキテクチャ・セマンティック系
- `/boundary:detect` - アーキテクチャ境界検出
- `/boundary:risk-assess` - 境界リスク評価
- `/semantic:tree-view` - セマンティックツリー表示
- `/semantic:node-build` - セマンティックノード構築

#### コンテキスト・メモリ管理
- `/context:optimize-prompt` - プロンプト最適化（トークン効率化）
- `/memory:checkpoint` - コンテキストチェックポイント作成
- `/memory:compress` - メモリ圧縮
- `/memory:recall` - メモリ呼び出し

#### ワークフロー・統合
- `/orchestration:start` - マルチエージェントタスク開始
- `/orchestration:status` - タスクステータス確認
- `/sync:bidirectional-sync` - GitHub-Linear双方向同期
- `/sync:linear-task-to-issue` - LinearタスクをGitHub Issueに変換

#### 言語・フレームワーク特化
- `/rust:audit-clean-arch` - Rustクリーンアーキテクチャ監査
- `/rust:suggest-refactor` - Rustリファクタリング提案
- `/svelte:component` - Svelteコンポーネント作成
- `/svelte:storybook` - Storybook統合

#### 推論・ワークフロー
- `/reasoning:multi-path` - 多経路推論
- `/wfgy:init` - WFGY推論システム初期化
- `/spec-workflow:parallel-tasks` - 並列タスク実行

#### スキル管理
- `/skills:build-skill` - 新規スキル作成
- `/skills:package-skill` - スキルパッケージ化

### エージェント機能（128ファイル）

Claude Codeの専門化エージェント（独立実行可能）:

**開発系**
- backend-architect: バックエンドアーキテクチャ設計
- full-stack-developer: フルスタック開発
- golang-pro: Go言語専門家
- legacy-modernizer: レガシーコード現代化

**データ/AI系**
- data-scientist: データ分析・機械学習
- ml-engineer: MLモデル開発
- database-optimizer: データベース最適化
- graphql-architect: GraphQL設計

**インフラ・DevOps系**
- devops-engineer: CI/CD・インフラ自動化
- kubernetes-specialist: K8sクラスター管理
- cloud-architect: クラウドアーキテクチャ

**品質・セキュリティ系**
- test-engineer: テスト戦略・自動化
- security-auditor: セキュリティ監査
- code-reviewer: コード品質レビュー

### フック機能（24ファイル）

Claude Codeのイベント駆動型自動化:

**主要フック**
- **SessionStart**: セッション開始時の自動設定（プロジェクト情報読み込み、環境チェック）
- **PreToolUse**: ツール実行前の承認・検証（危険なコマンド警告）
- **Stop**: エージェント停止時の完了度チェック（未完了タスク警告）

**設定方法**
`.claude/hooks/README.md`を参照してください。

**例: Svelte特化フック**
```bash
# Svelte開発用フックのインストール
./.claude/hooks/install-svelte-hooks.sh
```

### スキル機能（2スキル）

専門知識パッケージ:

#### cloudflare-manager
Cloudflare Workers/KV/R2/Pages管理

**機能**:
- Workers デプロイ・管理
- KV Storage 操作
- R2 オブジェクトストレージ
- Pages デプロイ
- DNS/ルート設定

**要件**:
- Bun ランタイム
- CLOUDFLARE_API_KEY 環境変数

#### linear-todo-sync
Linear-GitHub Todo同期

**機能**:
- Linear API から割り当てタスク取得
- マークダウンTodoリスト生成
- プロジェクトルートに自動配置

**要件**:
- Python 3.7+
- LINEAR_API_KEY 環境変数

**使用方法**:
```bash
# Linearタスク同期
/linear-todo-sync
```

### 翻訳機能

統合されたファイルは英語のままですが、必要に応じて日本語化できます:

```bash
# セクションヘッダーのみ翻訳（辞書ベース）
./scripts/translate_command.sh .claude/commands/dev/git-status.md

# Ollamaで完全翻訳（本文も含む）
./scripts/translate_command.sh .claude/commands/dev/git-status.md --full-translate
```

翻訳辞書: `scripts/dictionary.json`

### カテゴリー統合スクリプト

新しいカテゴリーを一括統合:

```bash
# カテゴリー別統合
./scripts/integrate_category.sh <category>

# 翻訳なしで統合
./scripts/integrate_category.sh <category> --no-translate

# バックアップなしで統合
./scripts/integrate_category.sh <category> --no-backup
```

### 統合検証

```bash
# 統合状態の検証
./scripts/validate_integration.sh

# カテゴリー別検証
./scripts/validate_integration.sh dev
```