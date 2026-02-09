# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 重要な注意事項

- 不具合を修正する前に、不具合を再現するテストコードを書き、失敗することを確認すること。その後、実装を変更すること。
- 日本語で受け答えすること

## リポジトリ概要

クロスプラットフォーム（Windows、macOS、Linux/WSL）対応の統合開発環境dotfiles。Emacs中心の構成で、AI統合ワークフロー、日本語環境、高度なZsh設定を含む。

## 主要な開発コマンド

```bash
# セットアップ
./link.sh                # シンボリックリンク作成
./test_zsh_config.zsh    # Zsh設定の検証とテスト
```

Emacs/Zsh/Git Worktree/tmux/Claude Commands の詳細コマンドは `docs/commands-reference.md` を参照。

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

## Claude Code モジュール構成

このdotfilesリポジトリには、2つのClaude Code モジュールディレクトリがあります：

### `.claude/` - dotfiles固有モジュール（8ファイル）
**目的**: このdotfilesリポジトリの開発・管理・運用に特化

```
.claude/
├── agents/           (2) - dotfiles専門エージェント
│   ├── dotfiles-engineer.md    - Emacs/Zsh/tmux/Keyboard設定変更
│   └── dotfiles-validator.md   - クロスプラットフォーム互換性検証
├── commands/dev/     (2) - dotfiles運用コマンド
│   ├── commit.md               - /dev:commit - 品質チェック統合コミット
│   └── pull-request.md         - /dev:pull-request - PR作成
├── hooks/            - dotfilesイベント駆動自動化
└── skills/           (4) - dotfiles設定アーキテクチャ知識
    ├── emacs-config/           - 4,800行、12モジュール構成
    ├── keyboard-config/        - Karabiner 4,209行 + skhd 230行 + yabai 71行
    ├── tmux-config/            - 3,800行、Claude Voice統合
    └── zsh-config/             - 4,600行、Zinit + 二段階ロード
```

詳細: `.claude/CLAUDE.md`

### `claude-home/` - 汎用モジュール（526ファイル）
**目的**: あらゆるソフトウェアプロジェクトで再利用可能な開発支援ツール

```
claude-home/
├── agents/          (132) - 専門化エージェント（開発/データ/インフラ/品質/WFGY推論）
├── commands/        (194) - 22カテゴリ（dev/test/deploy/security/docs/performance...）
├── hooks/           (24)  - イベント駆動自動化（SessionStart/PreToolUse/Stop）
└── skills/          (6)   - AI-DDL/Linear/Jira/GitHub Projects/Cloudflare/プロンプトエンジニアリング
```

詳細: `claude-home/CLAUDE.md`

### 使い分けガイド

| タスク | 使用先 | 例 |
|---|---|---|
| **dotfiles設定変更** | `.claude/` | Emacsキーバインド追加、Zshエイリアス追加、tmux設定調整 |
| **dotfiles品質検証** | `.claude/` | クロスプラットフォーム互換性検証、パフォーマンス測定 |
| **dotfiles運用** | `.claude/` | 設定変更のコミット、PR作成 |
| **汎用開発タスク** | `claude-home/` | コードレビュー、テスト作成、API設計、デプロイ、セキュリティ監査 |
| **汎用エージェント** | `claude-home/` | React開発、Rust実装、データ分析、ML開発、インフラ構築 |

**判断基準**: Emacs/Zsh/tmux/Keyboard設定に直接関連する → `.claude/`、それ以外 → `claude-home/`

## 詳細ドキュメント

| ドキュメント | 内容 |
|---|---|
| **Claude Code モジュール** | |
| `.claude/CLAUDE.md` | dotfiles固有モジュール詳細（agents/commands/hooks/skills） |
| `claude-home/CLAUDE.md` | 汎用モジュール詳細（194 commands、132 agents、24 hooks、6 skills） |
| **dotfiles アーキテクチャ** | |
| `docs/commands-reference.md` | Emacs/Zsh/Git Worktree/tmux/Claude Commands の詳細コマンド |
| `docs/architecture.md` | アーキテクチャ概要、ファイル役割、トラブルシューティング |
| **設定知識ベース** | |
| `.claude/skills/emacs-config/SKILL.md` | Emacs設定アーキテクチャ（4,800行、use-package、Copilot/Ellama統合） |
| `.claude/skills/zsh-config/SKILL.md` | Zsh設定アーキテクチャ（4,600行、Zinit、二段階ロード） |
| `.claude/skills/tmux-config/SKILL.md` | tmux設定アーキテクチャ（3,800行、Claude Voice統合、TPM） |
| `.claude/skills/keyboard-config/SKILL.md` | Keyboard設定アーキテクチャ（Karabiner/skhd/yabai） |
| **統合機能** | |
| `claude-home/INTEGRATION_REPORT.md` | Claude-Command-Suite統合履歴（362ファイル追加詳細） |
| `~/.tmux/plugin-config/README.md` | tmux resurrect/continuum設定の詳細 |
| `~/.tmux/docs/` | Claude Voice統合機能の詳細ドキュメント |
