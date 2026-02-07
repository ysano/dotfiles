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

## 詳細ドキュメント

| ドキュメント | 内容 |
|---|---|
| `docs/commands-reference.md` | Emacs/Zsh/Git Worktree/tmux/Claude Commands の詳細コマンド |
| `docs/architecture.md` | アーキテクチャ概要、ファイル役割、トラブルシューティング |
| `claude-home/INTEGRATION_REPORT.md` | Claude-Command-Suite統合機能の詳細（汎用ツール群） |
| `~/.tmux/plugin-config/README.md` | tmux resurrect/continuum設定の詳細 |
| `~/.tmux/docs/` | Claude Voice統合機能の詳細ドキュメント |
| `.claude/skills/{emacs,tmux,zsh}-config/` | 各コンテキスト固有の設定修正ガイド（Skill） |
| `.claude/agents/dotfiles-{engineer,validator}.md` | dotfiles固有のSubAgent（実装者・検証者） |
