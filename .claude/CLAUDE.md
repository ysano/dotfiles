**.claude/ - dotfiles固有開発・管理モジュール**

> dotfilesリポジトリの開発・管理・運用に特化した専用コンポーネント。
> 汎用的な開発支援ツールは `claude-home/` を参照。

## 🎯 設計原則：ドメイン分離

| ディレクトリ | 対象 | 判断基準 |
|---|---|---|
| `.claude/` | dotfiles固有 | Emacs/Zsh/tmux/Keyboard設定に直接関連 |
| `claude-home/` | 汎用 | あらゆるプロジェクトで再利用可能 |

## 📁 ディレクトリ構造

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
    ├── emacs-config/           - 4,800行、use-package、AI統合
    ├── keyboard-config/        - Karabiner/skhd/yabai設定
    ├── tmux-config/            - 3,800行、Claude Voice統合
    └── zsh-config/             - 4,600行、Zinit、二段階ロード
```

詳細: 各コンポーネントの`.md`ファイルまたは`SKILL.md`を参照

## 🔄 使い分けフロー

```mermaid
graph TD
    A[タスク発生] --> B{dotfiles固有か？}
    B -->|Yes| C[.claude/ 使用]
    B -->|No| D[claude-home/ 使用]

    C --> E{何をする？}
    E -->|設定変更| F[Task: dotfiles-engineer]
    E -->|検証| G[Task: dotfiles-validator]
    E -->|コミット| H[/dev:commit]
    E -->|PR作成| I[/dev:pull-request]

    D --> J{何をする？}
    J -->|開発一般| K[claude-home/commands/dev/*]
    J -->|テスト| L[claude-home/commands/test/*]
    J -->|エージェント| M[claude-home/agents/*]
```

### 使い分け判断例

| タスク | 使用先 | 理由 |
|---|---|---|
| Emacsキーバインド追加 | `.claude/` → `Task: dotfiles-engineer` | dotfiles固有（emacs-config知識必要） |
| Zshエイリアス追加 | `.claude/` → `Task: dotfiles-engineer` | dotfiles固有（zsh-config知識必要） |
| tmux設定のOS互換性検証 | `.claude/` → `Task: dotfiles-validator` | dotfiles固有（tmux-config知識必要） |
| dotfiles変更をコミット | `.claude/` → `/dev:commit` | dotfiles固有（品質チェック統合） |
| 汎用コードレビュー | `claude-home/` → `/dev:code-review` | 汎用（プロジェクト非依存） |
| TypeScriptテスト作成 | `claude-home/` → `/test:write-tests` | 汎用（dotfiles無関係） |

## 🚀 クイックスタート

### 基本ワークフロー

```bash
# ① Skillで現状把握
Skill: emacs-config

# ② エンジニアエージェント起動
Task: dotfiles-engineer
# プロンプト: "org-roamプラグインを追加"

# ③ 検証（オプション）
Task: dotfiles-validator
# プロンプト: "追加した設定の規約準拠を検証"

# ④ コミット
/dev:commit
```

詳細: `docs/dotfiles-workflows.md`

## 📚 関連ドキュメント

| ドキュメント | 内容 |
|---|---|---|
| `claude-home/CLAUDE.md` | 汎用モジュール（194 commands、132 agents、6 skills）詳細 |
| `docs/dotfiles-workflows.md` | 典型的なワークフロー3パターン |
| `docs/dotfiles-troubleshooting.md` | トラブルシューティングQ&A |
| `docs/architecture.md` | dotfiles全体アーキテクチャ |
| `.claude/skills/*/SKILL.md` | 各設定ファイルのアーキテクチャ詳細 |

---

**最終更新**: 2026-02-09
