# claude-home/ - Claude Code 汎用拡張モジュール

> **Claude Code のためのドメイン非依存の汎用開発支援ツール集**
>
> このディレクトリには、プロジェクト固有の設定（dotfiles専用）を除く、あらゆるソフトウェアプロジェクトで活用できる汎用的な Commands / Agents / Hooks / Skills が格納されています。

## 📁 ディレクトリ構造

```
claude-home/
├── commands/         (194ファイル) - カテゴリ別コマンド定義
├── agents/           (132ファイル) - 専門化エージェント定義
├── hooks/            (24ファイル)  - イベント駆動型自動化スクリプト
├── skills/           (6スキル)     - 知識ベース・API統合
├── INTEGRATION_REPORT.md           - Claude-Command-Suite統合履歴
├── FOLDER_STRUCTURE_MIGRATION_PLAN.md - カテゴリ分類履歴
└── settings.local.json             - ローカル権限設定
```

## 🎯 設計原則

### 1. ドメイン分離原則
- **汎用モジュール**: `claude-home/` - プロジェクト非依存の再利用可能コンポーネント
- **専用モジュール**: `.claude/skills/{emacs,tmux,zsh,keyboard}-config/` - dotfiles専用の実装知識

### 2. レイヤードアーキテクチャ
```
┌─────────────────────────────────────┐
│ Commands (UI層)                     │ ← ユーザー呼び出し可能なエントリポイント
│ 194ファイル、22カテゴリ              │    `/category:command` 形式で実行
└─────────────────────────────────────┘
            ↓
┌─────────────────────────────────────┐
│ Agents (ビジネスロジック層)          │ ← 専門化された自律的な処理単位
│ 132ファイル (開発/データ/インフラ/品質) │    Task ツールで起動
└─────────────────────────────────────┘
            ↓
┌─────────────────────────────────────┐
│ Skills (知識・API層)                 │ ← ドメイン知識とAPI統合
│ 6スキル (AI-DDL/Linear/Jira/GH/CF) │    Skill ツールで起動
└─────────────────────────────────────┘
            ↓
┌─────────────────────────────────────┐
│ Hooks (イベント処理層)               │ ← ライフサイクル自動化
│ 24ファイル (SessionStart/PreToolUse) │    Claude Codeイベント駆動
└─────────────────────────────────────┘
```

### 3. 出典追跡とオープンソース統合
- **出典管理**: `.gitattributes` で全ファイルの由来を追跡
  - `Claude-Command-Suite` 由来: 208 Commands + 128 Agents + Hooks + Skills
  - `Original`: adr.md, .claude/commands/dev/* など既存ファイル
- **翻訳戦略**: 段階的日本語化（`scripts/translate_command.sh` で辞書ベース翻訳）
- **検証ツール**: `scripts/check_command_sources.sh` で統計確認

## 🗂️ Commands（194ファイル / 22カテゴリ）

### カテゴリ分類
| カテゴリ | ファイル数 | 目的 | 代表例 |
|---|---|---|---|
| **dev** | 20 | 開発ワークフロー | `/dev:git-status`, `/dev:code-review` |
| **test** | 11 | テスト自動化 | `/test:write-tests`, `/test:test-coverage` |
| **deploy** | 10 | デプロイメント | `/deploy:prepare-release`, `/deploy:ci-setup` |
| **security** | 5 | セキュリティ監査 | `/security:security-audit` |
| **docs** | 7 | ドキュメント生成 | `/docs:generate-api-documentation` |
| **performance** | 9 | パフォーマンス最適化 | `/performance:optimize-bundle-size` |
| **setup** | 13 | プロジェクト初期化 | `/setup:setup-development-environment` |
| **project** | 15 | プロジェクト管理 | `/project:init-project`, `/project:milestone-tracker` |
| **team** | 16 | チームコラボレーション | `/team:sprint-planning`, `/team:standup-report` |
| **svelte** | 16 | Svelte開発支援 | `/svelte:component`, `/svelte:storybook` |
| **rust** | 7 | Rustクリーンアーキテクチャ | `/rust:audit-clean-arch` |
| **orchestration** | 11 | ワークフロー統合 | `/orchestration:start` |
| **semantic** | 6 | セマンティック解析 | `/semantic:tree-view` |
| **wfgy** | 7 | WFGY推論システム | `/wfgy:init`, `/wfgy:formula-all` |
| **boundary** | 5 | アーキテクチャ境界検出 | `/boundary:detect` |
| **reasoning** | 5 | AI推論強化 | `/reasoning:multi-path` |
| **memory** | 5 | メモリ最適化 | `/memory:checkpoint` |
| **skills** | 4 | スキル管理 | `/skills:build-skill` |
| **spec-workflow** | 4 | 仕様ワークフロー | `/spec-workflow:parallel-tasks` |
| **simulation** | 12 | シミュレーション | `/simulation:system-behavior-simulator` |
| **context** | 1 | コンテキスト管理 | `/context:optimize-prompt` |
| **特殊** | 3 | セッション継続 | `/handoff`, `/handoff-continue`, `/spec-elicitation` |

### 実行方法
```bash
# カテゴリ:コマンド形式
/dev:git-status
/test:write-tests
/security:security-audit

# 特殊コマンド（カテゴリなし）
/handoff            # コンテキスト引き継ぎ
/spec-elicitation   # 仕様抽出インタビュー
```

## 🤖 Agents（132ファイル / 8領域）

### 専門化エージェント分類
| 領域 | 代表エージェント | 役割 |
|---|---|---|
| **開発系** | `backend-architect`, `full-stack-developer`, `golang-pro`, `typescript-pro` | アーキテクチャ設計、実装 |
| **データ/AI系** | `data-scientist`, `ml-engineer`, `ai-engineer` | データ処理、ML/AI統合 |
| **インフラ系** | `devops-engineer`, `cloud-architect`, `kubernetes-specialist` | インフラ構築、運用 |
| **品質系** | `test-engineer`, `qa-expert`, `security-auditor`, `code-auditor` | テスト、品質保証 |
| **ドキュメント系** | `documentation-expert`, `api-documenter` | ドキュメント作成 |
| **チーム系** | `product-manager`, `ux-designer` | プロジェクト管理、UX設計 |
| **推論・メモリ系** | `semantic-architect`, `reasoning-validator`, `boundary-guardian` | WFGY推論システム（8エージェント） |
| **チケット管理系** | `github-project-ticket`, `ticket-sync`, `github-project-board` | AI-DDL統合 |

### WFGY推論システム（8エージェント）
**出典**: [onestardao/WFGY](https://github.com/onestardao/WFGY) - "万法归一" セマンティック推論システム

| エージェント | ペルソナ | 役割 |
|---|---|---|
| `semantic-architect` | Atlas | 永続的セマンティックメモリツリー構築 |
| `reasoning-validator` | Euclid | ロジックチェーン数学的検証 |
| `boundary-guardian` | Sentinel | ハルシネーション防止（知識境界監視） |
| `memory-curator` | Mnemonic | メモリ最適化・圧縮・マージ |
| `logic-synthesizer` | Synthesis | 並列解空間探索・最適解統合 |
| `decision-navigator` | Navigator | 戦略的意思決定ガイド |
| `knowledge-mapper` | Cartographer | 知識グラフ・概念関係マッピング |
| `cognitive-debugger` | Debugger | 推論失敗診断・修復 |

### エージェント起動方法
```bash
# Taskツールで起動
Task: semantic-architect
Task: reasoning-validator

# 推論ワークフロー例
1. semantic-architect で永続メモリツリー初期化
2. reasoning-validator で推論検証
3. boundary-guardian でハルシネーション検知
4. memory-curator でメモリ最適化
```

## 🎣 Hooks（24ファイル + スクリプト）

### イベントドリブン自動化
| Hook種別 | ファイル数 | 役割 | 例 |
|---|---|---|---|
| **SessionStart** | 8 | セッション開始時の自動設定 | プロジェクト構造読み込み、環境変数設定 |
| **PreToolUse** | 10 | ツール実行前の承認・検証 | git push前の確認、破壊的操作の防止 |
| **Stop** | 6 | エージェント停止時の完了度チェック | タスク完了確認、クリーンアップ |

### 設計パターン
- **シェルスクリプトベース**: Bashで実装（`.tmux/scripts/` などと同様）
- **イベント駆動**: Claude Codeのライフサイクルイベントに自動応答
- **カスタマイズ可能**: `hooks/` 内のファイルを編集して動作変更

## 🛠️ Skills（6スキル）

### スキル一覧
| スキル名 | 種別 | 役割 | API依存 |
|---|---|---|---|
| **ticket-management** | 理論 | AI-DDLチケット管理フレームワーク | なし |
| **github-projects-v2** | 知識ベース | GitHub Projects V2 API/CLI リファレンス | gh CLI |
| **linear** | API統合 | Linear GraphQL/MCP 統合 | LINEAR_API_TOKEN |
| **jira** | API統合 | Jira Cloud MCP 統合 | Atlassian MCP Plugin |
| **cloudflare-manager** | API統合 | Cloudflare Workers/KV/R2/Pages管理 | CLOUDFLARE_API_KEY |
| **prompt-engineering** | 理論 | Coding Agentプロンプトベストプラクティス | なし |

### AI-DDL（AI-Driven Development Lifecycle）
**中核理論**: `ticket-management` Skill で定義

| 観点 | 従来型 | AI-DDL |
|---|---|---|
| **粒度** | 1-3営業日 | コンテキストウィンドウ単位（Atomic Spec） |
| **ライフサイクル** | 線形5段階 | 循環型7段階（Agent Loop） |
| **規模定義** | 人数ベース | AI/人間ハイブリッド構成 |
| **鮮度管理** | 人間会議 | AI Janitor（自動再現検証） |
| **DoD** | 手動チェックリスト | Hooks強制 + 自動検証 |

**Atomic Spec 5要素**:
1. Context（背景）
2. Current Behavior（現状）
3. Expected Behavior（あるべき姿）
4. Constraints（制約条件）
5. Verification（検証方法）

**Agent Loop 7段階**:
1. Triage (AI自動)
2. Spec Definition (人間)
3. AI Planning (Plan Agent)
4. AI Implementation (Build Agent)
5. Auto-Verification (CI)
6. Human Review (ARE)
7. Done

**AI Janitor 3ルール**:
- Stale自動検証（7日超Bug）
- 4時間ルール（粒度超過検知）
- Churn 3回ルール（仕様曖昧性検知）

### スキル起動方法
```bash
# Skillツールで起動（理論・知識ベース読み込み）
Skill: ticket-management
Skill: github-projects-v2

# Skill内リファレンス構造
ticket-management/
├── SKILL.md               # 概要・クイックリファレンス
└── references/
    ├── atomic-spec.md     # チケット書き方詳細
    ├── agent-loop.md      # ライフサイクル詳細
    ├── organization-topology.md  # 規模別設計
    └── ai-native-practices.md    # DoD/メトリクス詳細
```

## 🔧 重要な設計決定

### 1. **カテゴリ分類とフラット構造の廃止**
- **移行日**: 2025年7月21日
- **Before**: フラット78ファイル
- **After**: 22カテゴリ194ファイル
- **利点**: `/category:command` 形式でカテゴリベース補完可能

### 2. **Claude-Command-Suite統合**
- **統合日**: 2026年1月20日
- **規模**: 362ファイル追加（Commands 119 + Agents 128 + Hooks 24 + Skills拡張）
- **翻訳方針**: 英語のまま統合、段階的日本語化（`scripts/translate_command.sh`）

### 3. **プラットフォーム選択基準**（AI-DDL準拠）
- **デフォルト**: GitHub Projects V2（中小規模、100名以下）
- **エンタープライズ**: Jira（100名超、複雑なワークフロー）
- **根拠**: `skills/ticket-management/references/organization-topology.md`

### 4. **認証とAPI依存性管理**
| 機能 | 依存API | 環境変数 | 設定方法 |
|---|---|---|---|
| Linear統合 | Linear GraphQL | `LINEAR_API_TOKEN` | https://linear.app/settings/api |
| Cloudflare管理 | Cloudflare API | `CLOUDFLARE_API_KEY` | Cloudflare Dashboard |
| GitHub操作 | GitHub API | なし（gh CLI認証） | `gh auth login` |
| Jira統合 | Atlassian MCP | なし（MCP Plugin認証） | Atlassian Claude Plugin |

## 📊 メトリクス・品質指標

### Commands品質
- **総数**: 194ファイル
- **カテゴリカバレッジ**: 22カテゴリ（開発ライフサイクル全域）
- **翻訳状況**: 英語版100%、日本語化段階的（辞書ベース翻訳ツール対応）

### Agents品質
- **総数**: 132エージェント
- **専門化率**: 8領域に分類（開発/データ/インフラ/品質/ドキュメント/チーム/推論/チケット管理）
- **WFGY統合**: 8エージェント（セマンティック推論システム）

### AI-DDL品質メトリクス
- **MTTV** (Mean Time To Verify): CI完了までの時間
- **Interaction Churn**: プロンプト往復回数（3回以下が目標）
- **AI-Confidence**: AI Planning段階での実装可能性スコア

## 🚀 使い方

### 初回セットアップ
```bash
# 1. 出典確認
./scripts/check_command_sources.sh

# 2. 必要に応じてAPI設定
export LINEAR_API_TOKEN="lin_api_***"
export CLOUDFLARE_API_KEY="***"

# 3. Hooksの有効化（必要に応じて）
# hooks/ 内のファイルをカスタマイズ
```

### 典型的なワークフロー

#### 1. 新機能開発（AI-DDL準拠）
```bash
# ① チケット作成（Atomic Spec 5要素）
Skill: ticket-management
# → Context / Current / Expected / Constraints / Verification を記述

# ② Agent Loopステージ遷移
# Triage (AI自動) → Spec Definition (人間) → AI Planning (Plan Agent)

# ③ 実装（Buildエージェント起動）
Task: full-stack-developer
# → 4時間以内、Churn 3回以内で実装

# ④ 自動検証
/test:write-tests
/security:security-audit

# ⑤ リリース
/deploy:prepare-release
```

#### 2. コードレビュー
```bash
/dev:code-review
Task: code-auditor
Task: architecture-auditor
```

#### 3. パフォーマンス最適化
```bash
/performance:performance-audit
Task: performance-engineer
/performance:optimize-bundle-size
```

#### 4. WFGY推論ワークフロー
```bash
# ① メモリツリー初期化
Task: semantic-architect
> *init-tree "ProjectName"

# ② 推論実行・検証
Task: reasoning-validator
> *validate-all "複雑な推論内容"

# ③ ハルシネーション検知
Task: boundary-guardian
> *detect-boundary "不確実なトピック"

# ④ メモリ最適化
Task: memory-curator
> *compress-memory
> *export-tree "knowledge.json"
```

## ⚠️ Gotchas・非自明な挙動

### 1. **Atomic Spec粒度制限**
- **制約**: コンテキストウィンドウ単位（2-4時間、変更5ファイル以下、PR差分300行以下）
- **理由**: AI Agentのコンテキスト限界を超えるとChurn増加
- **対処**: 粒度超過時は自動で差し戻し（4時間ルール）

### 2. **Linear Assignee単一制限**
- **制約**: Issue のAssigneeは**単一ユーザーのみ**（GitHubと異なる）
- **対処**: 複数人協業が必要な場合は Sub-task に分割

### 3. **GitHub Projects V2 カスタムフィールド**
- **注意**: V1とV2でAPIが完全に非互換（GraphQL必須）
- **ツール**: `gh project` CLI または GraphQL直接利用

### 4. **Cloudflare Workers デプロイ要件**
- **依存**: Bun runtime + 依存パッケージインストール必須
- **検証**: `cloudflare-manager/scripts/` で事前チェック

### 5. **WFGY セマンティック張力（ΔS）閾値**
- **安全**: ΔS < 0.4（高信頼）
- **危険**: ΔS ≥ 0.85（ハルシネーションリスク）
- **対処**: `boundary-guardian` が自動検知・フォールバック実行

## 🔄 メンテナンス

### 新規コマンド追加
```bash
# カテゴリ別追加（出典自動記録）
./scripts/add_new_command.sh dev new-command Original
./scripts/add_new_command.sh test integration-test Claude-Command-Suite
```

### 翻訳（段階的日本語化）
```bash
# セクションヘッダーのみ翻訳
./scripts/translate_command.sh .claude/commands/dev/git-status.md

# Ollama完全翻訳
./scripts/translate_command.sh .claude/commands/dev/git-status.md --full-translate
```

### 検証
```bash
# 統合検証（ファイル数・出典・構造）
./scripts/validate_integration.sh

# 出典統計
./scripts/check_command_sources.sh
```

## 📚 関連ドキュメント

| ドキュメント | 内容 |
|---|---|
| `INTEGRATION_REPORT.md` | Claude-Command-Suite統合履歴詳細 |
| `FOLDER_STRUCTURE_MIGRATION_PLAN.md` | カテゴリ分類履歴 |
| `agents/README.md` | WFGY推論システムエージェント詳細 |
| `skills/ticket-management/references/` | AI-DDL詳細リファレンス |
| `skills/linear/references/` | Linear API/MCP詳細 |
| `skills/github-projects-v2/references/` | GitHub Projects V2 CLI/GraphQL詳細 |
| `hooks/` | Hook種別別実装例 |

## 🎓 学習リソース

- **WFGY推論システム**: https://github.com/onestardao/WFGY
- **Claude-Command-Suite**: https://github.com/disler (DislerH)
- **AI-DDL理論**: `skills/ticket-management/references/atomic-spec.md`
- **Linear API**: https://developers.linear.app/docs/graphql/working-with-the-graphql-api
- **GitHub Projects V2**: https://docs.github.com/en/issues/planning-and-tracking-with-projects

---

**ステータス**: ✅ 統合完了・段階的日本語化進行中
**最終更新**: 2026年1月20日（Claude-Command-Suite統合）
**メンテナンス**: 新規コマンド追加時は `scripts/add_new_command.sh` 使用
