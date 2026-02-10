---
name: prompt-engineering
description: Coding Agent プロンプトのためのプロンプトエンジニアリング・コンテキストエンジニアリングのベストプラクティス
disable-model-invocation: false
user-invocable: true
---

Coding Agent プロンプト（skills, agents, context files）を作成・編集する際のガイドライン。

**重要**: 対応するプロンプトタイプの詳細リファレンスを必ず Read してから作業を開始すること。

## プロンプトタイプ早見表

| タイプ | 配置場所 | 呼び出し方 | 用途 | リファレンス |
|--------|----------|------------|------|--------------|
| **Skill** | `.claude/skills/<name>/SKILL.md` | `/skill-name` / 自動 | 再利用可能な知識・タスク・ワークフロー | `references/skill.md` |
| **Agent** | `.claude/agents/<name>.md` | Task tool | 特化型サブエージェント | `references/agent.md` |
| **Context File** | `CLAUDE.md` 等 | 自動ロード | 常時必要なプロジェクトコンテキスト | `references/context-file.md` |

> Commands は Skills に統合された。既存の `.claude/commands/` はそのまま動作する。

**その他のリファレンス**:
- `references/command.md` - レガシー Command フォーマット（Skills 推奨）
- `references/orchestration.md` - サブエージェントを呼び出すオーケストレーター設計
- `references/permission-syntax.md` - allowed-tools の権限構文
- `references/hooks.md` - ライフサイクルフック（14イベント、3ハンドラー型）

## コア原則

### 1. 単一責任
各プロンプトは一つの明確な目的を持つ。
- ✅ 環境セットアップのみ / コード実装のみ

### 2. 呼び出し元からの独立
「オーケストレーター」「親タスク」への参照を避け、入出力契約に集中。
- ✅ "提供されたコードを分析し、問題を特定..."

### 3. 簡潔さ
実行に必要な情報のみ。冗長な例、仮定パス、汎用パターンを削除。

### 4. ノイズ回避
- 複数言語の例を避ける（主要言語を選択）
- 仮定のファイルパスを避ける（CONTRIBUTING.md 等）
- LLM が推論できる詳細手順を省略

## フォーマットルール

- **H1 見出し**: Skills では使用可。Agents/Commands では禁止（フロントマターが代替）
- **言語**: `description` はプロジェクトの主要言語、本文も主要言語で可
- **XML タグ**: 複数セクションがある場合は構造化に活用

## オーケストレーション

サブエージェントを呼び出す場合:
1. **呼び出しテンプレート必須**: 完全な Task tool 使用例を含める
2. **責任分割**: サブエージェントは汎用的に、タスク固有はテンプレートに

## Context File の特別ルール

常時ロードされるため、特に慎重に:
- **80%ルール**: タスクの80%が必要とする情報のみ
- **インデックス優先**: 詳細はポインタで参照
- **コマンドの吟味**: LLM が自律実行するコマンドのみ
- **目標100行未満**

## Extended Thinking

複雑な推論には "think harder"、非常に深い分析には "ultrathink" をプロンプトに含める。
Skill 内に "ultrathink" を記述すると、Skill 実行時に extended thinking が有効化される。
