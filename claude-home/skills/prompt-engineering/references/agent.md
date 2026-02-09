## Agent プロンプト リファレンス

**配置**: `.claude/agents/<name>.md`
**呼び出し**: Task ツール（`subagent_type: "<name>"`）

### フォーマット

```markdown
---
name: agent-name
description: 1-2文の説明。"Use PROACTIVELY for..." で使用タイミングを明示
tools: Read, Write, Edit, Grep, Glob, Bash, ...
model: sonnet
---

**Role**: 1文で役割を定義

**Expertise**: 専門分野を列挙

**Key Capabilities**:
- 能力1
- 能力2

## Core Philosophy
[判断基準・原則]

## Process
[作業フロー]

## Output Format
[出力形式の定義]

## Constraints
[やらないこと・制約]
```

### 必須フロントマター

| フィールド | ルール |
|-----------|--------|
| **name** | ファイル名と一致（拡張子なし） |
| **description** | 1-2文。"Use PROACTIVELY for..." で起動条件を明示 |
| **tools** | 必要最小限のツールリスト |
| **model** | `sonnet`（標準）/ `haiku`（軽量タスク）/ `opus`（複雑推論） |

### H1 禁止

`#` で始めない。フロントマターの `name` と `description` がその役割を果たす。

### 単一責任

- 1エージェント = 1つの明確な専門領域
- 50-150行が適正範囲
- 500行超は分割を検討

### 避けるべきパターン

- `model:` フィールドの欠落
- 埋め込み YAML ブロック内の複雑な activation protocol
- 独自コマンド体系（`*help` など）→ 標準の tool use を使う
- 過度なペルソナ設定（名前、アイコン、挨拶）→ 能力と制約に集中

### 良い例

```yaml
---
name: code-auditor
description: Proactive code quality assurance specialist. Use PROACTIVELY after any code changes.
tools: Read, Grep, Glob, Bash, WebFetch
model: sonnet
---
```

### model 選択基準

| model | 用途 |
|-------|------|
| `haiku` | 定型的な検索・分類・フォーマット変換 |
| `sonnet` | 大半のタスク（デフォルト） |
| `opus` | 複雑な推論・アーキテクチャ設計 |
