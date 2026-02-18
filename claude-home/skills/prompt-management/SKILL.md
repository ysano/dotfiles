---
description: >
  Claude Code プロンプト（Skills, Agents, Context Files）を作成・更新・レビュー・削除する。
  Use when managing prompt files.
argument-hint: '[操作] [対象]'
disable-model-invocation: true
allowed-tools: 'Read, Edit, Write, Glob, Grep, Task(prompt-reviewer)'
---

`prompt-engineering` スキルのガイドラインとリファレンスを適用すること。

## 操作の決定

$ARGUMENTS から操作と対象を判定。不明な場合はユーザーに確認。

例: `/prompt-management review ./path/to/file.md` → 操作=レビュー, 対象=./path/to/file.md

| 操作 | 説明 |
|------|------|
| **作成** | 新規プロンプトを作成 |
| **更新** | 既存プロンプトを Read → 分析 → Edit |
| **レビュー** | 既存プロンプトを Read → 検証 → 改善提案（編集しない） |
| **削除** | 依存関係チェック後、確認を経て削除 |

## プロンプトタイプの選択

1. **Skill 作成** → `/skills:build-skill` にリダイレクト（専用ワークフロー）
2. **全セッションで必要?**（80%ルール）→ **Context File**（CLAUDE.md 等）
3. **特化サブエージェント?** → **Agent**（`.claude/agents/`）
4. **再利用ドキュメント** → **Document**（カスタムパス）

> Commands は Skills に統合済み。新規作成は Skill を推奨。

## ワークフロー

### 1. 現状把握
- 新規: タイプ決定、配置場所の確認
- 更新/レビュー: `Read` で対象ファイルを読み込み、構造を把握

### 2. 設計・実装

`prompt-engineering` スキルのリファレンスを参照（パスは `prompt-engineering` スキル内の `references/`）し、タイプに応じた設計:

- **Skill**: `prompt-engineering/references/skill.md`
- **Agent**: `prompt-engineering/references/agent.md`
- **Context File**: `prompt-engineering/references/context-file.md`
- **オーケストレーター**: `prompt-engineering/references/orchestration.md`

<constraints>
- 単一責任: 1プロンプト = 1目的
- 呼び出し元からの独立: 「オーケストレーター」「親タスク」を参照しない
- 簡潔さ: Skill は100行理想、Context File は100行未満必須
- フロントマター完備: description 必須、allowed-tools 推奨
</constraints>

### 3. 検証

- [ ] 単一責任が明確
- [ ] フロントマター必須フィールド完備
- [ ] 行数が適正（Skill: <100行理想、Context File: <100行必須）
- [ ] XML タグが全て閉じられている
- [ ] 呼び出し元への依存なし

**Agent 追加検証**（`prompt-engineering/references/agent.md` の価値判断基準を参照）:
- [ ] Claude ネイティブ能力を超える固有知識があるか
- [ ] 削除して vanilla Claude に同じタスクを依頼した場合、品質が落ちるか
- [ ] MCP 依存がある場合、公式 Marketplace プラグインで代替可能か確認済み

### 4. レビュー（任意）

多角的なフィードバックが必要な場合、並列で `prompt-reviewer` を起動:

```
Task tool:
  subagent_type: prompt-reviewer
  prompt: |
    以下のプロンプトをレビューしてください:

    [プロンプト内容]

    タイプ: [Skill|Agent|Context File]
    観点: 単一責任、簡潔さ、フォーマット準拠、オーケストレーション設計
```

### 5. 完了

ファイルの作成/更新を確認し、変更内容をユーザーに報告。
