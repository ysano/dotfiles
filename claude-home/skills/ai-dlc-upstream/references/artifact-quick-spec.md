# Quick Spec フォーマット仕様

AI-DLC 上流成果物契約。`/ai-dlc:quick-spec` が生成する軽量単一文書のフォーマット。
PRD + Architecture + Stories を 1 文書に集約する。

## 適用基準

| 条件 | Quick Spec | Full Pipeline |
|---|---|---|
| Story 数 | 1-5 | 6+ |
| チーム規模 | ソロ / Pod (1-10) | Squad / Enterprise |
| 期間 | 1-3 日 | 1 週間以上 |
| 技術的複雑度 | 低-中 | 高 |

Full Pipeline が必要な場合は `/ai-dlc:create-prd → /ai-dlc:create-architecture → /ai-dlc:create-stories` を案内。

## メタデータヘッダ

```yaml
---
type: ai-dlc-quick-spec
version: "1.0"
status: draft | review | approved
created: YYYY-MM-DD
story-count: {N}
---
```

## 必須セクション

### 1. Problem & Goal

問題とゴールを簡潔に記述（PRD の Problem Statement 相当）。

```markdown
**Problem**: {解決すべき問題を 1-2 文で}
**Goal**: {達成したいことを 1-2 文で}
**Success Criteria**: {成功の判断基準を箇条書き}
```

### 2. Solution Overview

技術的アプローチの概要（Architecture 相当）。

```markdown
**Approach**: {技術的アプローチを 2-3 文で}
**Key Decisions**:
- {判断 1}: {選択肢} — {理由}
- {判断 2}: {選択肢} — {理由}

**Out of Scope**:
- {含まれないもの}
```

### 3. Stories

Atomic Spec 5 要素スタブ形式で Story を列挙。

```markdown
#### Story 1: {動詞で始まるタイトル}
- **Size**: S | M
- **Dependencies**: None | Story N

**Context**: {背景を 1-2 文で}
**Current Behavior**: {現状 or "New feature"}
**Expected Behavior**: {期待する振る舞い}
**Constraints**:
- {制約}
**Verification**:
- [ ] {テスト項目}
```

### 4. Out of Scope

明示的に範囲外とするもの。将来の検討事項を含む。

## Quick Spec と Full Pipeline の対応表

| Quick Spec セクション | Full Pipeline 相当 |
|---|---|
| Problem & Goal | PRD: Problem Statement + Success Metrics |
| Solution Overview | Architecture: Executive Summary + ADR |
| Stories | Epic/Story: Atomic Spec スタブ群 |
| Out of Scope | PRD: Scope (Out of Scope) |

## 出力先

`docs/spec-{kebab-case}.md`（プロジェクトルート基準）
