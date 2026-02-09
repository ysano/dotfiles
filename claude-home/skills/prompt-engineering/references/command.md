## Command プロンプト リファレンス

**配置**: `.claude/commands/<category>/<name>.md` または `.claude/commands/<name>.md`
**呼び出し**: `/category:name` または `/name`

### フォーマット

```markdown
---
description: "動詞で始まる簡潔な説明（1行）"
---

## Instructions

[実行手順をここに記述]
```

### 必須要素

| 要素 | ルール |
|------|--------|
| **description** | フロントマターに必須。動詞で始める（"Create...", "Analyze..."） |
| **H1 禁止** | `#` で始めない。description がタイトルの役割を果たす |
| **セクション** | `## Instructions` または `## 実行手順` で開始 |

### 避けるべきパターン

- `# Title` の H1 ヘッダー → description フロントマターで代替
- `Remember to...` で終わるアドバイス → 指示として統合するか削除
- `## Best Practices` セクション → 指示に統合するか削除
- 300行超 → 単一責任に分割
- `$ARGUMENTS` の過度な使用 → 自然文で指示を記述

### 良い例

```markdown
---
description: "Perform comprehensive code quality review"
---

## Instructions

Analyze the codebase for quality, security, and maintainability issues:

1. **Code Quality**: Check naming, structure, complexity
2. **Security**: Identify OWASP Top 10 vulnerabilities
3. **Performance**: Find bottlenecks and inefficiencies

Output a structured report with severity levels and specific file:line references.
```

### 悪い例

```markdown
# Comprehensive Code Quality Review   ← H1 禁止

Perform comprehensive code quality review   ← description が本文に紛れている

## Instructions

...

## Best Practices   ← ノイズ
Remember to be constructive...   ← アドバイスであり指示ではない
```
