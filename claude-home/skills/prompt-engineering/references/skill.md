## Skill プロンプト リファレンス

**配置**: `.claude/skills/<name>/SKILL.md` + `references/` + `scripts/`
**呼び出し**: Skill ツール（自動ロード or 手動）

### ディレクトリ構造

```
skills/<name>/
├── SKILL.md          # エントリポイント（100行未満が目標）
├── references/       # 詳細リファレンス（必要に応じて Read）
│   ├── api.md
│   └── patterns.md
└── scripts/          # 決定的アンカー（毎回生成させない）
    └── validate.sh
```

### SKILL.md のフォーマット

```markdown
<skill-name> の概要説明（1-2文）

## クイックリファレンス

[最も頻繁に参照される情報をコンパクトに]

## 制約・Gotchas

<constraints>
- 制約1
- 制約2
</constraints>

## 詳細リファレンス

| パス | 内容 |
|------|------|
| `references/api.md` | API詳細 |
| `references/patterns.md` | パターン集 |
```

### 設計原則

| 原則 | 説明 |
|------|------|
| **100行未満** | SKILL.md は毎回ロードされる。詳細は references/ に |
| **インデックス優先** | 地図（参照先の案内）を書く。領土（全情報）は書かない |
| **constraints タグ** | 重要な制約は `<constraints>` タグで強調 |
| **scripts = 決定的アンカー** | LLM に毎回生成させず、検証済みスクリプトを配置 |

### references/ の設計

- 各ファイルは独立して読める（SKILL.md を読まなくても理解可能）
- ファイル名は内容を反映（api.md, patterns.md, gotchas.md）
- 1ファイル 100-200行が適正

### scripts/ の設計

- **目的**: トークン節約 + 一貫性 + 信頼性
- **Execute vs Read**: 結果だけ欲しい→実行、ロジック理解が必要→参照
- **エラーハンドリング**: スクリプト内で完結（Solve, Don't Punt）

### 良い例: ticket-management

```
ticket-management/
├── SKILL.md (98行)           ← インデックス + クイックリファレンス
└── references/
    ├── atomic-spec.md (154行) ← チケット書き方の詳細
    ├── agent-loop.md (107行)  ← ライフサイクル詳細
    └── organization-topology.md (129行)  ← 規模別設計
```

### 悪い例

```markdown
# スキル名   ← H1 禁止

[454行の巨大 SKILL.md]   ← 100行未満に圧縮すべき
[references/ なし]        ← 詳細は references/ に分離すべき
```
