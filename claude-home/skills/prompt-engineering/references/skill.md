## Skill プロンプト リファレンス

**配置**: `.claude/skills/<name>/SKILL.md` + `references/` + `scripts/`
**呼び出し**: `/skill-name`（ユーザー）、Skill ツール（Claude 自動）

> Commands（`.claude/commands/`）は Skills に統合された。既存の commands ファイルはそのまま動作するが、新規作成は Skills を推奨。

### ディレクトリ構造

```
skills/<name>/
├── SKILL.md          # エントリポイント（500行未満が上限、100行未満が理想）
├── references/       # 詳細リファレンス（必要に応じて Read）
│   ├── api.md
│   └── patterns.md
└── scripts/          # 決定的アンカー（毎回生成させない）
    └── validate.sh
```

### フロントマター

すべて任意。`description` のみ推奨。

| フィールド | 説明 |
|-----------|------|
| `name` | 表示名。省略時はディレクトリ名。小文字・数字・ハイフンのみ（最大64文字） |
| `description` | Skill の用途と発動条件。Claude が自動ロード判定に使用 |
| `argument-hint` | オートコンプリートで表示するヒント。例: `[issue-number]` |
| `disable-model-invocation` | `true` で Claude の自動ロードを禁止。手動専用ワークフロー向け |
| `user-invocable` | `false` で `/` メニューから非表示。バックグラウンド知識向け |
| `allowed-tools` | Skill アクティブ時に許可なしで使えるツール |
| `model` | Skill アクティブ時のモデル |
| `context` | `fork` でサブエージェント内で実行 |
| `agent` | `context: fork` 時のサブエージェント型（`Explore`, `Plan`, カスタム名） |
| `hooks` | Skill ライフサイクルにスコープされたフック |

### 文字列置換

| 変数 | 説明 |
|------|------|
| `$ARGUMENTS` | `/skill-name` に続く全引数。未使用時は末尾に `ARGUMENTS: <値>` を追加 |
| `$ARGUMENTS[N]` | 0始まりインデックスで引数アクセス（例: `$ARGUMENTS[0]`） |
| `$N` | `$ARGUMENTS[N]` の短縮形（例: `$0`, `$1`） |
| `${CLAUDE_SESSION_ID}` | セッションID |
| `` !`command` `` | シェルコマンドを事前実行し、出力で置換（動的コンテキスト注入） |

### 呼び出し制御

| 設定 | ユーザー起動 | Claude 起動 | 用途 |
|------|:---:|:---:|------|
| デフォルト | Yes | Yes | 一般的な知識・タスク |
| `disable-model-invocation: true` | Yes | No | deploy, commit 等の副作用あり操作 |
| `user-invocable: false` | No | Yes | バックグラウンド知識 |

### Skill の内容パターン

**リファレンス型**: 規約・パターン・ドメイン知識。インラインで実行。
**タスク型**: 具体的な手順。`disable-model-invocation: true` + `context: fork` が典型。

### references/ の設計

- 各ファイルは独立して読める（SKILL.md を読まなくても理解可能）
- ファイル名は内容を反映（api.md, patterns.md, gotchas.md）
- 1ファイル 100-200行が適正

### scripts/ の設計

- **目的**: トークン節約 + 一貫性 + 信頼性
- **Execute vs Read**: 結果だけ欲しい→実行、ロジック理解が必要→参照
- **エラーハンドリング**: スクリプト内で完結（Solve, Don't Punt）

### 設計原則

| 原則 | 説明 |
|------|------|
| **500行上限 / 100行理想** | SKILL.md はロード時に消費。詳細は references/ に |
| **インデックス優先** | 地図（参照先の案内）を書く。領土（全情報）は書かない |
| **constraints タグ** | 重要な制約は `<constraints>` タグで強調 |
| **scripts = 決定的アンカー** | LLM に毎回生成させず、検証済みスクリプトを配置 |
