Agent Calibration: AI-DLC 固有のエージェント検査・調整セレモニー。

## Why

従来のスクラムには存在しなかった AI-DLC 固有のイベント。
AI エージェントの動作品質、CLAUDE.md、SKILL 定義、Hooks 設定を定期的に検査・調整する。
レトロスペクティブが「人間のプロセス改善」なら、キャリブレーションは「AI エージェントのプロセス改善」を担う補完的セレモニー。

## What: セッション構成

### 基本情報

| 項目 | 内容 |
|---|---|
| **頻度** | 2 週間に 1 回（マイクロスプリント採用時でも固定） |
| **参加者** | シニアアーキテクト + ARE（AI Reliability Engineer） + プラットフォームチーム代表 |
| **所要時間** | 30-60 分 |
| **主催** | Agent Orchestration Coach（旧 Scrum Master） |

### 5 つのアジェンダ

| # | アジェンダ | 目的 |
|---|---|---|
| 1 | **AI-Confidence 分布分析** | 80 未満が頻出するタスクカテゴリの特定と対策 |
| 2 | **Interaction Churn 分析** | 高チャーンチケットのパターンと根本原因の特定 |
| 3 | **CLAUDE.md / SKILL 品質スキャン** | 完全性、鮮度、未文書化パターンの検出 |
| 4 | **Hooks 有効性レビュー** | ブロックログ分析、新ガードレールの検討 |
| 5 | **改善提案レポート** | 優先度付き推奨事項の策定 |

### 1. AI-Confidence 分布分析

- 80 未満チケットのカテゴリ別集計
- 低確信の原因パターン: Spec 品質不足 / 技術領域の知識不足 / 複雑なドメインロジック
- 対策: Spec テンプレート改善 / SKILL 追加 / タスク分割基準の調整

### 2. Interaction Churn 分析

- 3 回超チケットの一覧と原因分類（spec / scope / technical）
- 繰り返し発生するパターンの抽出
- Spec 記述ガイドラインの改善提案

### 3. CLAUDE.md / SKILL 品質スキャン

| 検査項目 | 方法 | 判定 |
|---|---|---|
| **完全性** | プロジェクト構造と CLAUDE.md の整合性 | 主要ディレクトリ/パターンが文書化済みか |
| **鮮度** | 最終更新日 vs 直近のコード変更 | 30 日以上未更新 = 要レビュー |
| **未文書化パターン** | 頻出するコードパターンの CLAUDE.md 未記載チェック | 繰り返し生成されるがルール化されていないパターン |

### 4. Hooks 有効性レビュー

- `settings.json` の Hooks 設定の棚卸し
- ブロックログの確認（過去 2 週間で発動した Hook の回数と妥当性）
- 新たに追加すべきガードレールの検討
- 不要になった Hook の除去検討

### 5. 改善提案レポート

優先度付きの推奨事項:
- **P0（即時）**: セキュリティ/品質リスクに直結する改善
- **P1（次スプリント）**: 生産性向上に寄与する改善
- **P2（バックログ）**: あれば良い改善

## How (for Claude Code)

### `/ai-dlc:calibrate` が実行するステップ

**CLAUDE.md / SKILL スキャン**:

```bash
# CLAUDE.md の存在と最終更新
ls -la CLAUDE.md .claude/CLAUDE.md 2>/dev/null
git log -1 --format="%ad" -- CLAUDE.md

# SKILL ファイル一覧と最終更新
find .claude/skills/ -name "SKILL.md" -exec git log -1 --format="%ad -- {}" -- {} \;

# プロジェクト構造の主要ディレクトリ
ls -d */ 2>/dev/null
```

**Hooks レビュー**:

```bash
# Claude Code Hooks 設定
cat .claude/settings.json 2>/dev/null | jq '.hooks // empty'

# Git Hooks
ls -la .git/hooks/ 2>/dev/null
```

**キャリブレーションレポートテンプレート**:

```markdown
## Agent Calibration Report - [Date]

### CLAUDE.md / SKILL Health
| File | Last Updated | Status | Notes |
|---|---|---|---|
| CLAUDE.md | [date] | [fresh/stale] | [notes] |
| [skill]/SKILL.md | [date] | [fresh/stale] | [notes] |

Undocumented patterns detected:
- [pattern]: found in [N] files, not in CLAUDE.md

### Hooks Review
| Hook | Event | Triggers (2wk) | Assessment |
|---|---|---|---|
| [name] | [event] | [N] | [effective/noisy/unused] |

Proposed new hooks:
- [hook description]: [rationale]

### Improvement Recommendations
| Priority | Action | Impact | Effort |
|---|---|---|---|
| P0 | [action] | [impact] | [effort] |
| P1 | [action] | [impact] | [effort] |
| P2 | [action] | [impact] | [effort] |
```

## Cross-ref

- AI-Confidence / Interaction Churn → `ticket-management` > `references/ai-native-practices.md`
- Hooks 構文詳細 → `prompt-engineering` Skill > `references/hooks.md`
- CLAUDE.md 設計 → `prompt-engineering` Skill
- Diagnostic Session との関係 → `references/diagnostic-session.md`（レトロは人間プロセス改善、キャリブレーションは AI プロセス改善）
