# Epic/Story フォーマット仕様

AI-DLC 上流成果物契約。`/ai-dlc:create-stories` が生成する文書のフォーマット。

## Epic フォーマット

```markdown
## Epic: {タイトル}

- **Goal**: {Epic のビジネスゴール}
- **Linked PRD**: docs/prd-{kebab-case}.md
- **Linked Architecture**: docs/architecture-{kebab-case}.md
- **FR Coverage**: FR-001, FR-002, ...
- **Priority**: P0 | P1 | P2 | P3
- **Stories**: {N} stories ({Ready: N} / {Needs Revision: N})
```

## Story フォーマット（Atomic Spec スタブ）

各 Story は Atomic Spec 5 要素のスタブ形式。PRD / Architecture から事前入力する。

```markdown
### Story: {動詞で始まるタイトル}

- **Epic**: {親 Epic 名}
- **FR**: FR-NNN
- **Size**: S | M | L
- **Status**: Ready | Needs Revision
- **Dependencies**: {依存する Story ID, なければ "None"}

#### Context
{PRD Problem Statement + Architecture Executive Summary から抽出}
{ビジネス背景 + 技術コンテキストを 2-3 文で}

#### Current Behavior
{PRD の現状ギャップから抽出、または "New feature - no current behavior"}
{ファイルパス・現在の振る舞いを具体的に}

#### Expected Behavior
{FR-NNN の要件 + Architecture Component Design から合成}
{変更後の振る舞いを具体的に}

#### Constraints
{PRD NFR + Architecture ADR Consequences + Quality Attributes から抽出}
- {制約 1}
- {制約 2}

#### Verification
{PRD Success Metrics + Architecture Quality Attributes 測定方法から変換}
- [ ] {自動テスト可能な検証項目 1}
- [ ] {自動テスト可能な検証項目 2}
```

## サイズガイドライン

| サイズ | 目安時間 | ファイル数 | PR 差分 | アクション |
|---|---|---|---|---|
| **S** | 1-2h | 1-2 files | ~100 lines | そのまま実行 |
| **M** | 2-4h | 3-5 files | ~300 lines | そのまま実行 |
| **L** | 4h+ | 5+ files | 300+ lines | **分割を検討** |

L サイズの Story を検出した場合は、分割候補を提示する。
AI Agent のコンテキストウィンドウ単位に収まることが最優先。

**分割戦略**（優先順）:
1. **機能境界**: ユーザーから見える機能単位で分割（例: 認証 / ダッシュボード / 通知）
2. **データフロー段階**: 入力→処理→出力の段階で分割（例: データ取得 / 変換 / 表示）
3. **バーティカルスライス**: 1 つの機能を薄く全レイヤーで実装（推奨）

水平レイヤー分割（フロントのみ / バックエンドのみ）は避ける。

Agent-Ready Summary では、サイズ超過は Missing Elements 列に `Exceeds size: {理由}` として報告する。

## Agent-Ready 判定基準

Story が Agent Loop Stage 2 (Spec Definition) をスキップして Stage 3 (AI Planning) に進める条件:

| 要素 | Ready 条件 | Needs Revision 条件 |
|---|---|---|
| **Context** | ビジネス + 技術背景が明確 | 曖昧、「要確認」等のプレースホルダーあり |
| **Current Behavior** | ファイルパスまたは「新機能」が明記 | 記述なし |
| **Expected Behavior** | 具体的な変更内容が記述済み | FR 番号のみで内容が空 |
| **Constraints** | 定量的制約が 1 つ以上 | 「特になし」または記述なし |
| **Verification** | 自動テスト可能な項目が 1 つ以上 | 「手動確認」のみ |

全 5 要素が Ready → Story Status: **Ready**
1 つ以上が Needs Revision → Story Status: **Needs Revision**（不足要素をフラグ表示）

## 依存グラフ

Story 間の依存関係を可視化:

```markdown
### Dependency Graph

{Story A} → {Story B} → {Story C}
                      → {Story D}  (parallel with C)
```

並列実行可能な Story グループを明示し、Agent 配置の最適化に使用。

## 出力先

`docs/stories-{kebab-case}.md`（プロジェクトルート基準）

## GitHub Issue 一括作成（オプション）

ユーザーの承認後、`gh issue create` で Story を Issue として作成:

```bash
gh issue create --title "{Story タイトル}" \
  --body "{Atomic Spec 5要素}" \
  --label "ai-dlc,story,size:{S|M|L}" \
  --milestone "{Sprint 名}"
```
