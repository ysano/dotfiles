# PRD (Product Requirements Document) フォーマット仕様

AI-DLC 上流成果物契約。`/ai-dlc:create-prd` が生成する文書のフォーマット。

## メタデータヘッダ

```yaml
---
type: ai-dlc-prd
version: "1.0"
status: draft | review | approved
created: YYYY-MM-DD
linked-architecture: docs/architecture-{kebab-case}.md  # 生成後にリンク
linked-epics: docs/stories-{kebab-case}.md              # 生成後にリンク
---
```

## 必須セクション

### 1. Problem Statement

解決すべき問題を明確に記述する。

- **ビジネスインパクト**: 定量的な影響（売上・コスト・時間・ユーザー数）
- **現状のギャップ**: 現在の状態とあるべき姿の差分
- **タイムライン**: 期限や緊急度の根拠

→ Atomic Spec **Context** にマッピング

### 2. User Personas

対象ユーザーを 2-3 名定義する。

```markdown
#### Persona: {名前}
- **役割**: {職種・立場}
- **ゴール**: {達成したいこと}
- **ペインポイント**: {現在の課題}
- **技術リテラシー**: {低/中/高}
```

### 3. Functional Requirements (FR-NNN)

番号付き機能要件。トレーサビリティチェーンの起点。

```markdown
| ID | 要件 | 優先度 | Persona |
|---|---|---|---|
| FR-001 | {動詞で始まる要件記述} | P0/P1/P2/P3 | {対象Persona} |
| FR-002 | ... | ... | ... |
```

- **P0**: Must-have（リリースブロッカー）
- **P1**: Should-have（初回リリースに含めたい）
- **P2**: Nice-to-have（次回以降でもよい）
- **P3**: Future（将来検討）

→ Atomic Spec **Expected Behavior** にマッピング

### 4. Non-Functional Requirements (NFR)

```markdown
| カテゴリ | 要件 | 指標 |
|---|---|---|
| Performance | {要件} | {応答時間, スループット等} |
| Security | {要件} | {暗号化方式, 認証方式等} |
| Scalability | {要件} | {同時接続数, データ量等} |
| Accessibility | {要件} | {WCAG レベル等} |
```

→ Atomic Spec **Constraints** にマッピング

### 5. Success Metrics

リリース後に測定する成功指標。

```markdown
| 指標 | ベースライン | 目標値 | 測定方法 |
|---|---|---|---|
| {KPI名} | {現在値} | {目標値} | {測定手段} |
```

→ Atomic Spec **Verification** にマッピング

### 6. Scope

```markdown
#### In Scope
- {含まれるもの}

#### Out of Scope
- {明示的に含まれないもの}

#### Assumptions
- {前提条件}
```

### 7. Risks

```markdown
| リスク | 影響度 | 発生確率 | 緩和策 |
|---|---|---|---|
| {リスク記述} | High/Med/Low | High/Med/Low | {対策} |
```

## Atomic Spec マッピング表

| PRD セクション | → Atomic Spec 要素 | 変換ルール |
|---|---|---|
| Problem Statement + Personas | **Context** | ビジネス背景 + 対象ユーザーを要約 |
| Problem Statement (ギャップ) | **Current Behavior** | 現状の振る舞いを技術的に記述 |
| Functional Requirements | **Expected Behavior** | FR-NNN を実装単位に分割 |
| NFR + Scope (制約) | **Constraints** | 定量的制約に変換 |
| Success Metrics | **Verification** | 自動テスト可能な検証手順に変換 |

## 出力先

`docs/prd-{kebab-case}.md`（プロジェクトルート基準）
