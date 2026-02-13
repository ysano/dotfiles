# Upstream Pipeline フロー

AI-DLC 上流パイプラインの全体フローと品質ゲート定義。

## パイプライン概要

```
┌─────────────────────── Upstream Pipeline ───────────────────────┐
│                                                                  │
│  /ai-dlc:create-prd                                             │
│       │                                                          │
│       ├── Discovery Interview (AskUserQuestion)                  │
│       ├── PRD Generation (artifact-prd.md format)                │
│       └── Output: docs/prd-{name}.md                            │
│              │                                                    │
│              ▼                                                    │
│  /ai-dlc:create-architecture                                     │
│       │                                                          │
│       ├── PRD 読み込み → FR/NFR 抽出                              │
│       ├── Architecture Design (ADR, Components, Data Model)      │
│       ├── AI-DLC 互換性チェック                                    │
│       └── Output: docs/architecture-{name}.md                    │
│              │                                                    │
│              ▼                                                    │
│  /ai-dlc:create-stories                                          │
│       │                                                          │
│       ├── PRD + Architecture 読み込み → FR-Component マッピング    │
│       ├── Epic 識別 → Story 分解 (Atomic Spec スタブ)             │
│       ├── Agent-Ready 判定                                        │
│       └── Output: docs/stories-{name}.md                         │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
                         │
                         ▼
┌─────────────────── Downstream (既存) ───────────────────────────┐
│                                                                  │
│  /ai-dlc:plan (Phase 1: Spec Quality Check)                     │
│       │                                                          │
│       ▼                                                          │
│  Agent Loop Stage 1: Triage (AI 自動)                            │
│       │                                                          │
│       ▼                                                          │
│  Agent Loop Stage 2: Spec Definition (人間)                      │
│       │  ← Atomic Spec スタブを完成版に仕上げ                     │
│       ▼                                                          │
│  Agent Loop Stage 3-7: AI Planning → Implementation → Done       │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## 軽量パス（Quick Spec）

```
/ai-dlc:quick-spec
     │
     ├── 簡易インタビュー (3-5 問)
     ├── 単一文書生成 (PRD + Architecture + Stories)
     └── Output: docs/spec-{name}.md
            │
            ▼
     /ai-dlc:plan → Agent Loop
```

Quick Spec は Story 数 5 以下のプロジェクトに適用。
超過時は Full Pipeline へのガイダンスを表示。

## 品質ゲート

各段階の出力品質を検証するゲート。

### Gate 1: PRD 完全性

| チェック項目 | 基準 | 不合格時 |
|---|---|---|
| Problem Statement | 定量的ビジネスインパクトを含む | 具体化を要求 |
| FR 番号付与 | 全要件に FR-NNN 付与済み | 番号付与を実行 |
| Success Metrics | 測定可能な指標が 1 つ以上 | 指標追加を要求 |
| Scope 境界 | In/Out of Scope が明確 | 境界明確化を要求 |

### Gate 2: Architecture 互換性

| チェック項目 | 基準 | 不合格時 |
|---|---|---|
| Atomic Spec 分割可能性 | 各コンポーネント 2-4h 単位 | 再分割を提案 |
| コンテキストウィンドウ適合 | インターフェースが AI 理解範囲内 | 簡素化を提案 |
| テスト自動化可能性 | CI 自動検証可能 | テスト戦略具体化 |
| ADR 完全性 | 主要判断に ADR 存在 | ADR 追加 |

### Gate 3: Story Agent-Ready 判定

| チェック項目 | 基準 | 不合格時 |
|---|---|---|
| Atomic Spec 5 要素 | 全要素入力済み | Needs Revision フラグ |
| サイズ制約 | S/M サイズ（L は分割） | 分割候補提示 |
| 依存関係明示 | Dependencies フィールド記入済み | 依存分析を実行 |

### Gate 4: /ai-dlc:plan 接続

`/ai-dlc:plan` Phase 1 "Spec Quality Check" が Upstream Stories を検証:
- Atomic Spec 5 要素スコア [0-5]/5
- Ready / Needs Revision の判定
- Needs Revision → Agent Loop Stage 2 (Spec Definition) で人間が完成

## 入出力マッピング

| 段階 | 入力 | 出力 | 次段階の入力として |
|---|---|---|---|
| create-prd | ユーザーインタビュー | `docs/prd-*.md` | create-architecture |
| create-architecture | PRD 文書 | `docs/architecture-*.md` | create-stories |
| create-stories | PRD + Architecture | `docs/stories-*.md` | /ai-dlc:plan |
| /ai-dlc:plan | Stories + Backlog | Sprint Plan | Agent Loop |

## BMAD 統合パス

```
BMAD Workflow → /ai-dlc:translate-upstream → AI-DLC Format → /ai-dlc:plan
```

`_bmad/` 検出時、各上流コマンドは BMAD 代替を情報バナーで案内。
変換詳細は `pluggability.md` を参照。
