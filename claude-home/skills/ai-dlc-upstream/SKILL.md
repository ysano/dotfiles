---
name: ai-dlc-upstream
description: >
  AI-DLC upstream workflow contract. Defines artifact formats for PRD, Architecture,
  Epic/Story decomposition. Use when creating product briefs, designing architecture,
  decomposing work into Agent Loop-ready tickets, or integrating BMAD workflows.
  Upstream commands (/ai-dlc:create-prd, /ai-dlc:create-architecture, /ai-dlc:create-stories,
  /ai-dlc:quick-spec, /ai-dlc:review, /ai-dlc:translate-upstream) reference this for format contracts.
user-invocable: false
---

AI-DLC 上流ワークフロー契約。要件定義から Story 分解までの成果物フォーマットを定義する。
下流（Agent Loop / セレモニー）は `ticket-management` / `ai-dlc-ceremonies` Skill を参照。

> **設計思想**: Skill が成果物フォーマット（契約）を定義し、Commands が実行ロジックを担う。
> 既存パターン（`ticket-management` = 理論、`/ai-dlc:plan` = 実行）に準拠。

## 成果物パイプライン

```
/ai-dlc:create-prd → /ai-dlc:create-architecture → /ai-dlc:create-stories → Agent Loop
     │                        │                            │
     └── PRD 文書              └── Architecture 文書         └── Atomic Spec スタブ群
                                                                     ↓
                                                              /ai-dlc:plan (既存)
```

軽量プロジェクト（1-5 Story）には `/ai-dlc:quick-spec` で単一文書に集約可能。

## 4 成果物契約

| 成果物 | フォーマット定義 | 生成コマンド | 用途 |
|---|---|---|---|
| **PRD** | `references/artifact-prd.md` | `/ai-dlc:create-prd` | 問題定義・要件・成功指標 |
| **Architecture** | `references/artifact-architecture.md` | `/ai-dlc:create-architecture` | システム設計・ADR・技術スタック |
| **Epic/Story** | `references/artifact-epic-story.md` | `/ai-dlc:create-stories` | Atomic Spec スタブ群への分解 |
| **Quick Spec** | `references/artifact-quick-spec.md` | `/ai-dlc:quick-spec` | 軽量単一文書（PRD+Arch+Stories） |

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/artifact-prd.md` | PRD フォーマット仕様、セクション定義、Atomic Spec マッピング | PRD 作成・レビュー時 |
| `references/artifact-architecture.md` | Architecture フォーマット仕様、ADR テンプレート | アーキテクチャ設計時 |
| `references/artifact-epic-story.md` | Epic/Story フォーマット、サイズガイドライン、Agent-Ready 判定 | Story 分解・トリアージ時 |
| `references/artifact-quick-spec.md` | 軽量スペック仕様（単一文書） | ソロ開発・小規模プロジェクト |
| `references/upstream-pipeline.md` | パイプラインフロー、品質ゲート、下流接続 | 全体ワークフロー設計時 |
| `references/pluggability.md` | BMAD 連携契約、変換ルール、マッピング表 | 外部文書変換・BMAD 統合時 |

## Atomic Spec との接続

上流成果物の各要素は下流の Atomic Spec 5 要素に変換される:

| PRD セクション | Architecture セクション | → Atomic Spec 要素 |
|---|---|---|
| Problem Statement | System Context | → **Context** |
| Current State / Gap | - | → **Current Behavior** |
| Functional Requirements | Component Design | → **Expected Behavior** |
| NFR / Constraints | Quality Attributes / ADR | → **Constraints** |
| Success Metrics | Test Strategy | → **Verification** |

## BMAD プラガビリティ

`_bmad/` ディレクトリが検出された場合、上流コマンドは情報バナーを表示する:

```
[INFO] BMAD workflow detected (_bmad/ exists).
You can use BMAD commands directly, or use /ai-dlc:translate-upstream to convert
BMAD artifacts to AI-DLC format.
```

自動委譲はしない（哲学の違い: ペルソナ駆動 vs 役割駆動）。
変換ルールの詳細は `references/pluggability.md` を参照。

## ticket-management / ai-dlc-ceremonies とのクロスリファレンス

| トピック | 参照先 |
|---|---|
| Atomic Spec 5 要素テンプレート | `ticket-management` > `references/atomic-spec.md` |
| Agent Loop 7 段階ライフサイクル | `ticket-management` > `references/agent-loop.md` |
| AI-Native DoD / 品質メトリクス | `ticket-management` > `references/ai-native-practices.md` |
| セレモニー運営パターン | `ai-dlc-ceremonies` > `references/` |
