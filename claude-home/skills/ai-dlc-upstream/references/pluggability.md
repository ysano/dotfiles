# BMAD プラガビリティ契約

AI-DLC と BMAD-METHOD の共存ルール。`/ai-dlc:translate-upstream` が変換時に参照する。

## 設計判断

BMAD は自動委譲せず、情報バナー + 明示的変換コマンドで対応する。

| 理由 | 詳細 |
|---|---|
| 哲学の違い | BMAD = ペルソナ駆動 / AI-DLC = 役割駆動 |
| コンテキスト制御 | 自動切替は予期しないコンテキスト混入を招く |
| ユーザー選択権 | どちらのワークフローを使うかはユーザーが決める |

## 検出ロジック

各上流コマンドの冒頭で実行:

```bash
# BMAD 検出
if [ -d "_bmad" ] || [ -d ".bmad" ]; then
  echo "[INFO] BMAD workflow detected."
fi
```

検出時の表示:

```
[INFO] BMAD workflow detected (_bmad/ exists).
You can use BMAD commands directly, or use /ai-dlc:translate-upstream to convert
BMAD artifacts to AI-DLC format.
```

## BMAD → AI-DLC 変換ルール

### PRD 変換

| BMAD セクション | → AI-DLC PRD セクション | 変換ルール |
|---|---|---|
| Project Overview | Problem Statement | ビジネスインパクトを抽出・定量化 |
| Target Audience | User Personas | Persona テンプレートに構造化 |
| Core Features | Functional Requirements | FR-NNN 番号を付与 |
| Technical Requirements | Non-Functional Requirements | カテゴリ別テーブルに変換 |
| Success Metrics | Success Metrics | ベースライン / 目標値 / 測定方法を追加 |
| Scope | Scope | In/Out/Assumptions に分類 |

### Architecture 変換

| BMAD セクション | → AI-DLC Architecture セクション | 変換ルール |
|---|---|---|
| Architecture Overview | Executive Summary | 2-3 文に要約 |
| System Design | System Context Diagram | Mermaid に変換 |
| Component Architecture | Component Design | 責務・インターフェース・データ所有権を追加 |
| Data Models | Data Model | エンティティ関係表に変換 |
| Technology Decisions | ADR-NNN + Technology Stack | ADR テンプレートに構造化 |
| Quality Attributes | Quality Attributes | 測定方法を追加 |

### Story 変換

| BMAD セクション | → AI-DLC Story セクション | 変換ルール |
|---|---|---|
| Epic Title/Goal | Epic フォーマット | FR リンクを追加 |
| User Story (As a...) | Story タイトル | 動詞で始まる形式に変換 |
| Acceptance Criteria | Atomic Spec 5 要素スタブ | 5 要素に分解・マッピング |
| Story Size | Size (S/M/L) | AI-DLC サイズガイドラインに再分類 |

### 不足要素の扱い

BMAD 文書にない AI-DLC 必須要素:

| 不足要素 | アクション |
|---|---|
| FR-NNN 番号 | 自動付与 |
| AI 実装適性 | `[TODO: AI適性評価]` プレースホルダー |
| Atomic Spec Constraints | NFR から推論、不足は `[TODO]` |
| Atomic Spec Verification | Acceptance Criteria から変換、不足は `[TODO]` |
| ADR 形式 | Technology Decisions を ADR テンプレートに変換 |

## AI-DLC コマンド ↔ BMAD ワークフロー マッピング

| AI-DLC コマンド | BMAD 相当 | 差異 |
|---|---|---|
| `/ai-dlc:create-prd` | `/create-prd` (PM persona) | AI-DLC: FR 番号体系 + Atomic Spec マッピング |
| `/ai-dlc:create-architecture` | `/create-architecture` (Architect persona) | AI-DLC: ADR 形式 + AI 互換性チェック |
| `/ai-dlc:create-stories` | `/create-epics-and-stories` | AI-DLC: Atomic Spec 5 要素スタブ + Agent-Ready 判定 |
| `/ai-dlc:quick-spec` | (該当なし) | 軽量パス |
| `/ai-dlc:review` | (該当なし) | AI-DLC 品質ゲート付きレビュー |
| `/ai-dlc:translate-upstream` | (該当なし) | BMAD → AI-DLC 変換 |

## ギャップレポート形式

`/ai-dlc:translate-upstream` が変換後に出力:

```markdown
## Conversion Gap Report

### Converted Successfully
- [x] Problem Statement → PRD Problem Statement
- [x] Features → FR-001 to FR-NNN

### Needs Manual Input
- [ ] AI Implementation Suitability per component
- [ ] Atomic Spec Constraints for Story 3, 5
- [ ] Success Metrics baseline values

### Structural Changes
- {description of structural reorganization applied during conversion}

### Not Applicable
- N/A items from source document
```
