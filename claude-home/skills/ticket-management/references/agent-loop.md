Agent Loop: AI Agent × 人間のハイブリッドチームにおける循環型チケットライフサイクル。

## 概要

従来の線形 5 段階（Backlog → Ready → In Progress → Review → Done）を、AI Agent の特性に合わせた循環型 7 段階に再定義。

**従来型との主な違い**:
- **Triage が自動化**: AI がカテゴリ分類・重複検知を実行
- **Planning と Implementation を分離**: AI の計画を人間が承認してから実装
- **Auto-Verification 段階を追加**: CI が自動検証し、人間レビュー前にフィルタ
- **循環型**: 任意の段階から前段階に差し戻し可能

## 7 段階詳細

| # | ステータス | 担当者 | 処理内容 | Exit Criteria |
|---|---|---|---|---|
| 1 | **Triage** | AI Auto | カテゴリ分類、重複検知、優先度推定 | カテゴリ・優先度が確定。重複なし |
| 2 | **Spec Definition** | 人間 | Atomic Spec 5 要素を記述 | 5 要素（Context/Current/Expected/Constraints/Verification）すべて記述済み |
| 3 | **AI Planning** | Claude Plan Agent | 実装計画を生成、ファイル変更一覧を提示 | 人間が計画を承認（Approve） |
| 4 | **AI Implementation** | Claude Build Agent | コード実装 + テスト作成 + PR 作成 | PR 作成完了。4 時間以内。Churn 3 回以内 |
| 5 | **Auto-Verification** | CI | Lint / Test / Security scan 実行 | 全チェックがグリーン |
| 6 | **Human Review** | ARE | ロジック検証、ハルシネーション検出 | レビュー承認 + マージ |
| 7 | **Done** | - | ドキュメント更新、メトリクス記録 | MTTV / Turns-Used / AI-Confidence 記録済み |

**ARE** = AI Review Engineer: AI 生成コードのレビューに特化した人間レビュアー。

## 差し戻しルール

差し戻しは品質担保のための正常なフロー。以下の条件で前段階に戻す:

| From | To | 条件 |
|---|---|---|
| AI Planning → | Spec Definition | 計画生成に必要な情報が不足。5 要素の補足が必要 |
| AI Implementation → | Spec Definition | Churn 3 回超。仕様が曖昧（対処フローは `ai-native-practices.md` 参照） |
| AI Implementation → | AI Planning | 計画通りに実装できない。計画の見直しが必要 |
| Auto-Verification → | AI Implementation | テスト失敗・lint エラー。AI が自動修正を試行 |
| Human Review → | AI Implementation | コード品質不足。修正指示付きで差し戻し |
| Human Review → | Spec Definition | 仕様自体に問題。要件の再定義が必要 |

**差し戻し上限**: 同一チケットで 3 回差し戻された場合、Epic レベルの再設計を検討。

## タイムアウトルール

各段階に滞留上限を設定し、ボトルネックを検出する:

| ステータス | 滞留上限 | アクション |
|---|---|---|
| Triage | 1 時間 | 自動完了しない場合は手動トリアージ |
| Spec Definition | 2 営業日 | 作成者にリマインド。ブロッカーを確認 |
| AI Planning | 30 分 | Plan Agent 応答なしの場合、手動計画に切り替え |
| AI Implementation | **4 時間** | Spec Definition に差し戻し（対処フローは `ai-native-practices.md` 参照） |
| Auto-Verification | 30 分 | CI 障害を調査。Flaky テストは除外して再実行 |
| Human Review | 4 時間 | レビュアーにリマインド。24 時間超で別レビュアーをアサイン |
| Done | - | 即時完了 |

4 時間ルールは AI-DDL の核心: AI が 4 時間で完了できない = Atomic Spec のサイズが不適切。

## プラットフォーム別マッピング

Agent Loop の 7 段階を各プラットフォームのステータスにマッピングする。

### Linear

| Agent Loop | Linear Status | WorkflowState 種別 |
|---|---|---|
| Triage | Triage | `triage` |
| Spec Definition | Backlog | `backlog` |
| AI Planning | Todo | `unstarted` |
| AI Implementation | In Progress | `started` |
| Auto-Verification | In Progress | `started`（ラベルで区別） |
| Human Review | In Review | `started` |
| Done | Done | `completed` |

### GitHub Projects

| Agent Loop | GitHub Status | 補足 |
|---|---|---|
| Triage | Triage | カスタムステータス追加 |
| Spec Definition | Backlog | デフォルトカラム |
| AI Planning | Ready | カスタムステータス |
| AI Implementation | In Progress | デフォルトカラム |
| Auto-Verification | In Progress | `ci-running` ラベルで区別 |
| Human Review | Review | カスタムステータス |
| Done | Done | デフォルトカラム |

### Jira

| Agent Loop | Jira Status | カテゴリ |
|---|---|---|
| Triage | Triage | To Do |
| Spec Definition | Open | To Do |
| AI Planning | Planning | To Do |
| AI Implementation | In Development | In Progress |
| Auto-Verification | In CI | In Progress |
| Human Review | Code Review | In Progress |
| Done | Done | Done |

プラットフォーム固有の設定方法は各プラットフォーム Skill（linear, github-projects-v2, jira）を参照。

## 小規模チーム向け簡略化

7 段階すべてが必要ない場合の簡略マッピング:

| 簡略ステータス | 対応する Agent Loop 段階 |
|---|---|
| Todo | Triage + Spec Definition |
| In Progress | AI Planning + AI Implementation + Auto-Verification |
| Review | Human Review |
| Done | Done |
