---
name: ticket-management
description: >
  AI-DLC（AI-Driven Development Lifecycle）に基づくチケット管理理論。
  Use when creating Atomic Specs, designing Agent Loops, running AI Janitor checks,
  splitting tasks for AI agents, reviewing ticket quality, defining AI-Native DoD,
  choosing ticket types, or designing hybrid team workflows.
  チケット操作エージェント (github-ticket-agent, ticket-sync-agent) が参照。
---

プラットフォームに依存しない AI-DLC チケット管理理論。
デフォルトは GitHub Projects V2、大規模（30 名以上）は Jira（選択基準は `references/organization-topology.md`）。
個別プラットフォームの操作方法は各プラットフォーム固有の Skill（github-projects-v2, jira）を参照。

> **出典**: `.steering/ai-dlc.md` — AI駆動開発ライフサイクル原典。本 Skill はその実践的抽出。

## AI-DLC とは

AI-Driven Development Lifecycle。従来の人間中心チケット管理を AI Agent × 人間のハイブリッドチーム向けに再定義したフレームワーク。

| 観点 | 従来型 | AI-DLC |
|---|---|---|
| **粒度** | 1-3 営業日 | コンテキストウィンドウ単位（Atomic Spec） |
| **ライフサイクル** | 線形 5 段階 | 循環型 7 段階（Agent Loop） |
| **規模定義** | 人数ベース | AI/人間ハイブリッド構成（トポロジー） |
| **鮮度管理** | 人間会議ベース | AI Janitor（自動再現検証） |
| **DoD** | 手動チェックリスト | Hooks 強制 + 自動検証 |
| **品質指標** | なし or 主観 | MTTV / Interaction Churn / AI-Confidence |

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/atomic-spec.md` | チケットの書き方、5 要素テンプレート、良い例/悪い例 | チケット作成・レビュー時 |
| `references/agent-loop.md` | 7 段階ライフサイクル、差し戻し・タイムアウトルール | ステータス設計・遷移判断時 |
| `references/organization-topology.md` | 規模別チーム構成、ツール選択、Agent Drift 防止 | チーム設計・ツール導入時 |
| `references/ai-native-practices.md` | AI-Native DoD、カスタムフィールド、品質メトリクス | DoD 定義・品質改善時 |

## Atomic Spec クイックリファレンス

チケットは以下の 5 要素で構成する（詳細は `references/atomic-spec.md`）:

1. **Context**（背景）: ビジネス + 技術コンテキスト
2. **Current Behavior**（現状）: ファイルパス指定、定量データ
3. **Expected Behavior**（あるべき姿）: 変更後の挙動
4. **Constraints**（制約条件）: 禁止事項、パフォーマンス要件
5. **Verification**（検証方法）: AI が自律実行できるテスト方法

## Agent Loop クイックリファレンス（循環型 7 段階）

1. **Triage** (AI 自動) → カテゴリ分類・重複検知 → Exit: カテゴリ・優先度確定
2. **Spec Definition** (人間) → Atomic Spec 5 要素記述 → Exit: 全要素記述済み
3. **AI Planning** (Plan Agent) → 実装計画生成 → Exit: 人間承認
4. **AI Implementation** (Build Agent) → コード + テスト + PR → Exit: PR 完了（4h 以内、Churn 3 回以内）
5. **Auto-Verification** (CI) → Lint / Test / Security → Exit: 全グリーン
6. **Human Review** (ARE) → ロジック・ハルシネーション検証 → Exit: 承認 + マージ
7. **Done** → メトリクス記録

差し戻し: 各段階から前段階へ戻れる（循環型）。詳細は `references/agent-loop.md` を参照。

## AI Janitor 3 ルール

1. **Stale 自動検証**: Backlog 30 日超の Bug は AI が再現テストを実行。再現不可ならクローズ候補
2. **4 時間ルール**: AI Implementation が 4 時間超 → 自動で Spec Definition に差し戻し（粒度超過）
3. **Churn 3 回ルール**: プロンプト往復 3 回超 → 仕様曖昧の兆候。4-6 回は Spec 補足、7 回超は Spec Definition に差し戻し

## チケットタイプ

| タイプ | 判断基準 | タイトル例 |
|---|---|---|
| **Task** | 明確なゴールを持つ作業単位 | `OAuth2 認証フローを実装する` |
| **Bug** | 期待動作と実際動作の乖離がある | `ログインページで500エラーが発生する` |
| **Improvement** | 既存機能の非機能的改善 | `検索 API のレスポンスを200ms以下にする` |
| **Story** | ユーザー価値を記述する要求 | `管理者としてユーザーを一括招待したい` |

## チケット階層

| レベル | 役割 | 粒度 |
|---|---|---|
| **Epic** | 複数スプリントにまたがるテーマ | 進捗は子チケットの完了率で追跡 |
| **Story / Task** | 独立してデリバリー可能な単位 | Atomic Spec 1 つ分 |
| **Sub-task** | 技術的分割（必要時のみ） | 同一 PR 内。2 階層までが目安 |

<constraints>
## 行動制約

### チケット作成時
- **5 要素必須**: Context / Current / Expected / Constraints / Verification のすべてを含むこと。1 つでも欠けている場合は作成を中止し、ユーザーに不足要素を確認すること
- **1 チケット 1 目的**: 複数目的を検出した場合は作成を中止し、分割を提案すること
- **動詞で開始**: タイトルが動詞で始まらない場合は修正を提案すること
- **重複チェック必須**: 作成前に同一タイトルの既存チケットを検索すること。検索を省略してはならない
- **粒度制限**: コンテキストウィンドウ単位（2-4 時間、変更 5 ファイル以下、PR 差分 300 行以下）を超える場合は分割を提案すること

### 差し戻し判定
- **4 時間ルール**: AI Implementation が 4 時間超過 → Spec Definition に差し戻すこと
- **Churn 3 回ルール**: プロンプト往復 3 回超 → `[Churn Alert]` を通知し、Spec の曖昧箇所を特定・補足すること。7 回超は Spec Definition への差し戻しを提案すること

### 鮮度管理
- **Stale Bug**: Backlog 30 日超の Bug を検知した場合、AI 再現テストを実行すること。再現不可ならクローズ候補としてマークすること
</constraints>
