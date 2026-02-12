# Sales Strategy：「人月積算」から「成果報酬型」へ

AI-DLC による生産性向上下で「人月単価 × 時間」を維持することは自殺行為。投入リソースではなく「成果（Outcome）」と「信頼性（Reliability）」を売るモデルへ転換する。

## Pricing Layer Cake（3 層価格モデル）

### Layer 1: Role-based Foundation（固定費）

- **対象**: ガバナンス、アーキテクチャ設計、セキュリティ監視、シニア・アーキテクト/ARE による高度判断
- **訴求**: AI エージェントが暴走しないためのガードレール維持費用。専門人材とインフラのコスト

### Layer 2: Usage Layer（従量課金）

- **対象**: AI エージェントが消費するトークン量、API コール数、コンピューティングリソース
- **訴求**: 実費精算ベースで透明性担保。管理手数料（マージン）を付加

-> 2026 年価格帯・トークン消費ベンチマーク・コスト算出式は `ai-dlc-estimate` の [ai-resource-cost.md](../../ai-dlc-estimate/references/ai-resource-cost.md) を参照

### Layer 3: Outcome Layer（成果報酬）

- **対象**: AI-DLC で達成された測定可能な成果
- **指標例**:
  - 完了チケット数（Atomic Spec ベース）
  - 機能実装数（Function Points）
  - ビジネス KPI（インシデント解決数、処理時間短縮、適格リード数）
- **訴求**: AI 生産性向上の果実をシェアするモデル。ROI が明確

## Sales Deck 構成（2026 年版）

| セクション | メッセージ | キーコンテンツ |
|---|---|---|
| The Agentic Opportunity | AI 導入は「検討」→「実装」フェーズ。勝敗は「制御（Governance）」 | 生産性格差データ、AI プロジェクト失敗率 |
| Our Approach: AI-DLC | 単に AI ツールを使うのではなく、制御する独自ライフサイクルを持つ | ケンタウロス・ポッド図、Atomic Spec 概念図、Hooks 防御システム |
| Cost & Speed Revolution | コスト削減 + Time-to-Market の圧倒的速度 | Before/After 比較（60 人月→10 人月+AI）、効率データ |
| Reliability as a Service | AI の速度と人間の品質保証を両立 | ARE によるハルシネーション・チェック、Hooks によるコンプライアンス・バイ・デザイン |

## 交渉スクリプト

### 対 経営層

「御社の競合は AI で開発速度を 10 倍にしています。しかし制御不能な AI は技術的負債とセキュリティリスクの塊です。我々の AI-DLC は、その速度を『安全に』提供できる唯一のフレームワークです。」

### 対 調達部門

「人月単価で見ると高く見えますが、成果物（Outcome）単位のコストで比較してください。AI-DLC により総コストは従来比 30% 削減、納期は半分。リスクは我々が負います。」
