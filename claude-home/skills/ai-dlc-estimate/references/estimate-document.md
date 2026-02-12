# AI-DLC 見積書テンプレートと自動生成

「価値再配分（Value Redistribution）」モデルに基づく見積書の構成設計と、自動生成の技術アーキテクチャ。

## 核心概念：価値再配分

従来の見積もりが「投入工数 → 請求額」だったのに対し、AI-DLC 見積もりは以下の 3 ステップで設計する:

1. **NBA（Next Best Alternative）算定**: AI 非導入時のコスト（アンカー価格）を算出
2. **差別化価値の証明**: AI 導入による削減コスト・回避リスク・新規収益を定量化
3. **Capture Rate 設定**: 創出価値の 10-30% をベンダー対価、70-90% をクライアント利益に

### アンカリングとティアリング

- **冒頭で「現状維持コスト（Cost of Inaction）」を提示**し、AI 導入の相対的優位を印象付ける
- **3 層ティアリング（松竹梅）**:
  - Tier 1: Audit / Pilot（低価格・低リスク、ROI 試算のみ）
  - Tier 2: Implementation（推奨、AEF 適用価値ベース価格）
  - Tier 3: Enterprise / Partnership（成果連動報酬含む）

## 見積書の標準 7 セクション構成

### Section 1: Executive Summary & ROI Snapshot

- SCR 法（Situation → Complication → Resolution）で課題解決の物語を構成
- ROI Summary Table: 投資額・回収期間・3 年間期待効果を 1 表に集約

### Section 2: Scope & Methodology (AI-DLC Approach)

- 学習データ準備〜デプロイ〜運用の各フェーズ定義
- **Confidence & SLA**: 目標精度（Precision/Recall）と未達時の対応方針
- 「確信度スコア XX% 未満は HITL フローへ回送」等の運用設計を明記

### Section 3: Value Comparison ("Why AI?" Section)

見積書の核心。**並行見積もり比較表**を配置:

| 評価指標 | 従来プロセス | AI-DLC プロセス | 効率化係数 |
|---|---|---|---|
| 処理能力 | 2,000 件/月（限界） | スケーラブル | Scale-Free |
| 平均応答時間 | 4 時間 | 5 秒 | 2,880x |
| 件あたり単価 | 1,500 円 | 250 円 | 6x 削減 |
| 年間総コスト | 3,600 万円 | 1,800 万円 | 50% ROI |
| 確信度リスク | ヒューマンエラー（低） | ハルシネーション（中） | HITL で補完 |

### Section 4: Commercial & Investment Plan

- Setup Fee（初期構築・データクレンジング）
- Usage / Operational Fee（トークン従量 or 月額固定）
- **Efficiency Multiplier**: AEF 適用前の「本来価値」vs 適用後の「提供価格」対比

### Section 5: Team & Capabilities

- プロジェクト体制図（ケンタウロス・ポッド構成）
- 主要メンバーの AI 専門性・過去実績（Case Study Blocks）

### Section 6: Risk Management & Prerequisites

- データ依存（クライアント提供データの質・量への前提条件）
- 第三者リスク（LLM プロバイダーの API 価格変更・仕様変更リスク）
- -> リスク一覧テンプレート詳細は [risk-register.md](risk-register.md) を参照

### Section 7: Terms & Conditions

- 支払条件、知的財産権（モデル・学習済み重みの帰属）、検収基準

## 法的・契約的考慮事項

### 精度保証と検収基準

従来の「仕様通り動作」→「統計的性能指標」へ:
> 「検収は事前合意の評価用データセット（Golden Dataset）に対し回答精度 80% 超過で完了。実運用データの精度低下は瑕疵に該当せず、別途チューニング費用を要する。」

### 第三者サービスの変動リスク

> 「推論にかかる API 利用料（OpenAI 等）は実費として貴社負担。為替・API 単価変動により月次請求額が変動する可能性がある。」

## ケーススタディ：製造業 RAG チャットボット

### 従来型見積もり

PM + 設計 + テスト = 8 人月 → **980 万円**（成果に関わらず固定）

### AI-DLC 型見積もり

1. **NBA**: ヘルプデスク 2,000 件/月 × 1,500 円 × 12 = **年間 3,600 万円**
2. **AI 導入後**: 自動化率 60% → 年間コスト 1,512 万円
3. **創出価値**: 3,600 万 - 1,512 万 = **2,088 万円**
4. **見積額**: 創出価値 × 25% ≒ **522 万円**（初年度）

従来型より安価に見えつつ、成功報酬を組み合わせてベンダーは長期的に高利益率を享受。

## 自動生成の技術スタック

| コンポーネント | 技術 | 理由 |
|---|---|---|
| レンダリング | Puppeteer (Headless Chrome) | CSS 完全サポート、グラフ描画可能 |
| ページネーション | Paged.js | W3C CSS Paged Media Polyfill |
| テンプレート | Handlebars / EJS | ロジックとデザインの分離 |

**フロー**: JSON データ → テンプレートエンジン → HTML 生成 → Paged.js 注入 → Puppeteer PDF 出力

### 実装のポイント

- `page.emulateMediaType('print')` で印刷用レンダリング
- `.pagedjs_pages` セレクタで Paged.js 完了を待機
- `break-inside: avoid` でテーブル行途中の改ページ禁止
- `display: table-header-group` で次ページにもヘッダー繰り返し

## 関連リファレンス

- [risk-register.md](risk-register.md) - Section 6 で添付するリスク一覧テンプレート
- [ai-resource-cost.md](ai-resource-cost.md) - Section 4 の Usage Fee 算出根拠
- [industry-aef.md](industry-aef.md) - Section 3 の効率化係数データ
- [contract-strategy.md](contract-strategy.md) - Section 7 の契約条件設計
