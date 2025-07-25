# システム動作シミュレーター

容量計画、ボトルネック識別、最適化戦略を伴う様々な負荷下でのシステムパフォーマンスをシミュレートします。

## 実行手順

パフォーマンス予測、ボトルネック識別、容量計画最適化のための包括的システム動作シミュレーションを作成します。以下のアプローチに従ってください: **$ARGUMENTS**

### 1. 前提条件評価

**重要なシステムコンテキスト検証:**

- **システムアーキテクチャ**: どのタイプのシステムの動作をシミュレートしますか？
- **パフォーマンス目標**: 目標パフォーマンス指標とSLAは何ですか？
- **負荷特性**: 予想される使用パターンとトラフィックプロファイルは何ですか？
- **リソース制約**: どのようなインフラストラクチャと予算制限が適用されますか？
- **最適化目標**: パフォーマンスのどの側面を最適化することが最も重要ですか？

**コンテキストが不明な場合は、体系的にガイド:**

```
システムアーキテクチャが不明:
「どのタイプのシステムの動作シミュレーションが必要ですか？
- Webアプリケーション: HTTPトラフィックパターンを持つユーザー向けアプリケーション
- APIサービス: プログラム的アクセスパターンを持つバックエンドサービス
- データ処理: スループット要件を持つバッチまたはストリーム処理
- データベースシステム: データストレージとクエリ処理最適化
- マイクロサービス: サービス間通信を持つ分散システム

システムコンポーネント、技術スタック、デプロイメントアーキテクチャを指定してください。」

パフォーマンス目標が不明:
「どのパフォーマンス目標を満たす必要がありますか？
- レスポンス時間: ユーザーリクエストの目標レイテンシ（p50、p95、p99）
- スループット: 秒あたりリクエスト数または分あたりトランザクション数
- 可用性: アップタイム目標と障害耐性要件
- スケーラビリティ: ユーザー成長と負荷処理能力
- リソース効率: CPU、メモリ、ストレージ、ネットワーク最適化」
```

### 2. システムアーキテクチャモデリング

**システムコンポーネントと相互作用を体系的にマッピング:**

#### コンポーネントアーキテクチャフレームワーク
```
システムコンポーネントマッピング:

アプリケーション層:
- フロントエンドコンポーネント: ユーザーインターフェース、SPAアプリ、モバイルアプリ
- アプリケーションサービス: ビジネスロジック、ワークフロー処理、APIエンドポイント
- バックグラウンドサービス: スケジュールジョブ、メッセージ処理、バッチ操作
- 統合サービス: 外部API呼び出し、Webhook処理、データ同期

データ層:
- プライマリデータベース: トランザクションデータストレージとクエリ処理
- キャッシュシステム: Redis、Memcached、CDN、アプリケーションレベルキャッシング
- メッセージキュー: 非同期通信とイベント処理
- 検索システム: Elasticsearch、Solr、またはデータベース検索機能

インフラストラクチャ層:
- ロードバランサー: トラフィック分散とヘルスチェック
- Webサーバー: HTTPリクエスト処理と静的コンテンツ配信
- アプリケーションサーバー: 動的コンテンツ生成とビジネスロジック
- ネットワークコンポーネント: ファイアウォール、VPN、トラフィックルーティング
```

#### 相互作用パターンモデリング
```
システム相互作用分析:

同期相互作用:
- リクエスト-レスポンス: 直接API呼び出しとデータベースクエリ
- サービスメッシュ: サービスディスカバリーを持つサービス間通信
- データベーストランザクション: ACID準拠とロッキングメカニズム
- 外部API呼び出し: サードパーティサービス依存とタイムアウト

非同期相互作用:
- メッセージキュー: Pub/subパターンとイベント駆動処理
- イベントストリーム: リアルタイムデータ処理と分析
- バックグラウンドジョブ: 遅延実行と非同期タスク処理
- 通知システム: プッシュ通知とメール配信
```

### 3. 負荷パターン分析

**使用パターンとトラフィック特性の詳細分析:**

#### トラフィック特性モデリング
```
負荷パターン分析:

時間的パターン:
- 日次サイクル: ピーク時間と低負荷期間の識別
- 週次パターン: 平日vs週末の使用差
- 季節性: 月次、四半期、年次の変動パターン
- イベント駆動: プロモーション、ローンチ、緊急事態の影響

地理的パターン:
- 地域分散: ユーザーベースの地理的分布
- タイムゾーン効果: 全世界ユーザーの時差による負荷分散
- 地域特性: 地域固有の使用パターンと好み
- CDN要件: コンテンツ配信最適化ニーズ

ユーザー行動パターン:
- セッション特性: 平均セッション時間とページビュー
- 機能使用: 人気機能vs低使用機能の分析
- ユーザージャーニー: 典型的なユーザーフローとパス
- 離脱パターン: ドロップオフポイントと改善機会
```

### 4. パフォーマンスボトルネック識別

**システム制約と性能限界の体系的分析:**

#### ボトルネック分析フレームワーク
```
パフォーマンス制約識別:

リソースボトルネック:
- CPU使用率: プロセッサー集約的操作と最適化機会
- メモリ制約: RAM使用パターンとメモリリーク識別
- ストレージI/O: ディスク読み書き性能と最適化
- ネットワーク帯域幅: データ転送制限と圧縮機会

アプリケーションボトルネック:
- アルゴリズム効率: O(n)複雑度とパフォーマンス改善
- データベースクエリ: スロークエリとインデックス最適化
- 外部API依存: サードパーティサービスのレイテンシー影響
- 同期処理: ブロッキング操作と非同期化機会

アーキテクチャボトルネック:
- 単一障害点: 可用性リスクと冗長化ニーズ
- スケーリング制限: 水平vs垂直スケーリング制約
- データ一貫性: 分散システムでの整合性vs性能トレードオフ
- サービス間通信: ネットワークレイテンシーと最適化
```

### 5. 容量計画と最適化

**将来の成長と性能要件への準備:**

#### 容量計画フレームワーク
```
容量計画戦略:

成長モデリング:
- ユーザー成長予測: 登録率と継続率の分析
- 使用量拡張: 機能使用の増加パターン
- データ成長: ストレージニーズの線形vs指数的成長
- トラフィック予測: 負荷増加の短期と長期予測

リソース計画:
- ハードウェア要件: サーバー、ストレージ、ネットワーク容量
- クラウドリソース: インスタンスタイプと自動スケーリング設定
- ライセンス要件: ソフトウェアライセンスとコスト予測
- 運用人員: サポートとメンテナンスのスタッフィング

最適化戦略:
- コスト効率: パフォーマンス向上のコスト対効果分析
- 段階的改善: 短期、中期、長期の最適化ロードマップ
- 技術負債: パフォーマンス影響のある技術負債の優先順位付け
- 監視戦略: 継続的パフォーマンス追跡と早期警告システム
```

### 6. シミュレーション実行と結果分析

**実際のテストシナリオと結果解釈:**

#### シミュレーション手法
```
システム動作テスト:

負荷テストシナリオ:
- ベースライン: 正常な負荷条件下での性能測定
- ピーク負荷: 予想最大負荷での性能検証
- ストレステスト: システム限界と破綻点の識別
- スパイクテスト: 突然の負荷増加への対応能力

パフォーマンス測定:
- レスポンス時間分布: 平均、中央値、パーセンタイル分析
- スループット測定: 秒あたり処理可能なリクエスト数
- エラー率監視: 負荷増加に伴うエラー発生パターン
- リソース使用率: CPU、メモリ、ディスク、ネットワークの使用状況

結果分析:
- 性能曲線: 負荷と性能の関係グラフ
- ボトルネック特定: 最初に限界に達するコンポーネント
- 改善提案: 具体的な最適化アクション項目
- ROI分析: 改善投資の効果予測
```

このコマンドは以下の最適化に役立ちます：
- システム性能の予測と計画
- ボトルネックの事前識別と解決
- 容量計画の精度向上
- コスト効率的なインフラストラクチャ投資