# Mermaid 全ダイアグラム描画テスト

## 1. フローチャート

```mermaid
flowchart LR
    A[開始] --> B{条件チェック}
    B -->|正常| C[処理実行]
    B -->|異常| D[エラー処理]
    C --> E[終了]
    D --> E
```

## 2. シーケンス図

```mermaid
sequenceDiagram
    participant U as 利用者
    participant S as サーバー
    participant D as データベース
    U->>S: ログイン要求
    S->>D: 認証情報照会
    D-->>S: 認証結果
    S-->>U: ログイン完了
```

## 3. クラス図

```mermaid
classDiagram
    class 動物 {
        +String 名前
        +int 年齢
        +鳴く()
    }
    class 犬 {
        +お手()
    }
    class 猫 {
        +ゴロゴロ()
    }
    動物 <|-- 犬
    動物 <|-- 猫
```

## 4. 状態遷移図

```mermaid
stateDiagram-v2
    [*] --> 待機中
    待機中 --> 実行中 : 開始
    実行中 --> 一時停止 : 停止
    一時停止 --> 実行中 : 再開
    実行中 --> [*] : 完了
```

## 5. ER図

```mermaid
erDiagram
    顧客 ||--o{ 注文 : 発注する
    注文 ||--|{ 注文明細 : 含む
    商品 ||--o{ 注文明細 : 対象
    顧客 {
        int 顧客ID PK
        string 氏名
        string メール
    }
    注文 {
        int 注文ID PK
        date 注文日
    }
```

## 6. ガントチャート

```mermaid
gantt
    title プロジェクト計画
    dateFormat YYYY-MM-DD
    section 設計
        画面設計     :a1, 2024-01-01, 7d
        DB設計       :a2, after a1, 5d
    section 開発
        フロントエンド :b1, after a2, 14d
        バックエンド   :b2, after a2, 14d
    section テスト
        結合テスト    :c1, after b1, 7d
```

## 7. 円グラフ

```mermaid
pie title 言語別使用割合
    "JavaScript" : 40
    "Python" : 30
    "Go" : 15
    "その他" : 15
```

## 8. Gitグラフ

```mermaid
gitGraph
    commit id: "初期コミット"
    commit id: "機能A追加"
    branch "開発"
    checkout "開発"
    commit id: "機能B開発中"
    commit id: "機能B完了"
    checkout main
    merge "開発"
    commit id: "リリース"
```

## 9. ユーザージャーニー

```mermaid
journey
    title 買い物体験
    section 閲覧
        サイト訪問: 5: 利用者
        商品検索: 3: 利用者
    section 購入
        カートに追加: 4: 利用者
        レジに進む: 3: 利用者
        支払い: 2: 利用者
    section 配送
        配送追跡: 4: 利用者
        商品受取: 5: 利用者
```

## 10. マインドマップ

```mermaid
mindmap
    root(プロジェクト)
        フロントエンド
            React
            CSS
        バックエンド
            API設計
            データベース
        インフラ
            CI/CD
            監視
```

## 11. タイムライン

```mermaid
timeline
    title 製品の歩み
    2020年 : アルファ版リリース
    2021年 : ベータ版リリース
           : 一般公開
    2022年 : v2.0リリース
    2023年 : 企業版提供開始
```

## 12. 四象限チャート

```mermaid
quadrantChart
    title 優先度マトリクス
    x-axis "工数小" --> "工数大"
    y-axis "効果小" --> "効果大"
    quadrant-1 "最優先"
    quadrant-2 "計画的に"
    quadrant-3 "委任する"
    quadrant-4 "見送り"
    "認証機能": [0.2, 0.8]
    "管理画面": [0.7, 0.9]
    "ログ整備": [0.3, 0.3]
    "旧機能改修": [0.8, 0.2]
```

## 13. サンキー図

```mermaid
sankey-beta

"Budget","Development",300
"Budget","Operations",200
"Budget","Personnel",500
"Development","Frontend",150
"Development","Backend",150
"Operations","Server",120
"Operations","Monitoring",80
```

> Note: sankey-beta は非ASCII文字未対応（mermaid v11）

## 14. XYチャート

```mermaid
xychart-beta
    title "月別売上推移"
    x-axis ["1月", "2月", "3月", "4月", "5月", "6月"]
    y-axis "売上" 0 --> 500
    bar [120, 200, 150, 300, 250, 450]
    line [100, 180, 160, 280, 240, 400]
```

## 15. ブロック図

```mermaid
block-beta
    columns 3
    A["画面"]:1
    B["APIゲートウェイ"]:1
    C["認証"]:1
    D["注文サービス"]:1
    E["在庫サービス"]:1
    F["通知サービス"]:1
    G["データベース"]:3

    A --> B
    B --> D
    B --> E
    B --> F
    D --> G
    E --> G
    F --> G
```

## 16. カンバン

```mermaid
kanban
    未着手
        t1[画面設計レビュー]
        t2[ドキュメント作成]
    進行中
        t3[API実装]
    完了
        t4[CI環境構築]
```

## 17. パケット図

```mermaid
packet-beta
    0-15: "送信元ポート"
    16-31: "宛先ポート"
    32-63: "シーケンス番号"
    64-95: "確認応答番号"
    96-99: "データオフセット"
    100-105: "予約"
    106-111: "フラグ"
    112-127: "ウィンドウサイズ"
```

## 18. アーキテクチャ図

```mermaid
architecture-beta
    group cloud(cloud)[Cloud]

    service api(server)[API] in cloud
    service db(database)[DB] in cloud
    service web(internet)[Browser]

    web:R -- L:api
    api:R -- L:db
```

> Note: architecture-beta は非ASCII文字未対応（mermaid v11）
