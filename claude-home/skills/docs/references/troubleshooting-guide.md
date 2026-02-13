# Troubleshooting Guide

一般的な問題のトラブルシューティングガイドを作成する手順。診断コマンド・エラーコード・解決手順。

## 1. システム概要とアーキテクチャ

- システムアーキテクチャとコンポーネントを文書化
- 依存関係と統合をマッピング
- クリティカルパスと障害点を特定
- システムトポロジー図を作成
- データフローとコミュニケーションパターンを文書化

## 2. 一般的な問題の特定

- 過去のサポートチケットと問題を収集
- 頻繁な問題についてチームメンバーにインタビュー
- エラーログと監視データを分析
- ユーザーフィードバックと苦情を確認
- システム障害のパターンを特定

## 3. トラブルシューティングフレームワーク

- 体系的な診断手順を確立
- 問題分離方法論を作成
- エスカレーションパスと手順を文書化
- ログと監視チェックポイントを設定
- 重大度レベルと応答時間を定義

## 4. 診断ツールとコマンド

必須の診断コマンドを文書化:

### システムヘルス
```bash
# システムリソース確認
top                    # CPU とメモリ使用率
df -h                 # ディスク容量
free -m               # メモリ使用量
netstat -tuln         # ネットワーク接続

# アプリケーションログ
tail -f /var/log/app.log
journalctl -u service-name -f

# データベース接続
mysql -u user -p -e "SELECT 1"
psql -h host -U user -d db -c "SELECT 1"
```

## 5. 問題カテゴリと解決策

各カテゴリごとに問題・症状・診断・解決策を文書化。

### パフォーマンス問題

**遅いレスポンスタイム**

**症状:** API リクエストが 5 秒以上かかる

**診断:**
```bash
# アプリケーションログで遅いクエリを確認
grep "slow query" /var/log/app.log

# データベースパフォーマンス確認
EXPLAIN ANALYZE SELECT * FROM table;
```

**解決策:**
1. データベースインデックスを追加
2. クエリを最適化
3. キャッシュレイヤーを実装

## 6. エラーコード文書化

### HTTP ステータスコード
- **400 Bad Request**: リクエストパラメータを確認
- **401 Unauthorized**: 認証トークンを確認
- **403 Forbidden**: 権限設定を確認
- **404 Not Found**: エンドポイント URL を確認
- **500 Internal Server Error**: サーバーログを確認
- **502 Bad Gateway**: アップストリームサービスを確認
- **503 Service Unavailable**: サービス状態を確認

### アプリケーション固有エラー
各エラーコードに対して:
- エラーメッセージ
- 発生原因
- 診断手順
- 解決策

## 7. 環境固有の問題

- 開発環境の問題を文書化
- ステージング / テスト環境の問題に対応
- 本番環境固有のトラブルシューティングをカバー
- ローカル開発セットアップの問題を含める

## 8. データベーストラブルシューティング

### データベース接続問題

**症状:** データベースに接続できない

**診断:**
```sql
-- アクティブな接続を確認
SHOW PROCESSLIST;

-- データベースサイズを確認
SELECT table_schema,
       ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) AS 'DB Size in MB'
FROM information_schema.tables
GROUP BY table_schema;

-- スロークエリを確認
SHOW VARIABLES LIKE 'slow_query_log';
```

**解決策:**
1. データベースサービスが実行中か確認
2. 接続文字列と認証情報を検証
3. ファイアウォール設定を確認
4. 接続プール設定を確認

## 9. ネットワークと接続性の問題

### ネットワークトラブルシューティング

**基本的な接続:**
```bash
# 基本的な接続テスト
ping example.com
telnet host port
curl -v https://api.example.com/health

# DNS 解決
nslookup example.com
dig example.com

# ネットワークルーティング
traceroute example.com
```

**SSL/TLS 問題:**
```bash
# SSL 証明書を確認
openssl s_client -connect example.com:443
curl -vI https://example.com
```

## 10. アプリケーション固有のトラブルシューティング

### メモリ問題

**Out of Memory エラー**

**Java アプリケーション:**
```bash
# ヒープ使用量を確認
jstat -gc [PID]
jmap -dump:format=b,file=heapdump.hprof [PID]

# ヒープダンプを分析
jhat heapdump.hprof
```

**Node.js アプリケーション:**
```bash
# メモリ使用量を監視
node --inspect app.js
# Chrome DevTools でメモリプロファイリング
```

## 11. セキュリティと認証の問題

### 認証失敗

**症状:** ログインできない / トークンが無効

**診断:**
1. 認証トークンの有効期限を確認
2. 権限と役割を確認
3. 認証ログを確認
4. セッションストレージを確認

**解決策:**
1. トークンをリフレッシュ
2. 権限設定を修正
3. セッションをクリア
4. 認証サービスを再起動

## 12. デプロイと設定の問題

### デプロイ失敗

**コンテナ問題:**
```bash
# コンテナステータスを確認
docker ps -a
docker logs container-name

# リソース制限を確認
docker stats

# コンテナをデバッグ
docker exec -it container-name /bin/bash
```

**Kubernetes 問題:**
```bash
# Pod ステータスを確認
kubectl get pods
kubectl describe pod pod-name
kubectl logs pod-name

# サービス接続を確認
kubectl get svc
kubectl port-forward pod-name 8080:8080
```

## 13. 監視とアラート設定

- ヘルスチェックと監視を構成
- ログ集約と分析を設定
- 重大な問題のアラートを実装
- システムメトリクス用ダッシュボードを作成
- 監視しきい値を文書化

## 14. エスカレーション手順

### 重大度レベル
- **Critical (P0)**: 本番全体停止 → 15 分以内にエスカレート
- **High (P1)**: 主要機能停止 → 1 時間以内にエスカレート
- **Medium (P2)**: 部分的機能低下 → 4 時間以内にエスカレート
- **Low (P3)**: 軽微な問題 → 次営業日にエスカレート

## チェックリスト

- [ ] システムアーキテクチャ図作成
- [ ] 一般的な問題トップ 10 を特定
- [ ] 診断コマンドリスト作成
- [ ] エラーコードリファレンス完成
- [ ] 環境別トラブルシューティング手順作成
- [ ] エスカレーションマトリクス文書化
- [ ] セルフサービス診断スクリプト作成

## 関連リファレンス

- `architecture-documentation.md` - システム理解のベース
- `onboarding-guide.md` - 環境セットアップ問題
- `api-documentation.md` - API エラーコード詳細
