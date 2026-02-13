# Rollback Deploy - デプロイロールバック

本番環境デプロイの問題発生時に安全に前バージョンへロールバックする手順。

## 1. インシデント評価と判断

ロールバック要否を迅速に判断。

**ロールバック判断基準**:
- Critical: データ損失、セキュリティ侵害、全体停止 → 即時ロールバック
- High: 重大なパフォーマンス低下、主要機能不全 → ロールバック
- Medium: 一部機能の問題、軽微なパフォーマンス低下 → Forward Fix 検討
- Low: UI の不具合、非クリティカルバグ → Forward Fix

**データマイグレーション確認**: DB マイグレーションが含まれる場合、データロールバック戦略を評価。

## 2. 緊急対応体制確立

インシデント対応チームを招集。

- インシデント記録作成
- コミュニケーションチャネル確立 (Slack 専用チャネル等)
- ステークホルダー通知
- タイムライン記録開始

## 3. 現状確認と目標バージョン特定

ロールバック対象を明確化。

```bash
# 現在のバージョン確認
kubectl get deployment myapp -o jsonpath='{.spec.template.spec.containers[0].image}'

# または Docker の場合
curl -s https://api.example.com/version

# ロールバック目標バージョン特定
git tag --sort=-version:refname | head -5
```

## 4. データベースバックアップ

ロールバック前にデータを保護。

```bash
# PostgreSQL
pg_dump -U user -d database > backup-$(date +%Y%m%d-%H%M%S).sql

# MySQL
mysqldump -u user -p database > backup-$(date +%Y%m%d-%H%M%S).sql
```

## 5. Kubernetes ロールバック

Kubernetes デプロイメントをロールバック。

```bash
# ロールバック履歴確認
kubectl rollout history deployment/myapp -n production

# 直前リビジョンへロールバック
kubectl rollout undo deployment/myapp -n production

# 特定リビジョンへロールバック
kubectl rollout undo deployment/myapp -n production --to-revision=3

# ロールバック進捗監視
kubectl rollout status deployment/myapp -n production --timeout=300s

# Pod 状態確認
kubectl get pods -n production -l app=myapp
```

## 6. Docker Swarm ロールバック

Docker Swarm サービスをロールバック。

```bash
# サービス履歴確認
docker service ps myapp-service --no-trunc

# 自動ロールバック
docker service update --rollback myapp-service

# または特定イメージへ更新
docker service update --image myapp:v1.2.9 myapp-service
```

## 7. 従来型デプロイロールバック

Blue-Green / Rolling デプロイのロールバック。

```bash
# Blue-Green: トラフィック切り替え
./switch-traffic.sh blue  # または green

# Symlink ベース
ln -sfn /releases/v1.2.9 /current
sudo systemctl restart myapp

# Rolling deployment
./deploy.sh v1.2.9 --strategy=rolling
```

## 8. ロードバランサーと CDN 更新

トラフィックルーティングを更新。

```bash
# ALB ターゲットグループ更新
aws elbv2 modify-target-group --target-group-arn $TG_ARN \
  --targets Id=old-instance-id

# CloudFront キャッシュクリア
aws cloudfront create-invalidation --distribution-id $DIST_ID --paths "/*"
```

## 9. ヘルスチェック検証

ロールバック成功を確認。

```bash
# エンドポイントヘルスチェック
curl -f https://api.example.com/health

# 重要エンドポイント検証
for endpoint in /api/users /api/data /api/auth; do
  if curl -f -s "https://api.example.com$endpoint" > /dev/null; then
    echo "✅ $endpoint OK"
  else
    echo "❌ $endpoint FAILED"
  fi
done

# レスポンスタイム測定
curl -w "Response time: %{time_total}s\n" -s -o /dev/null https://api.example.com/
```

## 10. トラフィック復旧と監視

段階的にトラフィックを復旧。

```bash
# メンテナンスモード解除
./disable-maintenance-mode.sh

# トラフィック段階的復旧
./restore-traffic.sh --gradual --percentage=10,25,50,100

# ログ監視
kubectl logs -f deployment/myapp -n production | grep -E "ERROR|WARN"

# メトリクス監視 (5分間)
./monitor.sh --duration=300
```

## 11. ポストロールバック検証

システムが正常稼働していることを確認。

- エラー率が正常範囲内
- レスポンスタイムがベースライン以下
- ユーザーからの報告を監視
- ビジネスメトリクスが正常

## 12. インシデント報告とポストモーテム

問題を文書化し、改善点を特定。

```markdown
## Rollback Incident Report

**Incident ID**: INC-2024-001
**Date**: 2024-01-15
**Duration**: 12 minutes
**Severity**: High

### Timeline
- 15:25 - Issue detected
- 15:30 - Rollback decision
- 15:35 - Rollback initiated
- 15:42 - Service restored

### Root Cause
Memory leak in new feature

### Lessons Learned
- Improve staging performance tests
- Add memory usage monitoring
- Consider canary deployments
```

## 関連リファレンス

- `hotfix-deploy.md` - 緊急修正デプロイ
- `setup-kubernetes-deployment.md` - Kubernetes 設定
- `prepare-release.md` - リリース準備
