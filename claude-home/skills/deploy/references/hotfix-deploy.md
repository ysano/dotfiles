# Hotfix Deploy - 緊急修正デプロイ

本番環境の緊急問題に対して迅速かつ安全に Hotfix をデプロイする手順。

## 1. インシデント評価とトリアージ

緊急度と影響を迅速に評価。

**Hotfix 判定基準**:
- Critical: セキュリティ侵害、データ損失、サービス停止 → 即時 Hotfix
- High: 重大なバグ、主要機能不全 → Hotfix
- Medium: 軽微なバグ → 通常リリースで対応
- Low: UI の問題 → 次回リリースで対応

**時間感度**: 30分以内に修正必須 → Hotfix、2時間以内 → Hotfix 検討、それ以上 → Forward Fix

## 2. インシデント対応体制確立

緊急対応チームを組織化。

- インシデント記録作成 (Jira, GitHub Issue 等)
- コミュニケーションチャネル確立 (#incident-hotfix-20240115)
- ステークホルダーへの初期通知
- タイムライン記録開始

## 3. Hotfix ブランチ作成

本番タグから Hotfix ブランチを作成。

```bash
# 本番バージョンから Hotfix ブランチ作成
git fetch --tags
git checkout tags/v1.2.3
git checkout -b hotfix/critical-auth-fix

# または trunk-based の場合
git checkout main
git pull origin main
git checkout -b hotfix/critical-auth-fix
```

## 4. 最小限の修正実装

問題の根本原因のみを修正。

**原則**:
- 修正範囲を最小化 (リファクタリング、最適化を含めない)
- 既存パターンを踏襲
- デバッグログを追加
- 単一責任: Hotfix に複数の問題修正を含めない

## 5. 加速テスト

重要なテストのみを実施。

```bash
# 関連テストのみ実行
npm test -- --testPathPattern=auth

# セキュリティスキャン
npm audit --audit-level=high

# 手動検証チェックリスト
# [ ] 修正が問題を解決している
# [ ] 新たな問題が発生していない
# [ ] クリティカルパスが正常動作
```

## 6. 迅速なコードレビュー

シニアメンバーによる緊急レビュー。

- セキュリティと正確性に焦点
- ペアプログラミング/スクリーンシェアで加速
- 承認プロセスは維持 (緊急でもレビューは必須)

## 7. バージョン更新とタグ付け

Hotfix バージョンを作成。

```bash
# パッチバージョン更新 (1.2.3 → 1.2.4)
npm version patch

# コミットとタグ
git add .
git commit -m "hotfix: fix critical authentication vulnerability

- Fix password validation bypass
- Add security logging
- Minimal changes to reduce risk

Fixes: #1234"

git tag -a v1.2.4 -m "Hotfix v1.2.4: Critical auth security fix"
git push origin hotfix/critical-auth-fix
git push origin v1.2.4
```

## 8. ステージング検証

ステージング環境で最終検証。

```bash
# ステージングデプロイ
./deploy-staging.sh v1.2.4

# クリティカルパステスト
curl -X POST https://staging.example.com/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"test"}'

# スモークテスト実行
npm run test:smoke:staging
```

## 9. 本番デプロイ

段階的に本番環境へデプロイ。

```bash
# Blue-Green デプロイ
./deploy-blue.sh v1.2.4
./health-check-blue.sh
./switch-traffic.sh blue

# Kubernetes ローリングアップデート
kubectl set image deployment/myapp myapp=registry.example.com/myapp:v1.2.4 -n production
kubectl rollout status deployment/myapp -n production

# Canary デプロイ (推奨)
./deploy-canary.sh v1.2.4 --percentage=10
# 5分監視後、問題なければ
./promote-canary.sh v1.2.4
```

## 10. リアルタイム監視

デプロイ直後の状態を監視。

```bash
# ヘルスチェック
watch -n 10 'curl -s https://api.example.com/health | jq .'

# エラーログ監視
kubectl logs -f deployment/myapp -n production | grep -E "ERROR|WARN"

# メトリクス監視 (15分)
# - エラー率 (< 1%)
# - レスポンスタイム (p95 < 500ms)
# - 認証成功率 (> 99%)
```

## 11. ロールバック準備

問題発生時の即時ロールバック手順。

```bash
# 自動ロールバックスクリプト準備
#!/bin/bash
if [ "$1" = "rollback" ]; then
  echo "Rolling back to v1.2.3"
  kubectl set image deployment/myapp myapp=registry.example.com/myapp:v1.2.3 -n production
  kubectl rollout status deployment/myapp -n production
fi
```

## 12. ステークホルダー通知

デプロイ完了を関係者に通知。

```markdown
## Hotfix Deployed: v1.2.4

**Status**: ✅ Successfully Deployed
**Time**: 2024-01-15 16:30 UTC
**Duration**: 18 minutes

**Issue Resolved**: Critical authentication vulnerability
**Impact**: No user impact, zero downtime
**Monitoring**: Enhanced monitoring active for 4 hours

**Next Steps**: Post-incident review scheduled for tomorrow 10:00 UTC
```

## 13. Main ブランチへマージバック

Hotfix を main ブランチに統合。

```bash
# Hotfix を main にマージ
git checkout main
git pull origin main
git merge hotfix/critical-auth-fix
git push origin main

# ブランチクリーンアップ
git branch -d hotfix/critical-auth-fix
git push origin --delete hotfix/critical-auth-fix
```

## 14. ポストモーテム実施

インシデントから学ぶ。

```markdown
## Hotfix Post-Mortem: v1.2.4

### Timeline
- 15:45 - Issue reported
- 15:50 - Hotfix decision
- 16:00 - Fix implemented
- 16:15 - Deployed to staging
- 16:30 - Deployed to production
- 16:48 - Verified stable

### Root Cause
Password validation logic allowed bypass in edge case

### Prevention
- Add security-focused E2E tests
- Implement automated security scanning in CI
- Schedule security audit
```

## 関連リファレンス

- `rollback-deploy.md` - ロールバック詳細手順
- `prepare-release.md` - 通常リリースプロセス
- `ci-setup.md` - CI パイプライン統合
