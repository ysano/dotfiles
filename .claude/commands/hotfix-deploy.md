# ホットフィックスデプロイコマンド

適切な安全対策と最小限のダウンタイムで、あらゆる本番環境に重要なホットフィックスを迅速にデプロイします。

## 実行手順

この緊急ホットフィックスデプロイプロセスに従ってください: **$ARGUMENTS**

1. **緊急事態評価とトリアージ**
   - 問題の深刻度と影響を評価
   - ホットフィックスが必要か、待つことができるかを判断
   - 影響を受けるシステムとユーザーへの影響を特定
   - 時間的緒急度とビジネス影響を估計
   - インシデントと判断根拠を文書化

2. **インシデント対応セットアップ**
   - インシデント管理システムでインシデント追跡を作成
   - ワールームまたはコミュニケーションチャンネルを設置
   - ステークホルダーとオンコールチームメンバーに通知
   - 明確なコミュニケーションプロトコルを確立
   - 初期インシデント詳細とタイムラインを文書化

3. **Branch and Environment Setup**
   ```bash
   # Create hotfix branch from production tag
   git fetch --tags
   git checkout tags/v1.2.3  # Latest production version
   git checkout -b hotfix/critical-auth-fix
   
   # Alternative: Branch from main if using trunk-based development
   git checkout main
   git pull origin main
   git checkout -b hotfix/critical-auth-fix
   ```

4. **迅速開発プロセス**
   - 変更を最小限にし、重要な問題のみに焦点を合わせる
   - リファクタリング、最適化、関連のない改善を避ける
   - 十分にテストされたパターンと確立されたアプローチを使用
   - トラブルシューティング目的で最小限のログを追加
   - 既存のコード規約とパターンに従う

5. **Accelerated Testing**
   ```bash
   # Run focused tests related to the fix
   npm test -- --testPathPattern=auth
   npm run test:security
   
   # Manual testing checklist
   # [ ] Core functionality works correctly
   # [ ] Hotfix resolves the critical issue
   # [ ] No new issues introduced
   # [ ] Critical user flows remain functional
   ```

6. **迅速コードレビュー**
   - シニアチームメンバーから迅速レビューを取得
   - セキュリティと正確性に焦点を当ててレビュー
   - 利用可能で時間が許す場合はペアプログラミングを使用
   - レビューの決定と根拠を迅速に文書化
   - 時間的プレッシャーの下でも適切な承認プロセスを確保

7. **Version and Tagging**
   ```bash
   # Update version for hotfix
   # 1.2.3 -> 1.2.4 (patch version)
   # or 1.2.3 -> 1.2.3-hotfix.1 (hotfix identifier)
   
   # Commit with detailed message
   git add .
   git commit -m "hotfix: fix critical authentication vulnerability
   
   - Fix password validation logic
   - Resolve security issue allowing bypass
   - Minimal change to reduce deployment risk
   
   Fixes: #1234"
   
   # Tag the hotfix version
   git tag -a v1.2.4 -m "Hotfix v1.2.4: Critical auth security fix"
   git push origin hotfix/critical-auth-fix
   git push origin v1.2.4
   ```

8. **Staging Deployment and Validation**
   ```bash
   # Deploy to staging environment for final validation
   ./deploy-staging.sh v1.2.4
   
   # Critical path testing
   curl -X POST staging.example.com/api/auth/login \
        -H "Content-Type: application/json" \
        -d '{"email":"test@example.com","password":"testpass"}'
   
   # Run smoke tests
   npm run test:smoke:staging
   ```

9. **Production Deployment Strategy**
   
   **Blue-Green Deployment:**
   ```bash
   # Deploy to blue environment
   ./deploy-blue.sh v1.2.4
   
   # Validate blue environment health
   ./health-check-blue.sh
   
   # Switch traffic to blue environment
   ./switch-to-blue.sh
   
   # Monitor deployment metrics
   ./monitor-deployment.sh
   ```
   
   **Rolling Deployment:**
   ```bash
   # Deploy to subset of servers first
   ./deploy-rolling.sh v1.2.4 --batch-size 1
   
   # Monitor each batch deployment
   ./monitor-batch.sh
   
   # Continue with next batch if healthy
   ./deploy-next-batch.sh
   ```

10. **Pre-Deployment Checklist**
    ```bash
    # Verify all prerequisites are met
    # [ ] Database backup completed successfully
    # [ ] Rollback plan documented and ready
    # [ ] Monitoring alerts configured and active
    # [ ] Team members standing by for support
    # [ ] Communication channels established
    
    # Execute production deployment
    ./deploy-production.sh v1.2.4
    
    # Run immediate post-deployment validation
    ./validate-hotfix.sh
    ```

11. **リアルタイム監視**
    ```bash
    # 重要なアプリケーションメトリクスを監視
    watch -n 10 'curl -s https://api.example.com/health | jq .'
    
    # エラー率とログを監視
    tail -f /var/log/app/error.log | grep -i "auth"
    
    # 重要なメトリクスを追跡:
    # - レスポンス時間とレイテンシ
    # - エラー率と例外カウント
    # - ユーザー認証成功率
    # - システムリソース使用率（CPU、メモリ）
    ```

12. **Post-Deployment Validation**
    ```bash
    # Run comprehensive validation tests
    ./test-critical-paths.sh
    
    # Test user authentication functionality
    curl -X POST https://api.example.com/auth/login \
         -H "Content-Type: application/json" \
         -d '{"email":"test@example.com","password":"testpass"}'
    
    # Validate security fix effectiveness
    ./security-validation.sh
    
    # Check overall system performance
    ./performance-check.sh
    ```

13. **コミュニケーションとステータス更新**
    - ステークホルダーに定期的なステータス更新を提供
    - 一貫したコミュニケーションチャンネルを使用
    - デプロイ進捗と結果を文書化
    - インシデント追跡システムを更新
    - 関連チームにデプロイ完了を通知

14. **Rollback Procedures**
    ```bash
    # Automated rollback script
    #!/bin/bash
    PREVIOUS_VERSION="v1.2.3"
    
    if [ "$1" = "rollback" ]; then
        echo "Rolling back to $PREVIOUS_VERSION"
        ./deploy-production.sh $PREVIOUS_VERSION
        ./validate-rollback.sh
        echo "Rollback completed successfully"
    fi
    
    # Manual rollback steps if automation fails:
    # 1. Switch load balancer back to previous version
    # 2. Validate previous version health and functionality
    # 3. Monitor system stability after rollback
    # 4. Communicate rollback status to team
    ```

15. **デプロイ後監視期間**
    - デプロイ後2-4時間システムを監視
    - エラー率とパフォーマンスメトリクスを注意深く監視
    - ユーザーフィードバックとサポートチケット量を確認
    - ホットフィックスが元の問題を解決していることを検証
    - 任意の問題や予期しない動作を文書化

16. **文書化とインシデントレポート**
    - 完全なホットフィックスプロセスとタイムラインを文書化
    - 学んだ教訓とプロセス改善を記録
    - 解決でインシデント管理システムを更新
    - インシデント後レビュー資料を作成
    - 将来の参考のためにチームと知識を共有

17. **Merge Back to Main Branch**
    ```bash
    # After successful hotfix deployment and validation
    git checkout main
    git pull origin main
    git merge hotfix/critical-auth-fix
    git push origin main
    
    # Clean up hotfix branch
    git branch -d hotfix/critical-auth-fix
    git push origin --delete hotfix/critical-auth-fix
    ```

18. **インシデント後活動**
    - インシデント後レビューミーティングをスケジュールし実施
    - ランブックと緊急手順を更新
    - プロセス改善を特定し実装
    - 監視とアラート設定を更新
    - 同様の問題を回避するための予防措置を計画

**ホットフィックスベストプラクティス:**

- **シンプルに保つ:** 重要な問題のみに焦点を当てた最小限の変更を行う
- **徹底的にテスト:** 時間的プレッシャーの下でもテスト基準を維持
- **明確にコミュニケーション:** プロセス全体を通してすべてのステークホルダーに情報を提供
- **注意深く監視:** 本番環境で修正を注意深く監視
- **すべてを文書化:** インシデント後レビューのためにすべての決定とアクションを記録
- **ロールバックを計画:** 常に変更を迅速に元に戻すテスト済みの方法を用意
- **学習と改善:** 各インシデントを使用してプロセスと手順を強化

**Emergency Escalation Guidelines:**

```bash
# Emergency contact information
ON_CALL_ENGINEER="+1-555-0123"
SENIOR_ENGINEER="+1-555-0124"
ENGINEERING_MANAGER="+1-555-0125"
INCIDENT_COMMANDER="+1-555-0126"

# Escalation timeline thresholds:
# 15 minutes: Escalate to senior engineer
# 30 minutes: Escalate to engineering manager
# 60 minutes: Escalate to incident commander
```

**重要な注意事項:**

- ホットフィックスは真の本番緊急事態のみに使用すべき
- 深刻度に疑問がある場合は、通常のリリースプロセスに従う
- 常にデプロイメントの速度よりシステムの安定性を優先
- すべての緊急変更に明確な監査証跡を維持
- 定期的な訓練は実際の緊急事態に対するチームの準備を確保するのに役立つ