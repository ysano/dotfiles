# デプロイロールバックコマンド

問題が検出された場合に、ダウンタイムとユーザーへの影響を最小限に抑えながら、本番環境のデプロイを安全かつ迅速にロールバックします。

## 実行手順

この体系的なロールバック手順に従ってください: **$ARGUMENTS**

1. **インシデント評価と判断**
   - 現在のデプロイ問題の深刻度と影響を評価
   - ロールバックが必要か、前進修正が良いかを判断
   - 影響を受けるシステム、ユーザー、ビジネス機能を特定
   - データ整合性と一負性への影響を考慮
   - 判断の根拠とタイムラインを文書化

2. **緊急対応のセットアップ**
   ```bash
   # インシデント対応チームを起動
   # コミュニケーションチャネルを設定
   # ステークホルダーに即座に通知
   
   # 緊急通知の例
   echo "🚨 ロールバック開始
   問題: v1.3.0デプロイ後の重要なパフォーマンス劣化
   アクション: v1.2.9にロールバック中
   予定時刻: 15分
   影響: 一時的なサービス中断の可能性
   ステータスチャンネル: #incident-rollback-202401"
   ```

3. **ロールバック前の安全チェック**
   ```bash
   # 現在の本番バージョンを確認
   curl -s https://api.example.com/version
   kubectl get deployments -o wide
   
   # システムステータスをチェック
   curl -s https://api.example.com/health | jq .
   
   # ターゲットロールバックバージョンを特定
   git tag --sort=-version:refname | head -5
   
   # ロールバック対象が存在しデプロイ可能であることを確認
   git show v1.2.9 --stat
   ```

4. **データベースの考慮事項**
   ```bash
   # 前バージョンからのデータベースマイグレーションをチェック
   ./check-migrations.sh v1.2.9 v1.3.0
   
   # マイグレーションが存在する場合、データベースロールバックを計画
   # 警告: データベースロールバックはデータ損失を引き起こす可能性
   # マイグレーションが存在する場合は前進修正を検討
   
   # ロールバック前にデータベースバックアップを作成
   ./backup-database.sh "pre-rollback-$(date +%Y%m%d-%H%M%S)"
   ```

5. **トラフィック管理の準備**
   ```bash
   # トラフィックリダイレクトの準備
   # オプション1: メンテナンスページ
   ./enable-maintenance-mode.sh
   
   # オプション2: ロードバランサー管理
   ./drain-traffic.sh --gradual
   
   # オプション3: サーキットブレーカー起動
   ./activate-circuit-breaker.sh
   ```

6. **Container/Kubernetesロールバック**
   ```bash
   # Kubernetesロールバック
   kubectl rollout history deployment/app-deployment
   kubectl rollout undo deployment/app-deployment
   
   # または特定のリビジョンにロールバック
   kubectl rollout undo deployment/app-deployment --to-revision=3
   
   # ロールバック進捗を監視
   kubectl rollout status deployment/app-deployment --timeout=300s
   
   # ポッドが動作していることを確認
   kubectl get pods -l app=your-app
   ```

7. **Docker Swarmロールバック**
   ```bash
   # サービス履歴をリスト
   docker service ps app-service --no-trunc
   
   # 前バージョンにロールバック
   docker service update --rollback app-service
   
   # または特定のイメージに更新
   docker service update --image app:v1.2.9 app-service
   
   # ロールバックを監視
   docker service ps app-service
   ```

8. **伝統的なデプロイメントロールバック**
   ```bash
   # Blue-Greenデプロイメントロールバック
   ./switch-to-blue.sh  # またはgreen、現在に依存
   
   # ローリングデプロイメントロールバック
   ./deploy-version.sh v1.2.9 --rolling
   
   # シンボリックリンクベースロールバック
   ln -sfn /releases/v1.2.9 /current
   sudo systemctl restart app-service
   ```

9. **ロードバランサーとCDNの更新**
   ```bash
   # ロードバランサーを旧バージョンに向けるよう更新
   aws elbv2 modify-target-group --target-group-arn $TG_ARN --targets Id=old-instance
   
   # 必要に応じてCDNキャッシュをクリア
   aws cloudfront create-invalidation --distribution-id $DIST_ID --paths \"/*\"
   
   # 必要に応じてDNSを更新（最後の手段、伝播遅延あり）
   # aws route53 change-resource-record-sets ...
   ```

10. **設定のロールバック**
    ```bash\n    # Rollback configuration files\n    git checkout v1.2.9 -- config/\n    \n    # Restart services with old configuration\n    sudo systemctl restart nginx\n    sudo systemctl restart app-service\n    \n    # Rollback environment variables\n    ./restore-env-vars.sh v1.2.9\n    \n    # Update feature flags\n    ./update-feature-flags.sh --disable-new-features\n    ```\n\n11. **データベースロールバック（必要な場合）**\n    ```sql\n    -- EXTREME CAUTION: Can cause data loss\n    \n    -- Check migration status\n    SELECT * FROM schema_migrations ORDER BY version DESC LIMIT 5;\n    \n    -- Rollback specific migrations (framework dependent)\n    -- Rails: rake db:migrate:down VERSION=20240115120000\n    -- Django: python manage.py migrate app_name 0001\n    -- Node.js: npm run migrate:down\n    \n    -- Verify database state\n    SHOW TABLES;\n    DESCRIBE critical_table;\n    ```\n\n12. **Service Health Validation**\n    ```bash\n    # Health check script\n    #!/bin/bash\n    \n    echo \"Validating rollback...\"\n    \n    # Check application health\n    if curl -f -s https://api.example.com/health > /dev/null; then\n        echo \"✅ Health check passed\"\n    else\n        echo \"❌ Health check failed\"\n        exit 1\n    fi\n    \n    # Check critical endpoints\n    endpoints=(\n        \"/api/users/me\"\n        \"/api/auth/status\"\n        \"/api/data/latest\"\n    )\n    \n    for endpoint in \"${endpoints[@]}\"; do\n        if curl -f -s \"https://api.example.com$endpoint\" > /dev/null; then\n            echo \"✅ $endpoint working\"\n        else\n            echo \"❌ $endpoint failed\"\n        fi\n    done\n    ```\n\n13. **Performance and Metrics Validation**\n    ```bash\n    # Check response times\n    curl -w \"Response time: %{time_total}s\\n\" -s -o /dev/null https://api.example.com/\n    \n    # Monitor error rates\n    tail -f /var/log/app/error.log | head -20\n    \n    # Check system resources\n    top -bn1 | head -10\n    free -h\n    df -h\n    \n    # Validate database connectivity\n    mysql -u app -p -e \"SELECT 1;\"\n    ```\n\n14. **Traffic Restoration**\n    ```bash\n    # Gradually restore traffic\n    ./restore-traffic.sh --gradual\n    \n    # Disable maintenance mode\n    ./disable-maintenance-mode.sh\n    \n    # Re-enable circuit breakers\n    ./deactivate-circuit-breaker.sh\n    \n    # Monitor traffic patterns\n    ./monitor-traffic.sh --duration 300\n    ```\n\n15. **Monitoring and Alerting**\n    ```bash\n    # Enable enhanced monitoring during rollback\n    ./enable-enhanced-monitoring.sh\n    \n    # Watch key metrics\n    watch -n 10 'curl -s https://api.example.com/metrics | jq .'\n    \n    # Monitor logs in real-time\n    tail -f /var/log/app/*.log | grep -E \"ERROR|WARN|EXCEPTION\"\n    \n    # Check application metrics\n    # - Response times\n    # - Error rates\n    # - User sessions\n    # - Database performance\n    ```\n\n16. **User Communication**\n    ```markdown\n    ## Service Update - Rollback Completed\n    \n    **Status:** ✅ Service Restored\n    **Time:** 2024-01-15 15:45 UTC\n    **Duration:** 12 minutes of degraded performance\n    \n    **What Happened:**\n    We identified performance issues with our latest release and \n    performed a rollback to ensure optimal service quality.\n    \n    **Current Status:**\n    - All services operating normally\n    - Performance metrics back to baseline\n    - No data loss occurred\n    \n    **Next Steps:**\n    We're investigating the root cause and will provide updates \n    on our status page.\n    ```\n\n17. **Post-Rollback Validation**\n    ```bash\n    # Extended monitoring period\n    ./monitor-extended.sh --duration 3600  # 1 hour\n    \n    # Run integration tests\n    npm run test:integration:production\n    \n    # Check user-reported issues\n    ./check-support-tickets.sh --since \"1 hour ago\"\n    \n    # Validate business metrics\n    ./check-business-metrics.sh\n    ```\n\n18. **Documentation and Reporting**\n    ```markdown\n    # Rollback Incident Report\n    \n    **Incident ID:** INC-2024-0115-001\n    **Rollback Version:** v1.2.9 (from v1.3.0)\n    **Start Time:** 2024-01-15 15:30 UTC\n    **End Time:** 2024-01-15 15:42 UTC\n    **Total Duration:** 12 minutes\n    \n    **Timeline:**\n    - 15:25 - Performance degradation detected\n    - 15:30 - Rollback decision made\n    - 15:32 - Traffic drained\n    - 15:35 - Rollback initiated\n    - 15:38 - Rollback completed\n    - 15:42 - Traffic fully restored\n    \n    **Impact:**\n    - 12 minutes of degraded performance\n    - ~5% of users experienced slow responses\n    - No data loss or corruption\n    - No security implications\n    \n    **Root Cause:**\n    Memory leak in new feature causing performance degradation\n    \n    **Lessons Learned:**\n    - Need better performance testing in staging\n    - Improve monitoring for memory usage\n    - Consider canary deployments for major releases\n    ```\n\n19. **Cleanup and Follow-up**\n    ```bash\n    # Clean up failed deployment artifacts\n    docker image rm app:v1.3.0\n    \n    # Update deployment status\n    ./update-deployment-status.sh \"rollback-completed\"\n    \n    # Reset feature flags if needed\n    ./reset-feature-flags.sh\n    \n    # Schedule post-incident review\n    ./schedule-postmortem.sh --date \"2024-01-16 10:00\"\n    ```\n\n20. **Prevention and Improvement**\n    - Analyze what went wrong with the deployment\n    - Improve testing and validation procedures\n    - Enhance monitoring and alerting\n    - Update rollback procedures based on learnings\n    - Consider implementing canary deployments\n\n**Rollback Decision Matrix:**\n\n| Issue Severity | Data Impact | Time to Fix | Decision |\n|---------------|-------------|-------------|----------|\n| Critical | None | > 30 min | Rollback |\n| High | Minor | > 60 min | Rollback |\n| Medium | None | > 2 hours | Consider rollback |\n| Low | None | Any | Forward fix |\n\n**Emergency Rollback Script Template:**\n```bash\n#!/bin/bash\nset -e\n\n# Emergency rollback script\nPREVIOUS_VERSION=\"${1:-v1.2.9}\"\nCURRENT_VERSION=$(curl -s https://api.example.com/version)\n\necho \"🚨 EMERGENCY ROLLBACK\"\necho \"From: $CURRENT_VERSION\"\necho \"To: $PREVIOUS_VERSION\"\necho \"\"\n\n# Confirm rollback\nread -p \"Proceed with rollback? (yes/no): \" confirm\nif [ \"$confirm\" != \"yes\" ]; then\n    echo \"Rollback cancelled\"\n    exit 1\nfi\n\n# Execute rollback\necho \"Starting rollback...\"\nkubectl set image deployment/app-deployment app=app:$PREVIOUS_VERSION\nkubectl rollout status deployment/app-deployment --timeout=300s\n\n# Validate\necho \"Validating rollback...\"\nsleep 30\ncurl -f https://api.example.com/health\n\necho \"✅ Rollback completed successfully\"\n```\n\n注意: ロールバックは最後の手段であるべきです。特にデータベースマイグレーションが関わる場合は、常に前進修正を最初に検討してください。