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
   # Activate incident response team
   # Set up communication channels
   # Notify stakeholders immediately
   
   # Example emergency notification
   echo "🚨 ROLLBACK INITIATED
   Issue: Critical performance degradation after v1.3.0 deployment
   Action: Rolling back to v1.2.9
   ETA: 15 minutes
   Impact: Temporary service interruption possible
   Status channel: #incident-rollback-202401"
   ```

3. **ロールバック前の安全チェック**
   ```bash
   # Verify current production version
   curl -s https://api.example.com/version
   kubectl get deployments -o wide
   
   # Check system status
   curl -s https://api.example.com/health | jq .
   
   # Identify target rollback version
   git tag --sort=-version:refname | head -5
   
   # Verify rollback target exists and is deployable
   git show v1.2.9 --stat
   ```

4. **データベースの考慮事項**
   ```bash
   # Check for database migrations since last version
   ./check-migrations.sh v1.2.9 v1.3.0
   
   # If migrations exist, plan database rollback
   # WARNING: Database rollbacks can cause data loss
   # Consider forward fix instead if migrations are present
   
   # Create database backup before rollback
   ./backup-database.sh "pre-rollback-$(date +%Y%m%d-%H%M%S)"
   ```

5. **トラフィック管理の準備**
   ```bash
   # Prepare to redirect traffic
   # Option 1: Maintenance page
   ./enable-maintenance-mode.sh
   
   # Option 2: Load balancer management
   ./drain-traffic.sh --gradual
   
   # Option 3: Circuit breaker activation
   ./activate-circuit-breaker.sh
   ```

6. **Container/Kubernetesロールバック**
   ```bash
   # Kubernetes rollback
   kubectl rollout history deployment/app-deployment
   kubectl rollout undo deployment/app-deployment
   
   # Or rollback to specific revision
   kubectl rollout undo deployment/app-deployment --to-revision=3
   
   # Monitor rollback progress
   kubectl rollout status deployment/app-deployment --timeout=300s
   
   # Verify pods are running
   kubectl get pods -l app=your-app
   ```

7. **Docker Swarmロールバック**
   ```bash
   # List service history
   docker service ps app-service --no-trunc
   
   # Rollback to previous version
   docker service update --rollback app-service
   
   # Or update to specific image
   docker service update --image app:v1.2.9 app-service
   
   # Monitor rollback
   docker service ps app-service
   ```

8. **伝統的なデプロイメントロールバック**
   ```bash
   # Blue-Green deployment rollback
   ./switch-to-blue.sh  # or green, depending on current
   
   # Rolling deployment rollback
   ./deploy-version.sh v1.2.9 --rolling
   
   # Symlink-based rollback
   ln -sfn /releases/v1.2.9 /current
   sudo systemctl restart app-service
   ```

9. **ロードバランサーとCDNの更新**
   ```bash
   # Update load balancer to point to old version
   aws elbv2 modify-target-group --target-group-arn $TG_ARN --targets Id=old-instance
   
   # Clear CDN cache if needed
   aws cloudfront create-invalidation --distribution-id $DIST_ID --paths \"/*\"
   
   # Update DNS if necessary (last resort, has propagation delay)
   # aws route53 change-resource-record-sets ...
   ```

10. **設定のロールバック**
    ```bash\n    # Rollback configuration files\n    git checkout v1.2.9 -- config/\n    \n    # Restart services with old configuration\n    sudo systemctl restart nginx\n    sudo systemctl restart app-service\n    \n    # Rollback environment variables\n    ./restore-env-vars.sh v1.2.9\n    \n    # Update feature flags\n    ./update-feature-flags.sh --disable-new-features\n    ```\n\n11. **データベースロールバック（必要な場合）**\n    ```sql\n    -- EXTREME CAUTION: Can cause data loss\n    \n    -- Check migration status\n    SELECT * FROM schema_migrations ORDER BY version DESC LIMIT 5;\n    \n    -- Rollback specific migrations (framework dependent)\n    -- Rails: rake db:migrate:down VERSION=20240115120000\n    -- Django: python manage.py migrate app_name 0001\n    -- Node.js: npm run migrate:down\n    \n    -- Verify database state\n    SHOW TABLES;\n    DESCRIBE critical_table;\n    ```\n\n12. **Service Health Validation**\n    ```bash\n    # Health check script\n    #!/bin/bash\n    \n    echo \"Validating rollback...\"\n    \n    # Check application health\n    if curl -f -s https://api.example.com/health > /dev/null; then\n        echo \"✅ Health check passed\"\n    else\n        echo \"❌ Health check failed\"\n        exit 1\n    fi\n    \n    # Check critical endpoints\n    endpoints=(\n        \"/api/users/me\"\n        \"/api/auth/status\"\n        \"/api/data/latest\"\n    )\n    \n    for endpoint in \"${endpoints[@]}\"; do\n        if curl -f -s \"https://api.example.com$endpoint\" > /dev/null; then\n            echo \"✅ $endpoint working\"\n        else\n            echo \"❌ $endpoint failed\"\n        fi\n    done\n    ```\n\n13. **Performance and Metrics Validation**\n    ```bash\n    # Check response times\n    curl -w \"Response time: %{time_total}s\\n\" -s -o /dev/null https://api.example.com/\n    \n    # Monitor error rates\n    tail -f /var/log/app/error.log | head -20\n    \n    # Check system resources\n    top -bn1 | head -10\n    free -h\n    df -h\n    \n    # Validate database connectivity\n    mysql -u app -p -e \"SELECT 1;\"\n    ```\n\n14. **Traffic Restoration**\n    ```bash\n    # Gradually restore traffic\n    ./restore-traffic.sh --gradual\n    \n    # Disable maintenance mode\n    ./disable-maintenance-mode.sh\n    \n    # Re-enable circuit breakers\n    ./deactivate-circuit-breaker.sh\n    \n    # Monitor traffic patterns\n    ./monitor-traffic.sh --duration 300\n    ```\n\n15. **Monitoring and Alerting**\n    ```bash\n    # Enable enhanced monitoring during rollback\n    ./enable-enhanced-monitoring.sh\n    \n    # Watch key metrics\n    watch -n 10 'curl -s https://api.example.com/metrics | jq .'\n    \n    # Monitor logs in real-time\n    tail -f /var/log/app/*.log | grep -E \"ERROR|WARN|EXCEPTION\"\n    \n    # Check application metrics\n    # - Response times\n    # - Error rates\n    # - User sessions\n    # - Database performance\n    ```\n\n16. **User Communication**\n    ```markdown\n    ## Service Update - Rollback Completed\n    \n    **Status:** ✅ Service Restored\n    **Time:** 2024-01-15 15:45 UTC\n    **Duration:** 12 minutes of degraded performance\n    \n    **What Happened:**\n    We identified performance issues with our latest release and \n    performed a rollback to ensure optimal service quality.\n    \n    **Current Status:**\n    - All services operating normally\n    - Performance metrics back to baseline\n    - No data loss occurred\n    \n    **Next Steps:**\n    We're investigating the root cause and will provide updates \n    on our status page.\n    ```\n\n17. **Post-Rollback Validation**\n    ```bash\n    # Extended monitoring period\n    ./monitor-extended.sh --duration 3600  # 1 hour\n    \n    # Run integration tests\n    npm run test:integration:production\n    \n    # Check user-reported issues\n    ./check-support-tickets.sh --since \"1 hour ago\"\n    \n    # Validate business metrics\n    ./check-business-metrics.sh\n    ```\n\n18. **Documentation and Reporting**\n    ```markdown\n    # Rollback Incident Report\n    \n    **Incident ID:** INC-2024-0115-001\n    **Rollback Version:** v1.2.9 (from v1.3.0)\n    **Start Time:** 2024-01-15 15:30 UTC\n    **End Time:** 2024-01-15 15:42 UTC\n    **Total Duration:** 12 minutes\n    \n    **Timeline:**\n    - 15:25 - Performance degradation detected\n    - 15:30 - Rollback decision made\n    - 15:32 - Traffic drained\n    - 15:35 - Rollback initiated\n    - 15:38 - Rollback completed\n    - 15:42 - Traffic fully restored\n    \n    **Impact:**\n    - 12 minutes of degraded performance\n    - ~5% of users experienced slow responses\n    - No data loss or corruption\n    - No security implications\n    \n    **Root Cause:**\n    Memory leak in new feature causing performance degradation\n    \n    **Lessons Learned:**\n    - Need better performance testing in staging\n    - Improve monitoring for memory usage\n    - Consider canary deployments for major releases\n    ```\n\n19. **Cleanup and Follow-up**\n    ```bash\n    # Clean up failed deployment artifacts\n    docker image rm app:v1.3.0\n    \n    # Update deployment status\n    ./update-deployment-status.sh \"rollback-completed\"\n    \n    # Reset feature flags if needed\n    ./reset-feature-flags.sh\n    \n    # Schedule post-incident review\n    ./schedule-postmortem.sh --date \"2024-01-16 10:00\"\n    ```\n\n20. **Prevention and Improvement**\n    - Analyze what went wrong with the deployment\n    - Improve testing and validation procedures\n    - Enhance monitoring and alerting\n    - Update rollback procedures based on learnings\n    - Consider implementing canary deployments\n\n**Rollback Decision Matrix:**\n\n| Issue Severity | Data Impact | Time to Fix | Decision |\n|---------------|-------------|-------------|----------|\n| Critical | None | > 30 min | Rollback |\n| High | Minor | > 60 min | Rollback |\n| Medium | None | > 2 hours | Consider rollback |\n| Low | None | Any | Forward fix |\n\n**Emergency Rollback Script Template:**\n```bash\n#!/bin/bash\nset -e\n\n# Emergency rollback script\nPREVIOUS_VERSION=\"${1:-v1.2.9}\"\nCURRENT_VERSION=$(curl -s https://api.example.com/version)\n\necho \"🚨 EMERGENCY ROLLBACK\"\necho \"From: $CURRENT_VERSION\"\necho \"To: $PREVIOUS_VERSION\"\necho \"\"\n\n# Confirm rollback\nread -p \"Proceed with rollback? (yes/no): \" confirm\nif [ \"$confirm\" != \"yes\" ]; then\n    echo \"Rollback cancelled\"\n    exit 1\nfi\n\n# Execute rollback\necho \"Starting rollback...\"\nkubectl set image deployment/app-deployment app=app:$PREVIOUS_VERSION\nkubectl rollout status deployment/app-deployment --timeout=300s\n\n# Validate\necho \"Validating rollback...\"\nsleep 30\ncurl -f https://api.example.com/health\n\necho \"✅ Rollback completed successfully\"\n```\n\nRemember: Rollbacks should be a last resort. Always consider forward fixes first, especially when database migrations are involved.