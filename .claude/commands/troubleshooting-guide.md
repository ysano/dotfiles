# トラブルシューティングガイド生成コマンド

あらゆるシステム、アプリケーション、コードベースのための包括的なトラブルシューティングドキュメンテーションを作成します。

## 実行手順

トラブルシューティングガイドを作成するために以下の体系的アプローチに従ってください：**$ARGUMENTS**

1. **システム概要とアーキテクチャ**
   - システムアーキテクチャとコンポーネントを文書化する
   - 依存関係と統合をマッピングする
   - 重要パスと故障ポイントを特定する
   - システムトポロジ図を作成する
   - データフローと通信パターンを文書化する

2. **一般的な問題の特定**
   - 過去のサポートチケットと問題を収集する
   - 頻繁な問題についてチームメンバーにインタビューする
   - エラーログと監視データを分析する
   - ユーザーフィードバックと苦情をレビューする
   - システム故障のパターンを特定する

3. **トラブルシューティングフレームワーク**
   - 体系的な診断手順を確立する
   - 問題分離方法論を作成する
   - エスカレーションパスと手順を文書化する
   - ログ出力と監視チェックポイントを設定する
   - 重大度レベルと対応時間を定義する

4. **診断ツールとコマンド**
   
   ```markdown
   ## 必須診断コマンド
   
   ### システムヘルス
   ```bash
   # システムリソースチェック
   top                    # CPUとメモリ使用率
   df -h                 # ディスク容量
   free -m               # メモリ使用率
   netstat -tuln         # ネットワーク接続
   
   # アプリケーションログ
   tail -f /var/log/app.log
   journalctl -u service-name -f
   
   # データベース接続
   mysql -u user -p -e "SELECT 1"
   psql -h host -U user -d db -c "SELECT 1"
   ```
   ```

5. **問題カテゴリと解決策**

   **パフォーマンス問題:**
   ```markdown
   ### レスポンス時間の遅延
   
   **症状:**
   - APIレスポンスが5秒以上
   - ユーザーインターフェースのフリーズ
   - データベースタイムアウト
   
   **診断手順:**
   1. システムリソースをチェック（CPU、メモリ、ディスク）
   2. アプリケーションログでエラーをレビュー
   3. データベースクエリパフォーマンスを分析
   4. ネットワーク接続とレイテンシをチェック
   
   **一般的な原因:**
   - データベースコネクションプールの果尽
   - 非効率なデータベースクエリ
   - アプリケーションのメモリリーク
   - ネットワーク帯域の制限
   
   **解決策:**
   - アプリケーションサービスを再起動
   - データベースクエリを最適化
   - コネクションプールサイズを増加
   - インフラストラクチャリソースをスケール
   ```

6. **Error Code Documentation**
   
   ```markdown
   ## Error Code Reference
   
   ### HTTP Status Codes
   - **500 Internal Server Error**
     - Check application logs for stack traces
     - Verify database connectivity
     - Check environment variables
   
   - **404 Not Found**
     - Verify URL routing configuration
     - Check if resources exist
     - Review API endpoint documentation
   
   - **503 Service Unavailable**
     - Check service health status
     - Verify load balancer configuration
     - Check for maintenance mode
   ```

7. **Environment-Specific Issues**
   - Document development environment problems
   - Address staging/testing environment issues
   - Cover production-specific troubleshooting
   - Include local development setup problems

8. **Database Troubleshooting**
   
   ```markdown
   ### Database Connection Issues
   
   **Symptoms:**
   - "Connection refused" errors
   - "Too many connections" errors
   - Slow query performance
   
   **Diagnostic Commands:**
   ```sql
   -- Check active connections
   SHOW PROCESSLIST;
   
   -- Check database size
   SELECT table_schema, 
          ROUND(SUM(data_length + index_length) / 1024 / 1024, 1) AS 'DB Size in MB' 
   FROM information_schema.tables 
   GROUP BY table_schema;
   
   -- Check slow queries
   SHOW VARIABLES LIKE 'slow_query_log';
   ```
   ```

9. **Network and Connectivity Issues**
   
   ```markdown
   ### Network Troubleshooting
   
   **Basic Connectivity:**
   ```bash
   # Test basic connectivity
   ping example.com
   telnet host port
   curl -v https://api.example.com/health
   
   # DNS resolution
   nslookup example.com
   dig example.com
   
   # Network routing
   traceroute example.com
   ```
   
   **SSL/TLS Issues:**
   ```bash
   # Check SSL certificate
   openssl s_client -connect example.com:443
   curl -vI https://example.com
   ```
   ```

10. **Application-Specific Troubleshooting**
    
    **Memory Issues:**
    ```markdown
    ### Out of Memory Errors
    
    **Java Applications:**
    ```bash
    # Check heap usage
    jstat -gc [PID]
    jmap -dump:format=b,file=heapdump.hprof [PID]
    
    # Analyze heap dump
    jhat heapdump.hprof
    ```
    
    **Node.js Applications:**
    ```bash
    # Monitor memory usage
    node --inspect app.js
    # Use Chrome DevTools for memory profiling
    ```
    ```

11. **Security and Authentication Issues**
    
    ```markdown
    ### Authentication Failures
    
    **Symptoms:**
    - 401 Unauthorized responses
    - Token validation errors
    - Session timeout issues
    
    **Diagnostic Steps:**
    1. Verify credentials and tokens
    2. Check token expiration
    3. Validate authentication service
    4. Review CORS configuration
    
    **Common Solutions:**
    - Refresh authentication tokens
    - Clear browser cookies/cache
    - Verify CORS headers
    - Check API key permissions
    ```

12. **Deployment and Configuration Issues**
    
    ```markdown
    ### Deployment Failures
    
    **Container Issues:**
    ```bash
    # Check container status
    docker ps -a
    docker logs container-name
    
    # Check resource limits
    docker stats
    
    # Debug container
    docker exec -it container-name /bin/bash
    ```
    
    **Kubernetes Issues:**
    ```bash
    # Check pod status
    kubectl get pods
    kubectl describe pod pod-name
    kubectl logs pod-name
    
    # Check service connectivity
    kubectl get svc
    kubectl port-forward pod-name 8080:8080
    ```
    ```

13. **Monitoring and Alerting Setup**
    - Configure health checks and monitoring
    - Set up log aggregation and analysis
    - Implement alerting for critical issues
    - Create dashboards for system metrics
    - Document monitoring thresholds

14. **Escalation Procedures**
    
    ```markdown
    ## Escalation Matrix
    
    ### Severity Levels
    
    **Critical (P1):** System down, data loss
    - Immediate response required
    - Escalate to on-call engineer
    - Notify management within 30 minutes
    
    **High (P2):** Major functionality impaired
    - Response within 2 hours
    - Escalate to senior engineer
    - Provide hourly updates
    
    **Medium (P3):** Minor functionality issues
    - Response within 8 hours
    - Assign to appropriate team member
    - Provide daily updates
    ```

15. **Recovery Procedures**
    - Document system recovery steps
    - Create data backup and restore procedures
    - Establish rollback procedures for deployments
    - Document disaster recovery processes
    - Test recovery procedures regularly

16. **Preventive Measures**
    - Implement monitoring and alerting
    - Set up automated health checks
    - Create deployment validation procedures
    - Establish code review processes
    - Document maintenance procedures

17. **Knowledge Base Integration**
    - Link to relevant documentation
    - Reference API documentation
    - Include links to monitoring dashboards
    - Connect to team communication channels
    - Integrate with ticketing systems

18. **Team Communication**
    
    ```markdown
    ## Communication Channels
    
    ### Immediate Response
    - Slack: #incidents channel
    - Phone: On-call rotation
    - Email: alerts@company.com
    
    ### Status Updates
    - Status page: status.company.com
    - Twitter: @company_status
    - Internal wiki: troubleshooting section
    ```

19. **Documentation Maintenance**
    - Regular review and updates
    - Version control for troubleshooting guides
    - Feedback collection from users
    - Integration with incident post-mortems
    - Continuous improvement processes

20. **Self-Service Tools**
    - Create diagnostic scripts and tools
    - Build automated recovery procedures
    - Implement self-healing systems
    - Provide user-friendly diagnostic interfaces
    - Create chatbot integration for common issues

**Advanced Troubleshooting Techniques:**

**Log Analysis:**
```bash
# Search for specific errors
grep -i "error" /var/log/app.log | tail -50

# Analyze log patterns
awk '{print $1}' access.log | sort | uniq -c | sort -nr

# Monitor logs in real-time
tail -f /var/log/app.log | grep -i "exception"
```

**Performance Profiling:**
```bash
# System performance
iostat -x 1
sar -u 1 10
vmstat 1 10

# Application profiling
strace -p [PID]
perf record -p [PID]
```

以下を必ず実行してください:
- トラブルシューティングガイドを最新に保つ
- 文書化されたすべての手順を定期的にテストする
- ユーザーからフィードバックを収集し、ガイドを改善する
- 有用な場合はスクリーンショットと視覚的補助を含める
- ガイドを検索可能で整理された状態にする