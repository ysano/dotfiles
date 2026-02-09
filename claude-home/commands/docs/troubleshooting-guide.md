---
description: "Generate troubleshooting documentation"
---

## Instructions

Follow this systematic approach to create troubleshooting guides: **$ARGUMENTS**

1. **System Overview and Architecture**
   - Document the system architecture and components
   - Map out dependencies and integrations
   - Identify critical paths and failure points
   - Create system topology diagrams
   - Document data flow and communication patterns

2. **Common Issues Identification**
   - Collect historical support tickets and issues
   - Interview team members about frequent problems
   - Analyze error logs and monitoring data
   - Review user feedback and complaints
   - Identify patterns in system failures

3. **Troubleshooting Framework**
   - Establish systematic diagnostic procedures
   - Create problem isolation methodologies
   - Document escalation paths and procedures
   - Set up logging and monitoring checkpoints
   - Define severity levels and response times

4. **Diagnostic Tools and Commands**
   
   ```markdown
   ## Essential Diagnostic Commands
   
   ### System Health
   ```bash
   # Check system resources
   top                    # CPU and memory usage
   df -h                 # Disk space
   free -m               # Memory usage
   netstat -tuln         # Network connections
   
   # Application logs
   tail -f /var/log/app.log
   journalctl -u service-name -f
   
   # Database connectivity
   mysql -u user -p -e "SELECT 1"
   psql -h host -U user -d db -c "SELECT 1"
   ```
   ```

5. **Issue Categories and Solutions**

   **Performance Issues:**
   ```markdown
   ### Slow Response Times
   
   **Symptoms:**
// ... (22 lines truncated)
   ```

6. **Error Code Documentation**
   
   ```markdown
   ## Error Code Reference
   
   ### HTTP Status Codes
// ... (15 lines truncated)
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
// ... (6 lines truncated)
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
// ... (16 lines truncated)
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
// ... (16 lines truncated)
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
// ... (9 lines truncated)
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
grep -i "error" /var/log/app.log | tail -50

# Analyze log patterns
// ... (5 lines truncated)
```

**Performance Profiling:**
```bash
# System performance
iostat -x 1
sar -u 1 10
// ... (6 lines truncated)
```
