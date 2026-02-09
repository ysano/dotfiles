---
description: "Setup continuous integration pipeline"
---

## Instructions

Follow this systematic approach to implement CI/CD: **$ARGUMENTS**

1. **Project Analysis**
   - Identify the technology stack and deployment requirements
   - Review existing build and test processes
   - Understand deployment environments (dev, staging, prod)
   - Assess current version control and branching strategy

2. **CI/CD Platform Selection**
   - Choose appropriate CI/CD platform based on requirements:
     - **GitHub Actions**: Native GitHub integration, extensive marketplace
     - **GitLab CI**: Built-in GitLab, comprehensive DevOps platform
     - **Jenkins**: Self-hosted, highly customizable, extensive plugins
     - **CircleCI**: Cloud-based, optimized for speed
     - **Azure DevOps**: Microsoft ecosystem integration
     - **AWS CodePipeline**: AWS-native solution

3. **Repository Setup**
   - Ensure proper `.gitignore` configuration
   - Set up branch protection rules
   - Configure merge requirements and reviews
   - Establish semantic versioning strategy

4. **Build Pipeline Configuration**
   
   **GitHub Actions Example:**
   ```yaml
   name: CI/CD Pipeline
   
   on:
// ... (19 lines truncated)
   ```

   **GitLab CI Example:**
   ```yaml
   stages:
     - test
     - build
// ... (11 lines truncated)
   ```

5. **Environment Configuration**
   - Set up environment variables and secrets
   - Configure different environments (dev, staging, prod)
   - Implement environment-specific configurations
   - Set up secure secret management

6. **Automated Testing Integration**
   - Configure unit test execution
   - Set up integration test running
   - Implement E2E test execution
   - Configure test reporting and coverage

   **Multi-stage Testing:**
   ```yaml
   test:
     strategy:
       matrix:
// ... (10 lines truncated)
   ```

7. **Code Quality Gates**
   - Integrate linting and formatting checks
   - Set up static code analysis (SonarQube, CodeClimate)
   - Configure security vulnerability scanning
   - Implement code coverage thresholds

8. **Build Optimization**
   - Configure build caching strategies
   - Implement parallel job execution
   - Optimize Docker image builds
   - Set up artifact management

   **Caching Example:**
   ```yaml
   - name: Cache node modules
     uses: actions/cache@v3
     with:
// ... (5 lines truncated)
   ```

9. **Docker Integration**
   - Create optimized Dockerfiles
   - Set up multi-stage builds
   - Configure container registry integration
   - Implement security scanning for images

   **Multi-stage Dockerfile:**
   ```dockerfile
   FROM node:18-alpine AS builder
   WORKDIR /app
   COPY package*.json ./
// ... (9 lines truncated)
   ```

10. **Deployment Strategies**
    - Implement blue-green deployment
    - Set up canary releases
    - Configure rolling updates
    - Implement feature flags integration

11. **Infrastructure as Code**
    - Use Terraform, CloudFormation, or similar tools
    - Version control infrastructure definitions
    - Implement infrastructure testing
    - Set up automated infrastructure provisioning

12. **Monitoring and Observability**
    - Set up application performance monitoring
    - Configure log aggregation and analysis
    - Implement health checks and alerting
    - Set up deployment notifications

13. **Security Integration**
    - Implement dependency vulnerability scanning
    - Set up container security scanning
    - Configure SAST (Static Application Security Testing)
    - Implement secrets scanning

   **Security Scanning Example:**
   ```yaml
   security:
     runs-on: ubuntu-latest
     steps:
// ... (6 lines truncated)
   ```

14. **Database Migration Handling**
    - Automate database schema migrations
    - Implement rollback strategies
    - Set up database seeding for testing
    - Configure backup and recovery procedures

15. **Performance Testing Integration**
    - Set up load testing in pipeline
    - Configure performance benchmarks
    - Implement performance regression detection
    - Set up performance monitoring

16. **Multi-Environment Deployment**
    - Configure staging environment deployment
    - Set up production deployment with approvals
    - Implement environment promotion workflow
    - Configure environment-specific configurations

   **Environment Deployment:**
   ```yaml
   deploy-staging:
     needs: test
     if: github.ref == 'refs/heads/develop'
// ... (16 lines truncated)
   ```

17. **Rollback and Recovery**
    - Implement automated rollback procedures
    - Set up deployment verification tests
    - Configure failure detection and alerts
    - Document manual recovery procedures

18. **Notification and Reporting**
    - Set up Slack/Teams integration for notifications
    - Configure email alerts for failures
    - Implement deployment status reporting
    - Set up metrics dashboards

19. **Compliance and Auditing**
    - Implement deployment audit trails
    - Set up compliance checks (SOC 2, HIPAA, etc.)
    - Configure approval workflows for sensitive deployments
    - Document change management processes

20. **Pipeline Optimization**
    - Monitor pipeline performance and costs
    - Implement pipeline parallelization
    - Optimize resource allocation
    - Set up pipeline analytics and reporting

**Best Practices:**

1. **Fail Fast**: Implement early failure detection
2. **Parallel Execution**: Run independent jobs in parallel
3. **Caching**: Cache dependencies and build artifacts
4. **Security**: Never expose secrets in logs
5. **Documentation**: Document pipeline processes and procedures
6. **Monitoring**: Monitor pipeline health and performance
7. **Testing**: Test pipeline changes in feature branches
8. **Rollback**: Always have a rollback strategy

**Sample Complete Pipeline:**
```yaml
name: Full CI/CD Pipeline

on:
// ... (45 lines truncated)
```

Start with basic CI and gradually add more sophisticated features as your team and project mature.
