---
description: "Prepare and validate release packages"
---

## Instructions

Follow this systematic approach to prepare a release: **$ARGUMENTS**

1. **Release Planning and Validation**
   - Determine release version number (semantic versioning)
   - Review and validate all features included in release
   - Check that all planned issues and features are complete
   - Verify release criteria and acceptance requirements

2. **Pre-Release Checklist**
   - Ensure all tests are passing (unit, integration, E2E)
   - Verify code coverage meets project standards
   - Complete security vulnerability scanning
   - Perform performance testing and validation
   - Review and approve all pending pull requests

3. **Version Management**
   ```bash
   # Check current version
   git describe --tags --abbrev=0
   
// ... (11 lines truncated)
   ```

4. **Code Freeze and Branch Management**
   ```bash
   # Create release branch from main
   git checkout main
   git pull origin main
// ... (5 lines truncated)
   ```

5. **Version Number Updates**
   - Update package.json, setup.py, or equivalent version files
   - Update version in application configuration
   - Update version in documentation and README
   - Update API version if applicable

   ```bash
   # Node.js projects
   npm version patch  # or minor, major
   
// ... (6 lines truncated)
   ```

6. **Changelog Generation**
   ```markdown
   # CHANGELOG.md
   
   ## [1.2.3] - 2024-01-15
// ... (20 lines truncated)
   ```

7. **Documentation Updates**
   - Update API documentation with new endpoints
   - Revise user documentation and guides
   - Update installation and deployment instructions
   - Review and update README.md
   - Update migration guides if needed

8. **Dependency Management**
   ```bash
   # Update and audit dependencies
   npm audit fix
   npm update
// ... (9 lines truncated)
   ```

9. **Build and Artifact Generation**
   ```bash
   # Clean build environment
   npm run clean
   rm -rf dist/ build/
// ... (10 lines truncated)
   ```

10. **Testing and Quality Assurance**
    - Run comprehensive test suite
    - Perform manual testing of critical features
    - Execute regression testing
    - Conduct user acceptance testing
    - Validate in staging environment

    ```bash
    # Run all tests
    npm test
    npm run test:integration
// ... (8 lines truncated)
    ```

11. **Security and Compliance Verification**
    - Run security scans and penetration testing
    - Verify compliance with security standards
    - Check for exposed secrets or credentials
    - Validate data protection and privacy measures

12. **Release Notes Preparation**
    ```markdown
    # Release Notes v1.2.3
    
    ## 🎉 What's New
// ... (24 lines truncated)
    ```

13. **Release Tagging and Versioning**
    ```bash
    # Create annotated tag
    git add .
    git commit -m "chore: prepare release v1.2.3"
// ... (14 lines truncated)
    ```

14. **Deployment Preparation**
    - Prepare deployment scripts and configurations
    - Update environment variables and secrets
    - Plan deployment strategy (blue-green, rolling, canary)
    - Set up monitoring and alerting for release
    - Prepare rollback procedures

15. **Staging Environment Validation**
    ```bash
    # Deploy to staging
    ./deploy-staging.sh v1.2.3
    
// ... (10 lines truncated)
    ```

16. **Production Deployment Planning**
    - Schedule deployment window
    - Notify stakeholders and users
    - Prepare maintenance mode if needed
    - Set up deployment monitoring
    - Plan communication strategy

17. **Release Automation Setup**
    ```yaml
    # GitHub Actions Release Workflow
    name: Release
    
// ... (34 lines truncated)
    ```

18. **Communication and Announcements**
    - Prepare release announcement
    - Update status page and documentation
    - Notify customers and users
    - Share on relevant communication channels
    - Update social media and marketing materials

19. **Post-Release Monitoring**
    - Monitor application performance and errors
    - Track user adoption of new features
    - Monitor system metrics and alerts
    - Collect user feedback and issues
    - Prepare hotfix procedures if needed

20. **Release Retrospective**
    - Document lessons learned
    - Review release process effectiveness
    - Identify improvement opportunities
    - Update release procedures
    - Plan for next release cycle

**Release Types and Considerations:**

**Patch Release (1.2.3 → 1.2.4):**
- Bug fixes only
- No new features
- Minimal testing required
- Quick deployment

**Minor Release (1.2.3 → 1.3.0):**
- New features (backward compatible)
- Enhanced functionality
- Comprehensive testing
- User communication needed

**Major Release (1.2.3 → 2.0.0):**
- Breaking changes
- Significant new features
- Migration guide required
- Extended testing period
- User training and support

**Hotfix Release:**
```bash
git checkout main
git pull origin main
git checkout -b hotfix/critical-bug-fix
// ... (11 lines truncated)
```
