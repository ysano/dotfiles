# リリース準備コマンド

包括的な検証とドキュメンテーションで、あらゆるプロジェクトのソフトウェアリリースを体系的に準備し、実行します。

## 実行手順

リリースを準備するための体系的なアプローチに従ってください: **$ARGUMENTS**

1. **リリース計画と検証**
   - リリースバージョン番号の決定（セマンティックバージョニング）
   - リリースに含まれるすべての機能のレビューと検証
   - 計画されたすべての問題と機能が完了しているか確認
   - リリース基準と受入要件の検証

2. **リリース前チェックリスト**
   - すべてのテストが成功していることを確認（単体、統合、E2E）
   - コードカバレッジがプロジェクト標準を満たしていることを検証
   - セキュリティ脆弱性スキャンの完了
   - パフォーマンステストと検証の実行
   - 保留中のすべてのpull requestのレビューと承認

3. **バージョン管理**
   ```bash
   # Check current version
   git describe --tags --abbrev=0
   
   # Determine next version (semantic versioning)
   # MAJOR.MINOR.PATCH
   # MAJOR: Breaking changes
   # MINOR: New features (backward compatible)
   # PATCH: Bug fixes (backward compatible)
   
   # Example version updates
   # 1.2.3 -> 1.2.4 (patch)
   # 1.2.3 -> 1.3.0 (minor)
   # 1.2.3 -> 2.0.0 (major)
   ```

4. **コードフリーズとブランチ管理**
   ```bash
   # Create release branch from main
   git checkout main
   git pull origin main
   git checkout -b release/v1.2.3
   
   # Alternative: Use main branch directly for smaller releases
   # Ensure no new features are merged during release process
   ```

5. **バージョン番号の更新**
   - package.json、setup.py、または同等のバージョンファイルを更新
   - アプリケーション設定のバージョンを更新
   - ドキュメンテーションとREADMEのバージョンを更新
   - 該当する場合はAPIバージョンを更新

   ```bash
   # Node.js projects
   npm version patch  # or minor, major
   
   # Python projects
   # Update version in setup.py, __init__.py, or pyproject.toml
   
   # Manual version update
   sed -i 's/"version": "1.2.2"/"version": "1.2.3"/' package.json
   ```

6. **チェンジログ生成**
   ```markdown
   # CHANGELOG.md
   
   ## [1.2.3] - 2024-01-15
   
   ### Added
   - New user authentication system
   - Dark mode support for UI
   - API rate limiting functionality
   
   ### Changed
   - Improved database query performance
   - Updated user interface design
   - Enhanced error handling
   
   ### Fixed
   - Fixed memory leak in background tasks
   - Resolved issue with file upload validation
   - Fixed timezone handling in date calculations
   
   ### Security
   - Updated dependencies with security patches
   - Improved input validation and sanitization
   ```

7. **ドキュメンテーションの更新**
   - 新しいエンドポイントでAPIドキュメンテーションを更新
   - ユーザードキュメンテーションとガイドの改訂
   - インストールとデプロイ手順の更新
   - README.mdのレビューと更新
   - 必要に応じて移行ガイドを更新

8. **依存関係管理**
   ```bash
   # Update and audit dependencies
   npm audit fix
   npm update
   
   # Python
   pip-audit
   pip freeze > requirements.txt
   
   # Review security vulnerabilities
   npm audit
   snyk test
   ```

9. **ビルドと成果物生成**
   ```bash
   # Clean build environment
   npm run clean
   rm -rf dist/ build/
   
   # Build production artifacts
   npm run build
   
   # Verify build artifacts
   ls -la dist/
   
   # Test built artifacts
   npm run test:build
   ```

10. **テストと品質保証**
    - 包括的なテストスイートの実行
    - 重要な機能の手動テストの実行
    - リグレッションテストの実行
    - ユーザー受入テストの実施
    - ステージング環境での検証

    ```bash
    # Run all tests
    npm test
    npm run test:integration
    npm run test:e2e
    
    # Check code coverage
    npm run test:coverage
    
    # Performance testing
    npm run test:performance
    ```

11. **セキュリティとコンプライアンス検証**
    - セキュリティスキャンとペネトレーションテストの実行
    - セキュリティ標準へのコンプライアンスの検証
    - 暴露されたシークレットや認証情報の確認
    - データ保護とプライバシー対策の検証

12. **リリースノートの準備**
    ```markdown
    # Release Notes v1.2.3
    
    ## 🎉 What's New
    - **Dark Mode**: Users can now switch to dark mode in settings
    - **Enhanced Security**: Improved authentication with 2FA support
    - **Performance**: 40% faster page load times
    
    ## 🔧 Improvements
    - Better error messages for form validation
    - Improved mobile responsiveness
    - Enhanced accessibility features
    
    ## 🐛 Bug Fixes
    - Fixed issue with file downloads in Safari
    - Resolved memory leak in background tasks
    - Fixed timezone display issues
    
    ## 📚 Documentation
    - Updated API documentation
    - New user onboarding guide
    - Enhanced troubleshooting section
    
    ## 🔄 Migration Guide
    - No breaking changes in this release
    - Automatic database migrations included
    - See [Migration Guide](link) for details
    ```

13. **リリースタグ付けとバージョニング**
    ```bash
    # Create annotated tag
    git add .
    git commit -m "chore: prepare release v1.2.3"
    git tag -a v1.2.3 -m "Release version 1.2.3
    
    Features:
    - Dark mode support
    - Enhanced authentication
    
    Bug fixes:
    - Fixed file upload issues
    - Resolved memory leaks"
    
    # Push tag to remote
    git push origin v1.2.3
    git push origin release/v1.2.3
    ```

14. **デプロイ準備**
    - デプロイスクリプトと設定の準備
    - 環境変数とシークレットの更新
    - デプロイ戦略の計画（ブルーグリーン、ローリング、カナリア）
    - リリース用の監視とアラートのセットアップ
    - ロールバック手順の準備

15. **ステージング環境検証**
    ```bash
    # Deploy to staging
    ./deploy-staging.sh v1.2.3
    
    # Run smoke tests
    npm run test:smoke:staging
    
    # Manual validation checklist
    # [ ] User login/logout
    # [ ] Core functionality
    # [ ] New features
    # [ ] Performance metrics
    # [ ] Security checks
    ```

16. **本番環境デプロイ計画**
    - デプロイウィンドウのスケジュール設定
    - ステークホルダーとユーザーへの通知
    - 必要に応じてメンテナンスモードの準備
    - デプロイ監視のセットアップ
    - コミュニケーション戦略の計画

17. **リリース自動化のセットアップ**
    ```yaml
    # GitHub Actions Release Workflow
    name: Release
    
    on:
      push:
        tags:
          - 'v*'
    
    jobs:
      release:
        runs-on: ubuntu-latest
        steps:
          - uses: actions/checkout@v3
          - name: Setup Node.js
            uses: actions/setup-node@v3
            with:
              node-version: '18'
          
          - name: Install dependencies
            run: npm ci
          
          - name: Run tests
            run: npm test
          
          - name: Build
            run: npm run build
          
          - name: Create Release
            uses: actions/create-release@v1
            env:
              GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
            with:
              tag_name: ${{ github.ref }}
              release_name: Release ${{ github.ref }}
              draft: false
              prerelease: false
    ```

18. **コミュニケーションとアナウンス**
    - リリースアナウンスの準備
    - ステータスページとドキュメンテーションの更新
    - 顧客とユーザーへの通知
    - 関連するコミュニケーションチャンネルでの共有
    - ソーシャルメディアとマーケティング資料の更新

19. **リリース後の監視**
    - アプリケーションのパフォーマンスとエラーの監視
    - 新機能のユーザー採用率の追跡
    - システムメトリクスとアラートの監視
    - ユーザーフィードバックと問題の収集
    - 必要に応じてホットフィックス手順の準備

20. **リリースレトロスペクティブ**
    - 学んだ教訓の文書化
    - リリースプロセスの有効性のレビュー
    - 改善機会の特定
    - リリース手順の更新
    - 次のリリースサイクルの計画

**リリースタイプと考慮事項：**

**パッチリリース (1.2.3 → 1.2.4):**
- バグ修正のみ
- 新機能なし
- 最小限のテストが必要
- 迅速なデプロイ

**マイナーリリース (1.2.3 → 1.3.0):**
- 新機能（後方互換性あり）
- 機能強化
- 包括的なテスト
- ユーザーへのコミュニケーションが必要

**メジャーリリース (1.2.3 → 2.0.0):**
- 破壊的変更
- 重要な新機能
- 移行ガイドが必要
- 延長されたテスト期間
- ユーザートレーニングとサポート

**ホットフィックスリリース：**
```bash
# Emergency hotfix process
git checkout main
git pull origin main
git checkout -b hotfix/critical-bug-fix

# Make minimal fix
git add .
git commit -m "hotfix: fix critical security vulnerability"

# Fast-track testing and deployment
npm test
git tag -a v1.2.4-hotfix.1 -m "Hotfix for critical security issue"
git push origin hotfix/critical-bug-fix
git push origin v1.2.4-hotfix.1
```

必ず実行すべきこと：
- リリース前にすべてを徹底的にテストする
- すべてのステークホルダーと明確にコミュニケーションを取る
- ロールバック手順を準備しておく
- デプロイ後のリリースを緊密に監視する
- 将来のリリースのためにすべてを文書化する