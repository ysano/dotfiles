# テストカバレッジコマンド

包括的なレポートと実行可能な推奨事項で、あらゆるコードベースのテストカバレッジを分析・改善します。

## 実行手順

テストカバレッジを分析・改善するための体系的アプローチに従ってください： **$ARGUMENTS**

1. **カバレッジツールの設定**
   - 適切なカバレッジツールの特定と構成：
     - JavaScript/Node.js: Jest, NYC, Istanbul
     - Python: Coverage.py, pytest-cov
     - Java: JaCoCo, Cobertura
     - C#: dotCover, OpenCover
     - Ruby: SimpleCov
   - カバレッジレポート形式の構成（HTML、XML、JSON）
   - カバレッジ闾値と品質ゲートの設定

2. **ベースラインカバレッジ分析**
   - カバレッジレポートで既存テストを実行
   - 包括的なカバレッジレポートの生成
   - 現在のカバレッジ率の文書化：
     - 行カバレッジ
     - ブランチカバレッジ
     - 関数カバレッジ
     - ステートメントカバレッジ
   - カバーされていないコード領域の特定

3. **カバレッジレポート分析**
   - ファイル・ディレクトリ別の詳細カバレッジレポートのレビュー
   - 重要なカバーされていないコードパスの特定
   - 条件ロジックのブランチカバレッジ分析
   - テストされていない関数とメソッドの検出
   - 時間経過によるカバレッジトレンドの検査

4. **クリティカルパスの特定**
   - カバレッジが不足しているビジネスクリティカルなコードの特定
   - 高リスク・低カバレッジ領域の優先順位付け
   - パブリックAPIとインターフェースに焦点を当てる
   - エラーハンドリングとエッジケースをターゲット
   - セキュリティに敏感なコードパスの検査

5. **テストギャップ分析**
   - カバーされていないコードの分類：
     - 早急なテストが必要なビジネスロジック
     - エラーハンドリングと例外パス
     - 設定とセットアップコード
     - ユーティリティ関数とヘルパー
     - 削除すべきデッドまたは旧式コード

6. **戦略的テスト作成**
   - カバーされていないビジネスロジックの単体テスト作成
   - カバーされていないワークフローの結合テスト追加
   - エラー状態とエッジケースのテスト作成
   - 設定と環境固有コードのテスト
   - バグが起きやすい領域のリグレッションテスト追加

7. **ブランチカバレッジの改善**
   - カバーされていない条件ブランチの特定
   - trueとfalse両方の条件のテスト
   - すべてのswitch/case文のカバー
   - 例外ハンドリングパスのテスト
   - ループ条件と反復の検証

8. **エッジケーステスト**
   - 境界条件と制限のテスト
   - null、空、無効入力のテスト
   - タイムアウトとネットワーク障害シナリオのテスト
   - リソース枚渇状態のテスト
   - 同時アクセスと競合状態のテスト

9. **Mock and Stub Strategy**
   - Mock external dependencies for better isolation
   - Stub complex operations to focus on logic
   - Use dependency injection for testability
   - Create test doubles for external services
   - Implement proper cleanup for test resources

10. **Performance Impact Assessment**
    - Measure test execution time with new tests
    - Optimize slow tests without losing coverage
    - Parallelize test execution where possible
    - Balance coverage goals with execution speed
    - Consider test categorization (fast/slow, unit/integration)

11. **Coverage Quality Assessment**
    - Ensure tests actually verify behavior, not just execution
    - Check for meaningful assertions in tests
    - Avoid testing implementation details
    - Focus on testing contracts and interfaces
    - Review test quality alongside coverage metrics

12. **Framework-Specific Coverage Enhancement**
    
    **For Web Applications:**
    - Test API endpoints and HTTP status codes
    - Test form validation and user input handling
    - Test authentication and authorization flows
    - Test error pages and user feedback

    **For Mobile Applications:**
    - Test device-specific functionality
    - Test different screen sizes and orientations
    - Test offline and network connectivity scenarios
    - Test platform-specific features

    **For Backend Services:**
    - Test database operations and transactions
    - Test message queue processing
    - Test caching and performance optimizations
    - Test service integrations and API calls

13. **Continuous Coverage Monitoring**
    - Set up automated coverage reporting in CI/CD
    - Configure coverage thresholds to prevent regression
    - Generate coverage badges and reports
    - Monitor coverage trends and improvements
    - Alert on significant coverage decreases

14. **Coverage Exclusion Management**
    - Properly exclude auto-generated code
    - Exclude third-party libraries and dependencies
    - Document reasons for coverage exclusions
    - Regularly review and update exclusion rules
    - Avoid excluding code that should be tested

15. **Team Coverage Goals**
    - Set realistic coverage targets based on project needs
    - Establish minimum coverage requirements for new code
    - Create coverage improvement roadmap
    - Review coverage in code reviews
    - Celebrate coverage milestones and improvements

16. **Coverage Reporting and Communication**
    - Generate clear, actionable coverage reports
    - Create coverage dashboards for stakeholders
    - Document coverage improvement strategies
    - Share coverage results with development team
    - Integrate coverage into project health metrics

17. **Mutation Testing (Advanced)**
    - Implement mutation testing to validate test quality
    - Identify tests that don't catch actual bugs
    - Improve test assertions and edge case coverage
    - Use mutation testing tools specific to your language
    - Balance mutation testing cost with quality benefits

18. **Legacy Code Coverage Strategy**
    - Prioritize high-risk legacy code for testing
    - Use characterization tests for complex legacy systems
    - Refactor for testability where possible
    - Add tests before making changes to legacy code
    - Document known limitations and technical debt

**Sample Coverage Commands:**

```bash
# JavaScript with Jest
npm test -- --coverage --coverage-reporters=html,text,lcov

# Python with pytest
pytest --cov=src --cov-report=html --cov-report=term

# Java with Maven
mvn clean test jacoco:report

# .NET Core
dotnet test --collect:"XPlat Code Coverage"
```

Remember that 100% coverage is not always the goal - focus on meaningful coverage that actually improves code quality and catches bugs.