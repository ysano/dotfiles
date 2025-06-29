# コードリファクタリングコマンド

機能を保持しながら保守性、パフォーマンス、可読性を向上させるために安全にコードをリファクタリングします。

## 実行手順

コードをリファクタリングするための体系的なアプローチに従う: **$ARGUMENTS**

1. **リファクタリング前分析**
   - リファクタリングが必要なコードとその理由を特定
   - 現在の機能と動作を完全に理解
   - 既存のテストとドキュメントの確認
   - すべての依存関係と使用ポイントの特定

2. **テストカバレッジ検証**
   - リファクタリング対象コードに包括的なテストカバレッジが存在することを確認
   - テストが不足している場合は、リファクタリング開始前に作成
   - すべてのテストを実行してベースラインを確立
   - 必要に応じて追加テストで現在の動作を文書化

3. **リファクタリング戦略**
   - リファクタリングの明確な目標を定義（パフォーマンス、可読性、保守性）
   - 適切なリファクタリング技法を選択:
     - メソッド/関数の抽出
     - クラス/コンポーネントの抽出
     - 変数/メソッドの名前変更
     - メソッド/フィールドの移動
     - ポリモーフィズムによる条件分岐の置換
     - デッドコードの除去
   - 小さな増分ステップでリファクタリングを計画

4. **Environment Setup**
   - Create a new branch: `git checkout -b refactor/$ARGUMENTS`
   - Ensure all tests pass before starting
   - Set up any additional tooling needed (profilers, analyzers)

5. **Incremental Refactoring**
   - Make small, focused changes one at a time
   - Run tests after each change to ensure nothing breaks
   - Commit working changes frequently with descriptive messages
   - Use IDE refactoring tools when available for safety

6. **Code Quality Improvements**
   - Improve naming conventions for clarity
   - Eliminate code duplication (DRY principle)
   - Simplify complex conditional logic
   - Reduce method/function length and complexity
   - Improve separation of concerns

7. **Performance Optimizations**
   - Identify and eliminate performance bottlenecks
   - Optimize algorithms and data structures
   - Reduce unnecessary computations
   - Improve memory usage patterns

8. **Design Pattern Application**
   - Apply appropriate design patterns where beneficial
   - Improve abstraction and encapsulation
   - Enhance modularity and reusability
   - Reduce coupling between components

9. **Error Handling Improvement**
   - Standardize error handling approaches
   - Improve error messages and logging
   - Add proper exception handling
   - Enhance resilience and fault tolerance

10. **Documentation Updates**
    - Update code comments to reflect changes
    - Revise API documentation if interfaces changed
    - Update inline documentation and examples
    - Ensure comments are accurate and helpful

11. **Testing Enhancements**
    - Add tests for any new code paths created
    - Improve existing test quality and coverage
    - Remove or update obsolete tests
    - Ensure tests are still meaningful and effective

12. **Static Analysis**
    - Run linting tools to catch style and potential issues
    - Use static analysis tools to identify problems
    - Check for security vulnerabilities
    - Verify code complexity metrics

13. **Performance Verification**
    - Run performance benchmarks if applicable
    - Compare before/after metrics
    - Ensure refactoring didn't degrade performance
    - Document any performance improvements

14. **Integration Testing**
    - Run full test suite to ensure no regressions
    - Test integration with dependent systems
    - Verify all functionality works as expected
    - Test edge cases and error scenarios

15. **Code Review Preparation**
    - Review all changes for quality and consistency
    - Ensure refactoring goals were achieved
    - Prepare clear explanation of changes made
    - Document benefits and rationale

16. **Documentation of Changes**
    - Create a summary of refactoring changes
    - Document any breaking changes or new patterns
    - Update project documentation if needed
    - Explain benefits and reasoning for future reference

17. **Deployment Considerations**
    - Plan deployment strategy for refactored code
    - Consider feature flags for gradual rollout
    - Prepare rollback procedures
    - Set up monitoring for the refactored components

重要: リファクタリングは内部構造を改善しながら外部動作を保持する必要がある。常に速度よりも安全性を優先し、プロセス全体を通じて包括的なテストカバレッジを維持する。