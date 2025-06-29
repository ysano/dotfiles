# ビルド最適化コマンド

プロジェクトのビルド時間、出力サイズ、効率性を改善するためにビルドプロセスを分析・最適化します。

## 実行手順

以下の体系的なアプローチでビルドパフォーマンスを最適化: **$ARGUMENTS**

1. **ビルドシステム分析**
   - 使用中のビルドシステムを特定（Webpack、Vite、Rollup、Gradle、Maven、Cargoなど）
   - ビルド設定ファイルと設定の確認
   - 現在のビルド時間と出力サイズの分析
   - 完全なビルドパイプラインと依存関係のマッピング

2. **パフォーマンスベースライン**
   - 異なるシナリオでの現在のビルド時間を測定:
     - クリーンビルド（最初から）
     - インクリメンタルビルド（キャッシュ付き）
     - 開発環境対本番環境ビルド
   - バンドルサイズとアセットサイズの文書化
   - ビルドプロセスの最も遅い部分の特定

3. **依存関係最適化**
   - ビルド依存関係とその影響の分析
   - ビルドプロセスから未使用の依存関係を除去
   - ビルドツールを最新安定版に更新
   - より高速な代替ビルドツールの検討

4. **キャッシュ戦略**
   - ビルドキャッシュの有効化と最適化
   - CI/CD用の永続キャッシュの設定
   - チーム開発用の共有キャッシュの設定
   - 可能な場合のインクリメンタルコンパイルの実装

5. **Bundle Analysis**
   - Analyze bundle composition and sizes
   - Identify large dependencies and duplicates
   - Use bundle analyzers specific to your build tool
   - Look for opportunities to split bundles

6. **Code Splitting and Lazy Loading**
   - Implement dynamic imports and code splitting
   - Set up route-based splitting for SPAs
   - Configure vendor chunk separation
   - Optimize chunk sizes and loading strategies

7. **Asset Optimization**
   - Optimize images (compression, format conversion, lazy loading)
   - Minify CSS and JavaScript
   - Configure tree shaking to remove dead code
   - Implement asset compression (gzip, brotli)

8. **Development Build Optimization**
   - Enable fast refresh/hot reloading
   - Use development-specific optimizations
   - Configure source maps for better debugging
   - Optimize development server settings

9. **Production Build Optimization**
   - Enable all production optimizations
   - Configure dead code elimination
   - Set up proper minification and compression
   - Optimize for smaller bundle sizes

10. **Parallel Processing**
    - Enable parallel processing where supported
    - Configure worker threads for build tasks
    - Optimize for multi-core systems
    - Use parallel compilation for TypeScript/Babel

11. **File System Optimization**
    - Optimize file watching and polling
    - Configure proper include/exclude patterns
    - Use efficient file loaders and processors
    - Minimize file I/O operations

12. **CI/CD Build Optimization**
    - Optimize CI build environments and resources
    - Implement proper caching strategies for CI
    - Use build matrices efficiently
    - Configure parallel CI jobs where beneficial

13. **Memory Usage Optimization**
    - Monitor and optimize memory usage during builds
    - Configure heap sizes for build tools
    - Identify and fix memory leaks in build process
    - Use memory-efficient compilation options

14. **Output Optimization**
    - Configure compression and encoding
    - Optimize file naming and hashing strategies
    - Set up proper asset manifests
    - Implement efficient asset serving

15. **Monitoring and Profiling**
    - Set up build time monitoring
    - Use build profiling tools to identify bottlenecks
    - Track bundle size changes over time
    - Monitor build performance regressions

16. **Tool-Specific Optimizations**
    
    **For Webpack:**
    - Configure optimization.splitChunks
    - Use thread-loader for parallel processing
    - Enable optimization.usedExports for tree shaking
    - Configure resolve.modules and resolve.extensions

    **For Vite:**
    - Configure build.rollupOptions
    - Use esbuild for faster transpilation
    - Optimize dependency pre-bundling
    - Configure build.chunkSizeWarningLimit

    **For TypeScript:**
    - Use incremental compilation
    - Configure project references
    - Optimize tsconfig.json settings
    - Use skipLibCheck when appropriate

17. **Environment-Specific Configuration**
    - Separate development and production configurations
    - Use environment variables for build optimization
    - Configure feature flags for conditional builds
    - Optimize for target environments

18. **Testing Build Optimizations**
    - Test build outputs for correctness
    - Verify all optimizations work in target environments
    - Check for any breaking changes from optimizations
    - Measure and document performance improvements

19. **Documentation and Maintenance**
    - Document all optimization changes and their impact
    - Create build performance monitoring dashboard
    - Set up alerts for build performance regressions
    - Regular review and updates of build configuration

特定のプロジェクトとチームワークフローに最大の影響をもたらす最適化に焦点を当てる。改善を定量化するため、常に前後を測定する。