# バンドルサイズ最適化手順

バンドルサイズの削減と読み込み速度の改善を以下の 10 ステップで実施する。

## 1. バンドル分析・評価

- webpack-bundle-analyzer 等で現在のバンドル構成を可視化
- 大きな依存関係と未使用コードを特定
- ビルド設定と最適化設定を評価
- ベースラインの計測値を記録 (サイズ、ロード時間)

## 2. ビルドツール設定

ビルドプロセス全体の最適化は `build-optimization.md` を参照。

- **Webpack**: splitChunks, minimizer (TerserPlugin, CssMinimizerPlugin)
- **Vite**: build.rollupOptions, manualChunks, chunkSizeWarningLimit
- 本番モードの最適化が有効であることを確認

## 3. コード分割・遅延読み込み

- ルートベースのコード分割を実装 (React.lazy, dynamic import)
- 重いコンポーネントを遅延読み込み
- 初期ロードに不要なモジュールを分離
- Suspense / fallback UI を設定

## 4. ツリーシェイキング・デッドコード除去

- package.json に `"sideEffects": false` を設定
- 名前付きインポートを使用 (`import { map } from 'lodash-es'`)
- barrel exports (index.ts) の影響を確認
- ES Modules 形式のパッケージを優先

## 5. 依存関係の最適化

- 各依存関係のバンドルサイズ影響を分析 (bundlephobia.com)
- 大きなライブラリの軽量代替を検討 (moment→dayjs, lodash→lodash-es)
- 機能重複する依存関係を統合
- ネイティブ API で代替可能なライブラリを除去

## 6. アセット最適化

- 画像: WebP/AVIF 変換、レスポンシブサイズ、遅延読み込み
- フォント: サブセット化、font-display: swap、WOFF2 形式
- SVG: インライン化 or スプライト化
- CSS: 未使用 CSS の除去 (PurgeCSS)

## 7. Module Federation (大規模アプリ)

- マイクロフロントエンド構成で共有依存関係を最適化
- リモートモジュールの動的読み込みを設定
- 共有スコープで重複を防止

## 8. プログレッシブ読み込み

- Resource Hints: preload (重要リソース), prefetch (次ページ), preconnect (外部ドメイン)
- Intersection Observer による遅延読み込み
- Critical CSS のインライン化
- 非同期スクリプト読み込み (async/defer)

## 9. 監視・計測

- CI/CD にバンドルサイズチェックを統合
- パフォーマンスバジェットを設定 (例: JS < 200KB gzip)
- PR ごとのサイズ変更レポートを自動生成
- 閾値超過時のビルド失敗を設定

## 10. 検証

- ビルド出力の正確性をテスト
- 全最適化がターゲット環境で動作することを検証
- 最適化による破壊的変更がないことを確認
- パフォーマンス改善を計測・文書化

See also: ビルドプロセスは `build-optimization.md`、CDN 配信は `cdn-optimization.md` を参照。
