# プロジェクト初期化コマンド

業界のベストプラクティス、適切なツール設定、フレームワーク固有の設定を用いて新しいプロジェクトを初期化します。

## 実行手順

1. **プロジェクト分析とセットアップ**
   - 引数からプロジェクトタイプとフレームワークを解析: `$ARGUMENTS`
   - 引数が提供されていない場合、現在のディレクトリを分析し、ユーザーにプロジェクトタイプとフレームワークを確認
   - 必要に応じてプロジェクトディレクトリ構造を作成
   - 選択されたフレームワークがプロジェクトタイプに適切であることを検証

2. **基本プロジェクト構造**
   - 必須ディレクトリの作成 (src/, tests/, docs/, etc.)
   - プロジェクトタイプに適した.gitignoreでGitリポジトリを初期化
   - プロジェクト説明とセットアップ手順を含むREADME.mdを作成
   - プロジェクトタイプとフレームワークに基づく適切なファイル構造をセットアップ

3. **フレームワーク固有の設定**
   - **Web/React**: TypeScript、Vite/Next.js、ESLint、PrettierでReactをセットアップ
   - **Web/Vue**: TypeScript、Vite、ESLint、PrettierでVue 3を設定
   - **Web/Angular**: TypeScriptとテスト環境でAngular CLIプロジェクトをセットアップ
   - **API/Express**: TypeScript、ミドルウェア、ルーティングでExpress.jsサーバーを作成
   - **API/FastAPI**: Python、Pydanticモデル、非同期サポートでFastAPIをセットアップ
   - **Mobile/React Native**: ナビゲーションと開発ツールでReact Nativeを設定
   - **Desktop/Electron**: レンダラーとメインプロセス構造でElectronをセットアップ
   - **CLI/Node**: commander.jsと適切なパッケージングでNode.js CLIを作成
   - **Library/NPM**: TypeScript、rollup/webpack、公開設定でライブラリをセットアップ

4. **開発環境セットアップ**
   - 適切なpackage.jsonでパッケージマネージャー (npm, yarn, pnpm) を設定
   - strictモードとパスマッピングでTypeScript設定をセットアップ
   - ESLintと言語固有のルールでlintingを設定
   - Prettierとpre-commitフックでコードフォーマッティングをセットアップ
   - 一貫したコーディング基準のためのEditorConfigを追加

5. **テストインフラストラクチャ**
   - テストフレームワーク (Jest, Vitest, Pytest, etc.) のインストールと設定
   - テストディレクトリ構造とサンプルテストのセットアップ
   - コードカバレッジレポートの設定
   - package.json/makefileにテストスクリプトを追加

6. **ビルドと開発ツール**
   - ビルドシステム (Vite, webpack, rollup, etc.) の設定
   - ホットリロード付き開発サーバーのセットアップ
   - 環境変数管理の設定
   - ビルド最適化とバンドリングの追加

7. **CI/CDパイプライン**
   - テストとデプロイメント用のGitHub Actionsワークフローを作成
   - Pull Request時の自動テストをセットアップ
   - Dependabotで自動依存関係更新を設定
   - READMEにステータスバッジを追加

8. **ドキュメントと品質**
   - インストールと使用方法を含む包括的なREADMEを生成
   - 開発ガイドラインを含むCONTRIBUTING.mdを作成
   - API文書生成をセットアップ (JSDoc, Sphinx, etc.)
   - コード品質バッジとシールドを追加

9. **セキュリティとベストプラクティス**
   - npm audit等でセキュリティスキャンを設定
   - 依存関係の脆弱性チェックをセットアップ
   - Webアプリケーション用のセキュリティヘッダーを追加
   - 環境固有のセキュリティ設定を構成

10. **プロジェクト検証**
    - すべての依存関係が正しくインストールされることを確認
    - 初期ビルドを実行して設定が動作することを確認
    - テストスイートを実行してテストセットアップを検証
    - lintingとフォーマッティングルールが適用されることを確認
    - 開発サーバーが正常に起動することを検証
    - 適切なプロジェクト構造で初回commitを作成