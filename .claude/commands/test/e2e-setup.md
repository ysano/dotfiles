# エンドツーエンドテストセットアップコマンド

任意のWebアプリケーションやサービス用の包括的なエンドツーエンドテストインフラストラクチャを設定します。

## 手順

E2Eテストを実装するための体系的なアプローチに従ってください： **$ARGUMENTS**

1. **技術スタック評価**
   - アプリケーションタイプ（Webアプリ、モバイルアプリ、APIサービス）を特定
   - 既存のテストインフラストラクチャを確認
   - 対象ブラウザとデバイスを決定
   - 現在のデプロイメントとステージング環境を評価

2. **E2Eフレームワーク選択**
   - スタックに基づいて適切なE2Eテストフレームワークを選択：
     - **Playwright**: モダンで高速、複数ブラウザサポート
     - **Cypress**: 開発者フレンドリー、優れたデバッグツール
     - **Selenium WebDriver**: クロスブラウザ、成熟したエコシステム
     - **Puppeteer**: Chrome特化、パフォーマンステストに適している
     - **TestCafe**: WebDriver不要、簡単セットアップ
   - チームの専門知識とプロジェクト要件を考慮

3. **テスト環境セットアップ**
   - 専用のテスト環境（ステージング、QA）を設定
   - サンプルデータを含むテストデータベースの設定
   - 環境変数と設定の構成
   - 環境の分離と再現性の確保

4. **フレームワークのインストールと設定**
   
   **Playwright用:**
   ```bash
   npm install -D @playwright/test
   npx playwright install
   npx playwright codegen # テストの記録
   ```

   **Cypress用:**
   ```bash
   npm install -D cypress
   npx cypress open
   ```

   **Selenium用:**
   ```bash
   npm install -D selenium-webdriver
   # ブラウザドライバのインストール
   ```

5. **テスト構造の整理**
   - 論理的なテストフォルダ構造の作成:
     ```
     e2e/
     ├── tests/
     │   ├── auth/
     │   ├── user-flows/
     │   └── api/
     ├── fixtures/
     ├── support/
     │   ├── commands/
     │   └── page-objects/
     └── config/
     ```
   - 機能またはユーザージャーニーごとにテストを整理
   - APIテストとUIテストを分離

6. **ページオブジェクトモデルの実装**
   - 保守性向上のためのページオブジェクトクラスの作成
   - 要素セレクターとインタラクションのカプセル化
   - 共通アクションのための再利用可能なメソッドの実装
   - ページオブジェクトの単一責任原則の遵守

   **ページオブジェクトの例:**
   ```javascript
   class LoginPage {
     constructor(page) {
       this.page = page;
       this.emailInput = page.locator('#email');
       this.passwordInput = page.locator('#password');
       this.loginButton = page.locator('#login-btn');
     }

     async login(email, password) {
       await this.emailInput.fill(email);
       await this.passwordInput.fill(password);
       await this.loginButton.click();
     }
   }
   ```

7. **テストデータ管理**
   - テストフィクスチャとサンプルデータの作成
   - 動的テストデータのためのデータファクトリの実装
   - 一貫したテスト状態のためのデータベースシーディングの設定
   - 環境固有のテストデータの使用
   - テストデータクリーンアップ戦略の実装

8. **コアユーザージャーニーテスト**
   - 重要なユーザーフローの実装:
     - ユーザー登録と認証
     - メインアプリケーションワークフロー
     - 支払いと取引フロー
     - 検索とフィルタリング機能
     - フォーム送信と検証

9. **クロスブラウザテストセットアップ**
   - 複数ブラウザでのテスト設定
   - ブラウザ固有の設定
   - レスポンシブデザインテストの実装
   - 異なるビューポートサイズでのテスト

   **Playwrightブラウザ設定:**
   ```javascript
   module.exports = {
     projects: [
       { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
       { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
       { name: 'webkit', use: { ...devices['Desktop Safari'] } },
       { name: 'mobile', use: { ...devices['iPhone 12'] } },
     ],
   };
   ```

10. **APIテスト統合**
    - UIテストと併行したAPIエンドポイントのテスト
    - APIリクエスト/レスポンス検証の実装
    - 認証と認可のテスト
    - APIとUI間のデータ整合性の検証

11. **ビジュアルテストセットアップ**
    - スクリーンショット比較テストの実装
    - ビジュアルリグレッションテストの設定
    - ビジュアル変更の許容レベルの設定
    - ビジュアルベースラインと更新の整理

12. **テストユーティリティとヘルパー**
    - カスタムコマンドとユーティリティの作成
    - 共通アサーションヘルパーの実装
    - 認証ヘルパーの設定
    - データベースと状態管理ユーティリティの作成

13. **エラーハンドリングとデバッグ**
    - 適切なエラーレポートとスクリーンショットの設定
    - 失敗したテストの動画記録の設定
    - 不安定なテストの再試行メカニズムの実装
    - デバッグツールとヘルパーの作成

14. **CI/CD統合**
    - CI/CDパイプラインでのE2Eテストの設定
    - 並列テスト実行の設定
    - 適切なテストレポートの実装
    - テスト環境プロビジョニングの設定

   **GitHub Actionsの例:**
   ```yaml
   - name: Run Playwright tests
     run: npx playwright test
   - uses: actions/upload-artifact@v3
     if: always()
     with:
       name: playwright-report
       path: playwright-report/
   ```

15. **パフォーマンステスト統合**
    - E2Eテストへのパフォーマンスアサーションの追加
    - ページロード時間とメトリクスの監視
    - 異なるネットワーク条件下でのテスト
    - Lighthouse監査統合の実装

16. **アクセシビリティテスト**
    - アクセシビリティテストツール（axe-core）の統合
    - キーボードナビゲーションフローのテスト
    - スクリーンリーダー互換性の検証
    - 色コントラストとWCAG準拠のチェック

17. **モバイルテストセットアップ**
    - モバイルデバイスエミュレーションの設定
    - レスポンシブデザインブレークポイントのテスト
    - タッチジェスチャーテストの実装
    - モバイル固有機能のテスト

18. **レポートと監視**
    - 包括的なテストレポートの設定
    - テスト結果通知の設定
    - テストメトリクスと分析の実装
    - テストヘルス監視のダッシュボード作成

19. **テストメンテナンス戦略**
    - テスト安定性監視の実装
    - UI変更に対する自動テスト更新の設定
    - テストレビューと更新プロセスの作成
    - テストメンテナンス手順の文書化

20. **セキュリティテスト統合**
    - 認証と認可フローのテスト
    - セキュリティヘッダー検証の実装
    - 入力サニタイゼーションとXSS防止のテスト
    - HTTPSとセキュアクッキー処理の検証

**E2Eテストのサンプル:**
```javascript
test('ユーザーが購入フローを完了できること', async ({ page }) => {
  // ナビゲーションとログイン
  await page.goto('/login');
  await page.fill('#email', 'test@example.com');
  await page.fill('#password', 'password');
  await page.click('#login-btn');

  // カートに商品を追加
  await page.goto('/products');
  await page.click('[data-testid="product-1"]');
  await page.click('#add-to-cart');

  // チェックアウト完了
  await page.goto('/checkout');
  await page.fill('#card-number', '4111111111111111');
  await page.click('#place-order');

  // 成功を確認
  await expect(page.locator('#order-confirmation')).toBeVisible();
});
```

重要なユーザージャーニーから始めて、徐々にカバレッジを拡張することを忘れないでください。実際の価値を提供する安定した保守可能なテストに焦点を当てましょう。