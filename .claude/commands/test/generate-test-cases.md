# テストケース生成

単体テスト、エッジケース、統合シナリオを含む既存コード向けの包括的なテストケースを自動生成します。

## 実行手順

1. **ターゲット分析とスコープ定義**
   - 引数からターゲットファイルまたは関数を解析: `$ARGUMENTS`
   - ターゲットが指定されていない場合、現在のディレクトリを分析し、特定のターゲットを促す
   - ターゲットコードの構造、依存関係、複雑度の検査
   - 関数シグネチャ、パラメーター、戻り値の型、副作用の特定
   - テストスコープの決定（単体、統合、または両方）

2. **コード構造分析**
   - 関数ロジック、分岐、制御フローの分析
   - 入力検証、エラーハンドリング、エッジケースの特定
   - 外部依存関係、API呼び出し、データベースインタラクションの検査
   - データ変換とビジネスロジックのレビュー
   - 非同期操作とエラーシナリオの特定

3. **テストケース生成戦略**
   - 正常操作フローのポジティブテストケースを生成
   - エラー状態と無効な入力のネガティブテストケースを作成
   - 境界条件と制限のエッジケースを生成
   - 外部依存関係の統合テストケースを作成
   - 複雑な操作のパフォーマンステストケースを生成

4. **単体テスト実装**
   - プロジェクトの命名規則に従ってテストファイルを作成
   - テストフレームワークのインポートと設定をセットアップ
   - 機能別に整理されたテストスイートを生成
   - 説明的な名前を持つ包括的なテストケースを作成
   - 各テストに対して適切なセットアップとティアダウンを実装

5. **モックとスタブの生成**
   - モックが必要な外部依存関係の特定
   - APIやサービスのモック実装を生成
   - データベースやファイルシステム操作のスタブデータを作成
   - 関数呼び出しを監視するスパイ関数をセットアップ
   - モックの戻り値とエラーシナリオを設定

6. **データ駆動テスト生成**
   - 様々な入力シナリオのテストデータセットを作成
   - 複数の入力組み合わせのパラメータ化テストを生成
   - 複雑なデータ構造のフィクスチャを作成
   - 一負したデータ生成のためのテストデータファクトリをセットアップ
   - 包括的なカバレッジのためのプロパティベーステストケースを生成

7. **統合テストシナリオ**
   - コンポーネント間の相互作用テストを生成
   - エンドツーエンドワークフローテストケースを作成
   - API統合テストシナリオを生成
   - 実データを使用したデータベース統合テストを作成
   - モジュール間統合テストケースを生成

8. **エラーハンドリングと例外テスト**
   - すべてのエラー状態と例外のテストを生成
   - タイムアウトとネットワーク障害シナリオのテストを作成
   - 無効な入力検証のテストを生成
   - リソース枯渇と制限のテストを作成
   - 同時アクセスと競合状態のテストを生成

9. **テスト品質とカバレッジ**
   - ターゲット関数の包括的なコードカバレッジを確保
   - すべてのコードブランチとパスのテストを生成
   - 成功と失敗両方のシナリオのテストを作成
   - テストアサーションが意味のある具体的なものであることを検証
   - テストが分離されており独立していることを確保

10. **テストドキュメンテーションとメンテナンス**
    - 明確なテスト説明とドキュメンテーションを生成
    - 複雑なテストシナリオを説明するコメントを作成
    - テストデータ要件とセットアップ手順を文書化
    - テストメンテナンスガイドラインとベストプラクティスを生成
    - テスト実行とデバッグの手順を作成
    - 生成されたテストが正常に実行され、意味のあるフィードバックを提供することを検証