# 依存関係監査コマンド

セキュリティ、パフォーマンス、メンテナンスの観点から、あらゆるコードベースのすべての依存関係を分析・監査します。

## 実行手順

以下の手順に従って包括的な依存関係監査を実行してください：

1. **依存関係の発見**
   - すべての依存関係管理ファイルの特定（package.json、requirements.txt、Cargo.toml、pom.xml等）
   - 直接依存関係対推移的依存関係のマッピング
   - ロックファイルとバージョン整合性のチェック
   - 開発用対本番用依存関係のレビュー

2. **バージョン分析**
   - 古いパッケージと利用可能な更新のチェック
   - メジャーバージョン更新が利用可能なパッケージの特定
   - セマンティックバージョニングコンプライアンスのレビュー
   - バージョン固定戦略の分析

3. **セキュリティ脆弱性スキャン**
   - 適切なツールを使用したセキュリティ監査の実行：
     - Node.jsプロジェクト用`npm audit`
     - Pythonプロジェクト用`pip-audit`
     - Rustプロジェクト用`cargo audit`
     - すべてのプラットフォーム用GitHubセキュリティアドバイザリ
   - 重大、高、中、低重要度の脆弱性の特定
   - 既知の攻撃手法とCVE参照のチェック

4. **ライセンスコンプライアンス**
   - 互換性のためのすべての依存関係ライセンスのレビュー
   - 制限的なライセンス（GPL、AGPL等）の特定
   - プロジェクトライセンスとのライセンス競合のチェック
   - ライセンス義務と要件の文書化

5. **依存関係の健全性評価**
   - パッケージのメンテナンス状態と活動のチェック
   - コントリビューター数とコミュニティサポートのレビュー
   - リリース頻度と安定性の分析
   - 放棄されたまたは非推奨のパッケージの特定

6. **サイズとパフォーマンス影響**
   - 各依存関係のバンドルサイズ影響の分析
   - 最適化可能な大きな依存関係の特定
   - 依存関係間の重複機能のチェック
   - tree-shakingとデッドコード除去の効果のレビュー

7. **代替案分析**
   - より良い代替案を持つ依存関係の特定
   - より軽量または効率的な置き換えのチェック
   - 機能重複と統合機会の分析
   - ネイティブ代替案のレビュー（組み込み関数対ライブラリ）

8. **依存関係の競合**
   - 依存関係間のバージョン競合のチェック
   - ピア依存関係問題の特定
   - 依存関係解決戦略のレビュー
   - 更新時の潜在的な破壊的変更の分析

9. **ビルドと開発影響**
   - ビルド時間に影響する依存関係のレビュー
   - 本番環境における開発専用依存関係のチェック
   - ツール依存関係と代替案の分析
   - オプション依存関係とその必要性のレビュー

10. **サプライチェーンセキュリティ**
    - タイポスクワッティングと悪意のあるパッケージのチェック
    - パッケージの真正性と署名のレビュー
    - 依存関係のソースとレジストリの分析
    - 疑わしいまたは異常な依存関係のチェック

11. **更新戦略計画**
    - セキュリティと安定性に基づいた優先順位付き更新計画の作成
    - 破壊的変更と必要なコード修正の特定
    - 更新時のテスト戦略の計画
    - 問題のある更新のロールバック手順の文書化

12. **監視と自動化**
    - 自動依存関係スキャンの設定
    - セキュリティアラートと通知の構成
    - 依存関係更新自動化ツールのレビュー
    - 定期監査スケジュールの確立

13. **文書化とレポート**
    - 包括的な依存関係インベントリの作成
    - 修正手順を含むすべてのセキュリティ発見事項の文書化
    - 優先度レベル付き更新推奨事項の提供
    - ステークホルダー向けエグゼクティブサマリーの生成

最も正確な結果を得るためにプラットフォーム固有のツールとデータベースを使用してください。明確なリスク評価と実行可能な推奨事項に焦点を当ててください。