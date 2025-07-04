# エラーデバッグコマンド

構造化されたアプローチを用いて、あらゆるコードベースのエラーを体系的にデバッグし、解決します。

## 実行手順

以下の包括的なデバッグ手法に従って解決してください：**$ARGUMENTS**

1. **エラー情報収集**
   - 完全なエラーメッセージ、スタックトレース、エラーコードを収集する
   - エラーが発生するタイミング、条件、頻度を記録する
   - エラーが発生する環境を特定する（dev、staging、prod）
   - エラーの前後の関連ログを収集する

2. **エラーの再現**
   - エラーを一貫して再現する最小限のテストケースを作成する
   - エラーをトリガーするための正確な手順を文書化する
   - 可能であれば異なる環境でテストする
   - エラー発生に影響するパターンや条件を記録する

3. **スタックトレース分析**
   - スタックトレースを下から上に読んでコールチェーンを理解する
   - エラーが発生した正確な行を特定する
   - エラーに至る実行パスをトレースする
   - 失敗したコードの明白な問題を探す

4. **コードコンテキスト調査**
   - エラー発生箇所周辺のコードを調査する
   - バグを引き起こした可能性のある最近の変更を確認する
   - エラー発生時の変数値と状態をレビューする
   - 関数パラメータと戻り値を分析する

5. **仮説の形成**
   - 証拠に基づいて根本原因に関する仮説を形成する
   - 一般的な原因を考慮する:
     - Nullポインタ/未定義参照
     - タイプの不一致
     - 競合状態
     - リソースの果尽
     - ロジックエラー
     - 外部依存の失敗

6. **デバッグツールのセットアップ**
   - 技術スタックに適切なデバッグツールをセットアップする
   - 必要に応じてdebugger、profiler、ログ出力を使用する
   - 戦略的な位置にbreakpointを設定する
   - まだ存在しない場合は監視とアラートを設定する

7. **体系的調査**
   - 各仮説を方法論的にテストする
   - バイナリサーチアプローチで問題を分離する
   - 戦略的なログ出力やprint文を追加する
   - データフローと変換をステップバイステップで確認する

8. **データ検証**
   - 入力データの形式と有効性を検証する
   - エッジケースと境界条件を確認する
   - データ状態に関する仮定を検証する
   - 異なるデータセットでテストしてパターンを分離する

9. **依存関係分析**
   - 外部依存とそのバージョンを確認する
   - ネットワーク接続とAPIの可用性を検証する
   - 設定ファイルと環境変数をレビューする
   - データベース接続とクエリ実行をテストする

10. **メモリとリソース分析**
    - メモリリークや過度なメモリ使用を確認する
    - CPUと1/Oリソース消費を監視する
    - 該当する場合はガベージコレクションパターンを分析する
    - リソースのデッドロックや競合を確認する

11. **並行性問題の調査**
    - マルチスレッドコードの競合状態を探す
    - 同期化メカニズムとロックを確認する
    - 非同期操作とPromise処理を分析する
    - 異なる負荷条件下でテストする

12. **根本原因の特定**
    - 原因が特定されたら、なぜそれが起こったのかを理解する
    - ロジックエラー、設計の欠陥、外部問題のいずれに該当するか判断する
    - 問題の範囲と影響を評価する
    - 他の箱所に類似の問題が存在するか検討する

13. **解決策の実装**
    - 根本原因に対処する修正を設計する
    - 複数の解決アプローチとトレードオフを検討する
    - 適切なエラー処理を含む修正を実装する
    - 必要に応じてバリデーションと防御的プログラミングを追加する

14. **修正のテスト**
    - 元のエラーケースに対して修正をテストする
    - エッジケースと関連シナリオをテストする
    - 新しい問題が無いことを確認するためのリグレッションテストを実行する
    - 様々な負荷とストレス条件下でテストする

15. **予防策**
    - 適切なunit testとintegration testを追加する
    - エラー処理とログ出力を改善する
    - 入力検証と防御的チェックを追加する
    - ドキュメンテーションとコードコメントを更新する

16. **監視とアラート**
    - 類似問題の監視を設定する
    - メトリクスとヘルスチェックを追加する
    - エラー闾値のアラートを設定する
    - より優れた可観性を実装する

17. **ドキュメンテーション**
    - エラー、調査プロセス、解決策を文書化する
    - トラブルシューティングガイドを更新する
    - 学びをチームと共有する
    - コンテキストを含むコードコメントを更新する

18. **解決後のレビュー**
    - エラーが早期に発見されなかった理由を分析する
    - 開発とテストプロセスをレビューする
    - 類似問題を防ぐための改善策を検討する
    - 必要に応じてコーディング標準やガイドラインを更新する

デバッグプロセス全体を通じて詳細なメモを維持し、エラーと修正の両方のより幅広い影響を考慮することを必ず実行してください。