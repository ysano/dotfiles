# Enhanced Claude Code Status Detection System

このシステムは、tmux環境でのClaude Codeステータス検出の精度を大幅に向上させる高度な検出フレームワークです。

## 主な改善点

### 🎯 精度向上
- **3層検出システム**: UI要素、コンテキスト分析、パターンマッチングの組み合わせ
- **信頼度スコアリング**: 各検出結果に信頼度を付与
- **詳細な状態定義**: 3状態から8状態への拡張

### ⚡ パフォーマンス最適化
- **早期終了**: 不要な処理をスキップ
- **効率的なパターンマッチング**: 正規表現の最適化
- **キャッシュフレンドリー**: メモリ使用量の最適化

### 🔧 運用性向上
- **段階的移行**: Blue-Green deploymentによる安全な切り替え
- **A/Bテスト**: 新旧システムの並行比較
- **自動ロールバック**: エラー発生時の自動復旧

## ファイル構成

```
~/.tmux/
├── scripts/
│   ├── claude-status.sh              # 既存のレガシー検出
│   ├── claude-status-enhanced.sh     # 新しい高精度検出
│   ├── claude-debug.sh               # デバッグ・監視ツール
│   └── claude-migration.sh           # 移行管理ツール
├── tests/
│   ├── unit/
│   │   └── test-enhanced-detection.sh # 単体テストスイート
│   └── fixtures/                     # テスト用データ
├── debug/                            # デバッグ出力
└── README-enhanced-detection.md      # このファイル
```

## 状態定義の拡張

| 状態 | アイコン | 説明 | 検出条件 |
|------|----------|------|----------|
| `idle` | ✅ | アイドル状態 | プロンプト表示中 |
| `thinking` | 🧠 | 思考中 | "Thinking..."等の表示 |
| `processing` | ⚡ | 処理実行中 | "Esc to interrupt"等 |
| `waiting_input` | 📝 | 入力待ち | "Enter your..."等 |
| `waiting_confirmation` | ⌛ | 確認待ち | "Do you want..."等 |
| `error_state` | ❌ | エラー状態 | エラーメッセージ表示 |
| `config_mode` | ⚙️ | 設定モード | "auto-accept"等 |
| `tool_execution` | 🔧 | ツール実行中 | "Running command..."等 |

## 使用方法

### 1. 基本的な状態確認

```bash
# 現在のウィンドウの状態を確認
~/.tmux/scripts/claude-status-enhanced.sh

# 特定のウィンドウ・ペインの状態を確認
~/.tmux/scripts/claude-status-enhanced.sh 1 0
```

### 2. デバッグモードでの詳細分析

```bash
# デバッグ出力を有効にして実行
CLAUDE_STATUS_DEBUG=1 ~/.tmux/scripts/claude-status-enhanced.sh

# 対話式デバッグセッション
~/.tmux/scripts/claude-debug.sh debug

# 現在の状態の詳細分析
~/.tmux/scripts/claude-debug.sh debug 1 0
```

### 3. リアルタイム監視

```bash
# 60秒間のリアルタイム監視
~/.tmux/scripts/claude-debug.sh monitor

# 特定ウィンドウを5分間監視
~/.tmux/scripts/claude-debug.sh monitor 1 300
```

### 4. パフォーマンス分析

```bash
# 100回の検出テストを実行
~/.tmux/scripts/claude-debug.sh perf 100

# 詳細なパフォーマンス統計
~/.tmux/scripts/claude-debug.sh perf 500 1
```

### 5. 新旧システムの比較

```bash
# 20回の比較テスト
~/.tmux/scripts/claude-debug.sh compare 1 20

# 移行管理ツールでA/Bテスト
~/.tmux/scripts/claude-migration.sh test 600 1
```

## 段階的移行プロセス

### Phase 1: 検証とテスト

```bash
# 1. 現在の状況確認
~/.tmux/scripts/claude-migration.sh status

# 2. システムヘルスチェック
~/.tmux/scripts/claude-debug.sh debug

# 3. パフォーマンステスト
~/.tmux/scripts/claude-debug.sh perf 100
```

### Phase 2: ハイブリッドモード（A/Bテスト）

```bash
# ハイブリッドモード有効化
~/.tmux/scripts/claude-migration.sh enable-hybrid

# 10分間のA/Bテスト実行
~/.tmux/scripts/claude-migration.sh test 600

# リアルタイム比較監視
~/.tmux/scripts/claude-debug.sh compare 1 50
```

### Phase 3: 本格移行

```bash
# 拡張検出システムへの切り替え
~/.tmux/scripts/claude-migration.sh enable-enhanced

# エラー監視の開始（自動ロールバック有効）
~/.tmux/scripts/claude-migration.sh monitor
```

### Phase 4: 問題時のロールバック

```bash
# 手動ロールバック
~/.tmux/scripts/claude-migration.sh rollback "Performance issues"

# 状況確認
~/.tmux/scripts/claude-migration.sh status
```

## テストスイートの実行

### 単体テストの実行

```bash
# 全テストの実行
~/.tmux/tests/unit/test-enhanced-detection.sh

# 特定のテストカテゴリー
CLAUDE_STATUS_DEBUG=1 ~/.tmux/tests/unit/test-enhanced-detection.sh

# テスト結果の確認
echo $?  # 0: 全テスト成功, 1: 一部失敗
```

### カスタムテストデータの作成

```bash
# 現在の画面をテスト用フィクスチャとして保存
tmux capture-pane -p -S -30 > ~/.tmux/tests/fixtures/custom_state.txt

# フィクスチャを使った検出テスト
# (テストスクリプトを修正して新しいフィクスチャを使用)
```

## トラブルシューティング

### よくある問題と解決方法

#### 1. 検出精度が低い

```bash
# デバッグモードで詳細分析
CLAUDE_STATUS_DEBUG=1 ~/.tmux/scripts/claude-status-enhanced.sh

# 現在の出力パターンを確認
tmux capture-pane -p -S -30

# パターンマッチングの調整が必要な場合は、
# claude-status-enhanced.sh内の正規表現を修正
```

#### 2. パフォーマンスが悪い

```bash
# パフォーマンス分析
~/.tmux/scripts/claude-debug.sh perf 100

# 100ms以上かかる場合は設定を確認
# TMUX_CAPTURE_LINES=30 → 20に減らす等
```

#### 3. 誤検出が多い

```bash
# 比較テストで問題箇所を特定
~/.tmux/scripts/claude-debug.sh compare 1 50

# 特定パターンでの誤検出を修正
# enhanced_pattern_matching()関数を調整
```

#### 4. システムが応答しない

```bash
# 緊急ロールバック
~/.tmux/scripts/claude-migration.sh rollback "Emergency rollback"

# システムヘルスチェック
~/.tmux/scripts/claude-debug.sh debug
```

## 設定のカスタマイズ

### 環境変数による調整

```bash
# デバッグ出力の有効化
export CLAUDE_STATUS_DEBUG=1

# 検出の詳細度調整
export TMUX_CAPTURE_LINES=50      # キャプチャ行数 (デフォルト: 30)
export TMUX_LOG_LEVEL=DEBUG       # ログレベル

# パフォーマンス調整
export TMUX_STATUS_INTERVAL=3     # 更新間隔（秒）
```

### スクリプトの直接カスタマイズ

重要な調整ポイント：

1. **状態パターンの追加** (`claude-status-enhanced.sh`の`enhanced_pattern_matching`関数)
2. **信頼度の調整** (`CONFIDENCE_LEVELS`連想配列)
3. **UI要素の検出** (`detect_ui_elements`関数)

## 監視とメンテナンス

### 定期的な健全性チェック

```bash
# 日次：簡易健全性チェック
~/.tmux/scripts/claude-migration.sh status

# 週次：詳細パフォーマンス分析
~/.tmux/scripts/claude-debug.sh perf 200

# 月次：包括的レポート生成
~/.tmux/scripts/claude-debug.sh report
```

### ログの確認と分析

```bash
# 検出ログの確認
tail -f ~/.tmux/debug/detection.log

# エラーログの分析
grep "ERROR" ~/.tmux/migration.log

# 検出統計の表示
jq '.type=="detection_event" | .detected_state' ~/.tmux/debug/metrics.json | sort | uniq -c
```

## 今後の拡張計画

### 短期的な改善（1-3ヶ月）
- 機械学習ベースの状態分類
- より詳細なコンテキスト分析
- プラグインアーキテクチャの導入

### 中期的な改善（3-6ヶ月）
- 複数AI統合サポート
- クラウド検出APIとの連携
- リアルタイムダッシュボード

### 長期的な展望（6ヶ月以降）
- AI駆動の自動最適化
- 多言語対応
- 業界標準プロトコルの策定

## サポートとコントリビューション

### 問題報告
- GitHub Issues: [dotfiles/issues](https://github.com/user/dotfiles/issues)
- デバッグレポート: `~/.tmux/scripts/claude-debug.sh report`

### 開発参加
1. フォークしてクローン
2. 機能ブランチの作成
3. テストの追加・実行
4. プルリクエストの送信

---

**注意**: この拡張システムは実験的な機能です。本番環境での使用前には十分なテストを実施してください。