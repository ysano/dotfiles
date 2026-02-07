# Tmux Claude Voice: 実装チェックリスト

## AI Agent実装チェックリスト

### **Phase 1: 基本監視システム**

- [x] `polling_monitor.sh`の作成（ポーリング監視、エラーハンドリング）
- [x] `functions.sh`の作成（ステータス判定、状態管理）
- [x] 依存関係チェック機能の実装
- [x] ログ出力機能の実装
- [x] 設定値バリデーションの実装
- [x] 単体テストの作成
- [x] tmux status-right統合の確認

### **Phase 2: 音声エンジン**

- [x] `sound_utils.sh`の作成
- [x] プラットフォーム検出機能の実装
- [x] 音声キャラクター選択機能の実装
- [x] システム通知音再生機能の実装
- [x] 音声合成機能の実装
- [x] 音量制御機能の実装

### **Phase 3: デシベルパンニング**

- [x] `panning_engine.sh`の作成
- [x] ウィンドウ位置検出機能の実装
- [x] Equal Power Pan Law計算の実装
- [x] ffplay連携機能の実装
- [x] 動的配置計算の実装

### **Phase 4: Ollama連携**

- [x] `ollama_utils.sh`の作成
- [x] モデル優先順位リストの実装
- [x] 外部API連携機能の実装
- [x] 要約生成機能の実装
- [x] フォールバック機能の実装

### **Phase 5: 統合と最適化**

- [x] `integration_test.sh`の作成
- [x] 全ファイルの統合テスト
- [x] パフォーマンス最適化
- [x] エラーハンドリングの強化
- [x] ドキュメントの作成
- [x] エンドツーエンドテスト
- [x] ポーリング監視テスト

### **Phase 6: Hooks 統合**

- [x] `hooks/status-update.sh`の作成（イベント共通エントリポイント）
- [x] `hooks/setup-hooks.sh`の作成（settings.json セットアップ）
- [x] ペインレベル状態管理の実装
- [x] イベント → ステータス変換ロジック
- [x] 重複排除ロジック（同一状態スキップ）
- [x] hooks タイムスタンプ管理
- [x] ウィンドウレベルアイコン集約
- [x] `polling_monitor.sh` の hooks フォールバック対応
- [x] `functions.sh` のペインレベル検出対応（`detect_claude_panes()`）
- [x] `sound_utils.sh` のOS別デフォルト通知音対応
- [x] `integration_test.sh` の hooks テスト追加

## 実装済みファイル一覧

### ✅ 完了済み

- [x] `polling_monitor.sh` - ポーリング監視スクリプト（hooks フォールバック付き）
- [x] `functions.sh` - 基本機能関数群（ペインレベル検出対応）
- [x] `sound_utils.sh` - 音声エンジン（OS別デフォルト音対応）
- [x] `panning_engine.sh` - デシベルパンニングエンジン
- [x] `ollama_utils.sh` - Ollama連携機能
- [x] `integration_test.sh` - 統合テスト（hooks テスト含む）
- [x] `toggle_notify_mode.sh` - 通知モード切り替え機能
- [x] `hooks/status-update.sh` - Hooks イベント共通エントリポイント
- [x] `hooks/setup-hooks.sh` - settings.json セットアップ

## 統合テスト結果

### Phase 1-5: ✅ 成功（22/22）

- **ファイル存在**: polling_monitor.sh, functions.sh, sound_utils.sh, panning_engine.sh, ollama_utils.sh
- **単体テスト**: polling_monitor.sh, functions.sh, sound_utils.sh, panning_engine.sh, ollama_utils.sh
- **依存関係**: 基本依存関係, 音声エンジン, パンニングエンジン, Ollama
- **設定読み込み**: 全設定項目
- **コンポーネント連携**: 全連携テスト
- **パフォーマンス**: ファイル読み込み時間, メモリ使用量

### Phase 6: Hooks テスト追加

- **ファイル存在**: hooks/status-update.sh, hooks/setup-hooks.sh
- **hooks シミュレート**: UserPromptSubmit→Busy, Stop→Idle, Notification→Waiting
- **タイムスタンプ**: hooks タイムスタンプ更新の正常性
- **セッション終了**: SessionEnd→状態クリア

### 🎉 実装完了状況

**Phase 1-5 すべて完了**: 22個のテストすべてが成功し、システムは本番利用の準備が整いました。

## 実装完了項目

### ✅ 修正完了

- [x] `integration_test.sh`の`main.sh`参照を`polling_monitor.sh`に修正
- [x] 依存関係チェック機能の実装
- [x] 設定値バリデーション機能の実装
- [x] WSL環境での音声エンジン依存関係チェックの改善
- [x] メモリ使用量測定の改善

### ✅ 依存関係確認済み

- [x] 基本依存関係（tmux, grep, awk, curl, jq）のインストール確認
- [x] 音声エンジン依存関係（ffplay）のインストール確認

## デバッグガイド

### よくある問題と解決方法

1. **tmuxコマンドが実行できない**

   ```bash
   # 解決方法: tmuxセッション内で実行されているか確認
   if [[ -z "$TMUX" ]]; then
       echo "tmuxセッション内で実行してください"
       exit 1
   fi
   ```

2. **ポーリング監視が動作しない**

   ```bash
   # 解決方法: status-right設定の確認
   tmux show-option -g status-right
   
   # 解決方法: 手動でポーリング監視をテスト
   ~/.tmux/claude/polling_monitor.sh
   
   # 解決方法: システム有効化状態の確認
   tmux show-option -gqv @claude_voice_enabled
   ```

3. **音声が再生されない**

   ```bash
   # 解決方法: 音声デバイスの確認
   # macOS
   system_profiler SPAudioDataType

   # WSL
   powershell.exe -Command "Get-WmiObject -Class Win32_SoundDevice"
   ```

4. **Ollamaに接続できない**

   ```bash
   # 解決方法: 接続テスト
   curl -s --max-time 5 "http://localhost:11434/api/tags" || echo "Ollamaサーバーに接続できません"
   ```

5. **ffplayが見つからない**

   ```bash
   # 解決方法: インストール確認
   # macOS
   brew install ffmpeg

   # Ubuntu/Debian
   sudo apt-get install ffmpeg
   ```

### ログレベルの設定

```bash
# デバッグモードの有効化
export TMUX_CLAUDE_VOICE_DEBUG=1

# 詳細ログの出力
if [[ "$TMUX_CLAUDE_VOICE_DEBUG" == "1" ]]; then
    log_debug() {
        echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') - $1" >&2
    }
else
    log_debug() { :; }
fi
```

### ポーリング監視のデバッグ

```bash
# ポーリング監視の手動テスト
test_polling_monitor() {
    echo "=== ポーリング監視テスト ==="
    
    # 設定確認
    echo "システム有効化状態: $(tmux show-option -gqv @claude_voice_enabled)"
    echo "ウィンドウパターン: $(tmux show-option -gqv @claude_voice_window_pattern)"
    
    # ポーリング監視実行
    if ~/.tmux/claude/polling_monitor.sh; then
        echo "✓ ポーリング監視: 成功"
    else
        echo "✗ ポーリング監視: 失敗"
        return 1
    fi
}
```

## ファイル構造

```
.tmux/claude/
├── polling_monitor.sh      # ポーリング監視（hooks フォールバック付き）
├── functions.sh           # 基本機能関数群（ペインレベル検出対応）
├── sound_utils.sh         # 音声エンジン（OS別デフォルト音対応）
├── panning_engine.sh      # デシベルパンニングエンジン
├── ollama_utils.sh        # Ollama連携機能
├── integration_test.sh    # 統合テスト（hooks テスト含む）
├── toggle_notify_mode.sh  # 通知モード切り替え
├── pan_test.sh            # パンニングテスト
└── hooks/                 # Claude Code hooks 統合
    ├── status-update.sh   # hooks イベント共通エントリポイント
    └── setup-hooks.sh     # ~/.claude/settings.json セットアップ
```

## 依存関係

### 必須依存関係

- `tmux` - ターミナルマルチプレクサ
- `bash` - シェルスクリプト実行環境
- `grep` - テキスト検索
- `awk` - テキスト処理
- `curl` - HTTP通信（Ollama連携用）
- `jq` - JSON処理（Ollama連携用）

### 音声関連依存関係

- **macOS**: `say`, `ffplay` (ffmpeg)
- **WSL**: `powershell.exe`, `ffplay` (ffmpeg)

### オプション依存関係

- `ollama` - ローカルLLM（要約機能用）
- `bc` - 数値計算（パンニング計算用）

## 設定ファイル例

```bash
# .tmux.conf または .tmux/claude.conf

# === Claude Voice 基本設定 ===
set -g @claude_voice_enabled "true"
set -g @claude_voice_interval "5"
set -g @claude_voice_window_pattern "Claude|claude|CLAUDE"

# === 音声エンジン設定 ===
set -g @claude_voice_sound_enabled "true"
# 通知音（未設定時はOS別デフォルト: macOS=Ping/Glass/Funk, WSL=chimes/notify/chord）
# set -g @claude_voice_sound_start ""
# set -g @claude_voice_sound_complete ""
# set -g @claude_voice_sound_waiting ""

# === 音声合成設定 ===
set -g @claude_voice_speech_rate "200"
set -g @claude_voice_volume_macos "0.8"
set -g @claude_voice_volume_wsl "80"
set -g @claude_voice_macos_voice "Kyoko"
set -g @claude_voice_wsl_voice "Haruka"

# === デシベルパンニング設定 ===
set -g @claude_voice_panning_enabled "true"
set -g @claude_voice_pan_range "1.0"
set -g @claude_voice_pan_identification "true"

# === Ollama連携設定 ===
set -g @claude_voice_ollama_host "localhost"
set -g @claude_voice_ollama_port "11434"
set -g @claude_voice_summary_enabled "true"
set -g @claude_voice_summary_length "30"

# === ポーリング自動監視設定 ===
set -g status-interval 5
set -g status-right '#(~/.tmux/claude/polling_monitor.sh)#[default] %H:%M '
```

## 最終確認事項

実装完了後、以下の項目を確認してください：

- [x] すべての依存関係がインストールされている
- [x] 各ファイルが独立してテスト可能
- [x] エラーハンドリングが適切に実装されている
- [x] ログ出力が適切に設定されている
- [x] 設定値のバリデーションが実装されている
- [x] 単体テストがすべて成功している
- [x] 統合テストが成功している
- [x] ポーリング監視が正常に動作している
- [x] tmux status-right統合が正常に動作している
- [x] 実際のtmux環境で動作確認が完了している
- [x] ドキュメントが最新の状態になっている

## 🎉 実装完了！

**tmux-claude-voiceシステムの実装が完了しました！**

- **Phase 1-6**: すべて完了
- **統合テスト**: 22/22 成功（Phase 5）+ hooks テスト追加（Phase 6）
- **ファイル**: 8個 + hooks ディレクトリ
- **機能**: Hooks 駆動 + ポーリング監視、ペインレベル検出、音声エンジン、デシベルパンニング、Ollama連携

システムは本番利用の準備が整っています。`.tmux.conf`に設定を追加してtmuxを再起動すれば、ポーリング監視が自動的に開始されます。

このチェックリストに従って実装することで、堅牢で保守性の高いtmux-claude-voiceシステムを構築できました。
