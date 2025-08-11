# Tmux Claude Voice: 実装チェックリスト

## AI Agent実装チェックリスト

### **Phase 1: 基本監視システム**

- [ ] `polling_monitor.sh`の作成（ポーリング監視、エラーハンドリング）
- [ ] `functions.sh`の作成（ステータス判定、状態管理）
- [ ] 依存関係チェック機能の実装
- [ ] ログ出力機能の実装
- [ ] 設定値バリデーションの実装
- [ ] 単体テストの作成
- [ ] tmux status-right統合の確認

### **Phase 2: 音声エンジン**

- [ ] `sound_utils.sh`の作成
- [ ] プラットフォーム検出機能の実装
- [ ] 音声キャラクター選択機能の実装
- [ ] システム通知音再生機能の実装
- [ ] 音声合成機能の実装
- [ ] 音量制御機能の実装

### **Phase 3: デシベルパンニング**

- [ ] `panning_engine.sh`の作成
- [ ] ウィンドウ位置検出機能の実装
- [ ] Equal Power Pan Law計算の実装
- [ ] ffplay連携機能の実装
- [ ] 動的配置計算の実装

### **Phase 4: Ollama連携**

- [ ] `ollama_utils.sh`の作成
- [ ] モデル優先順位リストの実装
- [ ] 外部API連携機能の実装
- [ ] 要約生成機能の実装
- [ ] フォールバック機能の実装

### **Phase 5: 統合と最適化**

- [ ] `integration_test.sh`の作成
- [ ] 全ファイルの統合テスト
- [ ] パフォーマンス最適化
- [ ] エラーハンドリングの強化
- [ ] ドキュメントの作成
- [ ] エンドツーエンドテスト
- [ ] ポーリング監視テスト

## 実装済みファイル一覧

### ✅ 完了済み

- [x] `polling_monitor.sh` - ポーリング監視スクリプト
- [x] `functions.sh` - 基本機能関数群
- [x] `sound_utils.sh` - 音声エンジン
- [x] `panning_engine.sh` - デシベルパンニングエンジン
- [x] `ollama_utils.sh` - Ollama連携機能
- [x] `integration_test.sh` - 統合テスト

### ❌ 未実装

- [ ] `toggle_notify_mode.sh` - 通知モード切り替え（オプション）

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
├── polling_monitor.sh      # ポーリング監視スクリプト
├── functions.sh           # 基本機能関数群
├── sound_utils.sh         # 音声エンジン
├── panning_engine.sh      # デシベルパンニングエンジン
├── ollama_utils.sh        # Ollama連携機能
└── integration_test.sh    # 統合テスト
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
set -g @claude_voice_sound_start "Submarine"
set -g @claude_voice_sound_complete "Funk" 
set -g @claude_voice_sound_waiting "Basso"

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

- [ ] すべての依存関係がインストールされている
- [ ] 各ファイルが独立してテスト可能
- [ ] エラーハンドリングが適切に実装されている
- [ ] ログ出力が適切に設定されている
- [ ] 設定値のバリデーションが実装されている
- [ ] 単体テストがすべて成功している
- [ ] 統合テストが成功している
- [ ] ポーリング監視が正常に動作している
- [ ] tmux status-right統合が正常に動作している
- [ ] 実際のtmux環境で動作確認が完了している
- [ ] ドキュメントが最新の状態になっている

このチェックリストに従って実装することで、堅牢で保守性の高いtmux-claude-voiceシステムを構築できます。
