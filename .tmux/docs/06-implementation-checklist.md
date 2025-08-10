# Tmux Claude Voice: 実装チェックリスト

## AI Agent実装チェックリスト

### **Phase 1: 基本監視システム**

- [ ] `main.sh`の作成（無限ループ、エラーハンドリング）
- [ ] `functions.sh`の作成（ステータス判定、状態管理）
- [ ] 依存関係チェック機能の実装
- [ ] ログ出力機能の実装
- [ ] 設定値バリデーションの実装
- [ ] 単体テストの作成

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

- [ ] 全ファイルの統合テスト
- [ ] パフォーマンス最適化
- [ ] エラーハンドリングの強化
- [ ] ドキュメントの作成
- [ ] エンドツーエンドテスト

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

2. **音声が再生されない**

   ```bash
   # 解決方法: 音声デバイスの確認
   # macOS
   system_profiler SPAudioDataType

   # WSL
   powershell.exe -Command "Get-WmiObject -Class Win32_SoundDevice"
   ```

3. **Ollamaに接続できない**

   ```bash
   # 解決方法: 接続テスト
   curl -s --max-time 5 "http://localhost:11434/api/tags" || echo "Ollamaサーバーに接続できません"
   ```

4. **ffplayが見つからない**

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

### パフォーマンス監視

```bash
# 実行時間の測定
measure_execution_time() {
    local start_time=$(date +%s.%N)
    "$@"
    local end_time=$(date +%s.%N)
    local execution_time=$(echo "$end_time - $start_time" | bc)
    log_debug "実行時間: ${execution_time}秒"
}
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
- [ ] 実際のtmux環境で動作確認が完了している
- [ ] ドキュメントが最新の状態になっている

このチェックリストに従って実装することで、堅牢で保守性の高いtmux-claude-voiceシステムを構築できます。

## ファイル構造の最終確認

実装すべきファイル一覧：

1. **`main.sh`** - メイン監視ループ
2. **`functions.sh`** - 基本機能関数群
3. **`sound_utils.sh`** - 音声エンジン
4. **`panning_engine.sh`** - デシベルパンニング
5. **`ollama_utils.sh`** - Ollama連携
6. **`platform_utils.sh`** - プラットフォーム依存処理
7. **`toggle_notify_mode.sh`** - 通知モード切り替え
8. **`README.md`** - インストール・設定ガイド

## 依存関係の確認

必要な外部コマンド：

- **基本**: `tmux`, `grep`, `bc`, `jq`, `curl`
- **音声**: `say` (macOS), `powershell.exe` (WSL), `afplay` (macOS), `ffplay`
- **AI**: `ollama` (オプション)

## 設定ファイルの例

`.tmux.conf`への追加設定例：

```bash
# Tmux Claude Voice 設定
set -g @claude_voice_enabled "true"
set -g @claude_voice_interval "5"
set -g @claude_voice_window_pattern "Claude|claude|CLAUDE"

# 音声設定
set -g @claude_voice_sound_enabled "true"
set -g @claude_voice_macos_voice "Kyoko"
set -g @claude_voice_wsl_voice "Haruka"

# Ollama設定
set -g @claude_voice_ollama_host "localhost"
set -g @claude_voice_ollama_port "11434"

# デシベルパンニング設定
set -g @claude_voice_panning_enabled "true"
set -g @claude_voice_pan_dynamic "true"

# キーバインド設定
bind-key n run-shell "~/.tmux/plugins/tmux-claude-voice/toggle_notify_mode.sh"
```
