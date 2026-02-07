# Tmux Claude Voice: 音声エンジン

## 4.4. プラットフォーム依存処理

OS間の差異は、以下の関数内で吸収します。

### 基本関数

```bash
# OS種別を取得する関数
get_os_type() {
    uname
}

# 音声合成でテキストを読み上げる関数
speak_text() {
    local text="$1"
    local os_type=$(get_os_type)

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: sayコマンド（設定可能な音声）
        local speech_rate=$(tmux show-option -gv @claude_voice_speech_rate 2>/dev/null || echo "200")
        local voice_name=$(tmux show-option -gv @claude_voice_macos_voice 2>/dev/null || echo "Kyoko")
        local voice_quality=$(tmux show-option -gv @claude_voice_voice_quality 2>/dev/null || echo "enhanced")

        # 音声品質に応じた音声名の選択
        if [[ "$voice_quality" == "enhanced" ]]; then
            case "$voice_name" in
                "Kyoko") voice_name="Kyoko Enhanced" ;;
                "Otoya") voice_name="Otoya Enhanced" ;;
            esac
        fi

        if command -v say >/dev/null 2>&1; then
            say -v "$voice_name" -r "$speech_rate" "$text" &
        fi
    else
        # WSL: PowerShell + System.Speech（設定可能な音声）
        local volume=$(tmux show-option -gv @claude_voice_volume_wsl 2>/dev/null || echo "80")
        local voice_name=$(tmux show-option -gv @claude_voice_wsl_voice 2>/dev/null || echo "Haruka")

        # Windows音声名の完全指定
        case "$voice_name" in
            "Haruka") voice_name="Microsoft Haruka Desktop" ;;
            "Ayumi") voice_name="Microsoft Ayumi Desktop" ;;
            "Ichiro") voice_name="Microsoft Ichiro Desktop" ;;
        esac

        powershell.exe -Command "
            Add-Type -AssemblyName System.Speech
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
            \$synth.SelectVoice('$voice_name')
            \$synth.Volume = $volume
            \$synth.Speak('$text')
        " &
    fi
}

# 通知音を再生する関数
play_notification_sound() {
    local sound_type="$1"  # start, complete, waiting, error
    local os_type=$(get_os_type)

    # 設定から音声名を取得（OS別デフォルト）
    local sound_name=$(tmux show-option -gv @claude_voice_sound_${sound_type} 2>/dev/null)
    if [[ -z "$sound_name" ]]; then
        if [[ "$os_type" == "Darwin" ]]; then
            case "$sound_type" in
                "start")    sound_name="Ping" ;;
                "complete") sound_name="Glass" ;;
                "waiting")  sound_name="Funk" ;;
                "error")    sound_name="Sosumi" ;;
                *)          sound_name="Submarine" ;;
            esac
        else
            case "$sound_type" in
                "start")    sound_name="chimes" ;;
                "complete") sound_name="notify" ;;
                "waiting")  sound_name="chord" ;;
                "error")    sound_name="ringout" ;;
                *)          sound_name="Windows Notify Messaging" ;;
            esac
        fi
    fi

    local sound_file=$(get_system_sound_path "$sound_name")

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: afplayを使用
        if command -v afplay >/dev/null 2>&1; then
            local volume=$(tmux show-option -gv @claude_voice_volume_macos 2>/dev/null || echo "0.8")
            afplay -v "$volume" "$sound_file" &
        fi
    else
        # WSL: WindowsのSoundPlayerを使用
        local volume=$(tmux show-option -gv @claude_voice_volume_wsl 2>/dev/null || echo "80")
        local windows_path=$(echo "$sound_file" | sed 's|/mnt/c/|C:/|g' | sed 's|/|\\|g')
        powershell.exe -Command "
            Add-Type -AssemblyName System.Windows.Forms
            \$player = New-Object System.Media.SoundPlayer
            \$player.SoundLocation = '$windows_path'
            \$player.Play()
        " &
    fi
}
```

### プラットフォーム固有の通知音ファイルパス取得

```bash
# プラットフォーム固有の通知音ファイルパスを取得する関数
get_system_sound_path() {
    local sound_name="$1"
    local os_type=$(get_os_type)

    if [[ "$os_type" == "Darwin" ]]; then
        # macOS: システム通知音のパス
        local sound_paths=(
            "/System/Library/Sounds/${sound_name}.aiff"
            "/System/Library/PrivateFrameworks/ToneLibrary.framework/Versions/A/Resources/AlertTones/${sound_name}.aiff"
            "${HOME}/Library/Sounds/${sound_name}.aiff"
        )

        for path in "${sound_paths[@]}"; do
            if [[ -f "$path" ]]; then
                echo "$path"
                return 0
            fi
        done

        # デフォルトのシステム音
        echo "/System/Library/Sounds/Submarine.aiff"
    else
        # WSL/Windows: Windowsシステム通知音のパス
        local windows_media_path="/mnt/c/Windows/Media"
        local sound_path="${windows_media_path}/${sound_name}.wav"

        if [[ -f "$sound_path" ]]; then
            echo "$sound_path"
        else
            # デフォルトのWindows通知音
            echo "${windows_media_path}/Windows Notify Messaging.wav"
        fi
    fi
}
```

### 利用可能音声の取得

```bash
# 利用可能なmacOS音声を取得する関数
get_available_macos_voices() {
    if command -v say >/dev/null 2>&1; then
        say -v "?" | grep -E "(Kyoko|Otoya)" | grep -E "(enhanced|compact)" | head -n 4
    fi
}

# 利用可能なWindows音声を取得する関数
get_available_windows_voices() {
    powershell.exe -Command "
        Add-Type -AssemblyName System.Speech
        \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
        \$synth.GetInstalledVoices() | ForEach-Object {
            \$voice = \$_.VoiceInfo
            if (\$voice.Culture.Name -eq 'ja-JP') {
                Write-Output \$voice.Name
            }
        }
    " 2>/dev/null
}
```

### 利用可能音声

- **macOS**: Kyoko（Enhanced）、Otoya（Enhanced）、Kyoko、Otoya
- **Windows**: Haruka、Ayumi、Ichiro

## 設定パラメータ

```bash
# 通知音設定（未設定時はOS別デフォルトが使用される）
set -g @claude_voice_sound_enabled "true"     # 通知音の有効化
# set -g @claude_voice_sound_start ""         # 処理開始音（macOS: Ping, WSL: chimes）
# set -g @claude_voice_sound_complete ""      # 処理完了音（macOS: Glass, WSL: notify）
# set -g @claude_voice_sound_waiting ""       # 待機音（macOS: Funk, WSL: chord）
# set -g @claude_voice_sound_error ""         # エラー音（macOS: Sosumi, WSL: ringout）

# 音声エンジン設定
set -g @claude_voice_speech_rate "200"        # 音声速度（WPM、macOS用）
set -g @claude_voice_volume_macos "0.8"       # macOS音量制御（0.0-1.0）
set -g @claude_voice_volume_wsl "80"          # WSL音量制御（0-100）
set -g @claude_voice_pan_law "equal_power"    # パンニング法則（equal_power/linear）

# 日本語音声キャラクター設定
set -g @claude_voice_macos_voice "Kyoko"      # macOS音声（Kyoko, Otoya, Kyoko Enhanced, Otoya Enhanced）
set -g @claude_voice_wsl_voice "Haruka"       # WSL音声（Haruka, Ayumi, Ichiro）
set -g @claude_voice_voice_quality "enhanced" # 音声品質（enhanced/standard）
```

## 実装チェックリスト

### Phase 2: 音声エンジン

- [ ] `sound_utils.sh`の作成
- [ ] プラットフォーム検出機能の実装
- [ ] 音声キャラクター選択機能の実装
- [ ] システム通知音再生機能の実装
- [ ] 音声合成機能の実装
- [ ] 音量制御機能の実装
- [ ] 利用可能音声検出機能の実装
- [ ] エラーハンドリングの実装
- [ ] 設定パラメータの実装
- [ ] 単体テストの作成

### テスト項目

1. **プラットフォーム検出テスト**
   - macOS環境での検出
   - WSL環境での検出
   - 音声デバイスの確認

2. **音声合成テスト**
   - macOS音声の再生
   - Windows音声の再生
   - 音声品質の確認
   - 音量制御の動作確認

3. **通知音テスト**
   - システム通知音の再生
   - ファイルパスの検出
   - フォールバック機能の動作確認

4. **統合テスト**
   - 実際のtmux環境での動作確認
   - 複数音声の同時再生
   - パフォーマンステスト

## トラブルシューティング

### よくある問題

1. **音声が再生されない**
   ```bash
   # 解決方法: 音声デバイスの確認
   # macOS
   system_profiler SPAudioDataType

   # WSL
   powershell.exe -Command "Get-WmiObject -Class Win32_SoundDevice"
   ```

2. **macOS音声が見つからない**
   ```bash
   # 解決方法: 利用可能音声の確認
   say -v "?" | grep -E "(Kyoko|Otoya)"
   ```

3. **Windows音声が見つからない**
   ```bash
   # 解決方法: 利用可能音声の確認
   powershell.exe -Command "
       Add-Type -AssemblyName System.Speech
       \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer
       \$synth.GetInstalledVoices() | ForEach-Object {
           \$voice = \$_.VoiceInfo
           if (\$voice.Culture.Name -eq 'ja-JP') {
               Write-Output \$voice.Name
           }
       }
   "
   ```

4. **通知音ファイルが見つからない**
   ```bash
   # 解決方法: ファイルパスの確認
   # macOS
   ls /System/Library/Sounds/*.aiff

   # WSL
   ls /mnt/c/Windows/Media/*.wav
   ```
