# iTerm2 Meta/Altキー設定手順

## iTerm2側の設定

1. **iTerm2を開く**
2. **Preferences（設定）を開く**: `Cmd + ,`
3. **Profiles → Keys タブを開く**
4. **Key Mappingsセクションで以下を確認/設定**:
   
   ### Option 1: Left Option keyの設定（推奨）
   - 「Left Option key」を「Esc+」に設定
   
   ### Option 2: 両方のOptionキーを設定
   - 「Left Option key」を「Esc+」に設定
   - 「Right Option key」を「Esc+」に設定

5. **追加の推奨設定**:
   - 「Apps can change this」のチェックを外す
   - 「Report modifiers using CSI u」のチェックを外す

## 設定反映の確認

1. tmuxを再起動:
   ```bash
   tmux kill-server
   tmux
   ```

2. Emacsでテスト:
   - `M-x` (Option+x) が動作するか確認
   - `M-f` (Option+f) で単語移動ができるか確認
   - `M-b` (Option+b) で単語後退ができるか確認

## トラブルシューティング

### それでも動作しない場合

1. **iTerm2の代替設定**:
   - Profiles → Keys → Key Mappingsで「Presets...」をクリック
   - 「Natural Text Editing」を選択してから、上記の設定を再度行う

2. **tmux設定の確認**:
   ```bash
   tmux show-options -g | grep -E "(escape-time|xterm-keys|terminal)"
   ```
   
   以下が表示されることを確認:
   - `escape-time 0`
   - `xterm-keys on`

3. **Emacsでの診断**:
   ```
   M-x describe-key RET
   ```
   その後、Option+任意のキーを押して、どのように認識されているか確認

## 参考情報

- iTerm2で「Esc+」設定にすると、OptionキーがEscapeシーケンスとして送信される
- これによりEmacsがMetaキーとして認識できるようになる
- tmuxの`escape-time 0`設定により、Escapeシーケンスの遅延がなくなる