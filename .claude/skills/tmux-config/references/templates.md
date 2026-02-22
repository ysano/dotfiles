# tmux Config テンプレートと実例

## status-right を変更したい場合

**問題**: `status.conf` を編集したが反映されない
**原因**: `resurrect.conf` が後から読み込まれ上書きしている
**解決**: `resurrect.conf` 側の `status-right` を編集する

```bash
# .tmux/plugin-config/resurrect.conf で設定（ここが最終的に有効）
set -g status-right '#{?client_prefix,#[reverse] C-z #[noreverse],} ...'
```

## 新しいキーバインドを追加したい場合

```bash
# .tmux/keybindings.conf に追加（prefix テーブル）
bind <key> <command>

# root テーブル（prefix 不要）の場合
bind -n <key> <command>

# OS固有の場合は対応する os/*.conf に追加
# 例: macOS のみ → .tmux/os/darwin.conf
```

## Emacs風キーテーブルにコマンドを追加

**やりたいこと**: `Prefix x b` でバッファ一覧（choose-tree）を開きたい

**配置先**: `.tmux/keybindings.conf`（emacsCX テーブルに追加）

```bash
# 既存: bind x switch-client -T emacsCX  (emacsCX テーブルへの切替)
# 既存の emacsCX バインド (0=kill, 1=break, 2=split-v, 3=split-h, ...)
bind -T emacsCX b choose-tree -Zs
```

**ポイント**: `bind -T emacsCX` で Prefix x の後に続くキーを定義。既存の 0/1/2/3/o/k/i と重複しないキーを選ぶ。

## OS固有のクリップボード設定

**やりたいこと**: WSL でシステムクリップボードとの連携を追加

**配置先**: `.tmux/os/wsl.conf`

```bash
# コピーモードで選択したテキストを Windows クリップボードにコピー
bind -T copy-mode M-w send-keys -X copy-pipe-and-cancel "clip.exe"
```

**ポイント**: OS固有設定は対応する `os/*.conf` に配置。`if-shell` 分岐で排他的に読み込まれるため、他 OS への影響なし。

## OS固有の設定を追加したい場合

```bash
# .tmux.conf 内の if-shell 分岐パターンに従う（WSL先行検出）
if-shell 'test -n "$WSL_DISTRO_NAME"' 'source-file ~/.tmux/os/wsl.conf'
if-shell 'uname | grep -q Darwin' 'source-file ~/.tmux/os/darwin.conf'
```

## Plugin Architecture

### TPM (Tmux Plugin Manager)
- プラグインディレクトリ: `.tmux/plugins/`
- `tpm` は `.tmux.conf` 最終行の `run '~/.tmux/plugins/tpm/tpm'` で起動

### resurrect/continuum
- 設定ファイル: `.tmux/plugin-config/resurrect.conf` (54行)
- 詳細設定ドキュメント: `.tmux/plugin-config/README.md` (206行)
- セッション保存/復元とバックグラウンド自動保存を管理
