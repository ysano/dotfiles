# dotfiles

個人用の設定ファイル（dotfiles）リポジトリです。

## 含まれるファイル

### シェル関連
- `.zshrc` - Zshの設定
- `.zprofile` - Zshのプロファイル設定
- `.zsh/` - Zsh関連の追加設定

### エディタ関連
- `.emacs.d/` - Emacsの設定

### Git関連
- `.gitconfig` - Gitの設定
- `.gitignore` - Gitの除外ファイル設定

### ターミナル関連
- `.tmux.conf` - Tmuxの共通設定
- `.tmux-linux.conf` - Linux用Tmux設定
- `.tmux-freebsd.conf` - FreeBSD用Tmux設定
- `.tmux-darwin.conf` - macOS用Tmux設定
- `terminfo-24bit.src` - 24ビットカラー対応terminfo

### X Window関連
- `.xinitrc` - X起動時の設定
- `.Xresources` - Xリソース設定

### macOS関連
- `.yabairc` - Yabaiウィンドウマネージャの設定
- `.skhdrc` - ショートカットキー設定
- `.Brewfile` - Homebrewパッケージリスト

### キーボードカスタマイズ
- `karabiner/` - Karabiner-Elements設定
- `mayu/` - Mayuキーボードカスタマイズ設定
- `keyboard-maestro/` - Keyboard Maestro設定

### WSL関連
- `wsl/` - WSL固有の設定

### その他
- `.aspell.conf` - スペルチェッカー設定
- `.rpmmacros` - RPMビルド設定

## 使い方

`link.sh`を実行すると、ホームディレクトリに各設定ファイルのシンボリックリンクが作成されます。
既存のファイルがある場合は`.orig`拡張子を付けてバックアップされます。

```bash
./link.sh
```

## メモ

- WSLとmacOSの両方で使用
- 環境ごとの差異は各設定ファイル内で分岐処理
