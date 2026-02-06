# tmux resurrect + continuum プラグイン設定

## 概要

このディレクトリには、tmux resurrect（セッション保存・復元）とtmux continuum（自動保存・自動復元）プラグインの設定が含まれています。

## インストール済みプラグイン

- **tmux-resurrect**: セッションの手動保存・復元
- **tmux-continuum**: セッションの自動保存・自動復元

## 設定ファイル

### resurrect.conf

プラグインの設定を一元管理するファイル。以下の設定が含まれています：

#### 基本設定

- **自動保存間隔**: 15分（変更がある場合のみ）
- **自動復元**: tmux起動時に自動的に前回のセッションを復元
- **復元対象プログラム**: vi, vim, nvim, emacs, man, less, more, tail, top, htop, python, node, npm, ssh, psql, mysql, sqlite3, redis-cli, mongosh

#### status-right統合

status-rightに以下が表示されます：
- CPU/メモリ情報（macOSの場合はtmux-mem-cpu-load）
- Claude Voice監視アイコン
- continuum保存状態アイコン（💾/⏳/🚫）
- 時刻

## 使用方法

### 手動操作

```bash
# セッション保存
Prefix + Ctrl-s

# セッション復元
Prefix + Ctrl-r
```

### 自動操作

- **自動保存**: 15分間隔で自動的に保存されます（変更がある場合のみ）
- **自動復元**: tmux起動時に自動的に前回のセッションが復元されます

### status-rightアイコン

- **💾**: 保存完了
- **⏳**: 保存中
- **🚫**: 自動保存無効

## 保存データ

### 保存場所

```
~/.local/share/tmux-resurrect/
```

### 保存される情報

- ウィンドウとペインの配置
- 実行中のプログラム（上記の復元対象プログラム）
- カレントディレクトリ
- ペインのサイズと位置

### 保存されない情報

- ペイン内のスクロールバック履歴（オプションで有効化可能）
- プログラムの内部状態（例: vimの未保存バッファ）
- 環境変数の変更

## カスタマイズ

### 自動保存間隔の変更

`resurrect.conf`の以下の行を編集：

```bash
set -g @continuum-save-interval '15'  # 分単位
```

### ペイン内容の復元（実験的機能）

`resurrect.conf`の以下の行をアンコメント：

```bash
# set -g @resurrect-capture-pane-contents 'on'
```

**注意**: この機能を有効にすると、保存・復元のパフォーマンスが低下する可能性があります。

### 自動復元の無効化

`resurrect.conf`の以下の行をコメントアウト：

```bash
set -g @continuum-restore 'on'
```

または、一時的に無効化：

```bash
tmux set -g @continuum-restore 'off'
tmux source ~/.tmux.conf
```

## トラブルシューティング

### プラグインが読み込まれない

```bash
# TPMを再インストール
rm -rf ~/.tmux/plugins/tpm
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux source ~/.tmux.conf

# プラグインを再インストール（tmux内で実行）
Prefix + I
```

### 保存データの削除（クリーンスタート）

```bash
rm -rf ~/.local/share/tmux-resurrect/*
```

### 設定の確認

```bash
# resurrect設定の確認
tmux show-options -g | grep "@resurrect"

# continuum設定の確認
tmux show-options -g | grep "@continuum"

# status-rightの確認
tmux show-options -g status-right
```

### 動作確認スクリプト

リポジトリルートにある`test_resurrect.sh`を実行して、すべての設定が正しく読み込まれているか確認できます：

```bash
cd ~/dotfiles
./test_resurrect.sh
```

## パフォーマンスチューニング

### 推奨設定（現在の設定）

- 自動保存間隔: 15分（バランスの取れた設定）
- ペイン内容の復元: OFF（パフォーマンス優先）
- 復元対象プログラム: 一般的な開発ツールのみ

### より頻繁な保存

自動保存間隔を短くする場合（例: 5分）：

```bash
set -g @continuum-save-interval '5'
```

**注意**: 間隔を短くすると、ディスクI/Oが増加します。

### より詳細な復元

ペイン内容も復元したい場合：

```bash
set -g @resurrect-capture-pane-contents 'on'
set -g @resurrect-strategy-vim 'session'
set -g @resurrect-strategy-nvim 'session'
```

**注意**: 復元時間が長くなり、パフォーマンスが低下する可能性があります。

## Claude Voice統合

このプラグイン設定は、既存のClaude Voice統合と完全に互換性があります：

- **独立動作**: resurrectはClaude Voice監視と干渉しません
- **status-right統合**: Claude Voiceアイコンとcontinuumアイコンが同時に表示されます
- **キーバインド**: Prefix + vテーブルは影響を受けません

## 参考リンク

- [tmux-resurrect](https://github.com/tmux-plugins/tmux-resurrect)
- [tmux-continuum](https://github.com/tmux-plugins/tmux-continuum)
- [TPM (Tmux Plugin Manager)](https://github.com/tmux-plugins/tpm)

## バージョン情報

- tmux: 3.6a以上推奨（2.9以上で動作）
- tmux-resurrect: latest
- tmux-continuum: latest

## 実装日

2026-02-04
