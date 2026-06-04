# コマンドリファレンス

ツール固有のデバッグコマンドは各 SKILL.md の Debugging セクションを参照。

## Git Worktree 管理

```bash
# worktree 操作
gwt create <branch-name> [base-branch]   # 新規作成
gwt create -s <branch-name> [base-branch] # 新規作成して移動
gwt list                                  # 一覧表示
gwt switch                               # fzf で切り替え
gwt remove <worktree-name>               # 削除
gwt remove -d <worktree-name>            # ブランチも一緒に削除
gwt remove -f <worktree-name>            # 強制削除
gwt clean                                # クリーンアップ
gwt help                                 # ヘルプ

# 短縮エイリアス
gw       # gwt
gwc      # gwt create
gwl      # gwt list
gwsw     # gwt switch
gwr      # gwt remove
```

テスト: `./test_git_worktree.zsh`

## tmux Claude Worktree ランチャー (prefix + w)

tmux 上で worktree を作り、その中で Claude Code を並列起動・切替・削除する popup ランチャー。
`gwt`（シェルの worktree 管理）と配置規約（`../worktrees/<repo>-<name>`）を共有しつつ、
Claude の起動まで一気通貫で行う。

```text
prefix + w                      # popup を開く
  ├─ 名前を入力して Enter        # その名前で新規 worktree を作成
  │    ├─ base ref [HEAD]       # 空 Enter で現 pane のリポジトリの HEAD を基点
  │    └─ モード選択
  │         監視あり (supervised)   # 現 window を pane 分割して claude 起動
  │         監視なし (unsupervised) # detached window で headless `claude -p`（タスク入力）
  └─ 既存 worktree を選択 → Enter
       ├─ 開く / 前面化          # 対応 pane を前面化、無ければ新 pane で `claude -c`
       └─ 削除                   # 安全削除（失敗時は強制削除を確認）
  （ESC で中止）
```

ポイント:

- **作成の軸**: popup は「prefix + w を押した pane」の cwd で動くため、**いま Claude を動かしているリポジトリの HEAD** を基点に作る。配置は常にメインリポジトリ基準（リンク worktree 内から呼んでもネストしない）。
- **`*.local.md` の複製**: 作成時、現作業ツリーの `*.local.md`（git 管理外のローカルメモ等）を相対パスを保って新 worktree へ再帰複製（`.git`/`node_modules` は除外）。
- **オプションの付け直し**: 監視あり pane は claude 終了後に worktree の cwd でシェルに落ちる。`claude -c --model opus --permission-mode acceptEdits` のように、その時必要なオプションで会話を継続しつつ再起動できる（事前設定は不要）。
- **削除**: 未コミット/未追跡があると安全削除が拒否され、強制削除の確認 `[y/N]` が出る。ブランチは既定 `-d`（未マージなら残す）、強制時のみ `-D`。削除しても **pane は残る**ので `exit` か `prefix + x` で閉じる。
- **監視は自動**: 起動した pane は既存の Claude Voice 検出（全セッション横断）が自動で拾うため、別途設定は不要。

設計詳細: `docs/superpowers/specs/2026-06-02-tmux-worktree-claude-launcher-design.md`

## tmux resurrect/continuum

```bash
# TPM 初回セットアップ
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
tmux source ~/.tmux.conf
# Prefix + I  でプラグインインストール

# セッション操作
# Prefix + Ctrl-s  セッション保存
# Prefix + Ctrl-r  セッション復元

# 保存データ
ls -la ~/.local/share/tmux-resurrect/
```

詳細: `~/.tmux/plugin-config/README.md`

## tmux Claude Voice

```bash
# Prefix + v  で Claude Voice メニュー
```

詳細: `~/.tmux/docs/` 配下のドキュメント
