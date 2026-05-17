# 0003. `~/.claude/` 配下の個別ファイル deploy 経路として `claude_files` 配列を採用

Date: 2026-05-17
Status: accepted

## Context

`~/.claude/` 配下にはホスト固有のスクリプト (statusline-command.sh など) や
Claude Code 用のグローバル設定ファイルが存在するが、これまで dotfiles の
deploy フロー (link.sh) では `.claude/` を直接扱う仕組みが無かった。

過去には `dotfiles/claude-home/` ディレクトリ経由で `~/.claude/agents`,
`~/.claude/commands`, `~/.claude/skills` などを symlink 展開していたが、
コミット `ea17c6c "refactor: remove claude-home, complete migration to
claude-plugins"` で claude-home/ ごと撤去された。撤去後の symlink は dangling
のまま放置され、9 件が `~/.claude/` 直下に残存していた。

今回 statusline-command.sh（実装場所はユーザーローカル、新マシン構築時に
再展開できない）を dotfiles 配下に取り込むにあたり、`~/.claude/` への
deploy 規約を明示する必要が生じた。

## Options Considered

### Option 1: link.sh の `files=()` 配列に追記
- Pros: 既存仕組みの素直な拡張。
- Cons: `files=()` は **`$HOME` 直下のドットファイル** 用 (`.zshrc` 等)。
  `~/.claude/statusline-command.sh` のような subdirectory 配下のファイルは
  対象外。配列の意味が崩れる。

### Option 2: `~/.claude` 全体を `dirs=()` で symlink 化
- Pros: 1 行で全部展開。
- Cons: Claude Code が `~/.claude/` 配下に生成する大量の動的データ
  (`projects/`, `sessions/`, `cache/`, `statsig/`, `telemetry/` など) を
  dotfiles リポに飲み込むことになり、git history が肥大化。実用不可能。

### Option 3: `claude-home/` を復活させて使い回す
- Pros: 過去の規約に揃う。
- Cons: 既に `ea17c6c` で意図的に廃止された経路を蘇生することは設計の
  後退。dangling symlink が再発する温床にもなる。

### Option 4: `claude_files=()` 配列を新設し、個別ファイル単位で symlink 展開
- Pros: 「`~/.claude/` 配下に個別配備したいホスト固有ファイル」という
  意図をそのまま表現できる。既存の files/dirs/config_dirs 配列パターンと
  同形で読みやすい。追加・削除も配列要素の編集のみ。
- Cons: ファイル数が増えると配列が長くなる（現状 1 件、将来も数件以内の
  想定なので問題なし）。

## Decision

**Option 4** を採用。link.sh に `claude_files=(statusline-command.sh)`
配列を追加し、msys/cygwin / unix の両ブランチで `~/.claude/$f` 形式で
symlink を生成するループを追加した。

deploy 元は `dotfiles/.claude/<file>` に統一する（`claude-home/` は復活させ
ない）。配置先 `~/.claude/<file>` は `ensure_parent_dir` で安全に親ディレクトリ
を作成してから展開する。

合わせて claude-home/ 撤去の残骸である dangling symlink 9 件を
`find ~/.claude -type l ! -exec test -e {} \; -delete` でクリーンアップ済み。

## Consequences

- ✅ 新マシン構築時に `./link.sh` 一回で `~/.claude/statusline-command.sh` も
  含めて全 deploy が完結する
- ✅ ホスト固有のスクリプトを今後追加する場合、`claude_files=()` に追記
  するだけで運用可能（規約が明示的）
- ✅ Claude Code が動的に生成するディレクトリ (`projects/`, `sessions/` 等)
  には触れない設計のため、git に flush されない
- ⚠️ `~/.claude/settings.json` のように Claude Code が自身で書き換える
  設定ファイルはこの仕組みで管理しない（symlink にすると Claude Code の
  update で symlink が壊される懸念があるため）。それらは手動同期 or 別経路
- ⚠️ 配列が肥大化した場合は将来 `glob` ベースの自動展開に切り替える余地

## AI-DLC Impact

- Agent Loop への影響: 無し
- Spec 変更の要否: 無し（deploy 規約は dotfiles の運用レイヤであり Spec
  対象外）
