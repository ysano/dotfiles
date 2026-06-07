# 0009. 言語ランタイム管理を asdf から mise へ移行

Date: 2026-06-07
Status: accepted

## Context

言語ランタイムのバージョン管理を **asdf 0.19.0 → mise 2026.6.1** へ移行した。
設定本体（shell 統合・補完・Makefile ターゲット）は本リポ（dotfiles）に閉じているため、
その管理方針・運用ナレッジも本リポに co-locate する（machine-management 側は
「言語ランタイムは mise が管轄」という PM 責務マップと作業履歴のみ持つ）。

管理対象は 9 ツール: golang, python, ruby, nodejs, yarn, poetry, bun, postgres, terraform。
グローバルの `~/.tool-versions`（$HOME 直置き）で管理し、mise がネイティブに読む。

移行を後押しした要因:

- **`asdf: not found` 警告の常態化**: asdf 0.16+ (Go 版) の shims は `exec asdf exec …` 形式で、
  `brew bundle` 等の非対話・最小環境で `/opt/homebrew/bin` が PATH に無いと本体を見失い警告を出していた。
  mise の shims は**自己完結バイナリ**なので構造的に解消される。
- mise は `.tool-versions` をネイティブに読むため設定ファイル無改変で移行できる。
- mise の core backend（ビルド済みバイナリ）と aqua backend（検証付きバイナリ）で
  プラグイン依存・ソースビルドを減らせる。

## Decision

**asdf を撤去し mise に全面移行する。** 方針は以下:

- 設定ファイルは **`~/.tool-versions` を維持**（mise がネイティブに読む。asdf へのロールバックも容易）。
- shell 統合は **shims 方式**（`.zprofile` の `setup_mise()` で `~/.local/share/mise/shims` を PATH prepend）。
  現行 asdf shims と挙動が同一で、プロジェクトごとの自動切替も維持。`mise activate` は採用しない。
- backend は **mise レジストリの既定に委譲**（踏襲方針）:
  - core（ビルド済み）: go, node, python, ruby, bun
  - aqua（検証付きバイナリ）: terraform
  - vfox プラグイン: yarn, poetry, postgres
- 補完は **Homebrew が site-functions に配置**するため自前生成しない（旧 asdf は自前生成していた）。
- 週次のプラグイン更新は `mise plugins update`（`.zsh/zinit_setup.zsh`）。
- mise **パッケージ自体**は machine-management の Brewfile が管理（`brew "mise"`）。本リポは設定と方針。

最適化（yarn→corepack, poetry→pipx 等の「mise 流」への寄せ替え）は postgres と同時に変数が
増えて切り分け困難になるため、本移行では行わず別タスクに分離する。

## Consequences

ポジティブ:
- `asdf: not found` 警告が解消（mise shims は自己完結）。
- terraform が aqua の検証付きバイナリに、core ツールはビルド済みで高速・堅牢。
- `~/.default-npm-packages` を mise が読むため、グローバル npm CLI（claude-code, gemini-cli, codex 等）が自動で引き継がれた。
- `mise doctor` / `mise prune` / `mise upgrade` 等の組込コマンドで保守がシンプルに
  （自作の `scripts/asdf-doctor.sh` 541行を撤去、Makefile も `mise-*` ターゲットへ）。

ネガティブ / トレードオフ:
- postgres は backend に関わらず mise 自前ディレクトリに**再ビルド**が必要だった（asdf の installs は流用不可）。
- vfox/asdf プラグイン backend（yarn/poetry/postgres）は mise が新規受付を停止した区分で、将来は先細り方向。
- shims 運用では mise が config を自動 trust しないため、`mise prune` 等の前に
  **global config の trust が必要**（下記の落とし穴を参照）。

## 落とし穴（次にやる人へ）

1. **`mise prune` は trust 前に現役ツールを削除対象と誤検出する**。shims 運用では global
   `~/.tool-versions` が untrusted のままで prune が「参照ゼロ」と誤判定する。
   先に `mise trust ~/.tool-versions` を実行すること。
2. **移行中の `brew bundle dump` は npm-g を破壊しうる**。dump は「現在アクティブな node の
   npm グローバル」を取り込むため、ランタイム切替の最中は別 node（Homebrew 版等）の globals を
   拾って Brewfile の npm 行を誤改変する。移行時は dump を全面採用せず差分だけ外科的に編集する。
3. **古いバージョンの python は attestation 検証で失敗する**。
   `No GitHub artifact attestations found for python@3.11.9` は
   `MISE_PYTHON_GITHUB_ATTESTATIONS=false mise install python@3.11.9` で回避（当該バージョンのみ）。
4. **エイリアスは自動解決**。`.tool-versions` の `golang`/`nodejs` は mise が `go`/`node` として
   解決するため改名不要。ただし `mise registry nodejs` のような直接参照はエラーになる。
5. `~/.asdf` の実体削除はベークイン期間後の残タスク（`rm -rf ~/.asdf`）。postgres 等の再ビルドが必要になる点に留意。

## 設定の所在

- shell 統合: `.zprofile` の `setup_mise()`
- 週次プラグイン更新: `.zsh/zinit_setup.zsh`
- 棚卸し/診断: `Makefile` の `mise-doctor` / `mise-prune` / `mise-sync`
- 運用 how-to: `docs/managed-tools.md` のパッケージ管理セクション
- パッケージ管理: machine-management の `Brewfile`（`brew "mise"`）、作業履歴は同 `docs/log/2026-06-07-asdf-to-mise.md`
