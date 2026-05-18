# 0006. Ghostty native bell と tmux hook の二重発火を tmux hook 一本化で解消

Date: 2026-05-18
Status: accepted

## Context

ADR 0001 では「Ghostty 1.3.1 の `bell-features = audio` が当環境で silent
NoOp になる」という診断に基づき、tmux `alert-bell` hook + `bell.sh` + `afplay`
経路を bell サウンドの代替として採用した。

その後 2026-05-18 のセッションで、ユーザーが「Claude Code の window から別の
window へ移ると音が鳴る」現象を報告。調査の結果:

- macOS ログに `ghostty: (AudioSession) ... setPlayState Started` が記録され、
  **Ghostty 自身が audio bell を再生していた**
- Ghostty のプロセス PID が ADR 0001 診断時の 13185 から 4083 に変わって
  おり、**Ghostty が再起動されていた**
- 再起動後の Ghostty では `bell-features = audio` が正常に鳴る

つまり ADR 0001 の「silent NoOp」は Ghostty の恒久的バグではなく、**当時の
起動インスタンス固有**の状態だった。真因は「`bell-features` / `bell-audio-path`
の変更は SIGUSR2 リロードでは audio engine に反映されず、Ghostty のフル再起動
が必要」だった可能性が高い（ADR 0001 では SIGUSR2 リロードのみで判定して
いた）。

結果として現在、**Ghostty native audio bell** と **tmux alert-bell hook** の
両方が有効化された状態になっていた。1 つの BEL を背景 window から発火させる
実測テストで、`bell.sh` (Funk.aiff) と Ghostty native (Hero.aiff) が同一
タイムスタンプ (13:31:44) でほぼ同時発火する**二重ベル**を確認した。

なお Ghostty の音声出力先はシステム既定の `BlackHole2ch`（Existential Audio
Inc. の仮想オーディオデバイス）であり、物理出力との関係はユーザーの
オーディオルーティング構成に依存する。

## Options Considered

### Option 1: tmux hook (bell.sh) に一本化し、Ghostty 側 audio を無効化
- Pros: `bell.sh` は `afplay` 直叩きで安定。SIGUSR2 リロードで反映されない
  問題が無い。クロスプラットフォーム (macOS/Linux/WSL/FreeBSD)。BlackHole
  ルーティングに依存しない。ADR 0001 の結論（tmux hook 採用）と一貫。
- Cons: Ghostty のネイティブ機能を使わない。Ghostty 単体（tmux 外）利用時は
  bell サウンドが鳴らない。

### Option 2: Ghostty native (bell-features=audio) に一本化し、tmux hook を撤去
- Pros: 端末本来の bell 機能に任せる素直な構成。`bell.conf`/`bell.sh` を
  削除でき構成がシンプルに。
- Cons: `bell-features` 変更にフル再起動が必要という運用上の脆さ。出力先が
  BlackHole 依存。macOS 専用でクロスプラットフォーム性を失う。ADR 0001 の
  採用判断を覆すことになる。

### Option 3: 両方残す（二重ベルを許容）
- Pros: 設定変更不要。
- Cons: 背景 window の BEL で Funk と Hero が同時に鳴り、明らかにノイズ。
  ユーザー体験として不適切。

## Decision

**Option 1** を採用。tmux hook (`bell.sh`) に一本化し、Ghostty 側の audio
bell を無効化する。

具体的な変更:

- Ghostty config (`~/Library/Application Support/com.mitchellh.ghostty/config`)
  の `bell-features = audio` を `bell-features = no-audio` に変更
  - `attention`（Dock バウンス）と `title`（🔔 プレフィックス）は視覚通知
    なので残す（音は出ないため二重にならない）
  - `no-audio` は Ghostty のデフォルト bell-features と同値
- この変更は SIGUSR2 リロードでは反映されないため、次回 Ghostty フル再起動
  で有効化される
- tmux 側 (`bell.conf` / `bell.sh`、commit 4b55b91) は変更なし

ADR 0001 は **superseded にしない**。tmux hook を採用するという結論は本 ADR
でも維持されるため。ADR 0001 の Context にある「silent NoOp」の理解は本 ADR
で補正されたものとして扱う。

## Consequences

- ✅ 二重ベルが解消され、bell サウンドは `bell.sh` 経由の 1 回のみになる
- ✅ bell 経路が単一になり、音源・音量・OS 分岐の管理が `bell.sh` に一元化
  される
- ✅ Ghostty の `bell-features` 変更にフル再起動が必要という脆さを回避できる
- ⚠️ 設定変更は次回 Ghostty 再起動まで反映されない。それまでは二重ベルが
  継続する（実害は軽微）
- ⚠️ Ghostty を tmux 無しで単体利用する場合は bell サウンドが鳴らない
  （`attention`/`title` の視覚通知は残る）
- ⚠️ Ghostty config はこのマシンのローカルファイルで dotfiles リポジトリ
  管理外。新マシンでは同じ変更を手動で行う必要がある（ADR 0003 の
  `claude_files` 機構は `~/.claude/` 用で Ghostty config は対象外）

## AI-DLC Impact

- Agent Loop への影響: 無し
- Spec 変更の要否: 無し。本 ADR は ADR 0001 の運用知見を補正する位置づけで、
  spec レベルの変更は伴わない
