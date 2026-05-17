# 0001. tmux alert-bell hook を Ghostty bell の代替経路として採用

Date: 2026-05-17
Status: accepted

## Context

Ghostty 1.3.1 stable で `bell-features = audio` を有効化していたが、tmux 内
zsh から `tput bel` / `printf '\a'` で BEL を送出してもサウンドが鳴らない
事象を確認した。

Layer-by-layer の systematic-debugging で以下を確定:

- BEL は tmux を通り Ghostty 側にも到達している（PTY 直接注入でも同様、
  `title` feature の 🔔 prefix は付与される、`UserNotifications` 問い合わせも
  発火する）
- しかし Ghostty のログに AVAudio / AudioToolbox / CoreAudio 関連の活動が
  一切無く、`audio` も `system` も silent NoOp になっている
- `bell-audio-path` を `/System/Library/Sounds/` から user 領域コピーに変更し
  config reload しても挙動変わらず、サンドボックスや path 不可読の問題でも
  ないと判定
- `afplay` 単体および macOS `say` は正常に音を出すため、システム音声経路と
  ハードウェアは健全

設定ミスではなく Ghostty 1.3.1 の audio bell 実装に固有の問題と推定される。
upstream 修正を待つ間も bell サウンドを使えるようにしたい。

## Options Considered

### Option 1: Ghostty upstream の修正を待つ
- Pros: 設計上もっとも素直。Ghostty の `bell-features` 機能が本来の意図で
  動く。
- Cons: いつ修正されるか不明（GitHub Issues に明確な該当 issue は無く 1.3.1
  既知バグとしての記載なし）。それまで毎セッション無音で運用継続することに
  なる。

### Option 2: tmux `alert-bell` hook + `run-shell -b` + 専用 script で afplay を直叩き
- Pros: Ghostty を経由せず CoreAudio に直結するため確実。tmux server 起動中は
  常時有効、Ghostty のバージョンアップにも非依存。`bell.sh` を cross-platform
  にすれば Linux/WSL でも同じインターフェースが使える。
- Cons: tmux 外の bell（純粋な Ghostty / 他ターミナル単体）はカバーされない。
  Ghostty 側 `bell-features = audio` 設定は dormant のままになる。

### Option 3: ターミナルエミュレータの乗り換え（iTerm2 等）
- Pros: ベルがネイティブに動く可能性が高い。
- Cons: dotfiles の Ghostty 個別設定（フォント・キーバインド・blend mode 等）
  および macOS option-as-alt の挙動などを再構築する必要があり、影響範囲が
  大きい。bell 一点のためのコストとしては過剰。

## Decision

**Option 2** を採用する。`.tmux/bell.sh`（cross-platform: afplay/paplay/aplay/
canberra/powershell.exe）と `.tmux/bell.conf`（`monitor-bell` 明示 +
`alert-bell` hook 登録）を作成し、`.tmux.conf` から source する。

Ghostty side の `bell-features = audio` 設定は dormant にした状態で残置し、
将来 Ghostty が修正された際に tmux hook を外す or 共存させるかを再判断する。

## Consequences

- ✅ tmux 内のあらゆる BEL イベント（プロセス出力、コマンド完了通知 etc.）が
  確実に音になる。`TMUX_BELL_SOUND` 環境変数で音源を上書き、
  `TMUX_BELL_DISABLED=true` で停止できる柔軟性を確保
- ✅ Ghostty のバージョンに依存しない安定経路を確保
- ⚠️ tmux 外の素の Ghostty 利用時は依然として bell 不発火（実害は少ないが
  対称性を欠く）
- ⚠️ tmux server が常に動作している前提。tmux 使わない hot key 操作などには
  及ばない（個人運用上は問題なし）

## AI-DLC Impact

- Agent Loop への影響: 無し（運用レイヤの代替経路のため Agent Loop の構造に
  影響しない）
- Spec 変更の要否: 無し（`spec-tmux-claude-voice-hardening.md` の対象外。
  Claude Voice の bell サウンド統合とは別系統のためコンフリクトしない）
