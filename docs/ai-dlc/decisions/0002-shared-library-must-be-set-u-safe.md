# 0002. 共有シェルライブラリは set -u 互換 (`${VAR:-}`) を必須とする

Date: 2026-05-17
Status: accepted

## Context

`~/.tmux/claude/hooks/status-update.sh` は `set -euo pipefail` で実行され、
冒頭で `source ~/.tmux/claude/functions.sh 2>/dev/null || true` する設計
だった。しかし functions.sh の冒頭行が:

```bash
[[ -n "$_CLAUDE_FUNCTIONS_LOADED" ]] && return 0 2>/dev/null
```

となっており、初回 source 時に `_CLAUDE_FUNCTIONS_LOADED` が未定義のため
`set -u` 違反で bash がその場で abort していた。`|| true` も `2>/dev/null` も
`set -u` のシェル終了を抑止できないため、Claude Code の 5 イベント
（UserPromptSubmit / Notification / Stop / SessionStart / SessionEnd）の hook
全てが**サイレント不発**となり、tmux ペインのステータス更新も TTS も数ヶ月
規模で無音化していた。

問題に気付かなかった理由として:

- `@claude_voice_icon_*` に古い値が残っていたため表面的には Claude Voice 統合
  が動作しているように見えた
- log file は `$TMPDIR/tmux-claude-voice.log` で macOS の launchd により周期
  クリアされるため、不発の痕跡が残らなかった

## Options Considered

### Option 1: 修正対象は functions.sh の該当 2 行のみ（局所修正）
- Pros: 影響範囲が小さく、テストも単純。
- Cons: 同じ罠を別の共有 lib で踏む可能性が残る。今後 sound_utils.sh,
  panning_engine.sh などを set -u 環境から source する際に再発リスク。

### Option 2: 呼び出し側で `set +u` → source → `set -u` のサンドイッチパターン
- Pros: 共有 lib 側を改修せずに済む。
- Cons: 呼び出し箇所が増えるたびに同じ防御を書く必要があり、漏れる。lib 側の
  防御責任を呼び出し側に押し付ける逆転。

### Option 3: 共有 lib 側で `${VAR:-}` 等の防御を一律必須とする規約化
- Pros: lib 単体で `set -euo pipefail` 配下でも安全に source 可能。呼び出し
  箇所が増えても自然に守られる。dotfiles のクロスプラットフォーム原則
  （グレースフル劣化）とも整合的。
- Cons: 既存 lib を一度全件確認するコストがかかる。新規 lib 追加時のレビュー
  観点として明示する必要がある。

## Decision

**Option 3** を採用し、本決定を ADR 化して dotfiles の運用規約に組み込む。

具体的なルール:

1. `~/.tmux/claude/` 配下および dotfiles 内のシェル共有ライブラリは、
   トップレベルで参照する変数すべてに `${VAR:-}` / `${VAR:-default}` /
   `${VAR-}` のいずれかでフォールバック値を付与する
2. 関数内も同様に、呼び出し側のスコープから来る環境変数は防御参照する
3. 多重 source 防止 sentinel は `[[ -n "${_LOADED_FLAG:-}" ]] && return 0`
   形式を必須とする
4. Code review および dotfiles-engineer/dotfiles-validator subagent 利用時は
   この観点を check item に含める

今回 functions.sh:6 と :21 を修正してこの規約を満たした。

## Consequences

- ✅ 共有 lib を `set -euo pipefail` 環境から安全に source できることが保証
  される
- ✅ サイレント不発の再発が構造的に防止される（呼び出し側の `|| true` に頼ら
  ない）
- ⚠️ 新規 lib 追加時にレビュアー（自分 or subagent）が見落とすと再発し得る
  → 規約として ADR/CLAUDE.md に明記し、validator skill で機械的に検知できる
  方向で将来補強する余地あり
- ⚠️ 既存 lib (sound_utils.sh, panning_engine.sh, ollama_utils.sh 等) の
  set -u 互換性は未確認。必要に応じて棚卸しが必要

## AI-DLC Impact

- Agent Loop への影響: 無し（コーディング規約の追加であり、Agent Loop 構造に
  影響しない）
- Spec 変更の要否: `spec-tmux-claude-voice-hardening.md` の S3 (集約ロジック
  統一) の Constraints に「functions.sh の source が失敗してもフォールバック
  動作すること」と記載があったが、これは「fallback よりも source 成功の保証」
  にシフトする。Spec の事後更新までは行わず、本 ADR を上位の規約として参照
  する運用とする
