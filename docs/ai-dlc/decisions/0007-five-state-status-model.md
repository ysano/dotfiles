# 0007. Claude Code ステータスを 5 状態モデルに再設計

Date: 2026-05-18
Status: accepted

## Context

tmux Claude Voice のウィンドウアイコンは当初 3 状態 (Busy/Waiting/Idle、加えて
時間ベースの Stale) だったが、運用で以下の問題が判明した:

1. **⌛ Waiting の意味喪失**: `determine_status` が `idle_prompt` と
   `permission_prompt` を両方 "Waiting"(⌛) にまとめていた。`idle_prompt` は
   「セッションがアイドル」というだけの通知で、放置すればどのセッションでも
   発火する。結果、時間が経つと全ウィンドウが ⌛ になり「要対応」の意味を
   失っていた。実際にユーザーが「Window 1 の ⌛ は何を待っているのか」と
   疑問を持った時点で、3 時間前の `idle_prompt` 由来の stale な Waiting が
   残っているだけだった。

2. **⚠ Stale の的外れな定義**: ⚠ は「Busy のまま hooks_ts が閾値(10分)超過」
   という時間ベース判定だった。しかし長時間タスクを誤検出する一方、本当に
   見るべき「Claude Code が継続不能エラーを返した状態」は捉えていなかった。

3. **AskUserQuestion がアイコンに反映されない**: 最も対応が必要な
   AskUserQuestion ダイアログは dialog_detector が音を鳴らすだけで
   ペイン状態を更新せず、アイコンが ⌛/❓ にならなかった。対応不要な
   `idle_prompt` が ⌛ になり、対応必須の AskUserQuestion がアイコンに
   出ないという逆転が起きていた。

## Options Considered

### Option 1: 3 状態のまま (Busy/Waiting/Idle)
- Pros: 変更不要。
- Cons: 上記の問題が残る。⌛ が意味を失い続ける。

### Option 2: 4 状態 — 「要対応」を 1 アイコンに集約
- permission_prompt + AskUserQuestion を 1 つの「要対応」アイコンに。
- Pros: シンプル。
- Cons: 「ツール承認待ち」と「質問・選択待ち」は対応の種類が違うのに
  区別できない。

### Option 3: 5 状態 — permission と question を別アイコンに分離
- ⚡ Busy / ⌛ Permission / ❓ Question / ✅ Idle / ⚠ Error
- Pros: アイコンが状態の「種類」を一意に表す。何の対応が必要かが一目で
  わかる。
- Cons: 状態数が増え、実装箇所 (集約・hook・dialog 検出・エラー検出) が
  増える。

## Decision

**Option 3** を採用。5 状態モデルとする。

| 状態 | アイコン | 意味 | 設定経路 |
|------|---------|------|---------|
| Busy | ⚡ | 作業中 | UserPromptSubmit hook / pane_title スピナー |
| Permission | ⌛ | ツール承認待ち | Notification permission_prompt hook |
| Question | ❓ | 質問・選択待ち | dialog_detector (AskUserQuestion) |
| Idle | ✅ | アイドル | Stop / SessionStart hook / idle_prompt |
| Error | ⚠ | 継続不能エラー | error_detector (pane 内容スキャン) |

主要な設計判断:

1. **アイコンの意味は「種類」**であって緊急度ではない。permission と
   AskUserQuestion を別アイコンにしたのは「何の対応か」を区別するため。
   - permission_prompt → ⌛ (承認を待っている)
   - AskUserQuestion → ❓ (Claude が質問している) — 「質問＝❓」が直感的

2. **`idle_prompt` は Idle に降格**。`idle_prompt` は「ただアイドル」であり
   対応不要。⌛ は `permission_prompt` (真にツール承認待ち) 専用にする。

3. **集約優先度**: Error > Permission > Question > Busy > Idle。
   1 ウィンドウに複数ペインがある場合、最も上位の状態のアイコンを表示。

4. **⚠ Error は時間ベース判定を撤去**し、「Claude Code が返す継続不能
   エラー」の検出に再定義。検出は pane 内容スキャン (実画面採取の文言):
   - `API Error: <code>` (API障害)
   - `Claude usage limit reached` / `monthly usage limit` (Usage超過)
   - `unable to respond to this request` (Usage Policy違反)
   API障害型のみ status.claude.com を 5 分キャッシュで参照し、Anthropic
   側インシデントの有無を通知に反映する。

実装は 2 コミットに分割: 5 状態モデル本体 (functions.sh / status-update.sh /
dialog_detector.sh / polling_monitor.sh) と、error_detector.sh の新規追加。

## Consequences

- ✅ アイコンが状態の種類を一意に表し、「⌛ が何を意味するか分からない」
  問題が解消。⌛ は真の「ツール承認待ち」だけを指す
- ✅ ⚠ が実際の継続不能エラー (API障害/Usage超過) を表すようになり、
  時間ベースの誤検出がなくなった
- ✅ AskUserQuestion が ❓ としてアイコンに反映される
- ⚠️ 状態を設定する経路が 4 つに分散 (hook / pane_title 補正 / dialog 検出 /
  エラー検出)。各経路が自分の担当状態のみ触るよう `correct_status_from_title`
  にガードを入れたが、経路間の責任分担の理解が今後の保守に必要
- ⚠️ エラー検出は pane 内容ベースのため、エラー文言を会話で引用すると
  誤検出する (エラー検出について議論しているセッション等)。
  `TMUX_CLAUDE_ERROR_DETECT_DISABLED=true` で回避可能
- ⚠️ エラー文言・スピナー文字・AskUserQuestion footer 等は Claude Code の
  バージョンアップで変わり得る。変わると検出が壊れるが、いずれも
  graceful degradation (アイコンが出ないだけ) で他機能に影響しない
- ⚠️ `@claude_voice_stale_threshold` (旧 Stale 用 tmux option) は未使用に
  なった。claude.conf から削除してよいが実害はないため残置

## AI-DLC Impact

- Agent Loop への影響: 無し
- Spec 変更の要否: `spec-tmux-claude-voice-hardening.md` の S5「Busy ハング
  検出」は時間ベースの ⚠ を前提としていたが、本 ADR で ⚠ の意味を再定義
  したため S5 は実質置き換えられた。spec は当時のスナップショットとして
  残し、本 ADR を上位の判断記録とする (ADR 0004 の運用方針に従う)。
  ADR 0005 (dialog 検出) の延長線上にある決定。
