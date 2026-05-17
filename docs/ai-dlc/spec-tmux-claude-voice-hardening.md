---
type: ai-dlc-quick-spec
version: "1.0"
status: draft
created: 2026-03-29
story-count: 5
---

# Quick Spec: tmux Claude Voice Hardening

## Problem & Goal

### Problem Statement

tmux Claude Voice ステータスシステムの深層分析により、以下の構造的問題が発見された:

1. **PreToolUse 残骸**: コミット 953fad3 で handler から PreToolUse 処理を除去したが、`settings.json` に hook 登録が残存。ツール使用のたびに無駄なシェルプロセスが起動される
2. **PANE_KEY デリミタ破壊 (Bug B)**: セッション名にアンダースコアを含む場合（例: `my_session`）、`_` デリミタによる逆引きが破壊され、ゴミ tmux option が蓄積する
3. **セッション間アイコン衝突 (Bug A)**: アイコンキーが `@claude_voice_icon_{window_index}` のみで、複数セッションの同一 window index が上書き合いになる
4. **集約ロジック 4 重重複 (Bug C)**: 同じ「ステータス集約→アイコン設定」ロジックが 4 箇所に散在し、保守性が低い
5. **ハング未検出**: Claude Code がハングした場合、Busy ⚡ アイコンが永続。タイムスタンプは記録されているが staleness 判定ロジックがない

### Goal

tmux Claude Voice ステータスシステムの堅牢性・保守性を向上させ、エッジケースでの正常動作を保証する。

### Success Criteria

- [ ] アンダースコア含むセッション名で polling cleanup が正常動作する
- [ ] 複数 tmux セッションでウィンドウアイコンが正しく独立表示される
- [ ] PreToolUse による不要なプロセス起動がゼロになる
- [ ] 集約ロジックが単一関数に統一される
- [ ] Busy 状態が N 分以上続いた場合に視覚的フィードバックがある
- [ ] 既存の統合テスト (`integration_test.sh`) が全パスする

## Solution Overview

### Technical Approach

1. **デリミタ安全エンコーディング**: `PANE_KEY` の生成を `session:window.pane` → `session__window_pane` に変更（セッション名のアンダースコアとデリミタを区別可能にする）
2. **セッション対応アイコンキー**: `@claude_voice_icon_{session}_{window}` 形式に変更し、`window-status-format` で `#{session_name}` を活用
3. **共通集約関数**: `aggregate_window_icon()` を 1 箇所に定義し、全スクリプトから参照
4. **Staleness 判定**: polling_monitor に `hooks_ts` の経過時間チェックを追加

### Key Decisions

| 決定 | 選択 | 理由 |
|------|------|------|
| デリミタ方式 | `__` (double underscore) でセッション名を区切り | 単一 `_` は window/pane 区切りに使用。セッション名に `__` が含まれる可能性は極低 |
| 共通関数の配置先 | `functions.sh` に追加 | 既存の共通関数群と同居。新ファイル作成不要 |
| ハング閾値 | 10 分（tmux option で設定可能） | 長時間タスクの誤検出を避けつつ、ハングを現実的に検知 |
| ハング時アイコン | `⚠` (U+26A0, 3 bytes UTF-8) | 既存 `head -c 3` と互換。Busy と区別可能 |

### Out of Scope

- polling_monitor での `pane_current_command` フォールバック追加（現状 hooks が主要経路のため影響小）
- 音声フィードバック系の変更
- panning_engine / ollama_utils の変更
- 新しい Hook イベント（PostToolUse 等）の追加

## Stories

### Story 1: PreToolUse 残骸削除とドキュメント整合性修正

**Size: S** (1 Concern: dead code/config removal, 2 files)

| Element | Content |
|---------|---------|
| **Context** | コミット 953fad3 で `status-update.sh` から PreToolUse ハンドラを除去し、`setup-hooks.sh` からも除去済み。しかし `~/.claude/settings.json` に PreToolUse hook 登録が残存し、ドキュメント `07-hooks-state-machine.md` にも PreToolUse の遷移が記載されたまま |
| **Current Behavior** | ツール使用のたびに `status-update.sh` が起動され、JSON パース後にタイムスタンプのみ更新して終了する（無駄なオーバーヘッド） |
| **Expected Behavior** | `settings.json` から PreToolUse エントリが削除され、不要なプロセス起動が発生しない。ドキュメントがコード実装と一致する |
| **Constraints** | `settings.json` は手動管理ファイルのため、`setup-hooks.sh` の再実行では修正されない。直接編集が必要 |
| **Verification** | 1. `settings.json` に PreToolUse が存在しないこと (`jq '.hooks.PreToolUse' < ~/.claude/settings.json` が null) 2. `07-hooks-state-machine.md` に PreToolUse の記載がないこと 3. ツール使用時にログに PreToolUse イベントが記録されないこと |

### Story 2: PANE_KEY デリミタ安全エンコーディング

**Size: M** (1 Concern: key encoding safety, 3-4 files)

| Element | Content |
|---------|---------|
| **Context** | `PANE_KEY` は `${PANE_TARGET//[:\.]/_}` で生成される。セッション名 `my_session:0.1` → `my_session_0_1` となり、`polling_monitor.sh` の逆引き `${pane_id%%_*}` で session=`my` に誤分解される |
| **Current Behavior** | アンダースコア含むセッション名の場合: (1) クリーンアップの pane_target 再構築が失敗 (2) ゴミ tmux option が蓄積 (3) アイコン再集約も不正 |
| **Expected Behavior** | セッション名にアンダースコアが含まれても、キーから正確に `session:window.pane` を再構築できる。デリミタ変更: セッション区切りに `__` (double underscore)、window/pane 区切りに `_` (single) を使用 → `my_session__0_1` |
| **Constraints** | 既存の tmux option キーとの互換性を考慮。デリミタ変更時に古い形式のキーが残存する可能性がある → マイグレーション or 起動時クリーンアップが必要。`status-update.sh` と `polling_monitor.sh` の両方で同一エンコーディングを使用すること |
| **Verification** | 1. セッション名 `test_session` を作成し、Claude Code 起動 → ステータス更新 → クリーンアップが正常動作 2. 通常セッション名（アンダースコアなし）で回帰がないこと 3. `polling_monitor.sh status` で正しいペインターゲットが表示されること |

### Story 3: 集約ロジック統一

**Size: M** (1 Concern: code deduplication, 3 files)

| Element | Content |
|---------|---------|
| **Context** | 同一の「ウィンドウ内全ペインステータスを集約しアイコンを設定する」ロジックが 4 箇所に存在: (1) `status-update.sh:aggregate_window_icon()` (2) `polling_monitor.sh:update_claude_status_icon()` (3) `polling_monitor.sh` クリーンアップ内インライン (4) `polling_monitor.sh` 自動登録内インライン |
| **Current Behavior** | 4 箇所が独立して同じロジックを実装。修正漏れ・動作差異のリスクがある |
| **Expected Behavior** | `functions.sh` に `aggregate_window_icon()` を定義し、4 箇所すべてがこの関数を呼び出す。`status-update.sh` は `functions.sh` を source する（現状は独自の `_log` 関数のみ） |
| **Constraints** | `status-update.sh` は `set -euo pipefail` で動作。`functions.sh` の source が失敗してもフォールバック動作すること。関数シグネチャ: `aggregate_window_icon <pane_target>` を維持 |
| **Verification** | 1. `grep -r "grep.*Busy\|grep.*Waiting" .tmux/claude/` で集約ロジックのインライン実装が `functions.sh` のみに存在すること 2. 統合テスト (`integration_test.sh`) の hooks ステータス更新テストが全パス 3. 複数ペイン環境でアイコン優先度（Waiting > Busy > Idle）が正しく動作すること |

### Story 4: セッション対応アイコンキー

**Size: M** (1 Concern: multi-session correctness, 3 files)

| Element | Content |
|---------|---------|
| **Context** | アイコンキーが `@claude_voice_icon_{window_index}` のみ。複数 tmux セッションで同一 window index を持つ場合、後から集約が走ったセッションの値で上書きされる |
| **Current Behavior** | セッション `main` (window 0, Busy ⚡) とセッション `work` (window 0, Idle ✅) が存在すると、`@claude_voice_icon_0` が上書きされ両セッションで同じアイコンが表示される |
| **Expected Behavior** | アイコンキーを `@claude_voice_icon_{session}_{window}` に変更。`window-status-format` を `#(tmux show-option -gqv '@claude_voice_icon_#{session_name}_#I' ...)` に更新し、セッションごとに独立したアイコン表示を実現 |
| **Constraints** | tmux の `window-status-format` 内で `#{session_name}` が `#()` コマンド内で正しく展開されることを事前検証すること。Story 2（デリミタ改善）および Story 3（集約統一）完了後に実装すること（依存関係） |
| **Verification** | 1. 2 つの tmux セッション（同一 window index）で異なる Claude Code ステータスを持たせ、各セッションで正しいアイコンが表示されること 2. 単一セッション環境で回帰がないこと 3. `tmux show-options -g \| grep @claude_voice_icon_` でセッション名付きキーが確認できること |

### Story 5: Busy ハング検出と警告表示

**Size: M** (1 Concern: liveness monitoring enhancement, 2-3 files)

| Element | Content |
|---------|---------|
| **Context** | Claude Code がハング（無限ループ、デッドロック）した場合、`pane_title` に "Claude Code" が残り続け、Hooks も Stop/SessionEnd を発火しないため、Busy ⚡ アイコンが永続する。`@claude_voice_hooks_ts_*` にタイムスタンプが記録されているが、staleness 判定ロジックが存在しない |
| **Current Behavior** | ハング時に Busy ⚡ が永久に表示され続け、ユーザーが気づかない |
| **Expected Behavior** | `polling_monitor.sh` が Busy 状態のペインの `hooks_ts` を検査し、設定値（デフォルト 10 分、`@claude_voice_stale_threshold`）を超過した場合にアイコンを `⚠` に変更する。tmux display-message で警告も表示する |
| **Constraints** | 通常の長時間タスク（大規模リファクタ等）を誤検出しないよう、閾値は十分に大きくする。ハング警告はあくまで情報提供であり、自動 kill 等の破壊的アクションは行わない。`⚠` (U+26A0) は 3 bytes UTF-8 で `head -c 3` と互換 |
| **Verification** | 1. テスト: `@claude_voice_hooks_ts` を 11 分前に手動設定 → polling 実行 → アイコンが `⚠` に変化すること 2. テスト: `@claude_voice_hooks_ts` を 5 分前に設定 → polling 実行 → アイコンが `⚡` のままであること 3. `@claude_voice_stale_threshold` を変更して閾値カスタマイズが機能すること 4. ハング警告後に Claude Code が復帰（Stop イベント発火）した場合、正常に Idle ✅ に遷移すること |

## Agent-Ready Summary

| Story | Size | Status | Missing Elements | Dependencies |
|---|---|---|---|---|
| S1: PreToolUse 残骸削除 | S | **Ready** | None | なし |
| S2: PANE_KEY デリミタ改善 | M | **Ready** | None | なし |
| S3: 集約ロジック統一 | M | **Ready** | None | S2 完了後 |
| S4: セッション対応アイコンキー | M | **Needs Revision** | `#{session_name}` の `#()` 内展開の事前検証が必要 | S2, S3 完了後 |
| S5: Busy ハング検出 | M | **Ready** | None | S3 完了後（集約関数を使用するため） |

### 実行順序

```
S1 (独立)  ──┐
S2 ─────────→ S3 ─────→ S4
                  └────→ S5
```

S1 は独立して即実行可能。S2 → S3 → S4 は依存チェーン。S5 は S3 完了後に並行実行可能。

## Next Steps

- [ ] Review and refine stories (especially S4 の `#{session_name}` 展開検証)
- [ ] Record significant design choices via `/ai-dlc:decision`
- [ ] Create GitHub Issues: `python3 plugins/ai-dlc/scripts/helpers/spec_to_issues.py --spec docs/ai-dlc/spec-tmux-claude-voice-hardening.md --milestone "Sprint 2026-04-05" --dry-run`
- [ ] Run `/ai-dlc:plan` to integrate into sprint planning
- [ ] Run `/ai-dlc:drive #N` to execute Agent Loop for ready stories
