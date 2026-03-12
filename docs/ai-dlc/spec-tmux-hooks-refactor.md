---
type: ai-dlc-quick-spec
version: "1.0"
status: draft
created: 2026-03-13
story-count: 3
---

# Quick Spec: tmux-hooks-refactor

tmux-claude-voice 監視システムから capture-pane ポーリングを廃止し、Claude Code hooks 駆動 + 軽量 liveness check アーキテクチャへ移行する。

## Problem & Goal

### Problem

現行の tmux-claude-voice は 5 秒間隔で `capture-pane`（ペイン全画面テキスト取得 + 正規表現マッチ）を全 Claude Code ペインに対して実行する。CC を 7 ペイン並行稼働する環境では毎回 7 回の capture-pane が走り、tmux status-right の描画コストが高い。

hooks 機構は設計・実装済みだが未有効化で、ポーリングとの二重パスが複雑性を増している。

### Goal

- `capture-pane` によるステータス判定を **完全廃止**（TTS 要約用の 1 回を除く）
- Claude Code hooks がすべてのステータス遷移を駆動
- ポーリングは「登録済みペインの生存確認（pane_title チェック）」のみに縮小
- hooks 未設定環境では監視機能は無効（後方互換は維持しない）

### Success Criteria

- `polling_monitor.sh` から `analyze_pane_content()` 呼び出しが 0 件
- `detect_claude_panes()` から capture-pane 呼び出しが 0 件
- hooks 駆動でステータス遷移が正確に動作（Busy/Waiting/Idle）
- 異常終了した CC ペインのアイコンが自動クリアされる

## Solution Overview

### Technical Approach

**Before（現行）:**

```
status-right (5s interval)
  → polling_monitor.sh
    → detect_claude_panes()        ← pane_title + cmd + capture-pane
    → process_claude_pane_polling() ← hooks ts check → capture-pane fallback
    → update_claude_status_icon()
```

**After（目標）:**

```
[Claude Code hooks]
  → status-update.sh              ← ステータス遷移を直接 push
    → tmux option に保存
    → アイコン集約

[status-right (5-15s interval)]
  → polling_monitor.sh
    → liveness_check()             ← 登録済みペインの pane_title 確認のみ
    → cleanup stale registrations
    → auto-register unregistered CC panes (safety net)
```

### Key Decisions

| 決定 | 理由 |
|---|---|
| hooks 必須（後方互換なし） | コード複雑性の大幅削減。setup-hooks.sh 実行を前提とする |
| capture-pane は TTS 要約のみ残す | Stop イベント時に 1 回だけ。Ollama 要約のソースとして必要 |
| detect_claude_panes() は診断用に残す | `polling_monitor.sh status` コマンドで使用。node フォールバックは除去 |
| 未登録 CC ペインの自動登録 | hooks 障害時のセーフティネット。title="Claude Code" で未登録なら Idle で登録 |

### Out of Scope

- TTS / Ollama 要約機能の変更
- 音声フィードバック（sound_utils.sh）の変更
- window_name.sh / system_metrics.sh の変更
- claude.conf のキーバインド変更
- status-interval の変更（別 Story で検討可）

## Stories

### Story 1: polling_monitor.sh — Hooks-only liveness architecture

**Size: M (2-4h)**

**Context:**
`polling_monitor.sh` は `status-right` から 5 秒間隔で呼び出され、Claude Code ペインの検出とステータス判定を行う。現在は `detect_claude_panes()` → `process_claude_pane_polling()`（hooks ts チェック → capture-pane フォールバック）の二重パスで動作している。

**Current Behavior:**
- `polling_monitor_main()` が `detect_claude_panes()` で全ペインをスキャンし、各ペインに対して `process_claude_pane_polling()` を実行
- `process_claude_pane_polling()` は hooks タイムスタンプが 30 秒以内なら hooks 状態を信頼、超過時は `analyze_pane_content()`（capture-pane）にフォールバック
- `analyze_pane_content()` が `capture-pane` + 正規表現で Busy/Waiting/Idle を判定

**Expected Behavior:**
- `polling_monitor_main()` を以下のフローに書き換え:
  1. `tmux show-options -g` から登録済み `@claude_voice_pane_status_*` を列挙
  2. 各登録済みペインについて `tmux list-panes` の `pane_title` で生存確認
  3. title に "Claude Code" を含まない → `cleanup_stale_pane_statuses()` でクリア
  4. `tmux list-panes` で title="Claude Code" かつ未登録のペインがあれば Idle で自動登録（セーフティネット）
  5. 全ウィンドウのアイコンを再集約
- `process_claude_pane_polling()` を削除（または liveness check 用に大幅簡素化）
- hooks 未設定時（`@claude_voice_enabled != true`）は従来通り早期 return

**Constraints:**
- `update_claude_status_icon()`, `cleanup_stale_pane_statuses()`, `clear_all_claude_icons()` は再利用
- `show_pane_status()` 診断コマンドは `analyze_pane_content()` を直接呼んでよい（手動実行のみ）
- `trigger_audio_feedback()` はポーリングから除去（hooks 側で発火するため）
- 対象ファイル: `polling_monitor.sh` のみ

**Verification:**
- [ ] `polling_monitor.sh` 内に `analyze_pane_content` 呼び出しが 0 件（`status` サブコマンド除く）
- [ ] CC 起動 → hooks で Busy/Idle 遷移 → window icon が正しく更新される
- [ ] CC を kill -9 → 次回ポーリングで icon がクリアされる
- [ ] hooks 未登録の CC ペインが自動的に Idle で登録される
- [ ] `polling_monitor.sh status` が引き続き動作する

---

### Story 2: functions.sh — detect_claude_panes() capture-pane 除去

**Size: S (1-2h)**

**Context:**
`detect_claude_panes()` は 3 段階の優先度で CC ペインを検出する: (1) pane_title, (2) cmd=="claude", (3) cmd=="node" + capture-pane。第 3 段階が唯一残る capture-pane 呼び出し。

**Current Behavior:**
- `cmd == "node"` の場合、`capture-pane` で先頭 50 行を取得し、Claude Code 特有のパターン（`tokens.*esc to interrupt` 等）で判定
- この分岐は「Claude Code が node プロセスとして間接起動される」ケースへの対応

**Expected Behavior:**
- `cmd == "node"` の capture-pane フォールバック分岐を削除
- 検出は `pane_title` と `cmd == "claude"` の 2 段階のみ
- `pane_title` が最優先で、`pane_current_command` がバージョン番号を返す問題（2.1.74）も回避済み

**Constraints:**
- `detect_claude_panes()` 自体は残す（`polling_monitor.sh status` 診断で使用）
- `analyze_pane_content()` は残す（診断用 + TTS 用途で参照可能性あり）
- 対象ファイル: `functions.sh` のみ

**Verification:**
- [ ] `functions.sh` 内の `capture-pane` 呼び出しが `analyze_pane_content()` 内の 1 箇所のみ
- [ ] `detect_claude_panes` が pane_title="✳ Claude Code" のペインを正しく検出
- [ ] `detect_claude_panes` が zsh ペインを誤検出しない
- [ ] `cmd == "node"` だが title に "Claude Code" を含まないペインが除外される

---

### Story 3: テスト・ドキュメント更新

**Size: S (1-2h)**

**Context:**
アーキテクチャが「Hooks + Polling ハイブリッド」から「Hooks 駆動 + Liveness Check」に変わるため、統合テストとドキュメントの更新が必要。

**Current Behavior:**
- `integration_test.sh` に capture-pane ベースのテストケースが含まれる
- `07-hooks-state-machine.md` に「30 秒フォールバック」の説明がある
- `01-system-overview.md` に「Hooks 駆動（主系統）+ ポーリング監視（副系統）」の構成図がある

**Expected Behavior:**
- `integration_test.sh`:
  - capture-pane ポーリングテストを liveness check テストに置き換え
  - hooks シミュレートテストは維持
  - 「未登録 CC ペインの自動登録」テストを追加
- `07-hooks-state-machine.md`:
  - 「30 秒フォールバック → capture-pane」の説明を「liveness check による生存確認」に更新
- `01-system-overview.md`:
  - アーキテクチャ図を更新（capture-pane パスを除去）

**Constraints:**
- 対象ファイル: `integration_test.sh`, `07-hooks-state-machine.md`, `01-system-overview.md`
- テストは tmux セッション外でもスキップ付きで通ること

**Verification:**
- [ ] `integration_test.sh` の全テストが PASS
- [ ] ドキュメント内に「capture-pane フォールバック」の記述が残っていない（TTS 用途の言及は可）
- [ ] アーキテクチャ図が新しいフローを正確に反映

## Agent-Ready Summary

| Story | Size | Status | Missing Elements |
|---|---|---|---|
| 1: polling_monitor.sh — Hooks-only liveness | M | Ready | None |
| 2: functions.sh — capture-pane 除去 | S | Ready | None |
| 3: テスト・ドキュメント更新 | S | Ready | None |

## Dependency Order

```
Story 2 (functions.sh)  ──┐
                          ├──▶ Story 3 (テスト・ドキュメント)
Story 1 (polling_monitor) ──┘
```

Story 1 と Story 2 は独立して実装可能。Story 3 は両方の完了後に実施。
