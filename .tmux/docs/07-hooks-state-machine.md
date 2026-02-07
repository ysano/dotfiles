# 07: Hooks ステートマシン

Claude Code hooks イベントによるペインステータス管理の状態遷移図。

## 状態一覧

| 状態 | アイコン | 意味 |
|---|---|---|
| `(none)` | — | Claude セッション未開始 / 終了済み |
| `Idle` | ✅ | 待機中（タスク完了後） |
| `Busy` | ⚡ | 処理中 |
| `Waiting` | ⌛ | ユーザー入力 / 許可待ち |

## 状態遷移図

```
                  SessionStart
                 ┌────────────┐
                 ▼            │
 ┌─────────────────────────────────────────────────────┐
 │                                                     │
 │   ┌────────┐  UserPromptSubmit   ┌────────┐        │
 │   │        │────────────────────▶│        │        │
 │   │  Idle  │      PreToolUse     │  Busy  │        │
 │   │   ✅   │◀────────────────────│   ⚡   │        │
 │   │        │        Stop         │        │        │
 │   └───┬────┘                     └───┬────┘        │
 │       │ ▲                            │             │
 │       │ │ Stop                       │             │
 │       │ │                            │             │
 │       │ │  Notification              │ Notification│
 │       │ │  (idle_prompt |            │ (idle_prompt│
 │       │ │   permission_prompt)       │  | perm..)  │
 │       ▼ │                            ▼             │
 │   ┌─────┴──┐  UserPromptSubmit   ┌────────┐       │
 │   │        │────────────────────▶│ (再入)  │       │
 │   │Waiting │      PreToolUse     │  Busy  │       │
 │   │   ⌛   │────────────────────▶│   ⚡   │       │
 │   │        │                     └────────┘       │
 │   └────────┘                                      │
 │                                                     │
 └──────────────────────┬──────────────────────────────┘
                        │ SessionEnd
                        ▼
                    ┌────────┐
                    │ (none) │  全 tmux option 削除
                    └────────┘
```

## 全遷移テーブル

### 遷移元: `(none)` — セッション未開始

| イベント | 遷移先 | 通知音 | TTS | 備考 |
|---|---|---|---|---|
| `SessionStart` | Idle | — | — | セッション開始 |
| `UserPromptSubmit` | Busy | start | — | 即座に作業開始 |

### 遷移元: `Idle` ✅ — 待機中

| イベント | 遷移先 | 通知音 | TTS | 備考 |
|---|---|---|---|---|
| `UserPromptSubmit` | Busy | start | — | ユーザーがプロンプト送信 |
| `PreToolUse` | Busy | start | — | ツール実行開始 |
| `Notification(idle_prompt)` | Waiting | waiting | — | 入力待ち通知 |
| `Notification(permission_prompt)` | Waiting | waiting | message 読み上げ | 許可要求 |
| `Stop` | ~~Idle~~ | — | — | 同一状態→スキップ |
| `SessionStart` | ~~Idle~~ | — | — | 同一状態→スキップ |
| `SessionEnd` | (none) | — | — | option 全削除 |

### 遷移元: `Busy` ⚡ — 処理中

| イベント | 遷移先 | 通知音 | TTS | 備考 |
|---|---|---|---|---|
| `UserPromptSubmit` | ~~Busy~~ | — | — | 同一状態→スキップ |
| `PreToolUse` | ~~Busy~~ | — | — | 同一状態→スキップ |
| `Notification(idle_prompt)` | Waiting | waiting | — | 処理完了→入力待ち |
| `Notification(permission_prompt)` | Waiting | waiting | message 読み上げ | ツール許可要求 |
| `Stop` | **Idle** | **complete** | **完了要約** | **主要遷移** |
| `SessionEnd` | (none) | — | — | option 全削除 |

### 遷移元: `Waiting` ⌛ — ユーザー待ち

| イベント | 遷移先 | 通知音 | TTS | 備考 |
|---|---|---|---|---|
| `UserPromptSubmit` | Busy | start | — | ユーザーが応答 |
| `PreToolUse` | Busy | start | — | 許可後ツール実行 |
| `Notification(idle_prompt)` | ~~Waiting~~ | — | — | 同一状態→スキップ |
| `Notification(permission_prompt)` | ~~Waiting~~ | — | — | 同一状態→スキップ |
| `Stop` | Idle | complete | 完了要約 | タスク完了 |
| `SessionEnd` | (none) | — | — | option 全削除 |

## 典型的なライフサイクル

### 1. 通常タスク（許可不要）

```
(none) ──SessionStart──▶ Idle ──UserPromptSubmit──▶ Busy ──Stop──▶ Idle
                          ✅         🔊 start         ⚡    🔊 complete  ✅
                                                            🗣 要約読み上げ
```

### 2. ツール許可が必要なタスク

```
Idle ──UserPromptSubmit──▶ Busy ──Notification(perm.)──▶ Waiting
 ✅        🔊 start         ⚡       🔊 waiting            ⌛
                                     🗣 許可内容読み上げ

Waiting ──UserPromptSubmit──▶ Busy ──Stop──▶ Idle
  ⌛          🔊 start          ⚡    🔊 complete  ✅
                                      🗣 要約読み上げ
```

### 3. 連続ツール実行（許可不要）

```
Idle ──Prompt──▶ Busy ──PreToolUse──▶ Busy ──PreToolUse──▶ Busy ──Stop──▶ Idle
 ✅    🔊start    ⚡     (skip)        ⚡     (skip)        ⚡   🔊complete  ✅
                                                                 🗣 要約
```

### 4. セッション終了

```
(任意の状態) ──SessionEnd──▶ (none)
                              tmux option 全削除
                              アイコン再集約
```

## 音声フィードバック詳細

### 通知音（`@claude_voice_sound_enabled == true`）

| 状態遷移 | 通知音タイプ | WSL デフォルト | macOS デフォルト |
|---|---|---|---|
| → Busy | `start` | chimes | Ping |
| → Waiting | `waiting` | chord | Funk |
| → Idle | `complete` | notify | Glass |
| エラー時 | `error` | ringout | Sosumi |

### TTS 読み上げ（`@claude_voice_summary_enabled == true`）

| 状態遷移 | 読み上げ内容 | ソース |
|---|---|---|
| → Waiting (permission_prompt) | ユーザーに期待する操作 | hooks JSON `.message` フィールド |
| → Idle (Stop) | 完了タスクの要約 | Ollama 要約 / キーワード抽出 |
| → Waiting (idle_prompt) | — (読み上げなし) | Stop の完了要約でカバー |
| → Busy | — (読み上げなし) | 通知音のみ |

## 重複排除ロジック

```
NEW_STATUS == PREV_STATUS → タイムスタンプのみ更新、音声・TTS スキップ
```

連続する `PreToolUse` イベントなど、同一ステータスへの遷移は
タイムスタンプ (`@claude_voice_hooks_ts_*`) のみ更新し、
音声フィードバックは発火しない。

## tmux option マップ

| option キー | 値の例 | 用途 |
|---|---|---|
| `@claude_voice_pane_status_{key}` | `Busy` / `Idle` / `Waiting` | ペイン単位の現在ステータス |
| `@claude_voice_status_{key}` | 同上 | 互換用（レガシー） |
| `@claude_voice_hooks_ts_{key}` | `1738934400` | 最終イベントの Unix タイムスタンプ |
| `@claude_voice_icon_{window}` | `⚡` / `⌛` / `✅` / `""` | ウィンドウレベル集約アイコン |

`{key}` = ペインターゲットの `.` `:` を `_` に置換（例: `0:1.1` → `0_1_1`）

### アイコン集約の優先度

ウィンドウ内に複数ペインがある場合、最も優先度の高いステータスのアイコンを表示:

```
⌛ Waiting > ⚡ Busy > ✅ Idle
```

## フォールバック: ポーリング監視

hooks タイムスタンプが **30 秒以上** 古い場合、`polling_monitor.sh` は
`tmux capture-pane` ベースの従来検出にフォールバックする。

```
hooks_ts が 30秒以内 → hooks を信頼、capture-pane スキップ
hooks_ts が 30秒超過 → capture-pane にフォールバック
hooks_ts が未設定   → capture-pane にフォールバック
```
