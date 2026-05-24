# tmux Claude Voice トラブルシューティング

完了要約読み上げ・通知音・パンニング・エラー検出が動かない場合の調査手順。
詳細な設計背景は `docs/ai-dlc/decisions/0007-five-state-status-model.md` および
`docs/ai-dlc/decisions/0008-prevent-config-drift-and-silent-toggles.md` を参照。

## 症状別 一次切り分け

| 症状 | まず確認 |
|---|---|
| **完了要約が読み上げられない** | `summary_enabled` の runtime 値 |
| **通知音が鳴らない** | `sound_enabled` の runtime 値、`afplay` の動作 |
| **トグル後すぐに復旧したい** | `tmux source-file ~/.tmux/claude.conf` |
| **ペインアイコンが更新されない** | hook の発火、`pane_current_command` |
| **エラー (⚠) を誤検出する** | `TMUX_CLAUDE_ERROR_DETECT_DISABLED=true` で一時無効化 |

## 設定ドリフト疑い時の手順

`.tmux/claude.conf` には ON と書いてあるのに機能が止まっている — 多くの場合
**トグルバインド (`Prefix + v + v` 等) の誤操作で runtime option が上書きされている** こと
が原因。ADR 0008 で自動診断を追加済み。

### 1. ドリフトログの確認

```bash
tail -100 "${TMPDIR}tmux-claude-voice.log" | grep -E "drift|TTS スキップ"
```

期待する出力例 (ドリフトしている場合):

```
[WARN] 2026-05-24 23:15:22 - config drift: summary_enabled conf=[true] runtime=[false]
[INFO] 2026-05-24 23:15:24 [hooks] TTS スキップ: summary_enabled=false (status=Idle, pane=1:3.2)
```

`drift` も `TTS スキップ` も出ていなければ別の原因 (ドリフトではない)。

### 2. runtime 値を直接確認

```bash
for key in enabled sound_enabled summary_enabled panning_enabled notify_mode; do
    printf '%-25s = [%s]\n' "$key" "$(tmux show-option -gqv @claude_voice_$key)"
done
```

`.tmux/claude.conf` の `set -g @claude_voice_<key> "<value>"` と食い違っていれば
ドリフトしている。

### 3. 初期値に戻す

```bash
tmux source-file ~/.tmux/claude.conf
```

これで `.tmux/claude.conf` の `set -g` 行が再実行され、トグルで上書きされた値が
初期値に戻る。

意図的に OFF にしていた場合は再度トグルすれば良い (`Prefix + v + v` 等)。

## トグル操作の音声フィードバック

ADR 0008 以降、トグルバインドは `display-message` に加えて短い `say` で
ON/OFF を読み上げる。気付かないうちに切り替わる事故を防ぐため:

- `Prefix + v + v` → 要約読み上げ ON/OFF (`toggle_summary.sh`)
- `Prefix + v + p` → パンニング ON/OFF (`toggle_panning.sh`)
- `Prefix + v + n` → 通知モード循環 (`toggle_notify_mode.sh`)

会議中など say が邪魔な状況では、`Prefix + v + n` で `system_only` に
落とすか、`tmux set-option -g @claude_voice_enabled false` で全機能停止。

## hook が発火しないペインの調査

`tmux-claude-voice.log` に当該ペインの「イベント受信」ログが無い場合、Claude Code 側で
hook が登録されていない可能性が高い:

```bash
# 当該ペインの TMUX_PANE を確認 (例: %7)
tmux display-message -p '#{pane_id}'

# log から該当ペインの hook 発火を探す
grep "TMUX_PANE=%7" "${TMPDIR}tmux-claude-voice.log" | tail -20
```

何も出なければ:

- セッション中に `~/.claude/settings.json` を編集すると hook が反映されない既知の
  挙動 (HANDOFF 引継ぎ参照)。**Claude Code セッションを再起動する** ことで解消
- polling_monitor が pane_title の点字スピナーを見て Busy/Idle は補正してくれるため、
  hook 不発でもアイコン・通知音は最低限機能する (TTS と permission/question 検出は失われる)

## エラー検出の誤検出

`⚠ Error` アイコンが付くが実際にはエラーでない場合:

```bash
# 一時無効化 (現セッションのみ)
tmux setenv -g TMUX_CLAUDE_ERROR_DETECT_DISABLED true

# 解除
tmux setenv -gu TMUX_CLAUDE_ERROR_DETECT_DISABLED
```

恒久対応はエラー検出のテキスト/赤色パターンの調整 (`error_detector.sh`)。
詳細は ADR 0007。

## 関連リソース

- `.tmux/claude/` — 実装ディレクトリ
- `.tmux/claude.conf` — 設定ファイル (`set -g @claude_voice_*` の初期値)
- `${TMPDIR}tmux-claude-voice.log` — ログ
- `docs/ai-dlc/decisions/0007-five-state-status-model.md` — 5 状態モデル設計
- `docs/ai-dlc/decisions/0008-prevent-config-drift-and-silent-toggles.md` — ドリフト対策
