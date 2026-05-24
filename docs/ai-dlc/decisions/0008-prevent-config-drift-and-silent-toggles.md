# 0008. claude.conf と tmux runtime option のドリフト対策、サイレントトグル防止

Date: 2026-05-24
Status: accepted

## Context

2026-05-24、完了要約 TTS (Stop hook → Ollama → Kyoko) が動かなくなる事象
が発生した。調査の結果:

- `.tmux/claude.conf` には `set -g @claude_voice_summary_enabled "true"`
  と書かれている
- しかし tmux runtime では `@claude_voice_summary_enabled = false` に
  なっており、`status-update.sh:192` のガードに引っかかって TTS 経路を
  スキップしていた
- ログ上、最後の「TTS: 完了要約読み上げ開始」が 09:41:07、それ以降
  permission TTS も含めて全 TTS ログが消失。Stop hook 自体は正常発火し
  続けていたが、ユーザーが気付くまで数時間気付けなかった

根本原因は `claude.conf:55` のトグルバインド:

```bash
bind-key -T claude_voice_table v run-shell "tmux set-option -g @claude_voice_summary_enabled \
  $(tmux show-option -gqv @claude_voice_summary_enabled | grep -q true && echo false || echo true); \
  tmux display-message 'Claude Voice要約: $(tmux show-option -gqv @claude_voice_summary_enabled)'"
```

`Prefix + v + v` で要約 ON/OFF が即座に切り替わる。問題点:

1. **トグルが無音**: フィードバックは `display-message` (status bar の
   一瞬のテキスト) のみ。視線が status bar に無いと気付けない
2. **状態ドリフトの自己診断機能なし**: tmux runtime option が
   `.tmux/claude.conf` の初期値と乖離しても誰も検知しない
3. **失敗が静か**: TTS が呼ばれない経路に分岐するだけで、ログ・通知
   ともに「スキップしました」とは記録されない
4. **`Prefix + v + v` は誤操作しやすい**: Prefix → v (table 切替) →
   即座にもう一度 v を連打 する手癖になりやすい

この種の「設定が暗黙に変わって、機能が静かに止まる」障害は、AI 駆動の
長時間セッションでは発見が遅れやすく、生産性への影響が大きい。

## Options Considered

### Option 1: 何もしない (現状維持)

- Pros: 実装ゼロ。
- Cons: 同じ事象が再発する。再発時にまた「TTS が動かない原因」を一から
  調査する必要があり、調査コストが大きい。

### Option 2: トグルバインドを撤去

- 設定変更は `.tmux/claude.conf` 編集 + `tmux source-file ~/.tmux.conf`
  のみとする。
- Pros: ドリフト源を根絶。
- Cons: 「一時的に要約 OFF にしたい」(集中作業中など) を即座に実現でき
  なくなる。便利機能を捨てることになり、過剰反応。

### Option 3: トグル時に音声フィードバック + ログ強化のみ

- トグルバインドはそのまま、フィードバックだけ強化:
  - `display-message` に加えて短い `say` で OFF/ON を読み上げ
  - status-update.sh:192 のガードが false に評価されたとき、ログに
    `[INFO] TTS スキップ: summary_enabled=false` を出す
- Pros: 実装が軽い (各 5-10 行)、誤操作に気付ける、ドリフト時もログを
  見ればすぐ判明。
- Cons: 「ログを能動的に見ないと気付かない」状態は残る。

### Option 4: ドリフト自己診断 + フィードバック強化

- Option 3 に加えて、polling_monitor の起動/定期実行時に
  `.tmux/claude.conf` の `set -g @claude_voice_*` 行と現在の tmux option
  を比較し、重要キー (`summary_enabled`, `sound_enabled`, `enabled`,
  `panning_enabled`, `notify_mode`) でドリフトがあれば WARN ログを出す。
- Pros: ドリフトを能動的に検出。ユーザーがログを開くまでもなく、
  別経路 (例: ステータスバーへの一時表示や say) で通知できる。
- Cons: polling_monitor のロジック増加。「ドリフトしているがユーザー
  が意図的にトグルした」ケースとの区別が必要 (誤検出すると煩い)。

### Option 5: トグル状態を別の保存先に分離

- conf 由来の初期値と動的トグル状態を別キーに分け、効果判定では両者を
  AND する。例: `@claude_voice_summary_enabled` (conf) と
  `@claude_voice_summary_runtime_override` (トグル) の組み合わせ。
- Pros: 「conf の値」と「ユーザーの動的選択」を明確に分離。`tmux
  source-file` で意図せずトグルが消えることもない。
- Cons: 大規模リファクタ。全消費箇所 (status-update.sh, polling_monitor,
  dialog_detector, error_detector 等) を書き換える必要があり、個人
  dotfiles の規模感に対して過剰。

## Decision

**Option 3 (フィードバック強化 + ログ強化) を採用** し、追加の保険として
**Option 4 のうち軽量な実装 (キー数を絞った診断ログ) を補助的に導入**
する。Option 5 は将来案として保留 (実装は行わない)。

具体的に実装する 3 つの変更:

### 変更 1: トグルバインドに音声フィードバックを追加

`.tmux/claude.conf:55` のバインドを編集し、`display-message` の後に
短い `say` を追加する:

```bash
bind-key -T claude_voice_table v run-shell "\
  tmux set-option -g @claude_voice_summary_enabled \
    \$(tmux show-option -gqv @claude_voice_summary_enabled | grep -q true && echo false || echo true); \
  new=\$(tmux show-option -gqv @claude_voice_summary_enabled); \
  tmux display-message \"Claude Voice要約: \$new\"; \
  say -v Kyoko -r 240 \"要約\$([ \"\$new\" = true ] && echo オン || echo オフ)\" >/dev/null 2>&1 &"
```

panning_enabled トグル (`Prefix + v + p`) も同様にする。

トレードオフ: トグルのたびに say が走るが、Kyoko で 240 wpm の極短
発話 (2-3 単語) なので 1 秒未満。気付くコストとして許容範囲。

### 変更 2: status-update.sh で TTS スキップをログに記録

`hooks/status-update.sh:192` のガードを次のように変更:

```bash
SUMMARY_ENABLED=$(tmux show-option -gqv @claude_voice_summary_enabled 2>/dev/null)
if [[ "$SUMMARY_ENABLED" == "true" ]]; then
    case "$NEW_STATUS" in
        # ... 既存の TTS 処理 ...
    esac
else
    # 重要: 初期値と乖離している場合はサイレント障害を防ぐためログ
    case "$NEW_STATUS" in
        "Permission"|"Idle")
            _log "INFO" "TTS スキップ: summary_enabled=${SUMMARY_ENABLED:-unset} (status=$NEW_STATUS)"
            ;;
    esac
fi
```

これで「TTS が動かない」と気付いた瞬間に `tail tmux-claude-voice.log`
すれば原因が即判明する。

### 変更 3: polling_monitor に軽量ドリフト診断を追加

`polling_monitor.sh` の冒頭 (常時実行される箇所) で、5 つの重要キーに
ついて conf と runtime を比較。差分があれば 1 分に 1 回まで WARN ログ:

```bash
check_config_drift() {
    local conf=~/.tmux/claude.conf
    [[ -f "$conf" ]] || return 0
    # 1 分以内に診断済みならスキップ
    local last=$(tmux show-option -gqv @claude_voice_drift_check_ts 2>/dev/null)
    local now=$(date +%s)
    [[ -n "$last" && $((now - last)) -lt 60 ]] && return 0
    tmux set-option -g @claude_voice_drift_check_ts "$now"

    local keys=(enabled sound_enabled summary_enabled panning_enabled notify_mode)
    for key in "${keys[@]}"; do
        local conf_val runtime_val
        conf_val=$(grep -E "^set -g @claude_voice_${key} " "$conf" \
                   | sed -E 's/.*"([^"]*)".*/\1/' | head -1)
        runtime_val=$(tmux show-option -gqv "@claude_voice_${key}" 2>/dev/null)
        if [[ -n "$conf_val" && "$conf_val" != "$runtime_val" ]]; then
            _log "WARN" "config drift: ${key} conf=[${conf_val}] runtime=[${runtime_val}]"
        fi
    done
}
```

これは「トグルした記憶があるか」を機械的に判断できないため、
**WARN を出すだけで自動修正はしない**。ユーザーが意図したトグルは
そのまま残し、意図しないドリフトの場合のみログから気付ける、という
トレードオフ。

### 変更 4: トラブルシューティングのドキュメント化

`docs/dotfiles-workflows.md` (または新規 `docs/tmux-claude-voice-troubleshooting.md`)
に「TTS が動かない / 通知音が出ない場合」セクションを追加し、以下を
明記:

1. `tmux show-option -gqv @claude_voice_summary_enabled` で実値を確認
2. `tail "${TMPDIR}tmux-claude-voice.log" | grep -E "TTS|drift"` で
   スキップやドリフトの記録を確認
3. 初期値に戻すには `tmux source-file ~/.tmux/claude.conf`

## Consequences

- ✅ トグルした瞬間に say で気付けるため、誤操作で OFF にしても即座に
  認識できる
- ✅ サイレント失敗が無くなる: TTS スキップが必ずログに残る
- ✅ ドリフトを polling_monitor が定期検出し、WARN ログに残す。事後
  調査が「`tmux show-option` で 1 行確認」で済む
- ⚠️ トグルのたびに say が走るので、無音環境 (会議中など) で意図的に
  OFF にする場合は say も鳴ってしまう。`Prefix + v + n` で notify_mode
  を `system_only` に落とすか、say を `>/dev/null` の代わりに volume
  チェックを入れる選択肢はあるが、本 ADR では「気付けることを優先」と
  して許容
- ⚠️ ドリフト診断は 5 キーのみ。`summary_length` や `pan_margin` 等の
  数値系チューニングはドリフト検出対象外 (誤検出が増えるため意図的に
  限定)
- ⚠️ ドリフト診断は 1 分間隔。`@claude_voice_drift_check_ts` という
  tmux option が 1 つ増える (ノイズ最小)
- ⚠️ `Prefix + v + v` バインド自体は残す。利便性を維持しつつ、誤操作の
  気付きを早めることで実用上の問題を解決する設計

## AI-DLC Impact

- Agent Loop への影響: 無し
- Spec 変更の要否: なし (運用改善)
- ADR 0007 (5 状態モデル) の運用上の補完。状態モデルが正しく動くため
  には設定値が conf 通りである必要があり、本 ADR がそれを保証する
- ADR 0004 (個人 dotfiles のライト運用) に従い、quick-spec や Issue を
  作らず本 ADR + 実装 commit で完結
