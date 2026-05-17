# 0005. AskUserQuestion 検出のため限定的 capture-pane を再導入

Date: 2026-05-17
Status: accepted

## Context

`spec-tmux-hooks-refactor.md` (2026-03-13) の Goal は
「**capture-pane によるステータス判定を完全廃止（TTS 要約用の 1 回を除く）**」
であり、`commit 6cd4ab5 "refactor(tmux): Hooks駆動+Liveness Checkアーキテクチャ
に移行"` で実装された。これにより 5 秒間隔の全 CC ペイン capture-pane が撤去
され、ステータス遷移はすべて Claude Code hooks が駆動する設計に統一された。

しかし、本セッションの調査で以下が判明:

- Claude Code の **AskUserQuestion ツール** は表示時に **Notification hook を
  emit しない**。matcher を `.*` に拡張しても、Stop hook の matcher も `*` で
  あるにもかかわらず、3 回連続テストで hook ログが完全に 0 行のままだった
- つまり「AskUserQuestion ダイアログ表示時にユーザーへ音で通知する」用途は
  現状の Hook 駆動アーキテクチャでは**捕捉不可能**

ユーザーの要望は「ダイアログ表示時に音を鳴らしたい」。元々の polling 実装には
pane content スキャンによる類似機能があったが、refactor で意図的に外された
歴史がある。これを部分的に復活させるかが論点。

## Options Considered

### Option 1: 何もしない（spec の方針を厳守）
- Pros: アーキテクチャ純度を維持。「Hook 駆動 + 軽量 liveness」のシンプル性
  を守れる。
- Cons: 実用上の不便を放置することになる。spec の "完全廃止" 文言は理想形で
  あって、実際は TTS 用の例外も既に存在しており、純度は既に 100% ではない。

### Option 2: polling のフル復活 (refactor 全体のロールバック)
- Pros: 過去にあった機能をすべて取り戻せる。
- Cons: 5s × N panes の capture-pane コストを再導入することになり、refactor の
  動機（7 panes 並行時の status-right 描画コスト）を再発させる。明らかに過剰。

### Option 3: ダイアログ検出に限定した capture-pane を新規モジュールで追加
- Pros: 既存 Hook 駆動 + Liveness Check アーキテクチャに追加レイヤとして共存。
  検出対象を「ダイアログの固定 footer 文字列」に絞ることで、ステータス遷移の
  ような複雑な状態判定は引き続き hooks に委ねる。`dialog_detector.sh` という
  独立モジュールに閉じ込めるため、不要なら 1 行 (`source` 行) を消すだけで
  停止できる。
- Cons: capture-pane の呼び出し回数自体は増える (5s × N panes)。footer 文字列
  `"Enter to select · ↑/↓ to navigate · Esc to cancel"` が Claude Code の
  バージョンアップで変更されると検出が壊れる脆さあり。

### Option 4: Claude Code に PreToolUse hook が再導入されるのを待つ
- Pros: 完全な hook 駆動を維持。
- Cons: ADR 0004 で「個人 dotfiles では AI-DLC quick-spec を issue 化せず直接
  実装する」を採択したばかりで、外部依存待ちは趣旨と合わない。tool_name で
  AskUserQuestion を識別できる保証もない (PreToolUse が emit されるか不明)。

## Decision

**Option 3** を採用。`dialog_detector.sh` を新設し、`polling_monitor.sh` から
呼び出す。具体的な実装上の制約:

1. **検出対象**: AskUserQuestion ダイアログの固定 footer 文字列のみ
   (`grep -qF "Enter to select"`)。誤検出を避けるため正規表現は使わない
2. **発火条件**: per-pane で `@claude_voice_dialog_active_<pane_key>` という
   tmux option による状態追跡を行い、**ダイアログ新規出現時の 1 回のみ**
   sound (Funk) を発火。連続発火しない
3. **無効化**: 環境変数 `TMUX_CLAUDE_DIALOG_DETECT_DISABLED=true` で全停止可能
4. **CC ペイン識別**: 同時発見した別バグ（pane_title が会話トピックで上書き
   される問題）を `pane_current_command == "claude*"` 判定に切り替えて修正
   （polling_monitor.sh と dialog_detector.sh の両方で）

実装は単一コミット (`a4f37d9`) に集約。

## Consequences

- ✅ AskUserQuestion 表示時に Funk サウンドで通知される。バックグラウンド作業
  中に Claude が入力待ちになったことに気付きやすい
- ✅ 副次効果として polling_monitor の CC 検出ロジックが修正され、長期的に
  status 集約と liveness cleanup が正しく機能するようになる
- ⚠️ capture-pane 呼び出しが 5s × N panes 増加 (refactor の動機と相反)。ただし
  検出関数内で sound 無効時は即 return するため最悪ケースのコストは抑制
- ⚠️ Footer 文字列が Claude Code のバージョンアップで変更されると検出が
  壊れる。固定パターンマッチのため将来的に脆いポイント。検出失敗時は単に音が
  鳴らないだけで他の機能に影響しない (graceful degradation)
- ⚠️ spec-tmux-hooks-refactor.md の Goal「capture-pane 完全廃止」を**厳密に
  読むと違反**。本 ADR の Decision でこれを上書きする (spec は当時の状態の
  スナップショットとして残置)

## AI-DLC Impact

- Agent Loop への影響: 無し
- Spec 変更の要否:
  - `spec-tmux-hooks-refactor.md` の Goal は厳密には反するが、ADR 0004 の
    軽量運用方針に従い spec は更新せず本 ADR を上位の判断記録とする。
    具体的には spec の Goal の "完全廃止 (TTS 要約用の 1 回を除く)" は
    "完全廃止 (TTS 要約用 1 回 + ダイアログ検出用 1 回を除く)" と読み替える
  - 将来 Claude Code が AskUserQuestion 用の hook を正式に emit するように
    なれば、本 ADR を superseded にして hook 駆動に戻すべき
