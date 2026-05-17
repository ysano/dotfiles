# 0004. 個人 dotfiles では AI-DLC quick-spec を issue 化せず直接実装する運用方針

Date: 2026-05-17
Status: accepted

## Context

`spec-tmux-claude-voice-hardening.md` (2026-03-29 作成) の Next Steps には
以下のフローが記載されていた:

```
- [ ] Create GitHub Issues: `python3 plugins/ai-dlc/scripts/helpers/spec_to_issues.py
      --spec docs/ai-dlc/spec-tmux-claude-voice-hardening.md
      --milestone "Sprint 2026-04-05" --dry-run`
- [ ] Run `/ai-dlc:plan` to integrate into sprint planning
- [ ] Run `/ai-dlc:drive #N` to execute Agent Loop for ready stories
```

しかし実際の運用では:

- 当該 spec の 5 ストーリー (S1-S5) はすべて実装済みだが、GitHub Issues は
  起票されておらず Sprint 2026-04-05 も切られていない
- 他の spec (`spec-tmux-window-name.md`, `spec-tmux-hooks-refactor.md`,
  `spec-asdf-cleanup.md`) も同様に `status: draft` のまま git に commit
  され、issue 化フローを通過していない
- このリポジトリは個人 dotfiles であり、複数人での sprint 運営や工数追跡
  ニーズが存在しない

AI-DLC の full ceremony (PRD → Architecture → Stories → Issues → Sprint →
Agent Loop) を個人 dotfiles に適用するのはオーバーキルであり、実態と
理想プロセスの乖離が継続するのは記録としても望ましくない。

## Options Considered

### Option 1: AI-DLC full ceremony を厳密に運用する
- Pros: AI-DLC の本来の効果（traceability、計画駆動の Agent Loop）が得られる。
- Cons: 個人 dotfiles では sprint / milestone / 複数 issue 管理のオーバー
  ヘッドが効用を上回る。実態として続いていない。

### Option 2: AI-DLC のうち軽量な部位 (quick-spec, ADR) のみ採用する
- Pros: 「何を作るか / なぜ作るか」の設計記録は spec で残せる。重要な決定は
  ADR で記録できる。GitHub Issues / Sprint 管理は省略でき、運用コストが低い。
  個人スケール（1 人 + AI agents）に適している。
- Cons: 工数追跡 / 進捗の数値可視化が無くなる。複数 contributor が増えた
  場合は再検討が必要。

### Option 3: AI-DLC を完全に廃して plain な commit 駆動に戻す
- Pros: もっとも軽量。
- Cons: 設計の経緯や代替案の検討が永続化されず、未来の自分が同じ問題に
  遭遇した時に再考のコストがかかる。今回の Ghostty bell 調査のように複雑な
  trade-off は記録する価値がある。

## Decision

**Option 2** を採用する。

個人 dotfiles における AI-DLC 運用ルール:

| 採用する要素 | 採用しない要素 |
|---|---|
| `docs/ai-dlc/spec-*.md` (quick-spec) | GitHub Issues 自動生成 (`spec_to_issues.py`) |
| `docs/ai-dlc/decisions/NNNN-*.md` (ADR) | Sprint / Milestone 管理 |
| `/ai-dlc:create-prd` 等の skill 利用 | `/ai-dlc:plan` (sprint planning) |
| 必要に応じた `/ai-dlc:drive` (Agent Loop) | `/ai-dlc:diagnose` (sprint retrospective) |
| spec の `status: draft` 保持 (慣例) | 厳密な status ライフサイクル管理 |

spec の Next Steps に列挙される issue 化 / sprint planning タスクは
「実施しない」を default とし、実施する場合のみ別途記録する。

## Consequences

- ✅ 軽量な「設計→決定→実装」サイクルを維持しつつ、重要な技術判断は ADR で
  永続化できる
- ✅ spec / ADR は markdown として git で管理されるので、新マシン構築や
  数ヶ月後の見直しでもアクセス容易
- ✅ AI-DLC skill 群のうち本当に価値のある部分 (quick-spec, decision,
  drive) のみ使い、ceremony オーバーヘッドを回避
- ⚠️ 複数 contributor を迎える状況が来た場合、issue 化 / sprint 管理を改めて
  検討する必要がある。その時はこの ADR を superseded にして再決定する
- ⚠️ AI-DLC 観点で「sprint diagnostic」「DORA metrics」等のレポートは生成
  されないため、AI-DLC dashboard 系の機能は無意味になる（個人運用では不要と
  割り切る）

## AI-DLC Impact

- Agent Loop への影響: Agent Loop は引き続き利用可（`/ai-dlc:drive` を必要時
  に手動起動）。ただし「Sprint に紐付くチケットを順次 drive」というワークフロー
  は無効化される
- Spec 変更の要否: 既存 spec の Next Steps セクションは「当時の計画スナップ
  ショット」として残すが、未達成チェックボックスを後追いで埋める運用は
  しない。新規 spec 作成時は Next Steps に issue 化を書かない、または
  「個人運用のためスキップ可」と明記する選択肢を取れる
