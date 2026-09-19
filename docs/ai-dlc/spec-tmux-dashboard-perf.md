---
type: ai-dlc-quick-spec
version: "1.0"
status: review
created: 2026-09-19
story-count: 4
phase: MLP
---

# Quick Spec: tmux agent dashboard（Prefix+w）の表示速度改善

## Problem & Goal

### Problem Statement

`Prefix+w` で開く agent dashboard（`.tmux/agents/dashboard.py`）は、開いてから最初に描画されるまで約 1.5 秒真っ白で、開いている間も操作していないのに CPU を使い続ける。2026-09-19 に実測した結果（pane 16 / worktree 48 / 表示 30 行 / リポジトリ約 8）、原因は独立した 3 つに分かれる。

| # | 症状 | 実測 | 原因 |
|---|---|---|---|
| ① | 初回描画まで | 1.41〜1.57 秒 | `registry.snapshot` が外部コマンドを 65 回実行（待ち約 1.0 秒）。`registry.py:16 repository` が cwd 16 件 × git 2 回（612ms）、`worktrees.py:61 _common_git_dir` が同じ問い合わせを 22 回（359ms）、`codex_status.py:218 is_agent_process` が 32 回プロセス表を全走査（180ms）、`read_panes` 158ms |
| ② | 1 フレームの描画 | 65〜70ms | `dashboard.py:400 _worktree_for_path` が毎フレーム 30 行 × 48 worktree = 2,880 回 `Path.resolve()`。`_run_dashboard` は `screen.timeout(100)` のたびに無条件で `_draw` するため、アイドルでも CPU の 6〜7 割を消費。master（65.9ms）でも同じで、列幅変更（70.5ms）が原因ではない |
| ③ | 開いている間の再取得 | 常時 約 40 コマンド/秒 | `dashboard.py:951,965` が「要求時刻 + 1.0 秒」で次回を決めるが、取得に 1.5 秒かかるため前回終了直後に次が始まり、切れ目なく git/tmux/ps が走る |

### Goal

体感速度を MLP のパフォーマンスバジェット内に収める。具体的には、初回描画を 0.5 秒以内、キー操作から描画までを 100ms 以内（描画処理自体は 5ms 以内）、アイドル時の CPU 消費と外部コマンド実行をほぼゼロにする。表示内容・キー操作・データの正しさは変えない。

### Success Criteria

- [x] `dashboard.py --dump` の実行時間（同等規模の session）が 1.5 秒 → **0.5 秒以下** — 実測 0.32〜0.37 秒（#63 / #64）
- [x] `render_lines` 1 回あたりが 65ms → **5ms 以下**（同一 snapshot、幅 160） — 実測 1.4〜1.5ms（#65）
- [x] `_rebuild` 後の描画で `Path.resolve` の呼び出しが **0 回**（#65）
- [x] キー入力も snapshot 更新もないループでは `_draw` が呼ばれない（#65）
- [x] 1 回の `registry.snapshot` で `git worktree list` が **リポジトリごとに 1 回**、`git rev-parse` が **cwd ごとに 1 回**（現状はどちらも 2〜3 倍） — git 66 → 23 回（#64）
- [x] 開いている間の外部コマンド実行が 約 40 回/秒 → ~~定常 5 回/秒 以下~~ **定常 8 回/秒 以下・CPU 15% 以下に見直し**（2026-09-20 決定） — 周期 1 秒 / 5 秒で実測 7.4 回/秒・CPU 12.7%（変更前 26.6 回/秒・CPU 41.1%）。当初の 5 回/秒は見積もりの誤りで、速い取得 1 回が 5 コマンド（tmux 4 + ps 1）かかるため 1 秒周期とは両立しない。2 秒 / 10 秒なら 4.5 回/秒・CPU 7.9% だが、agent の状態の反映が最大 1 秒遅れるため、状態の鮮度を優先して 1 秒 / 5 秒を採用した（#62 / #66）
- [x] 既存の全テスト（`python3 -m unittest`、現在 112 件）が通り、表示結果（`--dump` 出力）が変更前後で一致する — 161 件、幅 60/100/160 で一致

## Solution Overview

### Technical Approach

3 つの原因をそれぞれの層で直す。描画層は「変化したときだけ計算・描画する」に変え、行ごとの列の値は `DashboardModel._rebuild` 時に 1 回だけ求めて保持する。取得層は 1 回の snapshot 内で「cwd → common dir」「common dir → worktree 一覧」をメモ化して `registry.py` と `worktrees.py` で共有し、独立な git 呼び出しは `ThreadPoolExecutor` で並列化する。再取得は tmux だけを見る速い周期と git を読み直す遅い周期に分け、次回開始を前回の「完了時刻」から数える。

### Key Decisions

- **キャッシュの寿命は「1 回の snapshot 内」に限定**: プロセスをまたぐ永続キャッシュ（ディスク）は使わない — 1.5 秒 → 0.3〜0.4 秒の見込みで足り、古いデータが見える問題とロックの複雑さを持ち込まずに済む
- **性能の回帰防止は所要時間ではなく呼び出し回数で検証**: `Path.resolve` / git 実行 / `_draw` の回数をモックで数える — CI（GitHub Actions）でも安定し、環境非依存パスの原則とも整合する。所要時間は別途ベンチスクリプトで手動計測する
- **`is_agent_process` は関数の契約を保ったまま索引化**: `claude_status.py:161` と `codex_status.py:233` のポーリング経路も同じ関数を使うため、引数と戻り値を変えずに内部だけ「pane_pid から子孫をたどる」形にする — dashboard だけでなく常時ポーリングの負荷も下がる
- **再取得は 2 段（速い 1 秒 / 遅い 5 秒 + 操作直後）**: agent の状態（作業中・要対応）は tmux の pane option から得られ git を必要としない — 鮮度が要る情報だけを高頻度に保てる
- **実装順は ② → ①（git）→ ①（プロセス判定）→ ③**: ②は Python 内で完結し体感効果が最大、③は①のメモ化構造に依存する

### Metrics Definition

| 指標 | 計測方法 | ベースライン（2026-09-19） | バジェット |
|---|---|---|---|
| 起動時間 | `/usr/bin/time -p python3 dashboard.py --session $S --pane $P --dump` を 3 回、`real` の中央値 | 1.52 秒 | ≤ 0.5 秒 |
| 描画時間 | 同一 snapshot で `render_lines(model, 160)` を 20 回、平均 | 65.9ms（master）/ 70.5ms（列幅ブランチ） | ≤ 5ms |
| snapshot 内の外部コマンド数 | `cProfile` の `subprocess.run` 呼び出し回数 | 65 回 | ≤ 25 回 |
| 定常時の外部コマンド率 | `python3 bench_dashboard.py --steady 60` | 26.6 回/秒・CPU 41.1% | ≤ 8 回/秒・CPU ≤ 15%（当初 ≤ 5 回/秒から見直し） |

- **データソース**: 実 tmux session（計測時の pane 数・worktree 数・リポジトリ数を結果に併記する）
- **エッジケース**: pane 数や worktree 数が計測時と大きく違う場合は絶対値でなく変更前後の比で判断する。git が 2 秒でタイムアウトするリポジトリは計測から除外し、その旨を記録する

### Out of Scope

- snapshot のディスク永続キャッシュ、および骨組みだけ先に描画する方式（Story 2・3 の後も起動が 0.5 秒を超える場合のみ再検討）
- `dashboard_popup.sh` / `display-popup` 自体の起動コスト
- `read_panes` の tmux 2 回呼び出しの 1 回化（約 25ms、優先度低）
- 表示内容・列構成・キー操作の変更（列幅と window/pane 列は別 PR `feat/tmux-dashboard-column-layout` で対応済み）
- Claude Voice・通知まわりのポーリング処理（`polling_monitor.sh`）の見直し

## Stories

### Story 1: 描画ごとのパス解決をなくし、変化があったときだけ描画する

- **Size**: M
- **Dependencies**: None（ただし列幅 PR `feat/tmux-dashboard-column-layout` の master マージ後に着手。`dashboard.py` の同じ箇所を触るため）

**Context**: dashboard は 100ms ごとにループし、そのたびに全行の列の値を計算し直して描画している。キーを押すたびに 65ms 待たされ、開いているだけで CPU を消費する。MLP のインタラクション 100ms バジェットに対して余裕がない。

**Current Behavior**: `dashboard.py:536 column_layout` → `:523 _row_values` → `:417 row_columns` → `:400 _worktree_for_path` が毎フレーム呼ばれ、30 行 × 48 worktree = 2,880 回の `Path.resolve()` が走る（1 フレーム 65〜70ms）。`_run_dashboard`（`:935`）は `get_wch` のタイムアウトごとに無条件で `_draw`（`:797`）を呼ぶ。

**Expected Behavior**:
- worktree のパスは snapshot を受け取った時点で 1 回だけ正規化する
- 行ごとの列の値（リポジトリ・ブランチ・worktree・window・pane）は `DashboardModel._rebuild`（`:189`）で 1 回だけ求めて保持し、`column_layout` と描画はそれを読むだけにする
- `_draw` は、モデル（行・選択・展開）、メッセージ、スクロール位置、画面サイズのいずれかが変わったとき、および `C-l`（再表示）のときだけ呼ぶ
- `--dump` の出力は変更前後で一致する
- 変更前後を同じ手順で測れるベンチスクリプト `.tmux/agents/bench_dashboard.py`（起動時間・描画時間・外部コマンド数を出力）を追加する

**Constraints**:
- `format_row(row, width, expanded, snapshot=None)` を行 1 件だけで呼ぶ既存テストの使い方を壊さない（保持した値がない場合はその場で計算する）
- 外部依存を追加しない（標準ライブラリのみ）
- 絶対パスをテスト・スクリプトに直書きしない。CI で動くこと
- テストを先に書き、失敗を確認してから実装する
- 所要時間そのものを単体テストで assert しない（呼び出し回数で検証する）
- Phase MLP: コードドキュメントの追加、過度なモジュール分割、Analytics 以外の監視の追加は不要

**Verification**:
- [ ] `_rebuild` の後に `render_lines` を 5 回呼んでも `Path.resolve` の呼び出しが 0 回であることを、モックで数えるテストが通る
- [ ] キー入力も snapshot 更新もないループ反復で `_draw` が呼ばれないことを確認するテストが通る（描画要否の判定を純粋関数に切り出して検証する）
- [ ] snapshot 更新・選択移動・展開/折りたたみ・メッセージ表示/消滅・リサイズ・`C-l` のそれぞれで再描画されるテストが通る
- [ ] `cd .tmux/agents && python3 -m unittest` が全件通る
- [ ] `python3 bench_dashboard.py` の描画時間が 5ms 以下、変更前後の `--dump` 出力に差分がない

### Story 2: snapshot 内の git 呼び出しの重複をなくし並列化する

- **Size**: M
- **Dependencies**: None

**Context**: 初回描画までの 1.5 秒のうち約 970ms が git の待ち時間で、その大半は同じ問い合わせの繰り返しである。リポジトリは約 8 個しかないのに、cwd 16 件それぞれに対して 2 か所から同じ git コマンドを実行している。

**Current Behavior**: `registry.py:136 snapshot` は cwd ごとに `registry.py:16 repository`（`git rev-parse --git-common-dir` + `git worktree list` の 2 回）を呼び、続く `worktrees.list_worktrees` → `worktrees.py:126 _inventory` が同じ cwd 群に対して `:61 _common_git_dir` と `git worktree list` をもう一度実行する。すべて直列で、1 回約 17ms。

**Expected Behavior**:
- 1 回の snapshot の中で「cwd → common dir」と「common dir → worktree 一覧（porcelain のパース結果）」を 1 度だけ求め、`repository` 相当の情報と `_inventory` の両方がそれを共有する
- 互いに独立な git 呼び出しは `ThreadPoolExecutor` で並列に実行する
- snapshot の戻り値（`repos` / `agents` / `worktrees` / `pane_locations` など）は変更前と同じ内容になる
- git 管理外のディレクトリ、タイムアウト、失敗したリポジトリの扱い（その cwd を無視して続行）は現状どおり

**Constraints**:
- メモの寿命は 1 回の snapshot 呼び出し内に限る（モジュール変数に持ち越さない。worktree の作成・削除直後に古い一覧が見えないこと）
- `worktrees.poll_session`（`:347`、`:379` で `_common_git_dir` を使用）など、dashboard 以外の呼び出し元の挙動を変えない
- `registry.repository` と `worktrees.list_worktrees` の公開シグネチャは維持する（共有の仕組みは省略可能な引数で渡す）
- 並列度は上限を設ける（最大 8）。スレッド内の例外は握りつぶさず、現状と同じ「その cwd をスキップ」に落とす
- テストを先に書き、失敗を確認してから実装する。絶対パス直書き禁止、CI で動くこと
- Phase MLP: コードドキュメントの追加、過度なモジュール分割は不要

**Verification**:
- [ ] git 実行をモックし、同じリポジトリ配下の cwd を 3 件渡したとき `git worktree list` が 1 回だけ実行されることを確認するテストが通る
- [ ] 同じ cwd に対する `rev-parse --git-common-dir` が 1 回だけであることを確認するテストが通る
- [ ] 1 つのリポジトリで git が失敗・タイムアウトしても、他のリポジトリの結果が返るテストが通る
- [ ] 既存の `test_registry.py` / `test_worktrees.py` / `test_workspace_integration.py` が全件通る
- [ ] `python3 bench_dashboard.py` の外部コマンド数が 65 回 → 25 回以下になる

### Story 3: agent プロセス判定を pane 起点の探索に変える

- **Size**: S
- **Dependencies**: None

**Context**: pane で Claude / Codex が動いているかの判定が、pane × provider の組ごとにプロセス表を全件たどっており、snapshot 1 回で 180ms を使っている。同じ関数は常時動くポーリング（`claude_status.py:161` / `codex_status.py:233`）でも使われるため、dashboard を開いていないときの負荷にも効く。

**Current Behavior**: `codex_status.py:218 is_agent_process(pane_pid, processes, provider)` は、全プロセスについて実行ファイル名を `Path(executable).name` で取り出し、一致したものから親をたどって `pane_pid` に届くかを調べる。`registry.py:62` から 32 回（16 pane × 2 provider）呼ばれ、毎回全プロセスを走査する。

**Expected Behavior**:
- 引数と戻り値は変えず、内部を「`pane_pid` から子孫をたどり、その中に provider の実行ファイルがあるか」を調べる形にする
- 親 → 子の索引と実行ファイル名の取り出しは、プロセス表 1 つにつき 1 回だけ行う（同じ `processes` に対する繰り返し呼び出しで再計算しない）
- 判定結果は変更前と同じ（`node` ラッパー配下の `codex`、`.exe` 付き、循環した親子関係でも無限ループしない）

**Constraints**:
- `is_agent_process` / `is_codex_process` / `process_table` のシグネチャを変えない（`claude_status.py` と `registry.py` が import している）
- 索引のキャッシュは `processes` オブジェクトに紐づけ、別のプロセス表に古い索引を使わない
- テストを先に書き、失敗を確認してから実装する
- Phase MLP: コードドキュメントの追加は不要

**Verification**:
- [ ] 既存の判定テスト（`test_codex_status.py` / `test_claude_status.py` / `test_registry.py` の node ラッパー検出）が全件通る
- [ ] 子孫に provider がいる / いない / 別 pane の子孫にいる / 親子が循環している、の 4 ケースのテストが通る
- [ ] 同じ `processes` で 32 回呼んでも索引の構築が 1 回であることを確認するテストが通る
- [ ] `python3 bench_dashboard.py` で `is_agent_process` の合計が 180ms → 10ms 以下になる

### Story 4: 再取得を速い周期と遅い周期に分け、完了時刻から次回を数える

- **Size**: M
- **Dependencies**: Story 2

**Context**: dashboard を開いている間、取得が終わった直後に次の取得が始まり、git・tmux・ps が切れ目なく走り続ける。agent の状態は tmux から得られるので、毎秒 git を読み直す必要はない。

**Current Behavior**: `dashboard.py:951,965` は `next_refresh = now + 1.0` と「要求した時刻」から次回を決める。取得に 1.5 秒かかるため `SnapshotLoader.request`（`:725`）は前回の完了直後に毎回フル取得（git 含む）を始め、定常的に約 40 コマンド/秒を実行する。

**Expected Behavior**:
- 速い再取得（1 秒周期）: `read_panes` と `build_snapshot` だけを実行し、リポジトリ対応表と worktree 一覧は直近の遅い取得の結果を使う。worktree 行の `panes` / `status` は最新の pane 情報で更新する
- 遅い再取得（5 秒周期）: git を含むフル取得
- `n`（作成）/ `d`（削除）/ `s` `c` `x`（起動）/ `a`（自動切替）の直後は、周期を待たず遅い再取得をただちに行う
- 次回の開始時刻は、前回の取得が完了した時刻から数える
- 直近の遅い取得に含まれない cwd を持つ pane が現れた場合は、その回を遅い再取得に切り替える

**Constraints**:
- `SnapshotLoader` の「キー入力スレッドを取得で止めない」性質を保つ（取得は引き続きワーカースレッド）
- 取得失敗時は既存どおりメッセージを出して次回の更新を続ける
- 周期の値（1 秒 / 5 秒）は定数として 1 か所にまとめる
- テストを先に書き、失敗を確認してから実装する。時刻は注入可能にし、テストで実時間を待たない
- Phase MLP: コードドキュメントの追加、Analytics 以外の監視の追加は不要

**Verification**:
- [ ] 時刻を注入し、取得に 1.5 秒かかる想定でも次回開始が「完了 + 周期」になるテストが通る
- [ ] 速い再取得では git が 1 回も実行されないことを、モックで確認するテストが通る
- [ ] worktree 作成・削除の直後に遅い再取得が要求されるテストが通る
- [ ] 未知の cwd を持つ pane が現れたとき遅い再取得に切り替わるテストが通る
- [ ] 速い再取得後も agent の状態と worktree 行の pane 表示が最新になるテストが通る
- [ ] dashboard を 10 秒開いた計測で外部コマンドが 5 回/秒 以下になる

## Agent-Ready Summary

| Story | Size | Status | Missing Elements |
|---|---|---|---|
| 1. 描画ごとのパス解決をなくし、変化があったときだけ描画する | M | Ready | None |
| 2. snapshot 内の git 呼び出しの重複をなくし並列化する | M | Ready | None |
| 3. agent プロセス判定を pane 起点の探索に変える | S | Ready | None |
| 4. 再取得を速い周期と遅い周期に分け、完了時刻から次回を数える | M | Ready | None（Story 2 のマージ後に着手） |

## Out of Scope

- snapshot のディスク永続キャッシュ、骨組み先行描画（Story 2・3 完了後も起動 > 0.5 秒の場合のみ次の spec で検討）
- popup 起動シェル（`dashboard_popup.sh`）と `display-popup` のコスト
- `read_panes` の tmux 呼び出し 1 回化
- 表示内容・列構成・キー操作の変更
- Claude Voice・通知のポーリング処理の見直し
- 他リポジトリ（claude-plugins）への横展開 — 「呼び出し回数で性能回帰を検証する」型は再利用価値があるが、別途検討
