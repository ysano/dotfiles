# 設計書: claude-doctor — Claude Code セッション環境の一次診断

- 日付: 2026-07-09
- 状態: ドラフト（レビュー待ち）

## 目的・背景

Claude Code の長期運用で再発する 2 症状の一次診断を 1 コマンドに集約する。

1. **配送障害**（ツール結果が空で返る・`echo` すら空）: 2026-07-09 調査で、Bash 実行が `/tmp/cc-daemon-<uid>/` の pty デーモン socket 経由であること、socket は現行分しか残らないことを確認。4〜6 日走りっぱなしのセッションがデーモン再起動をまたぐと壊れる仮説が最有力。障害の瞬間に socket の生死を見られる手段が必要。
2. **リソース残骸の蓄積**: 数日齢の `claude -c` セッション・孤児 node プロセス（ppid=1）・肥大した telemetry jsonl（`~/.claude/metrics/`、自動 rotation は上流 ai-dlc v1.169.0 で実装済みだが配布ラグあり）・tmux-claude-voice の config drift（conf と runtime の乖離で毎分 WARN）。

いずれも「気づいた時に手で 5 コマンド打つ」状態だったため、ADHD 特性（確認手順の漏れ）対策としても 1 コマンド化する。

## 要件

- **read-only**: 診断と提案のみ。kill / rotate / conf 修正は一切実行しない（提案コマンドを表示する）。
- **依存最小**: bash + 標準 UNIX ツールのみ（jq 不要）。macOS 前提（`/bin/ps` BSD 形式）。
- **モダン CLI alias 非依存**: `ps`→procs / `du`→dust 等の alias 環境でも動くよう、絶対パス（`/bin/ps` 等）で呼ぶ。
- **1 画面完結**: セクションごとに ✅/⚠️ を付け、⚠️ には対処コマンドを添える。
- **常に exit 0**: 診断ツールであり、CI ゲートではない。

## 非対象（YAGNI）

- launchd / cron による定期実行はしない（手動運用で漏れが実感されてから検討）。
- 自動修復（kill・rotate・conf 書換）はしない。
- WSL / Linux 対応はしない（必要になった時点で分岐追加）。
- tmux-claude-voice 本体の診断（integration_test.sh の領分）はしない。

## 診断項目

| # | セクション | 判定 | ⚠️ 時の提案 |
|---|---|---|---|
| 1 | cc-daemon socket | `/tmp/cc-daemon-$(id -u)/` 配下の `.sock` を列挙、mtime 表示。ディレクトリ不在 or socket 0 件で ⚠️ | 該当セッションの再起動 |
| 2 | 長期セッション | `claude` CLI プロセスで etime に `-`（日単位）を含むものを ⚠️ 列挙 | pane の作業残を確認して畳む |
| 3 | 孤児プロセス | ppid=1 の node/claude 系プロセスを列挙（Claude.app Helper は除外） | `kill <pid>` |
| 4 | metrics 肥大 | `~/.claude/metrics/*.jsonl` が閾値（4MB）超で ⚠️ | `mv` による手動 rotate コマンド |
| 5 | config drift | `~/.tmux/claude.conf` の `@claude_voice_*` 全キーを tmux runtime と突合 | conf か runtime どちらに寄せるかの選択肢 |
| 6 | git index.lock | カレントが git repo かつ `.git/index.lock` が存在し、活動中 git プロセスが無ければ ⚠️（24h 超で stale 濃厚と注記） | `/bin/rm -f .git/index.lock` |

## 実装メモ

- 配置: `bin/claude-doctor`（PATH 導線は dotfiles 既存の bin/ 運用に従う）
- プロセス判定: `/bin/ps -eo pid,ppid,etime,rss,args`。「実 CLI セッション」= args が `claude`（node shim 含む）で始まり `/Applications/Claude.app` を含まないもの。42 プロセス中実体は 7 本だった 2026-07-09 の解剖結果に基づく除外規則。
- config drift 検出は `polling_monitor.sh:check_config_drift()` と同じ抽出規則（`set -g @claude_voice_<key> "<value>"`）。ただし polling_monitor がキー 5 個ハードコードなのに対し、本ツールは conf に存在する全キーを対象にする。
- 閾値 4MB は上流 ai-dlc の自動 rotation 閾値（5MB）より低く設定し、自動 rotation が来る前に人が気づける線とする。

## 関連

- 調査記録: unified-jss-build-1963 の memory `reference-claude-metrics-self-telemetry-not-injection`（2026-07-09）
- 上流 rotation 実装: ysano/claude-plugins `df35544`（#1056/#1062、v1.169.0 系）
- `.git/index.lock` 頻発の既知事象: unified-jss-build-1963 `CLAUDE.local.md §B5`
