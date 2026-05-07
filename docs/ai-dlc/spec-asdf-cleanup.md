---
type: ai-dlc-quick-spec
version: "1.0"
status: draft
created: 2026-05-07
story-count: 4
phase: Production
---

# Quick Spec: asdf 棚卸しと運用改善

## Problem & Goal

### Problem Statement

dotfiles 環境の asdf (0.19.0) は 13 プラグイン / 308 shim / 11 GB のリソースを消費している。プロジェクト 18 件の `.tool-versions` および global `~/.tool-versions` をクロスリファレンスした結果、以下が判明:

1. **未使用リソースの蓄積**: 完全に参照ゼロの version / plugin が約 3 GB 占有（python 3.8/3.9、nodejs 22.13.1、golang 1.21.6/1.23.6/1.24.0、php plugin）
2. **整合性ギャップ**: プロジェクトが要求するが未インストールのツール（python 3.12.9、azure-cli 2.75.0）が存在し、Homebrew runtime にサイレントフォールバックしている可能性
3. **再発防止策の不在**: 上記の調査ロジックが手動で属人的、半年後に同じ調査をやり直すコストが高い
4. **形骸化した設定**: `zinit_setup.zsh:106-110` の OMZ asdf plugin は asdf 0.16+ の Go バイナリ化により実質 no-op の可能性、要検証

### Goal

asdf 環境の未使用リソースを削除しディスクを ~3 GB 解放しつつ、再発防止のための診断ツール (`asdf-doctor`) を整備する。同時に asdf 0.16+ で形骸化した設定を整理し、保守可能な状態に維持する。

### Success Criteria

- [ ] `~/.asdf` ディスク使用量が 11 GB → ~8 GB に減少
- [ ] `asdf list` から未使用 versions / plugins が消えている
- [ ] プロジェクト要求（python 3.12.9、azure-cli 2.75.0）が全て install 済み
- [ ] `make asdf-doctor` で `.tool-versions` ⇄ installed の差分がレポート可能
- [ ] zsh 起動時に plugin-index が週次で自動更新される（バックグラウンド、起動時間影響ゼロ）
- [ ] OMZ asdf plugin の要否が検証され、不要なら削除（補完・PATH に回帰なし）
- [ ] `docs/managed-tools.md` に asdf 棚卸し運用方法が記載されている

## Solution Overview

### Technical Approach

**2 フェーズ構成**:

```
Phase A (Prerequisites): ローカル環境クリーンアップ          Phase B (本 Spec の Stories): dotfiles リポ運用改善
（dotfiles リポには commit しない、手動チェックリスト）       （commit する、AI Agent 実装可能）

  A1. 事前検証                                                Story 1: scripts/asdf-doctor.sh 実装
       - go.mod toolchain スキャン                             Story 2: Makefile + docs から公開
       - .tool-versions バックアップ                           Story 3: zsh plugin-index 週次フック
       - asdf list / plugin list スナップショット              Story 4: OMZ asdf plugin 整理
  A2. 補完インストール
       - python 3.12.9, azure-cli 2.75.0 等
  A3. 削除
       - php plugin、未使用 versions
```

**設計意図**:
- Phase A は冪等な手作業ベース（再現性は A1.3 のスナップショットで担保）
- Phase B の Stories は再現可能な仕組み化。Story 1 (`asdf-doctor.sh`) は Phase A の調査ロジックの自動化版
- Story 4 (OMZ 整理) は実機検証してからの判断（即削除しない）

### Key Decisions

| 決定 | 選択 | 理由 |
|------|------|------|
| **Phase A の扱い** | Solution Overview の Prerequisites に記載、Stories からは除外 | Phase A は dotfiles リポへの commit を伴わない手動チェックリスト。Story 化すると AI Agent 実装と混同される |
| **Story の粒度** | B1 (Makefile) + B5 (docs) を 1 Story に統合 | 同一 Concern「asdf-doctor の人間向け公開窓口」。1 PR で完結すべき |
| **削除自動化の不採用** | `make asdf-clean-execute` のような自動削除ターゲットは作らない | 誤削除のブラストラディウスが大きい。人間の確認を必須化 |
| **plugin-index 更新方式** | zsh 起動時の lazy + 週次 sentinel ガード + バックグラウンド (`&!`) | 起動時間 0 影響を担保。失敗は次回再試行（指数バックオフは過剰） |
| **stat コマンドの互換** | BSD `-f %m` と GNU `-c %Y` の OR フォールバック | macOS / Linux / WSL クロスプラットフォーム動作（dotfiles の設計原則） |
| **Story の commit 粒度** | Story 単位で個別 commit | rollback 単位を確保（`git revert <sha>` が効く） |

### Out of Scope

- mise への移行（別途検討の結果、見送り）
- asdf 自体のアップグレード方式変更（Homebrew 経由のまま）
- Homebrew と重複する runtime（node / python / ruby / go / yarn / terraform）の brew 側削除
- CI への asdf-doctor 配線（将来の余地として残すのみ、本 Spec では実装しない）
- 自動削除ターゲット（誤削除リスク回避のため意図的に未実装）

## Prerequisites: Phase A 手動チェックリスト

本 Spec の Stories 実装前に、ローカル環境で以下を実施する。dotfiles リポへの commit は発生しない。

### A1. 事前検証（読み取り専用）

```bash
# A1.1 Go toolchain スキャン（削除候補 golang 1.21.6/1.23.6/1.24.0 の参照確認）
find ~/ghq -name go.mod 2>/dev/null | xargs grep -h "^toolchain" | sort -u

# A1.2 バックアップ
cp ~/.tool-versions /tmp/asdf-tool-versions-$(date +%Y%m%d).bak

# A1.3 スナップショット
asdf plugin list --urls > /tmp/asdf-plugins-before.txt
asdf list > /tmp/asdf-versions-before.txt
```

### A2. 補完インストール（追加のみ・破壊なし）

```bash
asdf install python 3.12.9
asdf plugin add azure-cli && asdf install azure-cli 2.75.0
# nodejs lts 参照プロジェクトは具体バージョンに直す
```

### A3. 削除（A1.1 で確認済みのもののみ）

```bash
asdf plugin remove php
asdf uninstall python 3.8.20
asdf uninstall python 3.9.18
asdf uninstall nodejs 22.13.1
# A1.1 で go.mod toolchain 参照ゼロ確認済みの場合のみ
asdf uninstall golang 1.21.6
asdf uninstall golang 1.23.6
asdf uninstall golang 1.24.0
```

### A 完了後の検証

```bash
node --version    # ~/.tool-versions の指定と一致
python --version  # ~/.tool-versions の指定と一致
ruby --version    # ~/.tool-versions の指定と一致
go version        # ~/.tool-versions の指定と一致
/usr/bin/du -sh ~/.asdf  # 11 GB → ~8 GB
```

ロールバック手段: A1.3 スナップショットから `asdf install <plugin> <version>` を逐次実行。

## Stories

### Story 1: scripts/asdf-doctor.sh を実装する

**Size**: M (1 Concern: 診断ツール新設、1 ファイル + テスト fixture)

**Intent**: code

**Context**: Phase A の手動調査（プロジェクト `.tool-versions` ⇄ installed のクロスリファレンス）を再現可能なスクリプト化する。半年後の再棚卸し時、属人化した調査ロジックを誰でも実行可能にする。

**Current Behavior**: 棚卸し調査は手動の find / grep / awk の組み合わせで属人的。`scripts/` 配下に asdf 系スクリプトは未存在。

**Expected Behavior**: `scripts/asdf-doctor.sh` を新設し、以下 4 セクションを Markdown テーブル形式で出力する: ① 未使用 versions（installed だが参照ゼロ）② 不足 versions（参照されてるが未 install）③ 健全 versions ④ 未使用 plugins。引数 `--clean-suggestions` で ① ④ を `asdf uninstall/plugin remove` コマンド形式で出力。引数 `--json` で機械可読出力。`SEARCH_DIRS` env で検索パス上書き可能（デフォルト `~/ghq:~/work`）。go.mod の toolchain 行も収集対象。`asdf` 未インストール環境では graceful skip（exit 0、stderr に "asdf not found, skip"）。異常系として `SEARCH_DIRS` が空ディレクトリのケースもクラッシュせず空セクション出力すること。

**Constraints**: bash 4+ または zsh 互換。POSIX 準拠ツール（find, grep, sort, awk）のみ依存。**絶対パスハードコード禁止**（CI / 他マシン互換、CLAUDE.md 規約）。`set -euo pipefail` で fail-fast。`--help` で使用方法表示（Production phase のドキュメント要件）。fixture 駆動の smoke test を含めること（unit test だけでは silent false-negative を見逃すため）。後方互換性は不要（新規スクリプト）。

**Verification**:
1. `bash -n scripts/asdf-doctor.sh` で構文 OK
2. `./scripts/asdf-doctor.sh` を Phase A 完了後に実行し「未使用 versions」セクションが空に近いこと
3. `./scripts/asdf-doctor.sh --json | jq .` で JSON valid
4. `./scripts/asdf-doctor.sh --help` で使用方法が表示される
5. fixture (`tests/fixtures/asdf-doctor/` に dummy `.tool-versions` 群を配置) で smoke test 実行、期待 4 セクションが全て出力されること
6. `command -v asdf` 不在環境で graceful skip（exit 0）
7. `SEARCH_DIRS=/tmp/empty ./scripts/asdf-doctor.sh` で検出ゼロ時もクラッシュしない
8. 既存テスト（`./test_zsh_config.zsh` 等）が引き続きパスする

### Story 2: asdf-doctor を Makefile / docs から公開する

**Size**: S (1 Concern: 公開窓口、2 ファイル)

**Intent**: chore

**Dependencies**: Story 1

**Context**: Story 1 の `scripts/asdf-doctor.sh` を直接実行するのではなく、`make asdf-doctor` という統一的な人間向けインタフェース経由で呼び出せるようにする。同時に `docs/managed-tools.md` に運用フローを記載し発見可能性を高める。

**Current Behavior**: Makefile に asdf 系ターゲット未存在。`docs/managed-tools.md` に asdf 棚卸し運用の記述なし。

**Expected Behavior**: Makefile に 3 ターゲット追加: (1) `asdf-doctor` — `./scripts/asdf-doctor.sh` 実行 (2) `asdf-clean-dry` — `--clean-suggestions` 付きで実行 (3) `asdf-sync` — `asdf plugin update --all` 実行。各ターゲットに `## help_text` コメント形式の help 説明を付与。`docs/managed-tools.md` の「パッケージ管理」テーブル直下に運用 note を追記（`make asdf-doctor` で診断、削除候補は `make asdf-clean-dry` で確認後に手動実行を推奨）。

**Constraints**: Makefile target に `@test -x scripts/asdf-doctor.sh || { echo "Error: scripts/asdf-doctor.sh not found or not executable" >&2; exit 1; }` 前提チェックを入れること。自動削除ターゲット (`asdf-clean-execute` 的なもの) は作らない（Key Decisions 参照）。既存 Makefile のスタイル（CYAN/NC カラーコード等）に合わせること。後方互換性: 既存 target に影響しないこと。

**Verification**:
1. `make asdf-doctor` 実行成功（Story 1 のスクリプト経由実行）
2. `make asdf-clean-dry` で `--clean-suggestions` 出力
3. `make asdf-sync` で `asdf plugin update --all` 起動
4. `scripts/asdf-doctor.sh` を一時的に rename した状態で `make asdf-doctor` を叩き、明示的なエラーメッセージで失敗すること（前提チェック動作確認）
5. `docs/managed-tools.md` の追記が「パッケージ管理」セクション直下にあること、Markdown が破壊されていないこと
6. 既存 Makefile target（`make help` 等）が引き続き正常動作すること

### Story 3: zsh に plugin-index 週次自動更新フックを追加する

**Size**: S (1 Concern: 起動時 lazy フック、1 ファイル)

**Intent**: chore

**Context**: `~/.asdf/plugin-index` の最終更新は 2025-06、約 11 ヶ月前。古い plugin-index は新規 plugin の検索・install を阻害する。zsh 起動時に sentinel ファイルで週次ガードしつつバックグラウンド更新する。

**Current Behavior**: plugin-index 更新は完全に手動。ユーザーが意識しない限り永続的に古いまま放置される。

**Expected Behavior**: `.zsh/zinit_setup.zsh` の `setup_dev_tools()` 内に lazy フック追加。`${ASDF_DATA_DIR:-$HOME/.asdf}/.plugin-index-updated` を sentinel として、最終更新から 7 日経過していれば `( asdf plugin update --all >/dev/null 2>&1 && touch <sentinel> ) &!` をバックグラウンド実行。`stat` の BSD (`-f %m`) と GNU (`-c %Y`) フォールバックでクロスプラットフォーム対応。

**Constraints**: zsh 起動時間に **1ms も影響しない** こと（`&!` で disown）。失敗時は sentinel 未更新 → 次回再試行（指数バックオフは実装しない、過剰）。asdf 未インストール環境では何もしない（既存の `[[ -d "${ASDF_DATA_DIR:-$HOME/.asdf}" ]]` ガード内に配置）。MSYS2/Windows でも構文エラーにならないこと（stat フォールバックで対応）。後方互換性: 既存の `setup_dev_tools()` ロジックに影響しないこと。

**Verification**:
1. `zsh -n .zsh/zinit_setup.zsh` で構文 OK
2. zsh 再起動 → 数秒後に `~/.asdf/.plugin-index-updated` が生成される
3. 即時再起動 → バックグラウンドジョブが走らない（`jobs` で確認、stamp が新しいまま）
4. sentinel を `touch -t 202501010000 ~/.asdf/.plugin-index-updated` で 7 日以上前にして再起動 → 更新が走る
5. zsh 起動時間計測（`/usr/bin/time -p zsh -i -c exit` 5 回平均）で hook 追加前後の差が ±50ms 以内
6. asdf 未インストール環境（一時的に `asdf` を PATH から外す）で zsh 起動エラーが出ないこと
7. 既存テスト（`./test_zsh_config.zsh`）が引き続きパスする

### Story 4: OMZ asdf plugin の要否を検証し整理する

**Size**: S (1 Concern: 形骸化設定の整理、1 ファイル)

**Intent**: chore

**Context**: `.zsh/zinit_setup.zsh:106-110` で `OMZP::asdf/asdf.plugin.zsh` を読み込んでいる。ただし OMZ asdf plugin は asdf 0.x の `$ASDF_DIR/asdf.sh` を `source` する設計で、asdf 0.16+ で Go バイナリ化された後は `asdf.sh` が削除されており、実質 no-op の可能性がある。直前 commit `7c7a033` でカスタム shim PATH 設定を `setup_asdf()` に実装済みのため、OMZ plugin の役割は重複・形骸化している可能性が高い。

**Current Behavior**: `zinit ice wait lucid; zinit snippet OMZP::asdf/asdf.plugin.zsh` が `setup_dev_tools()` 内で常時ロードされている。zsh 起動時にエラーは出ていないが、実際に何の機能を提供しているか不明。

**Expected Behavior**: 以下 4 ステップで検証 → 削除確定 or 残置: (1) `zinit_setup.zsh:106-110` をコメントアウトして zsh 再起動 (2) `asdf --version`, `asdf current`, `node --version` の動作確認 (3) `asdf <TAB>` でサブコマンド補完が動くことを確認 (4) 全て OK → snippet 行を削除して commit。NG → コメントアウトを戻し、不足機能を analysis して別実装（直接 fpath 操作等）。`.zsh/functions.zsh:22` の `fpath` 設定（completions 経路）はそのまま残すこと（asdf 自身が `${ASDF_DATA_DIR}/completions` に生成）。

**Constraints**: 検証ステップ (1)-(3) が完了するまで commit しない。検証で問題が見つかった場合、削除ではなく代替実装を検討すること。`functions.zsh:22` の fpath 設定には触れない（独立した補完経路）。後方互換性: 既存の補完・PATH 解決を破壊しないこと（破壊的変更検出時は revert）。

**Verification**:
1. 削除前後で `asdf --version` の出力が一致
2. 削除前後で `asdf current` の出力が一致
3. 削除前後で `which node`, `which python` 等のパスが一致（shim 経由のまま）
4. 削除後 `asdf <TAB>` で `current/install/list/plugin/...` 等のサブコマンドが補完される
5. zsh 起動時間計測で削除前後の差が ±50ms 以内（できれば短縮）
6. 検証 NG → コメントアウト復元、Spec の Status を Needs Revision に戻し、analysis 結果を Spec に追記してから再判定
7. 既存テスト（`./test_zsh_config.zsh`）が引き続きパスする

## Agent-Ready Summary

| Story | Size | Status | Missing Elements | Dependencies |
|---|---|---|---|---|
| S1: asdf-doctor.sh 実装 | M | **Ready** | None | なし |
| S2: Makefile / docs 公開 | S | **Ready** | None | S1 完了後 |
| S3: zsh plugin-index 週次フック | S | **Ready** | None | なし |
| S4: OMZ asdf plugin 整理 | S | **Ready (検証フェーズあり)** | 検証 NG 時の代替実装は実装時に decide | なし |

### 実行順序

```
S1 ─────→ S2
S3 (独立、並行可)
S4 (独立、検証あり、並行可)
```

S1 完了後に S2 着手。S3 / S4 は完全独立、並行実装可能。

### Phase: Production の品質ゲート

`ai-dlc-phases` > `phase-quality-matrix.md` Production 行より、本 Spec の Stories は以下を満たすこと:

- ✅ **Tests**: 単体 + 統合必須（S1 は fixture smoke test、S3 は zsh 起動時間計測）
- ✅ **Security baseline**: シークレット禁止、CVE スキャン（asdf 自体は対象外、scripts は外部入力なしのため Baseline で十分）
- ✅ **Error Handling**: `set -euo pipefail`、graceful skip、明示的エラーメッセージ
- ✅ **Documentation**: `--help` text、Makefile の help コメント、`docs/managed-tools.md` 追記
- ✅ **Code Review**: PR で人間レビュー必須

## Out of Scope

- mise への移行（別途検討済み、見送り）
- asdf 0.x → 0.19 のアップグレード手順自動化（Homebrew 経由で済むため不要）
- Homebrew と重複する runtime の brew 側削除（過去の優先度問題は `7c7a033` で解決済み、これ以上の整理は YAGNI）
- `.asdf/downloads` キャッシュ管理（現在 20KB、無視可能なサイズ）
- CI への `asdf-doctor` 配線（GitHub Actions 連携は将来余地として graceful skip 機構を設計に含めるのみ）
- 自動削除コマンド（`asdf-clean-execute` 等）の実装（誤削除リスク回避のため意図的に未実装）
- nodejs `lts` リテラルの恒久対応（プロジェクト側で具体バージョンに直す方針）

## Next Steps

- [ ] **Phase A 実施**: 本 Spec の Prerequisites セクションを手動実行（dotfiles リポへの commit なし）
- [ ] Review and refine stories（特に S4 の検証 NG パターン）
- [ ] Record significant design choices via `/ai-dlc:decision`（mise 見送り判断、自動削除不採用）
- [ ] Create GitHub Issues: `python3 plugins/ai-dlc/scripts/helpers/spec_to_issues.py --spec docs/ai-dlc/spec-asdf-cleanup.md --milestone "Sprint 2026-05-XX" --dry-run`
- [ ] Run `/ai-dlc:plan` to integrate into sprint planning
- [ ] Run `/ai-dlc:drive #N` to execute Agent Loop for ready stories（S1 と S3 / S4 を並行起動可）

## 参考

- 直前 commit `7c7a033`: asdf shim を OS setup 後に配置して `.tool-versions` を有効化（本 Spec の `setup_asdf()` 関数の起源）
- 既存検証フレームワーク: `.claude/skills/deploy-config/scripts/validate.sh`（Story 1 のテストパターン参考）
- プロジェクト規約: `CLAUDE.md`（git add 個別指定、絶対パスハードコード禁止、日本語応答）
- 廃止: 旧 brainstorming spec `docs/superpowers/specs/2026-05-07-asdf-cleanup-design.md`（本 Spec に統合し削除）
