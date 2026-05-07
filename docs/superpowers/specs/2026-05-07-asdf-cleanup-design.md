# asdf 棚卸しと運用改善 設計仕様

- 作成日: 2026-05-07
- 状態: Draft（実装前レビュー待ち）
- 起点: asdf → mise マイグレーション検討の結果、移行は見送り、現状改善の方向で再合意

## 背景

dotfiles 環境の asdf (0.19.0) は 13 プラグイン / 308 shim / 11 GB を消費している。プロジェクト 18 件の `.tool-versions` および global `~/.tool-versions` とクロスリファレンスした結果、**約 3 GB ・ 5 versions ・ 1 plugin が完全に未使用**であることが判明。同時に、参照されているが未インストールの version（python 3.12.9, azure-cli 2.75.0）も発見された。

直前 commit `7c7a033` で asdf 統合の難所（Homebrew runtime との優先度問題）は通過済みのため、現時点が最も低リスクで棚卸しを行えるタイミングと判断。

## ゴール

1. **未使用リソースの削除**: 完全に参照ゼロの version / plugin を削除し、ディスクを ~3 GB 解放
2. **整合性の補完**: プロジェクトが要求しているが未インストールの tool を補完
3. **再発防止**: 同種の棚卸しを将来 1 コマンドで実施可能にする運用ツール整備
4. **設定の整理**: asdf 0.16+ で形骸化している可能性のある設定（OMZ asdf plugin）の要否確認と削除

## 非ゴール

- mise への移行（別途検討の結果、見送り）
- asdf 自体のアップグレード方式変更（Homebrew 経由のまま）
- Homebrew と重複する runtime（node, python, ruby, go, yarn, terraform）の brew 側削除
- CI への asdf-doctor 配線（将来の余地として残すのみ）

## アーキテクチャ

2 フェーズ構成:

```
Phase A: One-shot ローカル環境クリーンアップ           Phase B: dotfiles リポ運用改善
（dotfiles リポには commit しない）                  （dotfiles リポに commit）

  A1. 事前検証（go.mod toolchain スキャン、         B1. Makefile に asdf 系ターゲット追加
      バックアップ、スナップショット）                B2. scripts/asdf-doctor.sh 新設
  A2. 補完インストール                              B3. zsh: plugin-index 自動更新フック
  A3. 削除（plugin / version）                       B4. zinit_setup.zsh の OMZ asdf plugin 整理
                                                     B5. docs/managed-tools.md に運用 note 追記
```

設計意図:
- A は冪等な手作業ベース、B は再現可能な仕組み化
- B の `asdf-doctor.sh` は A での調査ロジックの自動化版
- B4 は実機検証してからの判断（即削除しない）

## Phase A: ローカル環境クリーンアップ

### A1. 事前検証（読み取り専用）

```bash
# A1.1 Go toolchain スキャン
find ~/ghq -name go.mod 2>/dev/null | xargs grep -h "^toolchain" | sort -u
# → 削除候補 golang 1.21.6 / 1.23.6 / 1.24.0 のいずれかがヒットすれば「保留」

# A1.2 バックアップ
cp ~/.tool-versions /tmp/asdf-tool-versions-$(date +%Y%m%d).bak

# A1.3 スナップショット
asdf plugin list --urls > /tmp/asdf-plugins-before.txt
asdf list > /tmp/asdf-versions-before.txt
```

### A2. 補完インストール

| ステップ | コマンド | 目的 |
|---|---|---|
| 2.1 | `asdf install python 3.12.9` | プロジェクト要求への補完 |
| 2.2 | `asdf plugin add azure-cli && asdf install azure-cli 2.75.0` | プロジェクト要求への補完 |
| 2.3 | nodejs `lts` 参照プロジェクトに具体バージョンを書き直す | 推奨方針（プロジェクトオーナーに依る場合は保留） |

### A3. 削除（A1.1 で確認済みのもののみ）

| ステップ | コマンド | 削除理由 |
|---|---|---|
| 3.1 | `asdf plugin remove php` | versions 0、参照ゼロ |
| 3.2 | `asdf uninstall python 3.8.20` / `asdf uninstall python 3.9.18` | global=3.11.9、参照ゼロ |
| 3.3 | `asdf uninstall nodejs 22.13.1` | global=24.15.0、参照ゼロ |
| 3.4 | `asdf uninstall golang 1.21.6 / 1.23.6 / 1.24.0` | global=1.22.12、`.tool-versions` 参照ゼロ（A1.1 で go.mod toolchain 参照ゼロを確認済みの場合のみ） |
| 3.5 | 動作確認 | `node --version` 等が `.tool-versions` 指定と一致すること |

### Phase A 安全機構

| 機構 | 内容 |
|---|---|
| **dry-run 起点** | A3 の削除コマンドは初回 `echo` プレフィックス付きで一覧表示し目視確認後に実行 |
| **段階的削除** | plugin → version の順、各ステップ後 `asdf list` で進捗確認 |
| **ロールバック** | A1.3 スナップショットから `asdf install` で逐次再構築（python 3.8 系 ~5 分、golang ~30 秒、nodejs ~3 分） |
| **golang 保留判定** | A1.1 で疑念があれば全保留、Phase B の doctor 完成後に再判定 |

## Phase B: dotfiles リポ運用改善

### B1. Makefile に asdf 系ターゲット追加

```makefile
asdf-doctor:        ## .tool-versions ⇄ installed の差分検出（読み取り専用）
	@./scripts/asdf-doctor.sh

asdf-clean-dry:     ## 削除候補を表示（実行はしない）
	@./scripts/asdf-doctor.sh --clean-suggestions

asdf-sync:          ## plugin-index と asdf プラグインを最新化
	@asdf plugin update --all
	@echo "Run 'asdf install' in each project to fetch missing tool versions."
```

設計判断: 自動削除ターゲットは作らない（誤削除リスク回避、人間の確認を必須化）。

### B2. `scripts/asdf-doctor.sh` 新設

機能仕様:

```
入力:   ~/.asdf/installs/, ~/.tool-versions
        $SEARCH_DIRS 配下の .tool-versions（デフォルト ~/ghq:~/work、env で上書き可）
        go.mod の toolchain 行

出力:   Markdown テーブル形式 4 セクション
          ① 未使用 versions（installed だが .tool-versions 参照ゼロ）
          ② 不足 versions（参照されてるが未 install）
          ③ 健全 versions（参照あり & install 済み）
          ④ 未使用 plugins（versions ゼロ かつ参照ゼロ）

引数:   --clean-suggestions  ① ④ を asdf uninstall/plugin remove コマンド形式で出力
        --json               機械可読出力（CI 連携用）

依存:   bash 4+ または zsh、find, grep, sort（POSIX 準拠ツールのみ）
        絶対パスハードコード禁止（CI 互換）
        asdf 未インストール環境では graceful skip（exit 0）
```

#### B2 期待出力スケッチ（出力フォーマット契約）

実装はこの出力を再現すること。

```markdown
# asdf doctor report (2026-05-07)

## Unused Versions (uninstall candidates)
| Plugin | Version | Disk | Last access |
|--------|---------|------|-------------|
| python | 3.8.20  | 750M | 2025-03-27  |
| python | 3.9.18  | 740M | 2024-10-25  |
...

## Missing Versions (referenced but not installed)
| Plugin    | Version | Referenced in |
|-----------|---------|---------------|
| python    | 3.12.9  | ~/ghq/.../foo |
| azure-cli | 2.75.0  | ~/ghq/.../bar |

## Healthy Versions
20 versions across 12 plugins, all referenced.
- python 3.11.9 (global)
- nodejs 24.15.0 (global)
- ... (一覧は省略可、`--verbose` で全表示)

## Unused Plugins (no versions, no references)
| Plugin | Action                       |
|--------|------------------------------|
| php    | asdf plugin remove php       |

## Index freshness
Last `asdf plugin update --all`: 2025-06-11 (330 days ago)
```

注: 上記日付・件数はサンプル。実装時は実環境の値を出力する（B3 の sentinel ファイル mtime と `~/.asdf/installs/` の集計から算出）。

### B3. zsh: plugin-index 自動更新フック

`.zsh/zinit_setup.zsh` の `setup_dev_tools()` 内に追加:

```zsh
# ASDF plugin-index の週次更新（バックグラウンド・非同期）
if [[ -d "${ASDF_DATA_DIR:-$HOME/.asdf}" ]]; then
    local stamp="${ASDF_DATA_DIR:-$HOME/.asdf}/.plugin-index-updated"
    local week=$((7 * 24 * 60 * 60))
    if [[ ! -f "$stamp" ]] || (( $(date +%s) - $(stat -f %m "$stamp" 2>/dev/null || stat -c %Y "$stamp" 2>/dev/null) > week )); then
        ( asdf plugin update --all >/dev/null 2>&1 && touch "$stamp" ) &!
    fi
fi
```

設計ポイント:
- `&!` で zsh disown — 起動時間に影響を与えない
- BSD `stat -f %m` と GNU `stat -c %Y` のフォールバックでクロスプラットフォーム
- 失敗時 sentinel 未更新 = 次回再試行（指数バックオフは過剰、実装しない）

### B4. OMZ asdf plugin の整理（検証 → 削除）

検証ステップ:

```
1. zinit_setup.zsh:106-110 をコメントアウトして zsh 再起動
2. asdf 動作確認: asdf --version, asdf current, node --version
3. completions 確認: asdf <TAB> でサブコマンド補完
4. 全て OK → 削除確定（zinit snippet 行を削除）
   NG → 役割を analysis して別実装（直接 fpath 操作）
```

注: `.zsh/functions.zsh:22` の `fpath` 設定はそのまま残す（asdf 自身が `${ASDF_DATA_DIR}/completions` に生成）。

### B5. `docs/managed-tools.md` に運用 note 追記

「パッケージ管理」テーブル直下に追記:

```markdown
> **asdf 棚卸し**: `make asdf-doctor` で .tool-versions と installed の整合性チェック。
> 削除候補は `make asdf-clean-dry` で確認後、手動 `asdf uninstall` を推奨。
```

### Phase B 成果物の依存関係

| 成果物 | 単独完結 | 依存 | 並行可能性 |
|---|---|---|---|
| B2 `scripts/asdf-doctor.sh` | ○ | なし | 先行（B1/B5 のブロッカー） |
| B1 `Makefile` ターゲット | × | B2 | B5 と並行可 |
| B5 `docs/managed-tools.md` 追記 | × | B1, B2 | B1 と並行可 |
| B3 zsh plugin-index フック | ○ | なし | 完全独立 |
| B4 OMZ asdf 整理 | ○ | なし | 完全独立 |

実装順: **B2 → (B1, B5 並行)**、**B3 並行**、**B4 並行**

## エラー処理 & ロールバック

### Phase A

| 失敗ケース | 検出 | 対応 |
|---|---|---|
| A1.1 で Go toolchain ヒット | grep 結果 ≠ 空 | 該当 version を削除候補から外し「保留」記録 |
| A2 plugin add 失敗 | 終了コード ≠ 0 | 即中断、A3 に進まない |
| A3 削除中に対象 version 使用中 | `asdf which` NotFound | 別 version を `.tool-versions` に書く / 再 install |
| A3 完了後に動作確認失敗 | `--version` が想定外 | A1.2 / A1.3 から復元 |

ロールバック: 全て手作業（自動 rollback スクリプトは作らない）。

### Phase B

| 失敗ケース | 検出 | 対応 |
|---|---|---|
| B2 doctor.sh 実行エラー | 終了コード ≠ 0 | `set -euo pipefail` で fail-fast |
| B3 plugin update ハング | bg のため zsh は影響なし | 次回再試行、`asdf-doctor` の "last-update" 表示で観測 |
| B4 削除後に補完が壊れる | `asdf <TAB>` 無反応 | 当該 commit を `git revert` |
| B1 依存ファイル不在 | `make asdf-doctor` で No such file | Makefile に `@test -x` 前提チェック |

ロールバック: 各変更を **個別 commit** に分け、`git revert` の粒度を確保。

commit 粒度:
- B2 (script 追加)
- B1 + B5 (Makefile + docs)
- B3 (zsh hook)
- B4 (OMZ 整理)

## 検証戦略

### Phase A

| ステップ | 検証 | 期待結果 |
|---|---|---|
| A1 完了時 | `cat /tmp/asdf-versions-before.txt` | スナップショット取得済み |
| A2 完了時 | `asdf list` | 補完対象が出現 |
| A3 完了時 | `asdf list` | 削除対象が消えている |
| A 全体完了時 | 各 global ツールの `--version` | `~/.tool-versions` と一致 |
| A 全体完了時 | `/usr/bin/du -sh ~/.asdf` | 11 GB → ~8 GB |
| A 全体完了時 | `ls ~/.asdf/shims \| wc -l` | 308 → 削減（具体値は計算困難） |

### Phase B

| ステップ | 検証 | 期待結果 |
|---|---|---|
| B2 単体 | `bash -n scripts/asdf-doctor.sh` | 構文 OK |
| B2 機能 | Phase A 後に `./scripts/asdf-doctor.sh` | 「未使用」セクションがほぼ空 |
| B2 機能 | `./scripts/asdf-doctor.sh --json \| jq .` | JSON valid |
| B1 | `make asdf-doctor` | スクリプト経由実行成功 |
| B3 | zsh 再起動 → sentinel 確認 | `~/.asdf/.plugin-index-updated` 生成 |
| B3 | 即時再起動 | bg ジョブ走らない（stamp 維持） |
| B4 | コメントアウト後 zsh 再起動 → asdf 動作 | OK なら削除確定 |
| 全体 | `.claude/skills/deploy-config/scripts/validate.sh` | PASS |
| 全体 | `zsh -n .zshrc .zprofile .zsh/zinit_setup.zsh` | 構文 OK |

### CI 互換性

`scripts/asdf-doctor.sh` は GitHub Actions（asdf 未インストール環境）でも動作:
- `command -v asdf >/dev/null || { echo "asdf not found, skip"; exit 0; }` で graceful skip
- 将来「リポ単体での lint CI」に組み込める余地を残す（今回は配線しない）

## オープン項目

- [ ] nodejs `lts` 参照プロジェクトの具体バージョン確定（A2.3）
- [ ] golang 削除候補の最終判定（A1.1 結果次第）
- [ ] OMZ asdf plugin の役割確定（B4 検証で判明）

## 参考

- 直前 commit `7c7a033`: asdf shim を OS setup 後に配置して `.tool-versions` を有効化
- 既存の検証フレームワーク: `.claude/skills/deploy-config/scripts/validate.sh`
- プロジェクト規約: `CLAUDE.md`（git add 個別指定、絶対パスハードコード禁止、日本語応答）
