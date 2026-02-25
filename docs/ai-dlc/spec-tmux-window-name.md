---
type: ai-dlc-quick-spec
version: "1.0"
status: draft
created: 2026-02-25
story-count: 2
---

# Quick Spec: tmux-window-name

## 1. Problem & Goal

**Problem**: tmux のウィンドウ名がブランチ名のみの表示で、複数プロジェクトを同時に開いた際にどのリポジトリの作業かを即座に判別できない。

**Goal**: ウィンドウ名に「プロジェクト名:ブランチ名」を表示し、プロジェクトの区別を一目で可能にする。

**Success Criteria**:
- git リポジトリ内のウィンドウで `project:branch` 形式が表示される
- git リポジトリ外のウィンドウではフォルダ名のみ表示される（既存動作を維持）
- tmux リロード後に正常動作する
- 既存の Claude Voice アイコン・ウィンドウフラグとの共存

## 2. Solution Overview

**Approach**: ウィンドウ名の算出ロジックを `status.conf` のインラインシェルコマンドから外部スクリプト（`window_name.sh`）に切り出す。`git rev-parse --show-toplevel` でリポジトリルートを取得し、その `basename` をプロジェクト名とする。既存の `system_metrics.sh` と同じ `~/.tmux/scripts/` パターンに従う。

**Key Decisions**:
- **スクリプト切り出し**: インライン維持 vs 外部スクリプト — インラインでは `git rev-parse` 追加でさらに複雑化するため、テスト容易性とメンテナンス性を優先しスクリプト化
- **表示形式**: `project:branch`（コロン区切り）— ユーザー選択。パス区切りの `/` と混同しない
- **最大文字数**: 20 → 30 に拡大 — `project:branch` は情報量が増えるため
- **ブランチ名短縮**: 深い階層のブランチ名は親セグメントを1文字に abbreviate（例: `feature/auth/login-page` → `f/a/login-page`）

**Out of Scope**:
- ステータスバー（status-right）の変更
- ウィンドウ名の色分け（プロジェクト別カラーリング等）
- git worktree 対応（通常の `rev-parse --show-toplevel` で動作する）

## 3. Stories

#### Story 1: ウィンドウ名ヘルパースクリプトを作成する

- **Size**: S
- **Dependencies**: None

**Context**: 現在 `status.conf` のインラインコマンドで `git branch --show-current || basename || pane_current_command` の優先順位ロジックを実装している。プロジェクト名の追加でロジックが複雑化するため、テスト可能な外部スクリプトに切り出す。

**Current Behavior**: ウィンドウ名のロジックが `status.conf` 内のインラインシェルコマンドに埋め込まれている。ブランチ名のみ表示。

**Expected Behavior**: `.tmux/scripts/window_name.sh` が以下の出力をする:
- 引数: `$1` = `pane_current_path`, `$2` = `pane_current_command`
- git リポジトリ内: `{project}:{short_branch}` を出力（例: `dotfiles:master`, `dotfiles:f/a/login-page`）
- ブランチ名短縮: `/` を含むブランチ名は親階層を1文字に abbreviate（末尾セグメントはそのまま）
  - `feature/auth/login-page` → `f/a/login-page`
  - `fix/bug-123` → `f/bug-123`
  - `master` → `master`（変換なし）
- git リポジトリ外: `basename` of `pane_current_path` を出力（例: `Downloads`）
- フォールバック: `pane_current_command` を出力（例: `zsh`）

**Constraints**:
- `#!/bin/sh`（POSIX sh 互換、クロスプラットフォーム対応）
- 既存 `system_metrics.sh` と同じディレクトリ・パターンに従う
- `git` コマンド未インストール環境でもエラーにならないこと

**Verification**:
- [ ] `window_name.sh /path/to/git-repo zsh` で `project:branch` が出力される（短縮あり）
- [ ] `window_name.sh /tmp zsh` でフォルダ名 `tmp` が出力される
- [ ] `window_name.sh /nonexistent zsh` で `zsh` が出力される
- [ ] `chmod +x` で実行権限が付与されている

#### Story 2: status.conf のウィンドウフォーマットをスクリプト呼び出しに置換する

- **Size**: S
- **Dependencies**: Story 1

**Context**: `status.conf` の `window-status-format` / `window-status-current-format` がインラインで複雑なシェルコマンドを実行している。Story 1 のスクリプトに置換する。

**Current Behavior**: `status.conf` 11-12行目でインラインコマンド `cd '#{pane_current_path}' && { git branch --show-current || basename ... } | head -c 20` を使用。ブランチ名のみ20文字で切り詰め。

**Expected Behavior**: `window-status-format` / `window-status-current-format` が `#(~/.tmux/scripts/window_name.sh '#{pane_current_path}' '#{pane_current_command}' | head -c 30)` を使用。`project:branch` 形式で30文字で切り詰め。Claude Voice アイコン・ウィンドウフラグは維持。

**Constraints**:
- `window-status-format`（非アクティブ）と `window-status-current-format`（アクティブ）の両方を変更
- Claude Voice アイコン `@claude_voice_icon_#I` の表示を維持
- `#F`（ウィンドウフラグ）の表示を維持
- `check_conflicts.sh` で競合が検出されないこと

**Verification**:
- [ ] `tmux source ~/.tmux.conf` でエラーなくリロードできる
- [ ] git リポジトリ内のウィンドウで `project:branch` 形式が表示される
- [ ] git リポジトリ外のウィンドウでフォルダ名が表示される
- [ ] Claude Voice アイコンが正常に表示される
- [ ] `.claude/skills/tmux-config/scripts/check_conflicts.sh` で競合なし

## 4. Out of Scope

- ステータスバー（status-left / status-right）の変更
- プロジェクト別カラーリング・アイコン表示
- `automatic-rename-format`（`base.conf` 30行目）の変更 — tmux 内部のリネームは別ロジック
- resurrect/continuum との連携変更

## Agent-Ready Summary

| Story | Size | Status | Missing Elements |
|---|---|---|---|
| Story 1: ウィンドウ名ヘルパースクリプトを作成する | S | Ready | None |
| Story 2: status.conf のウィンドウフォーマットを置換する | S | Ready | None |

## Next Steps

- [ ] Review and refine stories
- [ ] Run `/ai-dlc:drive #1` to execute Story 1
- [ ] Run `/ai-dlc:drive #2` to execute Story 2
- [ ] Run `check_conflicts.sh` for final validation
