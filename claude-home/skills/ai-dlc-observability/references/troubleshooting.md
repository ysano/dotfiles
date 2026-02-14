# Observability トラブルシューティング

## 症状と対処

### sessions データなし

**原因**: G5 Stop Hook (`metrics-collector.py`) が未稼働。

**対処**:
1. `~/.claude/hooks.json` に Stop Hook エントリがあるか確認
2. `ls ~/.claude/metrics/sessions.jsonl` でファイル存在を確認
3. Hook を手動テスト: 新しい Claude Code セッションを開始・終了し、ファイルを再確認

### DORA 全 null

**原因**: `gh` CLI 未認証、またはリポジトリに PR/Issue がない。

**対処**:
1. `gh auth status` で認証状態を確認
2. `gh pr list --state merged --limit 5` で PR 取得を確認
3. リモートリポジトリが設定されているか確認: `git remote -v`

### churn データなし

**原因**: `/tmp/claude-churn-cache/` がクリアされた（OS 再起動等）。

**対処**:
1. `ls /tmp/claude-churn-cache/` でキャッシュ存在を確認
2. G3 PostToolUse Hook (`churn-counter.py`) が設定されているか確認
3. 再起動後は次のセッションで自動再生成される

### spec スコアなし

**原因**: G4 PreToolUse Hook (`check-spec-existence.py`) 未稼働、または spec ファイルがない。

**対処**:
1. `ls /tmp/claude-spec-quality-cache/` でキャッシュ確認
2. プロジェクトに spec ファイル (`*.spec.md`, `SPEC.md` 等) が存在するか確認
3. G4 Hook の設定を確認

### Sprint Health 常に ATTENTION

**原因**: 全データソースが neutral (0.5) で返っている。

**対処**:
1. `--verbose` で各データソースの読み込み状態を確認
2. 最低でも sessions.jsonl + GitHub CLI の 2 つのデータソースを投入
3. churn cache と spec cache を追加すると精度が向上

### sprints.jsonl 書き込みエラー

**原因**: `~/.claude/metrics/` ディレクトリの権限不足。

**対処**:
```bash
mkdir -p ~/.claude/metrics
chmod 755 ~/.claude/metrics
```

### sessions.jsonl 肥大化

**原因**: ローテーション未実施。

**対処**:
```bash
# 確認
python3 .claude/skills/ai-dlc-observability/scripts/rotate-sessions.py --dry-run

# 実行（30日超をアーカイブ）
python3 .claude/skills/ai-dlc-observability/scripts/rotate-sessions.py

# カスタム日数
python3 .claude/skills/ai-dlc-observability/scripts/rotate-sessions.py --days 60
```

### Scale 閾値不一致

**原因**: チームプロジェクトを Solo 閾値で評価している。

**対処**:
```bash
# Pod (2-10名) で集計
python3 ... --team-size pod

# Squad (10-30名) で集計
python3 ... --team-size squad
```

### TTC が null

**原因**: Bug ラベル (`bug`, `bugfix`, `hotfix`) が付いた Issue がない、または未クローズ。

**対処**:
1. `gh issue list --label bug --state closed` で Bug Issue の存在を確認
2. Issue に適切なラベルが付いているか確認
3. Bug がなければ TTC=null は正常（Sprint Health では bug_count=0 → ELITE 扱い）
