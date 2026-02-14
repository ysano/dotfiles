# Observability トラブルシューティング

## Solo 環境セットアップ

AI-DLC Observability の Hook（G3/G4/G5）は `settings-ai-dlc.json` で定義されているが、
`~/.claude/settings.json` への登録は手動で行う必要がある。

> **将来**: Plugin Marketplace 配布時は自動登録される予定。

### 手順

1. **Hook スクリプトのデプロイ確認**

```bash
# link.sh でシンボリックリンク展開済みか確認
ls ~/.claude/hooks/{churn-counter.py,check-spec-existence.py,metrics-collector.py}
```

2. **`~/.claude/settings.json` に Hook エントリを追加**

既存の `hooks` オブジェクトに以下を **マージ** する（既存エントリは削除しない）:

```jsonc
{
  "hooks": {
    // --- 既存の hooks エントリはそのまま維持 ---

    // G4: Spec 存在チェック (PreToolUse に追加)
    "PreToolUse": [
      // ... 既存エントリ ...
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/check-spec-existence.py",
            "timeout": 5
          }
        ]
      }
    ],

    // G3: Churn カウンター (PostToolUse に追加)
    "PostToolUse": [
      {
        "matcher": "Write|Edit|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/churn-counter.py",
            "timeout": 5
          }
        ]
      }
    ],

    // G5: メトリクスコレクター (Stop に追加)
    "Stop": [
      {
        // ... 既存の Stop hooks ...
        "hooks": [
          // ... 既存エントリ ...
          {
            "type": "command",
            "command": ".claude/hooks/metrics-collector.py",
            "timeout": 15
          }
        ]
      }
    ]
  }
}
```

3. **メトリクスディレクトリ作成**

```bash
mkdir -p ~/.claude/metrics
```

4. **動作確認**

```bash
# 新しい Claude Code セッションを開始・終了し、データ生成を確認
ls ~/.claude/metrics/sessions.jsonl
ls /tmp/claude-churn-cache/
ls /tmp/claude-spec-quality-cache/
```

### 完全な Hook 参照

| Hook | イベント | スクリプト | 目的 |
|------|----------|-----------|------|
| G3 | PostToolUse (Write/Edit/MultiEdit) | `churn-counter.py` | ファイル変更頻度を記録 |
| G4 | PreToolUse (Write/Edit/MultiEdit) | `check-spec-existence.py` | Spec ファイル品質を評価 |
| G5 | Stop | `metrics-collector.py` | セッションメトリクスを収集 |

---

## 症状と対処

### sessions データなし

**原因**: G5 Stop Hook (`metrics-collector.py`) が未稼働。

**対処**:
1. `~/.claude/settings.json` の `hooks.Stop` に Hook エントリがあるか確認（上記セットアップ参照）
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
