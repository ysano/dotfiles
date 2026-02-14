# DORA Four Keys — AI-DLC 再定義

従来の DORA Four Keys を AI-DLC ワークフローに適合させた再定義。
Spec 駆動開発・エージェント協働を前提とした算出式と閾値を提供する。

## 因果チェーン

```
Spec Quality → AI-Confidence → VDF (Throughput)
    ↓                              ↓
Rework Rate ← Churn ←── SVLT (Speed)
    ↓                              ↓
TTC (Recovery) ←─────────── Sprint Health
```

Spec 品質が上流の起点。品質が低いと Churn → Rework → TTC 悪化の連鎖が発生する。

---

## 1. VDF (Value Delivery Frequency)

**再定義**: Spec 適合 PR が Sprint 期間内にマージされた頻度。

### 算出式

```python
def calc_vdf(merged_prs, sprint_days):
    """VDF = Spec 適合 PR 数 / Sprint 日数"""
    qualified = [pr for pr in merged_prs if pr.get("linked_issue")]
    return len(qualified) / max(sprint_days, 1)
```

- `qualified`: Issue にリンクされた PR（Spec 駆動の証拠）
- Issue リンクなしの PR は「adhoc fix」として VDF から除外

### 閾値テーブル

| Level | Solo (1名) | Pod (2-10名) | Squad (10-30名) |
|---|---|---|---|
| Elite | >= 2.0/day | >= 5.0/day | >= 10.0/day |
| High | >= 1.0/day | >= 2.0/day | >= 5.0/day |
| Medium | >= 0.5/day | >= 1.0/day | >= 2.0/day |
| Low | < 0.5/day | < 1.0/day | < 2.0/day |

### データソース

- `gh pr list --state merged --json number,title,mergedAt,body,headRefName`
- Issue リンク検出: PR body の `Closes #N` / `Fixes #N` / ブランチ名 `issue-N`

---

## 2. SVLT (Spec-to-Value Lead Time)

**再定義**: Issue 作成から PR マージまでの経過時間。3 フェーズに分解。

### 算出式

```python
def calc_svlt(issue_created, pr_created, pr_merged):
    """SVLT = PR.mergedAt - Issue.createdAt (hours)"""
    total = (pr_merged - issue_created).total_seconds() / 3600

    # 分解
    cognitive_lt = (pr_created - issue_created).total_seconds() / 3600
    verify_lt = (pr_merged - pr_created).total_seconds() / 3600

    return {
        "total_hours": total,
        "cognitive_lt_hours": cognitive_lt,  # Spec理解 → コーディング開始
        "verify_lt_hours": verify_lt,        # PR作成 → マージ
    }
```

### 閾値テーブル (total_hours)

| Level | Solo | Pod | Squad |
|---|---|---|---|
| Elite | < 4h | < 8h | < 24h |
| High | < 8h | < 24h | < 48h |
| Medium | < 24h | < 48h | < 96h |
| Low | >= 24h | >= 48h | >= 96h |

### データソース

- `gh pr list --state merged --json number,createdAt,mergedAt,body`
- Issue.createdAt: PR body から `Closes #N` を抽出し `gh issue view N --json createdAt`

---

## 3. Rework Rate

**再定義**: Sprint 中に高 Churn（3回以上修正）されたファイルの割合。

### 算出式

```python
def calc_rework_rate(churn_data, total_modified_files):
    """Rework Rate = high_churn_files / total_modified_files"""
    high_churn = [f for f, d in churn_data.items() if d["count"] >= 3]
    if total_modified_files == 0:
        return 0.0
    return len(high_churn) / total_modified_files
```

### 閾値テーブル

| Level | 全スケール共通 |
|---|---|
| Elite | < 0.05 (5%) |
| High | < 0.15 (15%) |
| Medium | < 0.30 (30%) |
| Low | >= 0.30 (30%) |

### データソース

- `/tmp/claude-churn-cache/{project}_churn.json` (G3 Hook)
- `sessions.jsonl` の `modified_files` フィールド

### OTel vs ローカルの使い分け

Rework Rate は完全にローカルデータ（churn cache）で算出可能。OTel 不要。

---

## 4. TTC (Time to Correct)

**再定義**: バグ Issue 作成から修正 PR マージまでの経過時間。

### 算出式

```python
def calc_ttc(bug_issue_created, fix_pr_merged):
    """TTC = fix_PR.mergedAt - bug_issue.createdAt (hours)"""
    return (fix_pr_merged - bug_issue_created).total_seconds() / 3600
```

### 閾値テーブル (hours)

| Level | Solo | Pod | Squad |
|---|---|---|---|
| Elite | < 1h | < 4h | < 8h |
| High | < 4h | < 8h | < 24h |
| Medium | < 8h | < 24h | < 48h |
| Low | >= 8h | >= 24h | >= 48h |

### データソース

- `gh issue list --label bug --state closed --json number,createdAt,closedAt`
- バグラベル: `bug`, `bugfix`, `hotfix`

### OTel vs ローカルの使い分け

TTC は GitHub CLI のみで算出。OTel は補助的（修正セッションのコスト分析）。

---

## DORA レベル判定

4 指標の総合レベルは最も低い指標に引き下げられる（Weakest Link ルール）。

```python
LEVEL_ORDER = {"ELITE": 4, "HIGH": 3, "MEDIUM": 2, "LOW": 1}

def overall_dora_level(vdf_level, svlt_level, rework_level, ttc_level):
    levels = [vdf_level, svlt_level, rework_level, ttc_level]
    # null (データ不足) は除外
    valid = [l for l in levels if l is not None]
    if not valid:
        return None
    return min(valid, key=lambda l: LEVEL_ORDER.get(l, 0))
```
