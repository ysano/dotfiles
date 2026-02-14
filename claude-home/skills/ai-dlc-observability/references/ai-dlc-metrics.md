# AI-DLC 固有メトリクス

DORA Four Keys を補完する AI-DLC 独自のメトリクス群。
AI エージェントの出力品質・効率・経済性を定量評価する。

---

## 1. AI-Confidence

AI エージェント出力への信頼度を 0-1 スケールで算出する複合指標。

### 4 要素と重み

| 要素 | 略称 | 重み | 算出方法 | データソース |
|---|---|---|---|---|
| Spec Quality | SQ | 0.30 | avg_spec_score / 5 | spec cache (G4) |
| Churn Inverse | CI | 0.25 | 1 - rework_rate | churn cache (G3) |
| Turns-Per-Resolution | TPR | 0.25 | clamp(1 - (avg_turns - 10) / 40, 0, 1) | sessions.jsonl (G5) |
| Session Efficiency | SE | 0.20 | edit_tool_ratio (Edit+Write / total_tools) | sessions.jsonl (G5) |

### 算出式

```python
def calc_ai_confidence(sq, ci, tpr, se):
    """AI-Confidence = SQ*0.30 + CI*0.25 + TPR*0.25 + SE*0.20"""
    return sq * 0.30 + ci * 0.25 + tpr * 0.25 + se * 0.20

def calc_sq(avg_spec_score):
    """Spec Quality: 0-5 スコアを 0-1 に正規化"""
    return avg_spec_score / 5.0

def calc_ci(rework_rate):
    """Churn Inverse: Rework Rate の逆数"""
    return 1.0 - min(rework_rate, 1.0)

def calc_tpr(avg_turns_per_session):
    """Turns-Per-Resolution: 10 turns = 理想, 50+ turns = 最低"""
    return max(0.0, min(1.0, 1.0 - (avg_turns_per_session - 10) / 40))

def calc_se(tool_counts):
    """Session Efficiency: 編集系ツールの使用比率"""
    edit_tools = sum(tool_counts.get(t, 0) for t in ["Edit", "Write", "MultiEdit"])
    total = sum(tool_counts.values())
    if total == 0:
        return 0.5  # 中立値
    return min(edit_tools / total, 1.0)
```

### 閾値

| Level | Score |
|---|---|
| Elite | >= 0.85 |
| High | >= 0.65 |
| Medium | >= 0.45 |
| Low | < 0.45 |

### Graceful Degradation

データソース欠損時はその要素を中立値 0.5 で代替:

| 欠損データ | 影響要素 | 代替値 |
|---|---|---|
| spec cache | SQ | 0.5 |
| churn cache | CI | 0.5 |
| sessions.jsonl | TPR, SE | 各 0.5 |

---

## 2. MTTV (Mean Time to Value)

価値デリバリーまでの平均所要時間。マクロ（PR サイクル）とミクロ（セッション内）の 2 粒度。

### MTTV Macro

```python
def calc_mttv_macro(merged_prs):
    """PR 作成からマージまでの中央値 (hours)"""
    durations = []
    for pr in merged_prs:
        created = parse_iso(pr["createdAt"])
        merged = parse_iso(pr["mergedAt"])
        durations.append((merged - created).total_seconds() / 3600)
    if not durations:
        return None
    return sorted(durations)[len(durations) // 2]
```

### MTTV Micro

```python
def calc_mttv_micro(sessions):
    """セッション内の 1 ターンあたり所要時間 (seconds)"""
    total_duration = 0
    total_turns = 0
    for s in sessions:
        if s.get("start_time") and s.get("end_time"):
            start = parse_iso(s["start_time"])
            end = parse_iso(s["end_time"])
            duration = (end - start).total_seconds()
            total_duration += duration
            total_turns += s.get("total_turns", 0)
    if total_turns == 0:
        return None
    return total_duration / total_turns
```

---

## 3. Token Efficiency

AI エージェントのコスト効率。OTel 必須。

### メトリクス

| 名前 | 算出式 | 単位 |
|---|---|---|
| Tokens/Commit | total_tokens / commit_count | tokens |
| Cost/PR | sum(session_cost) / pr_count | USD |
| Cost/Issue | sum(session_cost) / issue_count | USD |

### データソース

OTel Events (`claude_code.api_request`):
- `cost_usd`: API リクエスト毎のコスト
- `input_tokens`, `output_tokens`: トークン数

```bash
# OTel 有効化
export CLAUDE_CODE_ENABLE_TELEMETRY=1
```

### 注意事項

- Session JSONL には `costUSD` フィールドが**含まれない**
- OTel 未設定環境では Token Efficiency を算出不可 → Economics セクションをスキップ

---

## 4. Sprint Health Score

スプリント全体の健全性を 0-1 スケールで総合評価する複合指標。

### 5 要素均等加重

| 要素 | 重み | 算出方法 |
|---|---|---|
| VDF Score | 0.20 | DORA レベル → 数値変換 (Elite=1.0, High=0.75, Medium=0.5, Low=0.25) |
| SVLT Score | 0.20 | 同上 |
| Rework Score | 0.20 | 同上 |
| AI-Confidence | 0.20 | そのまま使用 (0-1) |
| Spec Coverage | 0.20 | score >= 3 の Issue 比率 |

### 算出式

```python
LEVEL_TO_SCORE = {"ELITE": 1.0, "HIGH": 0.75, "MEDIUM": 0.5, "LOW": 0.25}

def calc_sprint_health(vdf_level, svlt_level, rework_level,
                       ai_confidence, spec_coverage):
    scores = []
    for level in [vdf_level, svlt_level, rework_level]:
        if level:
            scores.append(LEVEL_TO_SCORE.get(level, 0.5))
        else:
            scores.append(0.5)  # データ不足時は中立値

    scores.append(ai_confidence if ai_confidence else 0.5)
    scores.append(spec_coverage if spec_coverage is not None else 0.5)

    return sum(scores) / len(scores)
```

### 閾値

| Level | Score |
|---|---|
| Healthy | >= 0.75 |
| Attention | >= 0.50 |
| Critical | < 0.50 |
