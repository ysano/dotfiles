# AI-DLC Board Templates

チーム規模別の GitHub Projects V2 ボード設計テンプレート。
`setup-ai-dlc-board.sh` で自動生成される XML タグの参考構造。

### `scale` 属性

`<github-project>` タグの `scale` 属性はチームスケールを示す。`status.md`, `diagnose.md`, `verify.md` コマンドが `aggregate-sprint.py` の `--team-size` を自動検出するために使用する。省略時は `solo` にフォールバックする。有効値: `solo`, `pod`, `squad`, `enterprise`。

## スケール別フィールド一覧

| Field | Type | Solo | Pod | Squad | Enterprise |
|---|---|---|---|---|---|
| Status | Single Select | 4 | 4 | 6 | 7 |
| Priority | Single Select | o | o | o | o |
| Size | Single Select | o | o | o | o |
| Iteration | Iteration | (任意) | (任意) | o | o |
| AI-Confidence | Number | - | o | o | o |
| Turns-Used | Number | - | o | o | o |
| Spec-Link | Text | - | o | o | o |
| Review-Priority | Single Select | - | o | o | o |
| Component | Single Select | - | - | o | o |
| Agent-Assigned | Single Select | - | - | o | o |
| MTTV-Hours | Number | - | - | o | o |
| Rework-Count | Number | - | - | o | o |
| Sprint-Goal | Text | - | - | o | o |
| Blocked-By | Text | - | - | o | o |
| Security-Flag | Single Select | - | - | - | o |
| Domain-Cluster | Single Select | - | - | - | o |
| Compliance-Tag | Single Select | - | - | - | o |
| Cost-USD | Number | - | - | - | o |
| Approval-Status | Single Select | - | - | - | o |

## XML テンプレート

### Solo (4 フィールド)

```xml
<github-project id="PVT_..." url="https://github.com/users/OWNER/projects/N" scale="solo">
  <field name="Status" id="PVTSSF_...">
    <option name="Todo" id="..."/>
    <option name="In Progress" id="..."/>
    <option name="Review" id="..."/>
    <option name="Done" id="..."/>
  </field>
  <field name="Priority" id="PVTSSF_...">
    <option name="P0" id="..."/>
    <option name="P1" id="..."/>
    <option name="P2" id="..."/>
  </field>
  <field name="Size" id="PVTSSF_...">
    <option name="S" id="..."/>
    <option name="M" id="..."/>
    <option name="L" id="..."/>
  </field>
</github-project>
```

### Pod (8 フィールド)

```xml
<github-project id="PVT_..." url="https://github.com/users/OWNER/projects/N" scale="pod">
  <field name="Status" id="PVTSSF_...">
    <option name="Todo" id="..."/>
    <option name="In Progress" id="..."/>
    <option name="Review" id="..."/>
    <option name="Done" id="..."/>
  </field>
  <field name="Priority" id="PVTSSF_...">
    <option name="P0" id="..."/>
    <option name="P1" id="..."/>
    <option name="P2" id="..."/>
  </field>
  <field name="Size" id="PVTSSF_...">
    <option name="S" id="..."/>
    <option name="M" id="..."/>
    <option name="L" id="..."/>
  </field>
  <field name="AI-Confidence" id="PVTF_..."/>
  <field name="Turns-Used" id="PVTF_..."/>
  <field name="Spec-Link" id="PVTF_..."/>
  <field name="Review-Priority" id="PVTSSF_...">
    <option name="High" id="..."/>
    <option name="Medium" id="..."/>
    <option name="Low" id="..."/>
  </field>
  <field name="Iteration" id="PVTIF_..."/>
</github-project>
```

### Squad (14 フィールド)

```xml
<github-project id="PVT_..." url="https://github.com/orgs/ORG/projects/N" scale="squad">
  <field name="Status" id="PVTSSF_...">
    <option name="Triage" id="..."/>
    <option name="Backlog" id="..."/>
    <option name="Ready" id="..."/>
    <option name="In Progress" id="..."/>
    <option name="Review" id="..."/>
    <option name="Done" id="..."/>
  </field>
  <field name="Priority" id="PVTSSF_...">
    <option name="P0" id="..."/>
    <option name="P1" id="..."/>
    <option name="P2" id="..."/>
  </field>
  <field name="Size" id="PVTSSF_...">
    <option name="S" id="..."/>
    <option name="M" id="..."/>
    <option name="L" id="..."/>
  </field>
  <field name="AI-Confidence" id="PVTF_..."/>
  <field name="Turns-Used" id="PVTF_..."/>
  <field name="Spec-Link" id="PVTF_..."/>
  <field name="Review-Priority" id="PVTSSF_...">
    <option name="High" id="..."/>
    <option name="Medium" id="..."/>
    <option name="Low" id="..."/>
  </field>
  <field name="Component" id="PVTSSF_...">
    <option name="Pod-A" id="..."/>
    <option name="Pod-B" id="..."/>
    <option name="Pod-C" id="..."/>
  </field>
  <field name="Agent-Assigned" id="PVTSSF_...">
    <option name="AI" id="..."/>
    <option name="Human" id="..."/>
    <option name="Pair" id="..."/>
  </field>
  <field name="MTTV-Hours" id="PVTF_..."/>
  <field name="Rework-Count" id="PVTF_..."/>
  <field name="Sprint-Goal" id="PVTF_..."/>
  <field name="Blocked-By" id="PVTF_..."/>
  <field name="Iteration" id="PVTIF_..."/>
</github-project>
```

### Enterprise (20 フィールド)

```xml
<github-project id="PVT_..." url="https://github.com/orgs/ORG/projects/N" scale="enterprise">
  <field name="Status" id="PVTSSF_...">
    <option name="Triage" id="..."/>
    <option name="Backlog" id="..."/>
    <option name="Ready" id="..."/>
    <option name="In Progress" id="..."/>
    <option name="In CI" id="..."/>
    <option name="Review" id="..."/>
    <option name="Done" id="..."/>
  </field>
  <field name="Priority" id="PVTSSF_...">
    <option name="P0" id="..."/>
    <option name="P1" id="..."/>
    <option name="P2" id="..."/>
  </field>
  <field name="Size" id="PVTSSF_...">
    <option name="S" id="..."/>
    <option name="M" id="..."/>
    <option name="L" id="..."/>
  </field>
  <field name="AI-Confidence" id="PVTF_..."/>
  <field name="Turns-Used" id="PVTF_..."/>
  <field name="Spec-Link" id="PVTF_..."/>
  <field name="Review-Priority" id="PVTSSF_...">
    <option name="High" id="..."/>
    <option name="Medium" id="..."/>
    <option name="Low" id="..."/>
  </field>
  <field name="Component" id="PVTSSF_...">
    <option name="Pod-A" id="..."/>
    <option name="Pod-B" id="..."/>
    <option name="Pod-C" id="..."/>
  </field>
  <field name="Agent-Assigned" id="PVTSSF_...">
    <option name="AI" id="..."/>
    <option name="Human" id="..."/>
    <option name="Pair" id="..."/>
  </field>
  <field name="MTTV-Hours" id="PVTF_..."/>
  <field name="Rework-Count" id="PVTF_..."/>
  <field name="Sprint-Goal" id="PVTF_..."/>
  <field name="Blocked-By" id="PVTF_..."/>
  <field name="Security-Flag" id="PVTSSF_...">
    <option name="None" id="..."/>
    <option name="Review-Required" id="..."/>
    <option name="Approved" id="..."/>
  </field>
  <field name="Domain-Cluster" id="PVTSSF_...">
    <option name="Cluster-A" id="..."/>
    <option name="Cluster-B" id="..."/>
    <option name="Cluster-C" id="..."/>
  </field>
  <field name="Compliance-Tag" id="PVTSSF_...">
    <option name="None" id="..."/>
    <option name="SOC2" id="..."/>
    <option name="HIPAA" id="..."/>
    <option name="PCI" id="..."/>
  </field>
  <field name="Cost-USD" id="PVTF_..."/>
  <field name="Approval-Status" id="PVTSSF_...">
    <option name="Pending" id="..."/>
    <option name="Approved" id="..."/>
    <option name="Rejected" id="..."/>
  </field>
  <field name="Iteration" id="PVTIF_..."/>
</github-project>
```

## ステータスマッピング

### Solo/Pod (4 statuses)

| GitHub Status | Agent Loop 段階 | Entry | Exit |
|---|---|---|---|
| Todo | Triage + Spec Definition | Issue 作成 | Spec 5/5 + 承認 |
| In Progress | AI Planning + Implementation + Auto-Verification | Agent 割当 | PR 作成 + CI 緑 |
| Review | Human Review | PR review ready | 承認 + マージ |
| Done | Done | マージ | メトリクス記録 |

サブステートはラベルで区別: `ai-planning`, `ci-running`

### Squad (6 statuses)

| GitHub Status | Agent Loop 段階 | Entry | Exit |
|---|---|---|---|
| Triage | Triage (AI Auto) | Project 追加 | カテゴリ確定 |
| Backlog | Spec Definition | Triage 完了 | Atomic Spec 5/5 |
| Ready | AI Planning | Spec 承認 | 計画承認 |
| In Progress | AI Implementation + Auto-Verification | 計画承認 | PR + CI 緑 |
| Review | Human Review | CI 緑 | 承認 + マージ |
| Done | Done | マージ | メトリクス記録 |

### Enterprise (7 statuses)

| GitHub Status | Agent Loop 段階 | Entry | Exit |
|---|---|---|---|
| Triage | Triage (AI Auto) | Project 追加 | カテゴリ確定 |
| Backlog | Spec Definition | Triage 完了 | Atomic Spec 5/5 |
| Ready | AI Planning | Spec 承認 | 計画承認 |
| In Progress | AI Implementation | 計画承認 | PR 作成 |
| In CI | Auto-Verification | PR 作成 | 全チェック緑 |
| Review | Human Review | CI 緑 | 承認 + マージ |
| Done | Done | マージ | メトリクス記録 |

## ビュー設定

### Board View (全スケール共通)

- **Column**: Status
- **Sort**: Priority (P0 優先)
- **Group**: なし (Solo/Pod) / Component (Squad+)
- **Card fields**: Priority, Size, AI-Confidence, Assignee

### Table View (分析用)

推奨カラム: Title, Status, Priority, Size, AI-Confidence, Turns-Used, Spec-Link, Assignee

フィルタプリセット:
- Sprint Active: `milestone:@current is:open`
- Ready for AI: `status:Ready -label:blocked`
- Review Queue: `status:Review`
- Stale Items: `updated:<@today-30d is:open`

### Roadmap View (Squad+ 計画用)

- **Date field**: Iteration
- **Group**: Priority or Component

## Built-in Workflows

| Workflow | Trigger | Action |
|---|---|---|
| Item added | Issue/PR 追加 | Status → Todo (Solo/Pod) / Triage (Squad+) |
| Item closed | Issue クローズ | Status → Done |
| PR merged | PR マージ | Status → Done |
| Auto-archive | Done + 14 日経過 | アーカイブ |

## セットアップ手順

```bash
# 1. スクリプトでプロジェクト作成
setup-ai-dlc-board.sh --owner OWNER --title "Sprint N" --scale pod --repo REPO

# 2. 出力された XML タグを CLAUDE.md に貼り付け

# 3. Built-in Workflows を手動設定（Settings > Workflows）

# 4. Iteration フィールドを必要に応じて GraphQL で作成
```
