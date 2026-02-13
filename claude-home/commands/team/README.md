# Team Commands

Commands for team collaboration, project management, and workflow optimization.

## Available Commands

| Command | Description | Notes |
|---------|-------------|-------|
| `architecture-review` | Review and improve system architecture | |
| `decision-quality-analyzer` | Analyze decision quality with bias detection and scenario testing | |
| `dependency-mapper` | Map and visualize project dependencies | |
| `estimate-assistant` | Generate data-backed project time estimates | |
| `github-issue-automation` | Automate GitHub Issue creation, triage, and lifecycle | Delegates to `github-ticket-agent` agent |
| `github-project-manager` | Manage sprints and milestones via GitHub Projects V2 | Delegates to `github-board-agent` agent |
| `github-workflow-automation` | Automate workflows with GitHub Actions and Project automation | |
| `issue-triage` | Triage and prioritize issues effectively | |
| `memory-spring-cleaning` | Clean and organize project memory files | |
| `migration-assistant` | Plan and execute system migrations | |
| `retrospective-analyzer` | Analyze sprint retrospectives for insights | |
| `sprint-planning` | Plan and organize sprint workflows | |
| `standup-report` | Generate daily standup reports from git and project data | |
| `team-workload-balancer` | Balance team workload distribution | |

## Related

- `session-learning-capture` is now a **Stop hook** in `hooks/session-learning-capture.sh` (auto-triggers on session end)
- Agent delegates use `github-projects-v2` and `ticket-management` skills for platform knowledge
- For AI-Native team ceremonies (Atomic Spec quality check, Interaction Churn analysis, Output/Outcome Done gap), see `/ai-dlc:*` commands:
  - `/team:sprint-planning` → `/ai-dlc:plan` (adds AI pre-analysis, Spec quality check)
  - `/team:standup-report` → `/ai-dlc:digest` (adds 4h-rule violation, Churn alerts)
  - `/team:retrospective-analyzer` → `/ai-dlc:diagnose` (adds Churn root cause, CLAUDE.md improvement)
