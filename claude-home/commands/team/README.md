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
| `team-workload-balancer` | Balance team workload distribution | |

## Related

- `session-learning-capture` is now a **Stop hook** in `hooks/session-learning-capture.sh` (auto-triggers on session end)
- Agent delegates use `github-projects-v2` and `ticket-management` skills for platform knowledge
- For AI-Native team ceremonies (sprint planning, daily digest, retrospectives), see `/ai-dlc:*` commands in `commands/ai-dlc/README.md`
