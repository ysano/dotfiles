"""Factory functions for test data."""


def make_session(
    *,
    project_dir="/home/user/project",
    start_time="2026-02-10T09:00:00Z",
    end_time="2026-02-10T10:00:00Z",
    total_turns=20,
    user_turns=10,
    assistant_turns=10,
    tool_counts=None,
    modified_files=None,
):
    return {
        "project_dir": project_dir,
        "start_time": start_time,
        "end_time": end_time,
        "total_turns": total_turns,
        "user_turns": user_turns,
        "assistant_turns": assistant_turns,
        "tool_counts": tool_counts or {"Edit": 5, "Read": 10, "Bash": 5},
        "modified_files": modified_files or ["src/main.py", "src/utils.py"],
    }


def make_pr(
    *,
    number=1,
    title="feat: add feature",
    body="Closes #1",
    headRefName="feature-branch",
    createdAt="2026-02-10T10:00:00Z",
    mergedAt="2026-02-10T12:00:00Z",
):
    return {
        "number": number,
        "title": title,
        "body": body,
        "headRefName": headRefName,
        "createdAt": createdAt,
        "mergedAt": mergedAt,
    }


def make_issue(
    *,
    number=1,
    title="bug: something broken",
    createdAt="2026-02-10T08:00:00Z",
    closedAt="2026-02-10T10:00:00Z",
    labels=None,
):
    return {
        "number": number,
        "title": title,
        "createdAt": createdAt,
        "closedAt": closedAt,
        "labels": labels or [],
    }


def make_churn_data(**files):
    """Create churn data dict. Usage: make_churn_data(**{"src/a.py": 5, "src/b.py": 1})"""
    return {path: {"count": count} for path, count in files.items()}


def make_spec_scores(**specs):
    """Create spec scores dict. Usage: make_spec_scores(**{"spec.md": 4, "spec2.md": 2})"""
    return {path: {"score": score} for path, score in specs.items()}


def make_sprint(
    *,
    sprint_id="2026-02-03_2026-02-09",
    sprint_health=0.75,
    ai_confidence=0.70,
    vdf=1.5,
    svlt_hours=8.0,
    rework_rate=0.10,
    ttc_hours=None,
    mttv_macro=None,
    mttv_micro=None,
    session_count=10,
    total_turns=200,
    project_dir="/home/user/project",
    team_size="solo",
):
    """Create a sprint record matching aggregate-sprint.py output schema."""
    return {
        "sprint_id": sprint_id,
        "project_dir": project_dir,
        "team_size": team_size,
        "dora": {
            "vdf": {"value": vdf, "level": "HIGH"},
            "svlt": {"value_hours": svlt_hours, "level": "HIGH"},
            "rework_rate": {"value": rework_rate, "level": "HIGH"},
            "ttc": {"value_hours": ttc_hours, "level": "MEDIUM"} if ttc_hours is not None else None,
        },
        "ai_dlc": {
            "sprint_health": {"value": sprint_health, "level": "HEALTHY"},
            "ai_confidence": {"value": ai_confidence, "level": "HIGH"},
            "mttv_macro_hours": mttv_macro,
            "mttv_micro_seconds": mttv_micro,
        },
        "activity": {
            "session_count": session_count,
            "total_turns": total_turns,
        },
    }
