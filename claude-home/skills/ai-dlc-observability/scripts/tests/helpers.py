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
