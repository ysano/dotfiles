"""Factory functions for test data."""

from datetime import datetime, timezone

# Import classification functions from aggregate-sprint for dynamic level calc
from aggregate_sprint import (
    classify_level_higher_better,
    classify_level_lower_better,
    get_dora_thresholds,
)


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


def _classify_health_level(value):
    """Sprint Health level: HEALTHY >= 0.75, ATTENTION >= 0.50, else CRITICAL."""
    if value is None:
        return None
    if value >= 0.75:
        return "HEALTHY"
    if value >= 0.50:
        return "ATTENTION"
    return "CRITICAL"


def _classify_ai_confidence_level(value):
    """AI-Confidence level: ELITE >= 0.85, HIGH >= 0.65, MEDIUM >= 0.45, else LOW."""
    if value is None:
        return None
    if value >= 0.85:
        return "ELITE"
    if value >= 0.65:
        return "HIGH"
    if value >= 0.45:
        return "MEDIUM"
    return "LOW"


def make_pod_sessions(member_count=3, project_dir="/home/team/project",
                      base_date="2026-02-10", turns_per_member=20):
    """Create multiple sessions simulating Pod members.

    Each member gets a 1-hour session staggered by start time,
    with shared and unique modified files.
    """
    sessions = []
    for i in range(member_count):
        hour = 9 + i
        sessions.append(make_session(
            project_dir=project_dir,
            start_time=f"{base_date}T{hour:02d}:00:00Z",
            end_time=f"{base_date}T{hour+1:02d}:00:00Z",
            total_turns=turns_per_member,
            user_turns=turns_per_member // 2,
            assistant_turns=turns_per_member - turns_per_member // 2,
            tool_counts={"Edit": 5, "Read": 8, "Bash": 3, "Write": 2},
            modified_files=[f"src/module_{i}.py", "src/shared.py"],
        ))
    return sessions


def make_reviewed_pr(number, body, reviewers=2, rounds=1, rejected=False,
                     createdAt="2026-02-10T10:00:00Z",
                     mergedAt="2026-02-10T14:00:00Z"):
    """Create a PR with review metadata for Pod workflow.

    Extends make_pr with reviewDecision, reviews list, and reviewRounds.
    """
    pr = make_pr(
        number=number,
        body=body,
        createdAt=createdAt,
        mergedAt=mergedAt,
    )
    pr["reviewDecision"] = "CHANGES_REQUESTED" if rejected else "APPROVED"
    pr["reviews"] = [
        {"author": f"reviewer-{j+1}", "state": "APPROVED"}
        for j in range(reviewers)
    ]
    if rejected and rounds > 1:
        pr["reviews"] = [
            {"author": "reviewer-1", "state": "CHANGES_REQUESTED"},
        ] + pr["reviews"]
    pr["reviewRounds"] = rounds
    return pr


def make_sprint(
    *,
    sprint_id="2026-02-03_2026-02-09",
    sprint_health=0.75,
    ai_confidence=0.70,
    ai_confidence_components=None,
    vdf=1.5,
    svlt_hours=8.0,
    rework_rate=0.10,
    ttc_hours=None,
    mttv_macro=None,
    mttv_micro=None,
    session_count=10,
    total_turns=200,
    user_turns=100,
    assistant_turns=100,
    top_tools=None,
    file_count=20,
    project_dir="/home/user/project",
    team_size="solo",
    spec_coverage=None,
    vdf_qualified_prs=None,
    vdf_total_prs=None,
    svlt_cognitive_hours=None,
    svlt_verify_hours=None,
    svlt_sample_count=None,
    rework_high_churn_files=None,
    rework_total_files=None,
    ttc_bug_count=None,
    alerts=None,
):
    """Create a sprint record matching aggregate-sprint.py full output schema.

    All 11 top-level keys are produced:
        sprint_id, since, until, project_dir, team_size, computed_at,
        dora, ai_dlc, activity, economics, alerts

    Level fields are dynamically computed using aggregate-sprint.py thresholds.
    """
    thresholds = get_dora_thresholds(team_size)

    # --- since / until derivation ---
    if "_" in sprint_id:
        parts = sprint_id.split("_", 1)
        since = parts[0]
        until = parts[1]
    else:
        since = None
        until = None

    # --- computed_at ---
    computed_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # --- dora.vdf ---
    if vdf is not None:
        vdf_level = classify_level_higher_better(vdf, thresholds["vdf"])
        q_prs = vdf_qualified_prs if vdf_qualified_prs is not None else max(1, int(vdf * 7))
        t_prs = vdf_total_prs if vdf_total_prs is not None else q_prs + 2
        dora_vdf = {
            "value": vdf,
            "level": vdf_level,
            "qualified_prs": q_prs,
            "total_prs": t_prs,
            "days": 7,
        }
    else:
        dora_vdf = None

    # --- dora.svlt ---
    if svlt_hours is not None:
        svlt_level = classify_level_lower_better(svlt_hours, thresholds["svlt"])
        cog_h = svlt_cognitive_hours if svlt_cognitive_hours is not None else round(svlt_hours * 0.6, 1)
        ver_h = svlt_verify_hours if svlt_verify_hours is not None else round(svlt_hours * 0.4, 1)
        s_count = svlt_sample_count if svlt_sample_count is not None else 5
        dora_svlt = {
            "value_hours": svlt_hours,
            "level": svlt_level,
            "cognitive_lt_hours": cog_h,
            "verify_lt_hours": ver_h,
            "sample_count": s_count,
        }
    else:
        dora_svlt = None

    # --- dora.rework_rate ---
    if rework_rate is not None:
        rework_level = classify_level_lower_better(rework_rate, thresholds["rework"])
        hc_files = rework_high_churn_files if rework_high_churn_files is not None else max(0, int(rework_rate * 20))
        r_total = rework_total_files if rework_total_files is not None else 20
        dora_rework = {
            "value": rework_rate,
            "level": rework_level,
            "high_churn_files": hc_files,
            "total_files": r_total,
        }
    else:
        dora_rework = None

    # --- dora.ttc ---
    if ttc_hours is not None:
        ttc_level = classify_level_lower_better(ttc_hours, thresholds["ttc"])
        b_count = ttc_bug_count if ttc_bug_count is not None else 3
        dora_ttc = {
            "value_hours": ttc_hours,
            "level": ttc_level,
            "bug_count": b_count,
        }
    else:
        dora_ttc = None

    # --- ai_dlc.ai_confidence ---
    if ai_confidence_components is not None:
        ai_conf_components = ai_confidence_components
    else:
        ai_conf_components = {"sq": 0.70, "ci": 0.75, "tpr": 0.65, "se": 0.60}
    ai_conf_level = _classify_ai_confidence_level(ai_confidence)

    # --- ai_dlc.sprint_health ---
    health_level = _classify_health_level(sprint_health)

    # --- activity ---
    default_tools = top_tools if top_tools is not None else {"Edit": 50, "Read": 80, "Bash": 30}

    return {
        "sprint_id": sprint_id,
        "since": since,
        "until": until,
        "project_dir": project_dir,
        "team_size": team_size,
        "computed_at": computed_at,
        "dora": {
            "vdf": dora_vdf,
            "svlt": dora_svlt,
            "rework_rate": dora_rework,
            "ttc": dora_ttc,
        },
        "ai_dlc": {
            "sprint_health": {"value": sprint_health, "level": health_level},
            "ai_confidence": {
                "value": ai_confidence,
                "level": ai_conf_level,
                "components": ai_conf_components,
            },
            "mttv_macro_hours": mttv_macro,
            "mttv_micro_seconds": mttv_micro,
            "spec_coverage": spec_coverage,
        },
        "activity": {
            "session_count": session_count,
            "total_turns": total_turns,
            "user_turns": user_turns,
            "assistant_turns": assistant_turns,
            "top_tools": default_tools,
            "file_count": file_count,
        },
        "economics": {
            "available": False,
            "note": "test fixture",
        },
        "alerts": alerts if alerts is not None else [],
    }
