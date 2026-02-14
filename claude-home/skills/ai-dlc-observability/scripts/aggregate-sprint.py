#!/usr/bin/env python3
"""
AI-DLC Sprint Metrics Aggregator (L3).
Collects L2 session data + GitHub CLI output and produces sprint-level metrics.
Outputs JSON to stdout and appends to ~/.claude/metrics/sprints.jsonl.

Usage:
    python3 aggregate-sprint.py
    python3 aggregate-sprint.py --since 2026-02-10 --until 2026-02-14
    python3 aggregate-sprint.py --project-dir /home/user/project
"""
import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
from collections import Counter
from datetime import datetime, timezone, timedelta


# --- Constants ---

METRICS_DIR = os.path.expanduser("~/.claude/metrics")
SESSIONS_FILE = os.path.join(METRICS_DIR, "sessions.jsonl")
SPRINTS_FILE = os.path.join(METRICS_DIR, "sprints.jsonl")

CHURN_CACHE_DIR = "/tmp/claude-churn-cache"
SPEC_CACHE_DIR = "/tmp/claude-spec-quality-cache"

# DORA thresholds (Solo scale)
DORA_THRESHOLDS = {
    "vdf": {"ELITE": 2.0, "HIGH": 1.0, "MEDIUM": 0.5},
    "svlt": {"ELITE": 4, "HIGH": 8, "MEDIUM": 24},     # hours (lower is better)
    "rework": {"ELITE": 0.05, "HIGH": 0.15, "MEDIUM": 0.30},  # (lower is better)
    "ttc": {"ELITE": 1, "HIGH": 4, "MEDIUM": 8},        # hours (lower is better)
}

AI_CONFIDENCE_WEIGHTS = {"sq": 0.30, "ci": 0.25, "tpr": 0.25, "se": 0.20}

LEVEL_TO_SCORE = {"ELITE": 1.0, "HIGH": 0.75, "MEDIUM": 0.5, "LOW": 0.25}


# --- Utility ---

def parse_iso(ts_str):
    """Parse ISO 8601 timestamp string to datetime (UTC)."""
    if not ts_str:
        return None
    try:
        # Handle various ISO formats
        ts_str = ts_str.replace("Z", "+00:00")
        if "+" not in ts_str and ts_str.count("-") <= 2:
            ts_str += "+00:00"
        return datetime.fromisoformat(ts_str)
    except (ValueError, TypeError):
        return None


def classify_level_higher_better(value, thresholds):
    """Classify a metric where higher value = better (e.g., VDF)."""
    if value is None:
        return None
    if value >= thresholds["ELITE"]:
        return "ELITE"
    if value >= thresholds["HIGH"]:
        return "HIGH"
    if value >= thresholds["MEDIUM"]:
        return "MEDIUM"
    return "LOW"


def classify_level_lower_better(value, thresholds):
    """Classify a metric where lower value = better (e.g., SVLT, Rework)."""
    if value is None:
        return None
    if value < thresholds["ELITE"]:
        return "ELITE"
    if value < thresholds["HIGH"]:
        return "HIGH"
    if value < thresholds["MEDIUM"]:
        return "MEDIUM"
    return "LOW"


def run_gh_command(args, project_dir):
    """Run a gh CLI command and return parsed JSON or None on failure."""
    try:
        result = subprocess.run(
            ["gh"] + args,
            capture_output=True, text=True, timeout=30,
            cwd=project_dir,
        )
        if result.returncode != 0:
            return None
        return json.loads(result.stdout) if result.stdout.strip() else None
    except (subprocess.TimeoutExpired, FileNotFoundError, json.JSONDecodeError):
        return None


# --- L2 Data Loaders ---

def load_sessions(since_dt, until_dt, project_dir):
    """Load sessions.jsonl entries within the date range for the given project."""
    sessions = []
    if not os.path.isfile(SESSIONS_FILE):
        return sessions

    try:
        with open(SESSIONS_FILE, "r") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    entry = json.loads(line)
                except json.JSONDecodeError:
                    continue

                # Filter by project
                entry_project = entry.get("project_dir", "")
                if project_dir and entry_project != project_dir:
                    continue

                # Filter by date range using start_time
                start_time = parse_iso(entry.get("start_time"))
                if start_time:
                    if since_dt and start_time < since_dt:
                        continue
                    if until_dt and start_time > until_dt:
                        continue

                sessions.append(entry)
    except OSError:
        pass

    return sessions


def aggregate_sessions(sessions):
    """Aggregate session-level data into sprint-level activity metrics."""
    session_count = len(sessions)
    total_turns = 0
    user_turns = 0
    assistant_turns = 0
    tool_counts = Counter()
    all_files = set()
    session_durations = []

    for s in sessions:
        total_turns += s.get("total_turns", 0)
        user_turns += s.get("user_turns", 0)
        assistant_turns += s.get("assistant_turns", 0)

        for tool, count in s.get("tool_counts", {}).items():
            tool_counts[tool] += count

        for f in s.get("modified_files", []):
            all_files.add(f)

        # Calculate session duration
        start = parse_iso(s.get("start_time"))
        end = parse_iso(s.get("end_time"))
        if start and end:
            duration = (end - start).total_seconds()
            if duration > 0:
                session_durations.append(duration)

    # Top tools (top 10)
    top_tools = dict(tool_counts.most_common(10))

    return {
        "session_count": session_count,
        "total_turns": total_turns,
        "user_turns": user_turns,
        "assistant_turns": assistant_turns,
        "top_tools": top_tools,
        "file_count": len(all_files),
        "modified_files": sorted(all_files),
        "session_durations": session_durations,
    }


def load_churn_data(project_dir):
    """Load churn cache for the project."""
    # Derive project name same as churn-counter.py
    parts = project_dir.rstrip("/").split("/")
    project_name = "_".join(parts[-2:]) if len(parts) >= 2 else parts[-1]
    cache_path = os.path.join(CHURN_CACHE_DIR, f"{project_name}_churn.json")

    if not os.path.isfile(cache_path):
        return None

    try:
        with open(cache_path, "r") as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return None


def load_spec_scores(project_dir):
    """Load spec quality cache for the project."""
    project_hash = hashlib.md5(project_dir.encode()).hexdigest()[:12]
    cache_path = os.path.join(SPEC_CACHE_DIR, f"{project_hash}_scores.json")

    if not os.path.isfile(cache_path):
        return None

    try:
        with open(cache_path, "r") as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return None


# --- DORA Metrics ---

def extract_issue_number(pr):
    """Extract linked issue number from PR body or branch name."""
    # Check PR body for "Closes #N", "Fixes #N", etc.
    body = pr.get("body", "") or ""
    match = re.search(r'(?:closes?|fixes?|resolves?)\s+#(\d+)', body, re.IGNORECASE)
    if match:
        return int(match.group(1))

    # Check branch name
    branch = pr.get("headRefName", "") or ""
    match = re.search(r'(?:GH-|issue-|#)?(\d{2,})', branch)
    if match:
        return int(match.group(1))

    return None


def calc_dora(merged_prs, closed_issues, bug_issues, sprint_days, project_dir):
    """Calculate DORA Four Keys from GitHub data."""
    dora = {
        "vdf": None,
        "svlt": None,
        "rework_rate": None,
        "ttc": None,
    }

    if merged_prs is None:
        return dora

    # --- VDF ---
    qualified_prs = [pr for pr in merged_prs if extract_issue_number(pr)]
    vdf_value = len(qualified_prs) / max(sprint_days, 1)
    vdf_level = classify_level_higher_better(vdf_value, DORA_THRESHOLDS["vdf"])
    dora["vdf"] = {
        "value": round(vdf_value, 2),
        "level": vdf_level,
        "qualified_prs": len(qualified_prs),
        "total_prs": len(merged_prs),
        "days": sprint_days,
    }

    # --- SVLT ---
    svlt_hours_list = []
    cognitive_hours_list = []
    verify_hours_list = []

    for pr in qualified_prs:
        issue_num = extract_issue_number(pr)
        if not issue_num:
            continue

        pr_created = parse_iso(pr.get("createdAt"))
        pr_merged = parse_iso(pr.get("mergedAt"))
        if not pr_created or not pr_merged:
            continue

        # Get issue creation time
        issue_data = run_gh_command(
            ["issue", "view", str(issue_num), "--json", "createdAt"],
            project_dir,
        )
        if not issue_data:
            continue

        issue_created = parse_iso(issue_data.get("createdAt"))
        if not issue_created:
            continue

        total_h = (pr_merged - issue_created).total_seconds() / 3600
        cognitive_h = (pr_created - issue_created).total_seconds() / 3600
        verify_h = (pr_merged - pr_created).total_seconds() / 3600

        svlt_hours_list.append(total_h)
        cognitive_hours_list.append(cognitive_h)
        verify_hours_list.append(verify_h)

    if svlt_hours_list:
        median_svlt = sorted(svlt_hours_list)[len(svlt_hours_list) // 2]
        median_cognitive = sorted(cognitive_hours_list)[len(cognitive_hours_list) // 2]
        median_verify = sorted(verify_hours_list)[len(verify_hours_list) // 2]
        svlt_level = classify_level_lower_better(median_svlt, DORA_THRESHOLDS["svlt"])
        dora["svlt"] = {
            "value_hours": round(median_svlt, 1),
            "level": svlt_level,
            "cognitive_lt_hours": round(median_cognitive, 1),
            "verify_lt_hours": round(median_verify, 1),
            "sample_count": len(svlt_hours_list),
        }

    # --- TTC ---
    if bug_issues:
        ttc_hours_list = []
        for issue in bug_issues:
            created = parse_iso(issue.get("createdAt"))
            closed = parse_iso(issue.get("closedAt"))
            if created and closed:
                ttc_h = (closed - created).total_seconds() / 3600
                ttc_hours_list.append(ttc_h)

        if ttc_hours_list:
            median_ttc = sorted(ttc_hours_list)[len(ttc_hours_list) // 2]
            ttc_level = classify_level_lower_better(median_ttc, DORA_THRESHOLDS["ttc"])
            dora["ttc"] = {
                "value_hours": round(median_ttc, 1),
                "level": ttc_level,
                "bug_count": len(ttc_hours_list),
            }

    return dora


# --- AI-DLC Metrics ---

def calc_ai_confidence(spec_scores, churn_data, activity):
    """Calculate AI-Confidence composite metric."""
    components = {}

    # SQ: Spec Quality
    if spec_scores:
        scores = [entry.get("score", 0) for entry in spec_scores.values()]
        avg_score = sum(scores) / len(scores) if scores else 0
        components["sq"] = round(avg_score / 5.0, 2)
    else:
        components["sq"] = 0.5  # neutral

    # CI: Churn Inverse
    if churn_data is not None:
        total_files = activity.get("file_count", 0)
        if total_files > 0:
            high_churn = sum(1 for d in churn_data.values()
                             if isinstance(d, dict) and d.get("count", 0) >= 3)
            rework_rate = high_churn / total_files
            components["ci"] = round(1.0 - min(rework_rate, 1.0), 2)
        else:
            components["ci"] = 0.5
    else:
        components["ci"] = 0.5

    # TPR: Turns-Per-Resolution
    session_count = activity.get("session_count", 0)
    total_turns = activity.get("total_turns", 0)
    if session_count > 0:
        avg_turns = total_turns / session_count
        tpr = max(0.0, min(1.0, 1.0 - (avg_turns - 10) / 40))
        components["tpr"] = round(tpr, 2)
    else:
        components["tpr"] = 0.5

    # SE: Session Efficiency
    top_tools = activity.get("top_tools", {})
    if top_tools:
        edit_tools = sum(top_tools.get(t, 0) for t in ["Edit", "Write", "MultiEdit"])
        total_tool_uses = sum(top_tools.values())
        if total_tool_uses > 0:
            components["se"] = round(min(edit_tools / total_tool_uses, 1.0), 2)
        else:
            components["se"] = 0.5
    else:
        components["se"] = 0.5

    # Composite
    value = sum(
        components[k] * AI_CONFIDENCE_WEIGHTS[k]
        for k in AI_CONFIDENCE_WEIGHTS
    )
    value = round(value, 2)

    # Level
    if value >= 0.85:
        level = "ELITE"
    elif value >= 0.65:
        level = "HIGH"
    elif value >= 0.45:
        level = "MEDIUM"
    else:
        level = "LOW"

    return {
        "value": value,
        "level": level,
        "components": components,
    }


def calc_rework_rate(churn_data, total_modified_files):
    """Calculate Rework Rate from churn data."""
    if churn_data is None:
        return None

    high_churn_files = [
        f for f, d in churn_data.items()
        if isinstance(d, dict) and d.get("count", 0) >= 3
    ]

    if total_modified_files == 0:
        rate = 0.0
    else:
        rate = len(high_churn_files) / total_modified_files

    level = classify_level_lower_better(rate, DORA_THRESHOLDS["rework"])

    return {
        "value": round(rate, 2),
        "level": level,
        "high_churn_files": len(high_churn_files),
        "total_files": total_modified_files,
    }


def calc_mttv_macro(merged_prs):
    """Calculate MTTV Macro: median PR cycle time in hours."""
    if not merged_prs:
        return None
    durations = []
    for pr in merged_prs:
        created = parse_iso(pr.get("createdAt"))
        merged = parse_iso(pr.get("mergedAt"))
        if created and merged:
            hours = (merged - created).total_seconds() / 3600
            durations.append(hours)
    if not durations:
        return None
    return round(sorted(durations)[len(durations) // 2], 1)


def calc_mttv_micro(sessions):
    """Calculate MTTV Micro: seconds per turn across sessions."""
    total_duration = 0
    total_turns = 0
    for s in sessions:
        start = parse_iso(s.get("start_time"))
        end = parse_iso(s.get("end_time"))
        if start and end:
            duration = (end - start).total_seconds()
            if duration > 0:
                total_duration += duration
                total_turns += s.get("total_turns", 0)
    if total_turns == 0:
        return None
    return round(total_duration / total_turns, 1)


def calc_sprint_health(dora, ai_confidence_value, spec_coverage, bug_count):
    """Calculate Sprint Health Score (0-1). 6 elements equally weighted."""
    scores = []

    # DORA levels → scores (VDF, SVLT)
    for key in ["vdf", "svlt"]:
        entry = dora.get(key)
        if entry and entry.get("level"):
            scores.append(LEVEL_TO_SCORE.get(entry["level"], 0.5))
        else:
            scores.append(0.5)  # data unavailable → neutral

    # TTC: null semantics differ — no bugs = ELITE, not "unknown"
    ttc = dora.get("ttc")
    if ttc and ttc.get("level"):
        scores.append(LEVEL_TO_SCORE.get(ttc["level"], 0.5))
    elif bug_count == 0:
        scores.append(1.0)  # no bugs to fix = best possible state
    else:
        scores.append(0.5)  # bugs exist but TTC data missing

    # Rework Rate
    rework = dora.get("rework_rate")
    if rework and rework.get("level"):
        scores.append(LEVEL_TO_SCORE.get(rework["level"], 0.5))
    else:
        scores.append(0.5)

    # AI-Confidence
    scores.append(ai_confidence_value if ai_confidence_value is not None else 0.5)

    # Spec Coverage
    scores.append(spec_coverage if spec_coverage is not None else 0.5)

    value = round(sum(scores) / len(scores), 2)

    if value >= 0.75:
        level = "HEALTHY"
    elif value >= 0.50:
        level = "ATTENTION"
    else:
        level = "CRITICAL"

    return {"value": value, "level": level}


# --- Alerts ---

def generate_alerts(churn_data, spec_scores, dora):
    """Generate alert messages from metrics data."""
    alerts = []

    # High churn files
    if churn_data:
        for filepath, data in churn_data.items():
            if isinstance(data, dict) and data.get("count", 0) >= 5:
                alerts.append({
                    "level": "error",
                    "message": f"High churn: {filepath} ({data['count']} edits)",
                })
            elif isinstance(data, dict) and data.get("count", 0) >= 3:
                alerts.append({
                    "level": "warn",
                    "message": f"Churn warning: {filepath} ({data['count']} edits)",
                })

    # Low spec quality
    if spec_scores:
        for spec_path, entry in spec_scores.items():
            score = entry.get("score", 0)
            if score < 3:
                name = os.path.basename(spec_path)
                alerts.append({
                    "level": "warn",
                    "message": f"Low spec quality: {name} ({score}/5)",
                })

    # DORA alerts
    for key, label in [("vdf", "VDF"), ("svlt", "SVLT"), ("rework_rate", "Rework Rate"), ("ttc", "TTC")]:
        entry = dora.get(key)
        if entry and entry.get("level") == "LOW":
            alerts.append({
                "level": "error",
                "message": f"DORA {label} at LOW level",
            })

    return alerts


# --- Main ---

def main():
    parser = argparse.ArgumentParser(description="AI-DLC Sprint Metrics Aggregator")
    parser.add_argument("--since", type=str, default=None,
                        help="Start date (YYYY-MM-DD). Default: 7 days ago")
    parser.add_argument("--until", type=str, default=None,
                        help="End date (YYYY-MM-DD). Default: today")
    parser.add_argument("--project-dir", type=str, default=None,
                        help="Project directory. Default: CLAUDE_PROJECT_DIR or cwd")
    args = parser.parse_args()

    # Resolve dates
    now = datetime.now(timezone.utc)
    if args.until:
        until_dt = datetime.strptime(args.until, "%Y-%m-%d").replace(
            hour=23, minute=59, second=59, tzinfo=timezone.utc
        )
    else:
        until_dt = now

    if args.since:
        since_dt = datetime.strptime(args.since, "%Y-%m-%d").replace(
            tzinfo=timezone.utc
        )
    else:
        since_dt = until_dt - timedelta(days=7)

    since_str = since_dt.strftime("%Y-%m-%d")
    until_str = until_dt.strftime("%Y-%m-%d")
    sprint_id = f"{since_str}_{until_str}"
    sprint_days = max((until_dt - since_dt).days, 1)

    # Resolve project dir
    project_dir = args.project_dir or os.environ.get("CLAUDE_PROJECT_DIR") or os.getcwd()

    # --- Step 1: Sessions ---
    sessions = load_sessions(since_dt, until_dt, project_dir)
    activity = aggregate_sessions(sessions)

    # --- Step 2: Churn cache ---
    churn_data = load_churn_data(project_dir)

    # --- Step 3: Spec quality cache ---
    spec_scores = load_spec_scores(project_dir)
    spec_coverage = None
    if spec_scores:
        total_specs = len(spec_scores)
        good_specs = sum(1 for e in spec_scores.values() if e.get("score", 0) >= 3)
        spec_coverage = round(good_specs / total_specs, 2) if total_specs > 0 else None

    # --- Step 4: GitHub CLI ---
    gh_since = since_dt.strftime("%Y-%m-%dT00:00:00Z")

    merged_prs = run_gh_command(
        ["pr", "list", "--state", "merged", "--search",
         f"merged:>={since_str}",
         "--json", "number,title,createdAt,mergedAt,body,headRefName",
         "--limit", "100"],
        project_dir,
    )

    closed_issues = run_gh_command(
        ["issue", "list", "--state", "closed",
         "--json", "number,title,createdAt,closedAt,labels",
         "--limit", "200"],
        project_dir,
    )

    bug_issues = None
    if closed_issues:
        bug_labels = {"bug", "bugfix", "hotfix"}
        bug_issues = [
            issue for issue in closed_issues
            if any(
                label.get("name", "").lower() in bug_labels
                for label in issue.get("labels", [])
            )
            and parse_iso(issue.get("closedAt"))
            and parse_iso(issue.get("closedAt")) >= since_dt
        ]

    # --- Step 5: Compute metrics ---

    # DORA
    dora = calc_dora(merged_prs, closed_issues, bug_issues, sprint_days, project_dir)

    # Rework Rate (from churn, not GitHub)
    rework = calc_rework_rate(churn_data, activity["file_count"])
    dora["rework_rate"] = rework

    # AI-DLC
    ai_confidence = calc_ai_confidence(spec_scores, churn_data, activity)
    mttv_macro = calc_mttv_macro(merged_prs) if merged_prs else None
    mttv_micro = calc_mttv_micro(sessions)
    actual_bug_count = len(bug_issues) if bug_issues else 0
    sprint_health = calc_sprint_health(
        dora, ai_confidence["value"], spec_coverage, actual_bug_count
    )

    # Alerts
    alerts = generate_alerts(churn_data, spec_scores, dora)

    # --- Step 6: Output ---
    output = {
        "sprint_id": sprint_id,
        "since": since_str,
        "until": until_str,
        "project_dir": project_dir,
        "computed_at": now.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "dora": dora,
        "ai_dlc": {
            "ai_confidence": ai_confidence,
            "mttv_macro_hours": mttv_macro,
            "mttv_micro_seconds": mttv_micro,
            "spec_coverage": spec_coverage,
            "sprint_health": sprint_health,
        },
        "activity": {
            "session_count": activity["session_count"],
            "total_turns": activity["total_turns"],
            "user_turns": activity["user_turns"],
            "assistant_turns": activity["assistant_turns"],
            "top_tools": activity["top_tools"],
            "file_count": activity["file_count"],
        },
        "economics": {
            "available": False,
            "note": "OTel 未設定 — CLAUDE_CODE_ENABLE_TELEMETRY=1 で有効化",
        },
        "alerts": alerts,
    }

    # Print to stdout
    print(json.dumps(output, indent=2, ensure_ascii=False))

    # Append to sprints.jsonl
    try:
        os.makedirs(METRICS_DIR, exist_ok=True)
        with open(SPRINTS_FILE, "a") as f:
            f.write(json.dumps(output, ensure_ascii=False) + "\n")
    except OSError as e:
        print(f"Warning: failed to write to {SPRINTS_FILE}: {e}", file=sys.stderr)


if __name__ == "__main__":
    main()
