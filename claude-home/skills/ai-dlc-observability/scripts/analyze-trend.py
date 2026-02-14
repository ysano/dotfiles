#!/usr/bin/env python3
"""
AI-DLC Sprint Trend Analyzer (L4).
Reads sprints.jsonl history and detects improving/declining/oscillating trends.
Outputs JSON or Markdown suitable for /ai-dlc:calibrate Section 4.

Usage:
    python3 analyze-trend.py
    python3 analyze-trend.py --lookback 5 --format markdown
    python3 analyze-trend.py --sprints ~/.claude/metrics/sprints.jsonl
    python3 analyze-trend.py --project-dir /home/user/project --verbose
"""
import argparse
import json
import os
import subprocess
import sys


# --- Constants ---

SPRINTS_FILE = os.path.expanduser("~/.claude/metrics/sprints.jsonl")

LEVEL_TO_SCORE = {"ELITE": 1.0, "HIGH": 0.75, "MEDIUM": 0.5, "LOW": 0.25}

# (name, dotpath, higher_is_better, weight)
METRIC_DEFS = [
    ("Sprint Health",   "ai_dlc.sprint_health.value", True,  2.0),
    ("AI-Confidence",   "ai_dlc.ai_confidence.value",  True,  2.0),
    ("VDF",             "dora.vdf.value",               True,  1.0),
    ("SVLT",            "dora.svlt.value_hours",        False, 1.0),
    ("Rework Rate",     "dora.rework_rate.value",       False, 1.0),
    ("TTC",             "dora.ttc.value_hours",         False, 1.0),
    ("MTTV Macro",      "ai_dlc.mttv_macro_hours",     False, 1.0),
    ("MTTV Micro",      "ai_dlc.mttv_micro_seconds",   False, 1.0),
    ("Session Count",   "activity.session_count",       None,  0.0),
    ("Total Turns",     "activity.total_turns",         None,  0.0),
]


# --- Utility ---

def log(msg, verbose=False):
    """Print diagnostic message to stderr if verbose is enabled."""
    if verbose:
        print(f"[analyze-trend] {msg}", file=sys.stderr)


def resolve_nested(obj, path):
    """Resolve dotted path like 'dora.vdf.value' into a value. Returns None on miss."""
    parts = path.split(".")
    current = obj
    for part in parts:
        if current is None or not isinstance(current, dict):
            return None
        current = current.get(part)
    return current


# --- Data Loading ---

def load_sprints(path, project_dir=None):
    """Load sprints from JSONL, optionally filter by project_dir, sort by sprint_id."""
    if not os.path.isfile(path):
        return []

    sprints = []
    with open(path, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue
            if project_dir and entry.get("project_dir") != project_dir:
                continue
            sprints.append(entry)

    sprints.sort(key=lambda s: s.get("sprint_id", ""))
    return sprints


def extract_series(sprints, path):
    """Extract (sprint_id, value) pairs for a metric path, skipping nulls."""
    series = []
    for s in sprints:
        val = resolve_nested(s, path)
        if val is not None:
            series.append((s.get("sprint_id", "?"), val))
    return series


# --- Trend Detection ---

def detect_trend(values, higher_is_better):
    """Detect trend direction from a list of numeric values.

    Returns one of: improving, declining, stable, oscillating, insufficient_data.
    For higher_is_better=None (neutral): returns increasing, decreasing, stable,
    oscillating, or insufficient_data.
    """
    if len(values) < 2:
        return "insufficient_data"

    # Stable check: all values within +-10% of mean
    mean = sum(values) / len(values)
    if mean != 0 and all(abs(v - mean) / abs(mean) <= 0.10 for v in values):
        return "stable"
    if mean == 0 and all(v == 0 for v in values):
        return "stable"

    # Compute transitions: +1 (increase), -1 (decrease), 0 (same)
    transitions = []
    for i in range(1, len(values)):
        if values[i] > values[i - 1]:
            transitions.append(1)
        elif values[i] < values[i - 1]:
            transitions.append(-1)
        else:
            transitions.append(0)

    ups = transitions.count(1)
    downs = transitions.count(-1)

    # Direction changes
    direction_changes = 0
    for i in range(1, len(transitions)):
        if transitions[i] != 0 and transitions[i - 1] != 0 and transitions[i] != transitions[i - 1]:
            direction_changes += 1

    # Near-monotonic: at most 1 reversal and at least 2 in main direction
    if downs <= 1 and ups >= 2:
        # predominantly increasing
        if higher_is_better is True:
            return "improving"
        elif higher_is_better is False:
            return "declining"
        else:
            return "increasing"

    if ups <= 1 and downs >= 2:
        # predominantly decreasing
        if higher_is_better is True:
            return "declining"
        elif higher_is_better is False:
            return "improving"
        else:
            return "decreasing"

    # Oscillating: 3+ direction changes
    if direction_changes >= 3:
        return "oscillating"

    # Fallback: majority vote
    if ups > downs:
        if higher_is_better is True:
            return "improving"
        elif higher_is_better is False:
            return "declining"
        else:
            return "increasing"
    elif downs > ups:
        if higher_is_better is True:
            return "declining"
        elif higher_is_better is False:
            return "improving"
        else:
            return "decreasing"

    return "stable"


def compute_metric_trend(sprints, path, higher_is_better):
    """Compute full trend info for one metric."""
    series = extract_series(sprints, path)
    values = [v for _, v in series]
    direction = detect_trend(values, higher_is_better)

    if len(values) >= 2:
        delta = values[-1] - values[0]
    else:
        delta = None

    return {
        "direction": direction,
        "delta": round(delta, 4) if delta is not None else None,
        "latest": values[-1] if values else None,
        "points": len(values),
        "values": series,
    }


# --- Aggregation ---

def build_history_table(sprints):
    """Build flat summary list for each sprint."""
    rows = []
    for s in sprints:
        rows.append({
            "sprint_id": s.get("sprint_id", "?"),
            "health": resolve_nested(s, "ai_dlc.sprint_health.value"),
            "health_level": resolve_nested(s, "ai_dlc.sprint_health.level"),
            "ai_confidence": resolve_nested(s, "ai_dlc.ai_confidence.value"),
            "vdf": resolve_nested(s, "dora.vdf.value"),
            "svlt_hours": resolve_nested(s, "dora.svlt.value_hours"),
            "rework_pct": _pct(resolve_nested(s, "dora.rework_rate.value")),
            "sessions": resolve_nested(s, "activity.session_count"),
            "team_size": s.get("team_size", "?"),
        })
    return rows


def _pct(val):
    """Convert decimal to percentage, or None."""
    if val is None:
        return None
    return round(val * 100, 1)


def compute_overall_trend(trends):
    """Weighted overall assessment. Neutral (weight=0) metrics excluded."""
    direction_score = {"improving": 1.0, "stable": 0.5, "declining": 0.0, "oscillating": 0.25}
    total_weight = 0.0
    weighted_sum = 0.0

    for name, _path, _hib, weight in METRIC_DEFS:
        if weight == 0:
            continue
        t = trends.get(name)
        if not t or t["direction"] == "insufficient_data":
            continue
        score = direction_score.get(t["direction"])
        if score is None:
            continue
        weighted_sum += score * weight
        total_weight += weight

    if total_weight == 0:
        return {"direction": "insufficient_data", "score": None}

    overall = weighted_sum / total_weight
    if overall >= 0.70:
        direction = "improving"
    elif overall >= 0.40:
        direction = "stable"
    else:
        direction = "declining"

    return {"direction": direction, "score": round(overall, 2)}


# --- Recommendations ---

def generate_recommendations(trends, sprints):
    """Generate prioritized action items from trend data."""
    recs = []

    for name, _path, _hib, weight in METRIC_DEFS:
        t = trends.get(name)
        if not t:
            continue

        # P0: declining 3+ sprints on weighted metrics
        if t["direction"] == "declining" and t["points"] >= 3 and weight > 0:
            recs.append({
                "priority": "P0",
                "metric": name,
                "action": f"{name} has been declining over {t['points']} sprints. Investigate root cause immediately.",
            })

    # P0: latest health < 0.50
    health_t = trends.get("Sprint Health")
    if health_t and health_t["latest"] is not None and health_t["latest"] < 0.50:
        recs.append({
            "priority": "P0",
            "metric": "Sprint Health",
            "action": f"Sprint Health critically low ({health_t['latest']}). Review all DORA/AI-DLC components.",
        })

    for name, _path, _hib, weight in METRIC_DEFS:
        t = trends.get(name)
        if not t:
            continue

        # P1: oscillating
        if t["direction"] == "oscillating" and weight > 0:
            recs.append({
                "priority": "P1",
                "metric": name,
                "action": f"{name} is oscillating. Stabilize process or investigate external factors.",
            })

    # P1: VDF declining while sessions increasing
    vdf_t = trends.get("VDF")
    sessions_t = trends.get("Session Count")
    if (vdf_t and sessions_t
            and vdf_t["direction"] == "declining"
            and sessions_t["direction"] in ("increasing", "improving")):
        recs.append({
            "priority": "P1",
            "metric": "VDF",
            "action": "VDF declining while session count increasing. Possible efficiency drop.",
        })

    for name, _path, _hib, weight in METRIC_DEFS:
        t = trends.get(name)
        if not t:
            continue

        # P2: insufficient data for key metrics
        if t["direction"] == "insufficient_data" and weight >= 1.0:
            recs.append({
                "priority": "P2",
                "metric": name,
                "action": f"Insufficient data for {name}. Ensure sprint aggregation runs regularly.",
            })

    # P2: stable at low health
    if (health_t and health_t["direction"] == "stable"
            and health_t["latest"] is not None and health_t["latest"] < 0.60):
        recs.append({
            "priority": "P2",
            "metric": "Sprint Health",
            "action": f"Sprint Health stable but low ({health_t['latest']}). Plan targeted improvements.",
        })

    return recs


# --- Git Correlation ---

def git_log_correlation(project_dir, since_sprint_id):
    """Retrieve CLAUDE.md/SKILL.md change log (best-effort)."""
    if not project_dir:
        return None

    since_date = since_sprint_id.split("_")[0] if since_sprint_id else None
    if not since_date:
        return None

    try:
        result = subprocess.run(
            ["git", "log", f"--since={since_date}", "--format=%h %ad %s", "--date=short",
             "--", "CLAUDE.md", ".claude/CLAUDE.md", ".claude/skills/*/SKILL.md"],
            capture_output=True, text=True, timeout=10,
            cwd=project_dir,
        )
        if result.returncode != 0:
            return None
        lines = [l.strip() for l in result.stdout.strip().splitlines() if l.strip()]
        return lines if lines else None
    except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
        return None


# --- Formatting ---

def _arrow(direction):
    """Return an arrow symbol for trend direction."""
    arrows = {
        "improving": "^",
        "declining": "v",
        "increasing": "^",
        "decreasing": "v",
        "stable": "=",
        "oscillating": "~",
        "insufficient_data": "--",
    }
    return arrows.get(direction, "?")


def _fmt(val):
    """Format a value for display, None → '--'."""
    if val is None:
        return "--"
    if isinstance(val, float):
        return f"{val:.2f}"
    return str(val)


def format_json(result):
    """Format result as JSON string."""
    return json.dumps(result, indent=2, ensure_ascii=False)


def format_markdown(result):
    """Format result as Markdown for /ai-dlc:calibrate embedding."""
    lines = []
    lines.append("## L4 Sprint Trend Analysis")
    lines.append("")

    # Overall
    overall = result.get("overall", {})
    lines.append(f"**Overall Trend**: {overall.get('direction', '?')} "
                 f"(score: {_fmt(overall.get('score'))})")
    lines.append("")

    # History table
    history = result.get("history", [])
    if history:
        lines.append("### Sprint History")
        lines.append("")
        lines.append("| Sprint | Health | AI-Conf | VDF | SVLT(h) | Rework(%) | Sessions |")
        lines.append("|---|---|---|---|---|---|---|")
        for row in history:
            lines.append(
                f"| {row['sprint_id']} "
                f"| {_fmt(row.get('health'))} "
                f"| {_fmt(row.get('ai_confidence'))} "
                f"| {_fmt(row.get('vdf'))} "
                f"| {_fmt(row.get('svlt_hours'))} "
                f"| {_fmt(row.get('rework_pct'))} "
                f"| {_fmt(row.get('sessions'))} |"
            )
        lines.append("")

    # Team size warning
    team_sizes = result.get("team_sizes", [])
    if len(team_sizes) > 1:
        lines.append(f"> **Warning**: mixed team_size across sprints: {', '.join(team_sizes)}")
        lines.append("")

    # Metric trends
    trends = result.get("trends", {})
    if trends:
        lines.append("### Metric Trends")
        lines.append("")
        lines.append("| Metric | Direction | Delta | Latest | Points |")
        lines.append("|---|---|---|---|---|")
        for name, _path, _hib, _w in METRIC_DEFS:
            t = trends.get(name)
            if not t:
                continue
            lines.append(
                f"| {name} "
                f"| {_arrow(t['direction'])} {t['direction']} "
                f"| {_fmt(t.get('delta'))} "
                f"| {_fmt(t.get('latest'))} "
                f"| {t.get('points', 0)} |"
            )
        lines.append("")

    # Git correlation
    git_log = result.get("git_correlation")
    if git_log:
        lines.append("### Configuration Change History")
        lines.append("")
        for entry in git_log:
            lines.append(f"- {entry}")
        lines.append("")

    # Recommendations
    recs = result.get("recommendations", [])
    if recs:
        lines.append("### Recommendations")
        lines.append("")
        lines.append("| Priority | Metric | Action |")
        lines.append("|---|---|---|")
        for r in recs:
            lines.append(f"| {r['priority']} | {r['metric']} | {r['action']} |")
        lines.append("")

    return "\n".join(lines)


# --- Main ---

def main():
    parser = argparse.ArgumentParser(description="AI-DLC Sprint Trend Analyzer (L4)")
    parser.add_argument("--sprints", type=str, default=SPRINTS_FILE,
                        help=f"Path to sprints.jsonl (default: {SPRINTS_FILE})")
    parser.add_argument("--lookback", type=int, default=None,
                        help="Number of most recent sprints to analyze (default: all)")
    parser.add_argument("--project-dir", type=str, default=None,
                        help="Filter sprints by project directory")
    parser.add_argument("--format", type=str, default="json", choices=["json", "markdown"],
                        help="Output format (default: json)")
    parser.add_argument("--verbose", action="store_true",
                        help="Print diagnostic info to stderr")
    args = parser.parse_args()

    project_dir = args.project_dir or os.environ.get("CLAUDE_PROJECT_DIR")
    log(f"sprints={args.sprints}, lookback={args.lookback}, project_dir={project_dir}", args.verbose)

    # Load
    sprints = load_sprints(args.sprints, project_dir)
    log(f"loaded {len(sprints)} sprints", args.verbose)

    if args.lookback and len(sprints) > args.lookback:
        sprints = sprints[-args.lookback:]
        log(f"trimmed to {len(sprints)} sprints (lookback={args.lookback})", args.verbose)

    # Team size warning
    team_sizes = sorted(set(s.get("team_size", "?") for s in sprints))
    if len(team_sizes) > 1:
        log(f"WARNING: mixed team_size: {team_sizes}", args.verbose)

    # Compute trends
    trends = {}
    for name, path, hib, _weight in METRIC_DEFS:
        trends[name] = compute_metric_trend(sprints, path, hib)
        log(f"{name}: {trends[name]['direction']} ({trends[name]['points']} points)", args.verbose)

    # Overall
    overall = compute_overall_trend(trends)
    log(f"overall: {overall['direction']} (score={overall['score']})", args.verbose)

    # History table
    history = build_history_table(sprints)

    # Git correlation
    oldest_id = sprints[0].get("sprint_id") if sprints else None
    git_log = git_log_correlation(project_dir, oldest_id)

    # Recommendations
    recs = generate_recommendations(trends, sprints)

    # Build result
    result = {
        "sprint_count": len(sprints),
        "team_sizes": team_sizes,
        "overall": overall,
        "trends": trends,
        "history": history,
        "recommendations": recs,
        "git_correlation": git_log,
    }

    # Output
    if args.format == "markdown":
        print(format_markdown(result))
    else:
        print(format_json(result))


if __name__ == "__main__":
    main()
