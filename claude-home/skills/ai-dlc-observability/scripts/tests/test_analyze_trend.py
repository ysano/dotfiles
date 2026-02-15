"""Tests for analyze-trend.py (L4 Sprint Trend Analyzer)."""
import io
import json
import os
import subprocess
from datetime import datetime, timedelta
from unittest.mock import patch

import pytest
from helpers import make_sprint
from analyze_trend import (
    resolve_nested,
    load_sprints,
    extract_series,
    detect_trend,
    compute_metric_trend,
    compute_overall_trend,
    build_history_table,
    generate_recommendations,
    git_log_correlation,
    format_json,
    format_markdown,
    main,
    METRIC_DEFS,
)


# --- TestResolveNested ---

class TestResolveNested:
    def test_simple_path(self):
        obj = {"a": {"b": {"c": 42}}}
        assert resolve_nested(obj, "a.b.c") == 42

    def test_top_level(self):
        obj = {"x": 10}
        assert resolve_nested(obj, "x") == 10

    def test_missing_key(self):
        obj = {"a": {"b": 1}}
        assert resolve_nested(obj, "a.c.d") is None

    def test_none_intermediate(self):
        obj = {"a": {"b": None}}
        assert resolve_nested(obj, "a.b.c") is None


# --- TestLoadSprints ---

class TestLoadSprints:
    def test_load_basic(self, tmp_path):
        f = tmp_path / "sprints.jsonl"
        s1 = make_sprint(sprint_id="2026-02-03_2026-02-09", project_dir="/proj")
        s2 = make_sprint(sprint_id="2026-02-10_2026-02-16", project_dir="/proj")
        f.write_text(json.dumps(s1) + "\n" + json.dumps(s2) + "\n")
        result = load_sprints(str(f))
        assert len(result) == 2

    def test_filter_project_dir(self, tmp_path):
        f = tmp_path / "sprints.jsonl"
        s1 = make_sprint(sprint_id="s1", project_dir="/proj-a")
        s2 = make_sprint(sprint_id="s2", project_dir="/proj-b")
        f.write_text(json.dumps(s1) + "\n" + json.dumps(s2) + "\n")
        result = load_sprints(str(f), project_dir="/proj-a")
        assert len(result) == 1
        assert result[0]["project_dir"] == "/proj-a"

    def test_sort_by_sprint_id(self, tmp_path):
        f = tmp_path / "sprints.jsonl"
        s1 = make_sprint(sprint_id="2026-02-10_2026-02-16")
        s2 = make_sprint(sprint_id="2026-02-03_2026-02-09")
        f.write_text(json.dumps(s1) + "\n" + json.dumps(s2) + "\n")
        result = load_sprints(str(f))
        assert result[0]["sprint_id"] == "2026-02-03_2026-02-09"
        assert result[1]["sprint_id"] == "2026-02-10_2026-02-16"

    def test_sort_by_since_when_custom_sprint_names(self, tmp_path):
        """Custom sprint names should sort by 'since' date, not sprint_id."""
        f = tmp_path / "sprints.jsonl"
        # Sprint 2 has earlier since date but later alphabetical name
        s1 = make_sprint(sprint_id="Sprint 2")
        s1["since"] = "2026-02-03"
        s1["until"] = "2026-02-09"
        s2 = make_sprint(sprint_id="Sprint 1")
        s2["since"] = "2026-02-10"
        s2["until"] = "2026-02-16"
        f.write_text(json.dumps(s1) + "\n" + json.dumps(s2) + "\n")
        result = load_sprints(str(f))
        # Should be sorted by since date, not sprint_id
        assert result[0]["sprint_id"] == "Sprint 2"  # since=02-03
        assert result[1]["sprint_id"] == "Sprint 1"  # since=02-10

    def test_sort_same_since_uses_computed_at(self, tmp_path):
        """Same since date should use computed_at as tiebreaker."""
        f = tmp_path / "sprints.jsonl"
        s1 = make_sprint(sprint_id="Sprint 2")
        s1["since"] = "2026-02-10"
        s1["computed_at"] = "2026-02-15T14:00:00Z"
        s2 = make_sprint(sprint_id="Sprint 1")
        s2["since"] = "2026-02-10"
        s2["computed_at"] = "2026-02-15T10:00:00Z"
        f.write_text(json.dumps(s1) + "\n" + json.dumps(s2) + "\n")
        result = load_sprints(str(f))
        assert result[0]["sprint_id"] == "Sprint 1"  # earlier computed_at
        assert result[1]["sprint_id"] == "Sprint 2"  # later computed_at

    def test_empty_file(self, tmp_path):
        f = tmp_path / "sprints.jsonl"
        f.write_text("")
        assert load_sprints(str(f)) == []

    def test_missing_file(self, tmp_path):
        assert load_sprints(str(tmp_path / "nonexistent.jsonl")) == []


# --- TestExtractSeries ---

class TestExtractSeries:
    def test_basic_extraction(self):
        sprints = [
            make_sprint(sprint_id="s1", vdf=1.0),
            make_sprint(sprint_id="s2", vdf=2.0),
        ]
        series = extract_series(sprints, "dora.vdf.value")
        assert len(series) == 2
        assert series[0] == ("s1", 1.0)
        assert series[1] == ("s2", 2.0)

    def test_null_skip(self):
        sprints = [
            make_sprint(sprint_id="s1", ttc_hours=4.0),
            make_sprint(sprint_id="s2", ttc_hours=None),
            make_sprint(sprint_id="s3", ttc_hours=2.0),
        ]
        series = extract_series(sprints, "dora.ttc.value_hours")
        assert len(series) == 2
        assert series[0][0] == "s1"
        assert series[1][0] == "s3"

    def test_all_null(self):
        sprints = [
            make_sprint(sprint_id="s1", ttc_hours=None),
            make_sprint(sprint_id="s2", ttc_hours=None),
        ]
        series = extract_series(sprints, "dora.ttc.value_hours")
        assert series == []

    def test_missing_path(self):
        sprints = [make_sprint(sprint_id="s1")]
        series = extract_series(sprints, "nonexistent.path.here")
        assert series == []


# --- TestDetectTrend ---

class TestDetectTrend:
    # insufficient data
    def test_single_value(self):
        assert detect_trend([1.0], True) == "insufficient_data"

    def test_empty(self):
        assert detect_trend([], True) == "insufficient_data"

    # stable
    def test_stable_values(self):
        assert detect_trend([0.75, 0.76, 0.74, 0.75], True) == "stable"

    def test_stable_zeros(self):
        assert detect_trend([0, 0, 0], True) == "stable"

    # improving (higher_is_better=True, increasing)
    def test_improving_higher_better(self):
        assert detect_trend([0.5, 0.6, 0.7, 0.8], True) == "improving"

    # declining (higher_is_better=True, decreasing)
    def test_declining_higher_better(self):
        assert detect_trend([0.8, 0.7, 0.6, 0.5], True) == "declining"

    # improving (higher_is_better=False, decreasing values)
    def test_improving_lower_better(self):
        assert detect_trend([10, 8, 6, 4], False) == "improving"

    # declining (higher_is_better=False, increasing values)
    def test_declining_lower_better(self):
        assert detect_trend([4, 6, 8, 10], False) == "declining"

    # oscillating
    def test_oscillating(self):
        assert detect_trend([1, 3, 1, 3, 1, 3, 1], True) == "oscillating"

    # neutral (higher_is_better=None)
    def test_neutral_increasing(self):
        assert detect_trend([10, 20, 30, 40], None) == "increasing"

    def test_neutral_decreasing(self):
        assert detect_trend([40, 30, 20, 10], None) == "decreasing"

    # near-monotonic with one reversal
    def test_mostly_increasing(self):
        # 3 ups, 1 down → near-monotonic increasing
        assert detect_trend([1, 2, 3, 2.5, 4], True) == "improving"


# --- TestComputeOverallTrend ---

class TestComputeOverallTrend:
    def test_all_improving(self):
        trends = {}
        for name, path, hib, w in METRIC_DEFS:
            trends[name] = {"direction": "improving", "points": 5}
        result = compute_overall_trend(trends)
        assert result["direction"] == "improving"
        assert result["score"] >= 0.70

    def test_all_declining(self):
        trends = {}
        for name, path, hib, w in METRIC_DEFS:
            trends[name] = {"direction": "declining", "points": 5}
        result = compute_overall_trend(trends)
        assert result["direction"] == "declining"
        assert result["score"] <= 0.40

    def test_neutral_excluded(self):
        """Weight=0 metrics (Session Count, Total Turns) should be excluded."""
        trends = {}
        for name, path, hib, w in METRIC_DEFS:
            if w == 0:
                trends[name] = {"direction": "declining", "points": 5}
            else:
                trends[name] = {"direction": "improving", "points": 5}
        result = compute_overall_trend(trends)
        assert result["direction"] == "improving"

    def test_all_insufficient(self):
        trends = {}
        for name, path, hib, w in METRIC_DEFS:
            trends[name] = {"direction": "insufficient_data", "points": 0}
        result = compute_overall_trend(trends)
        assert result["direction"] == "insufficient_data"


# --- TestGenerateRecommendations ---

class TestGenerateRecommendations:
    def _trends_with(self, overrides):
        trends = {}
        for name, path, hib, w in METRIC_DEFS:
            trends[name] = {"direction": "stable", "points": 5, "latest": 0.75, "delta": 0}
        trends.update(overrides)
        return trends

    def test_p0_declining(self):
        trends = self._trends_with({
            "VDF": {"direction": "declining", "points": 4, "latest": 1.0, "delta": -0.5},
        })
        recs = generate_recommendations(trends, [])
        p0s = [r for r in recs if r["priority"] == "P0" and r["metric"] == "VDF"]
        assert len(p0s) >= 1

    def test_p0_low_health(self):
        trends = self._trends_with({
            "Sprint Health": {"direction": "stable", "points": 3, "latest": 0.40, "delta": 0},
        })
        recs = generate_recommendations(trends, [])
        p0s = [r for r in recs if r["priority"] == "P0" and r["metric"] == "Sprint Health"]
        assert len(p0s) >= 1

    def test_p1_oscillating(self):
        trends = self._trends_with({
            "SVLT": {"direction": "oscillating", "points": 5, "latest": 8, "delta": 0},
        })
        recs = generate_recommendations(trends, [])
        p1s = [r for r in recs if r["priority"] == "P1" and r["metric"] == "SVLT"]
        assert len(p1s) >= 1

    def test_p1_vdf_down_sessions_up(self):
        trends = self._trends_with({
            "VDF": {"direction": "declining", "points": 2, "latest": 1.0, "delta": -0.3},
            "Session Count": {"direction": "increasing", "points": 3, "latest": 15, "delta": 5},
        })
        recs = generate_recommendations(trends, [])
        p1_vdf = [r for r in recs if r["priority"] == "P1" and r["metric"] == "VDF"]
        assert len(p1_vdf) >= 1

    def test_p2_insufficient_data(self):
        trends = self._trends_with({
            "TTC": {"direction": "insufficient_data", "points": 1, "latest": None, "delta": None},
        })
        recs = generate_recommendations(trends, [])
        p2s = [r for r in recs if r["priority"] == "P2" and r["metric"] == "TTC"]
        assert len(p2s) >= 1


# --- TestGitLogCorrelation ---

class TestGitLogCorrelation:
    def test_returns_none_without_project_dir(self):
        assert git_log_correlation(None, "2026-02-01") is None

    def test_returns_none_without_since_date(self):
        assert git_log_correlation("/tmp", None) is None

    def test_returns_none_with_empty_since(self):
        assert git_log_correlation("/tmp", "") is None

    def test_handles_file_not_found(self, monkeypatch):
        def mock_run(*args, **kwargs):
            raise FileNotFoundError("git not found")
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert git_log_correlation("/tmp", "2026-02-01") is None

    def test_uses_since_date_directly(self, monkeypatch):
        """since_date is passed directly to git --since, not parsed from sprint_id."""
        captured_args = {}
        def mock_run(args, **kwargs):
            captured_args["args"] = args
            captured_args["cwd"] = kwargs.get("cwd")
            from unittest.mock import MagicMock
            result = MagicMock()
            result.returncode = 0
            result.stdout = "abc1234 2026-02-05 feat: update CLAUDE.md\n"
            return result
        monkeypatch.setattr(subprocess, "run", mock_run)
        result = git_log_correlation("/home/user/project", "2026-02-01")
        assert result is not None
        assert "--since=2026-02-01" in captured_args["args"]
        assert captured_args["cwd"] == "/home/user/project"


# --- TestFormatMarkdown ---

class TestFormatMarkdown:
    def _make_result(self, **overrides):
        base = {
            "sprint_count": 3,
            "team_sizes": ["solo"],
            "overall": {"direction": "improving", "score": 0.80},
            "trends": {
                "Sprint Health": {"direction": "improving", "delta": 0.10, "latest": 0.80, "points": 3},
            },
            "history": [
                {"sprint_id": "s1", "health": 0.70, "ai_confidence": 0.65,
                 "vdf": 1.0, "svlt_hours": 8.0, "rework_pct": 10.0, "sessions": 10, "team_size": "solo"},
            ],
            "recommendations": [],
            "git_correlation": None,
        }
        base.update(overrides)
        return base

    def test_contains_table(self):
        md = format_markdown(self._make_result())
        assert "| Sprint |" in md
        assert "| s1 " in md

    def test_arrows(self):
        md = format_markdown(self._make_result())
        assert "^ improving" in md

    def test_null_as_dash(self):
        result = self._make_result(history=[
            {"sprint_id": "s1", "health": None, "ai_confidence": None,
             "vdf": None, "svlt_hours": None, "rework_pct": None, "sessions": None, "team_size": "solo"},
        ])
        md = format_markdown(result)
        assert "| -- " in md


# --- TestIntegration ---

class TestIntegration:
    def _write_sprints(self, tmp_path, sprints):
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")
        return str(f)

    def test_json_output(self, tmp_path):
        sprints = [
            make_sprint(sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}", sprint_health=0.5 + i * 0.05)
            for i in range(1, 5)
        ]
        path = self._write_sprints(tmp_path, sprints)
        from analyze_trend import main
        import io
        from unittest.mock import patch

        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                main()
                output = json.loads(mock_out.getvalue())

        assert "trends" in output
        assert "overall" in output
        assert output["sprint_count"] == 4

    def test_markdown_output(self, tmp_path):
        sprints = [
            make_sprint(sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}")
            for i in range(1, 4)
        ]
        path = self._write_sprints(tmp_path, sprints)
        from analyze_trend import main
        import io
        from unittest.mock import patch

        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "markdown"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                main()
                md = mock_out.getvalue()

        assert "## L4 Sprint Trend Analysis" in md
        assert "Sprint History" in md

    def test_lookback(self, tmp_path):
        sprints = [
            make_sprint(sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}")
            for i in range(1, 8)
        ]
        path = self._write_sprints(tmp_path, sprints)
        from analyze_trend import main
        import io
        from unittest.mock import patch

        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--lookback", "3", "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                main()
                output = json.loads(mock_out.getvalue())

        assert output["sprint_count"] == 3

    def test_empty_sprints(self, tmp_path):
        path = self._write_sprints(tmp_path, [])
        from analyze_trend import main
        import io
        from unittest.mock import patch

        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                main()
                output = json.loads(mock_out.getvalue())

        assert output["sprint_count"] == 0
        assert output["overall"]["direction"] == "insufficient_data"


# =============================================================================
# Scenario Fixtures — multi-sprint patterns for L3→L4 pipeline testing
# =============================================================================

def _week_id(base_date, week_offset):
    """Generate sprint_id like '2026-01-05_2026-01-11' from base + offset."""
    start = base_date + timedelta(weeks=week_offset)
    end = start + timedelta(days=6)
    return f"{start.strftime('%Y-%m-%d')}_{end.strftime('%Y-%m-%d')}"


_BASE = datetime(2026, 1, 5)


def _make_improving_sprints():
    """6 sprints with all metrics consistently improving."""
    sprints = []
    for i in range(6):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, i),
            vdf=0.5 + i * 0.5,           # 0.5 → 3.0 (higher=better ↑)
            svlt_hours=24.0 - i * 4.0,    # 24 → 4 (lower=better ↓)
            rework_rate=0.30 - i * 0.05,  # 0.30 → 0.05 (lower=better ↓)
            sprint_health=0.50 + i * 0.08, # 0.50 → 0.90
            ai_confidence=0.45 + i * 0.09, # 0.45 → 0.90
            session_count=8 + i,
            total_turns=160 + i * 20,
        ))
    return sprints


def _make_degrading_sprints():
    """6 sprints with all metrics consistently worsening."""
    sprints = []
    for i in range(6):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, i),
            vdf=3.0 - i * 0.5,            # 3.0 → 0.5 (higher=better ↓)
            svlt_hours=4.0 + i * 5.0,      # 4 → 29 (lower=better ↑)
            rework_rate=0.05 + i * 0.06,   # 0.05 → 0.35 (lower=better ↑)
            sprint_health=0.90 - i * 0.10,  # 0.90 → 0.40
            ai_confidence=0.90 - i * 0.11,  # 0.90 → 0.35
            session_count=15 - i,
            total_turns=300 - i * 30,
        ))
    return sprints


def _make_recovery_sprints():
    """8 sprints: S1-S3 degrade, S4-S8 recover."""
    sprints = []
    # Degrade phase (3 sprints)
    for i in range(3):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, i),
            vdf=2.0 - i * 0.5,            # 2.0 → 1.0
            svlt_hours=6.0 + i * 6.0,     # 6 → 18
            rework_rate=0.08 + i * 0.08,   # 0.08 → 0.24
            sprint_health=0.80 - i * 0.12, # 0.80 → 0.56
            ai_confidence=0.80 - i * 0.10, # 0.80 → 0.60
        ))
    # Recovery phase (5 sprints)
    for i in range(5):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, 3 + i),
            vdf=1.0 + i * 0.5,             # 1.0 → 3.0
            svlt_hours=18.0 - i * 3.5,     # 18 → 4
            rework_rate=0.24 - i * 0.04,    # 0.24 → 0.08
            sprint_health=0.56 + i * 0.08,  # 0.56 → 0.88
            ai_confidence=0.60 + i * 0.07,  # 0.60 → 0.88
        ))
    return sprints


def _make_oscillating_sprints():
    """7 sprints with alternating up/down pattern."""
    vdf_vals = [1.0, 2.5, 0.5, 2.5, 0.5, 2.5, 1.0]
    svlt_vals = [8.0, 20.0, 4.0, 25.0, 5.0, 18.0, 8.0]
    health_vals = [0.70, 0.50, 0.80, 0.45, 0.85, 0.50, 0.70]
    sprints = []
    for i in range(7):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, i),
            vdf=vdf_vals[i],
            svlt_hours=svlt_vals[i],
            rework_rate=0.10,  # keep stable to isolate oscillation
            sprint_health=health_vals[i],
            ai_confidence=0.65,
        ))
    return sprints


def _make_sparse_sprints():
    """3 sprints with TTC/MTTV all None — minimal data."""
    sprints = []
    for i in range(3):
        sprints.append(make_sprint(
            sprint_id=_week_id(_BASE, i),
            vdf=1.0 + i * 0.3,
            svlt_hours=None,
            rework_rate=None,
            ttc_hours=None,
            mttv_macro=None,
            mttv_micro=None,
            sprint_health=0.60,
            ai_confidence=0.55,
        ))
    return sprints


def _write_sprints_file(tmp_path, sprints):
    """Write sprints to a JSONL file and return the path."""
    f = tmp_path / "sprints.jsonl"
    f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")
    return str(f)


def _run_pipeline(tmp_path, sprints, extra_args=None):
    """Write sprints, run analyze-trend main(), return parsed JSON output."""
    path = _write_sprints_file(tmp_path, sprints)
    args = ["analyze-trend.py", "--sprints", path, "--format", "json"]
    if extra_args:
        args.extend(extra_args)
    with patch("sys.argv", args):
        with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
            main()
            return json.loads(mock_out.getvalue())


# =============================================================================
# TestMakeSprintSchema — verify full schema fidelity
# =============================================================================

class TestMakeSprintSchema:
    """Verify make_sprint() produces all 11 top-level keys with correct structure."""

    def test_all_top_level_keys(self):
        s = make_sprint()
        expected_keys = {
            "sprint_id", "since", "until", "project_dir", "team_size",
            "computed_at", "dora", "ai_dlc", "activity", "economics", "alerts",
        }
        assert set(s.keys()) == expected_keys

    def test_dora_vdf_subfields(self):
        s = make_sprint(vdf=2.0)
        vdf = s["dora"]["vdf"]
        assert vdf is not None
        assert set(vdf.keys()) == {"value", "level", "qualified_prs", "total_prs", "days"}
        assert vdf["value"] == 2.0

    def test_dora_svlt_subfields(self):
        s = make_sprint(svlt_hours=6.0)
        svlt = s["dora"]["svlt"]
        assert svlt is not None
        assert set(svlt.keys()) == {"value_hours", "level", "cognitive_lt_hours", "verify_lt_hours", "sample_count"}
        assert svlt["value_hours"] == 6.0

    def test_dora_rework_subfields(self):
        s = make_sprint(rework_rate=0.10)
        rw = s["dora"]["rework_rate"]
        assert rw is not None
        assert set(rw.keys()) == {"value", "level", "high_churn_files", "total_files"}
        assert rw["value"] == 0.10

    def test_dora_ttc_subfields(self):
        s = make_sprint(ttc_hours=3.0)
        ttc = s["dora"]["ttc"]
        assert ttc is not None
        assert set(ttc.keys()) == {"value_hours", "level", "bug_count"}
        assert ttc["value_hours"] == 3.0

    def test_dora_ttc_none_when_no_hours(self):
        s = make_sprint(ttc_hours=None)
        assert s["dora"]["ttc"] is None

    def test_ai_dlc_ai_confidence_components(self):
        s = make_sprint()
        ai_conf = s["ai_dlc"]["ai_confidence"]
        assert "components" in ai_conf
        assert set(ai_conf["components"].keys()) == {"sq", "ci", "tpr", "se"}

    def test_ai_dlc_sprint_health(self):
        s = make_sprint(sprint_health=0.80)
        health = s["ai_dlc"]["sprint_health"]
        assert health["value"] == 0.80
        assert health["level"] == "HEALTHY"

    def test_activity_all_subfields(self):
        s = make_sprint()
        act = s["activity"]
        expected = {"session_count", "total_turns", "user_turns", "assistant_turns", "top_tools", "file_count"}
        assert set(act.keys()) == expected

    def test_economics_default(self):
        s = make_sprint()
        assert s["economics"]["available"] is False

    def test_alerts_default_empty(self):
        s = make_sprint()
        assert s["alerts"] == []

    def test_alerts_custom(self):
        s = make_sprint(alerts=[{"level": "warn", "message": "test alert"}])
        assert len(s["alerts"]) == 1

    def test_since_until_from_date_sprint_id(self):
        s = make_sprint(sprint_id="2026-02-03_2026-02-09")
        assert s["since"] == "2026-02-03"
        assert s["until"] == "2026-02-09"

    def test_since_until_none_for_simple_id(self):
        s = make_sprint(sprint_id="s1")
        assert s["since"] is None
        assert s["until"] is None

    # --- Dynamic level calculation ---

    def test_vdf_elite_level(self):
        s = make_sprint(vdf=3.0, team_size="solo")
        assert s["dora"]["vdf"]["level"] == "ELITE"

    def test_vdf_low_level(self):
        s = make_sprint(vdf=0.3, team_size="solo")
        assert s["dora"]["vdf"]["level"] == "LOW"

    def test_svlt_elite_level(self):
        s = make_sprint(svlt_hours=2.0, team_size="solo")
        assert s["dora"]["svlt"]["level"] == "ELITE"

    def test_rework_high_level(self):
        s = make_sprint(rework_rate=0.10, team_size="solo")
        assert s["dora"]["rework_rate"]["level"] == "HIGH"

    def test_health_critical_level(self):
        s = make_sprint(sprint_health=0.30)
        assert s["ai_dlc"]["sprint_health"]["level"] == "CRITICAL"

    def test_health_attention_level(self):
        s = make_sprint(sprint_health=0.55)
        assert s["ai_dlc"]["sprint_health"]["level"] == "ATTENTION"

    def test_ai_confidence_elite_level(self):
        s = make_sprint(ai_confidence=0.90)
        assert s["ai_dlc"]["ai_confidence"]["level"] == "ELITE"

    def test_ai_confidence_low_level(self):
        s = make_sprint(ai_confidence=0.30)
        assert s["ai_dlc"]["ai_confidence"]["level"] == "LOW"

    def test_backward_compat_simple_call(self):
        """make_sprint(sprint_id='s1', vdf=1.0) still works."""
        s = make_sprint(sprint_id="s1", vdf=1.0)
        assert s["sprint_id"] == "s1"
        assert s["dora"]["vdf"]["value"] == 1.0
        assert s["ai_dlc"]["sprint_health"]["value"] == 0.75

    def test_rework_rate_none(self):
        s = make_sprint(rework_rate=None)
        assert s["dora"]["rework_rate"] is None

    def test_svlt_none(self):
        s = make_sprint(svlt_hours=None)
        assert s["dora"]["svlt"] is None

    def test_vdf_none(self):
        s = make_sprint(vdf=None)
        assert s["dora"]["vdf"] is None


# =============================================================================
# TestL3toL4Pipeline — end-to-end scenario testing
# =============================================================================

class TestL3toL4Pipeline:
    """Test L3 (aggregate-sprint.py schema) → L4 (analyze-trend.py) pipeline."""

    # --- Improving ---

    def test_improving_overall(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        assert result["overall"]["direction"] == "improving"

    def test_improving_vdf_trend(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        assert result["trends"]["VDF"]["direction"] == "improving"

    def test_improving_health_trend(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        assert result["trends"]["Sprint Health"]["direction"] == "improving"

    def test_improving_ai_conf_trend(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        assert result["trends"]["AI-Confidence"]["direction"] == "improving"

    def test_improving_no_p0(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        p0s = [r for r in result["recommendations"] if r["priority"] == "P0"]
        assert len(p0s) == 0

    # --- Degrading ---

    def test_degrading_overall(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_degrading_sprints())
        assert result["overall"]["direction"] == "declining"

    def test_degrading_has_p0(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_degrading_sprints())
        p0s = [r for r in result["recommendations"] if r["priority"] == "P0"]
        assert len(p0s) >= 1

    def test_degrading_svlt_declining(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_degrading_sprints())
        assert result["trends"]["SVLT"]["direction"] == "declining"

    def test_degrading_vdf_declining(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_degrading_sprints())
        assert result["trends"]["VDF"]["direction"] == "declining"

    # --- Recovery ---

    def test_recovery_overall_not_declining(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_recovery_sprints())
        assert result["overall"]["direction"] in ("improving", "stable")

    def test_recovery_lookback3_improving(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_recovery_sprints(), ["--lookback", "3"])
        assert result["overall"]["direction"] == "improving"

    def test_recovery_sprint_count(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_recovery_sprints())
        assert result["sprint_count"] == 8

    # --- Oscillating ---

    def test_oscillating_detects_oscillation(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_oscillating_sprints())
        osc_metrics = [
            name for name, t in result["trends"].items()
            if t["direction"] == "oscillating"
        ]
        assert len(osc_metrics) >= 2, f"Expected 2+ oscillating, got: {osc_metrics}"

    def test_oscillating_has_p1(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_oscillating_sprints())
        p1s = [r for r in result["recommendations"] if r["priority"] == "P1"]
        assert len(p1s) >= 1

    # --- Sparse ---

    def test_sparse_ttc_insufficient(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_sparse_sprints())
        assert result["trends"]["TTC"]["direction"] == "insufficient_data"

    def test_sparse_mttv_insufficient(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_sparse_sprints())
        assert result["trends"]["MTTV Macro"]["direction"] == "insufficient_data"

    def test_sparse_vdf_has_data(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_sparse_sprints())
        assert result["trends"]["VDF"]["points"] == 3

    def test_sparse_has_p2(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_sparse_sprints())
        p2s = [r for r in result["recommendations"] if r["priority"] == "P2"]
        assert len(p2s) >= 1

    # --- Cross-format ---

    def test_history_table_has_all_keys(self, tmp_path):
        result = _run_pipeline(tmp_path, _make_improving_sprints())
        row = result["history"][0]
        expected = {"sprint_id", "health", "health_level", "ai_confidence",
                    "vdf", "svlt_hours", "rework_pct", "sessions", "team_size"}
        assert set(row.keys()) == expected

    def test_markdown_roundtrip(self, tmp_path):
        """Ensure markdown output is valid for the improving scenario."""
        path = _write_sprints_file(tmp_path, _make_improving_sprints())
        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "markdown"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                main()
                md = mock_out.getvalue()
        assert "## L4 Sprint Trend Analysis" in md
        assert "improving" in md

    def test_json_roundtrip_serializable(self, tmp_path):
        """Ensure JSON output round-trips cleanly."""
        result = _run_pipeline(tmp_path, _make_degrading_sprints())
        serialized = json.dumps(result, ensure_ascii=False)
        deserialized = json.loads(serialized)
        assert deserialized["overall"]["direction"] == "declining"


# =============================================================================
# TestCommandFieldPaths — verify fields consumed by /ai-dlc:* commands
# =============================================================================

class TestCommandFieldPaths:
    """Verify that all field paths referenced by commands exist in make_sprint output."""

    def test_metric_defs_all_resolvable(self):
        """Every METRIC_DEFS dotpath resolves to a non-None value (with full data)."""
        s = make_sprint(
            vdf=1.5, svlt_hours=8.0, rework_rate=0.10,
            ttc_hours=4.0, mttv_macro=2.5, mttv_micro=30.0,
        )
        for name, path, _hib, _w in METRIC_DEFS:
            val = resolve_nested(s, path)
            assert val is not None, f"METRIC_DEFS path '{path}' for '{name}' resolved to None"

    def test_status_command_dora_fields(self):
        """status.md references dora.vdf, dora.svlt, dora.rework_rate, dora.ttc."""
        s = make_sprint(ttc_hours=2.0)
        for key in ["vdf", "svlt", "rework_rate", "ttc"]:
            assert s["dora"][key] is not None, f"dora.{key} is None"

    def test_status_command_ai_dlc_fields(self):
        """status.md references ai_dlc.ai_confidence and ai_dlc.sprint_health."""
        s = make_sprint()
        assert s["ai_dlc"]["ai_confidence"]["value"] is not None
        assert s["ai_dlc"]["ai_confidence"]["components"] is not None
        assert s["ai_dlc"]["sprint_health"]["value"] is not None

    def test_status_command_economics_field(self):
        """status.md checks economics.available."""
        s = make_sprint()
        assert "available" in s["economics"]

    def test_diagnose_command_alerts_field(self):
        """diagnose.md references alerts array for churn display."""
        s = make_sprint(alerts=[
            {"level": "warn", "message": "Churn warning: src/a.py (3 edits)"},
        ])
        assert isinstance(s["alerts"], list)
        assert s["alerts"][0]["level"] == "warn"
        assert "edits" in s["alerts"][0]["message"]

    def test_build_history_table_complete_keys(self):
        """build_history_table produces all keys expected by format_markdown."""
        sprints = [make_sprint(sprint_id="s1", ttc_hours=2.0)]
        table = build_history_table(sprints)
        assert len(table) == 1
        row = table[0]
        for key in ["sprint_id", "health", "health_level", "ai_confidence",
                     "vdf", "svlt_hours", "rework_pct", "sessions", "team_size"]:
            assert key in row, f"Missing key '{key}' in history table row"

    def test_build_history_table_rework_none(self):
        """build_history_table handles rework_rate=None gracefully (_pct(None)→None)."""
        sprints = [make_sprint(rework_rate=None)]
        table = build_history_table(sprints)
        assert table[0]["rework_pct"] is None


# =============================================================================
# TestScenarioFixtures — validate fixture integrity
# =============================================================================

class TestScenarioFixtures:
    """Validate scenario fixture data quality."""

    @pytest.mark.parametrize("factory,expected_count", [
        (_make_improving_sprints, 6),
        (_make_degrading_sprints, 6),
        (_make_recovery_sprints, 8),
        (_make_oscillating_sprints, 7),
        (_make_sparse_sprints, 3),
    ])
    def test_sprint_count(self, factory, expected_count):
        sprints = factory()
        assert len(sprints) == expected_count

    @pytest.mark.parametrize("factory", [
        _make_improving_sprints,
        _make_degrading_sprints,
        _make_recovery_sprints,
        _make_oscillating_sprints,
        _make_sparse_sprints,
    ])
    def test_json_serializable(self, factory):
        """All sprints round-trip through JSON."""
        for s in factory():
            serialized = json.dumps(s)
            deserialized = json.loads(serialized)
            assert deserialized["sprint_id"] == s["sprint_id"]

    @pytest.mark.parametrize("factory", [
        _make_improving_sprints,
        _make_degrading_sprints,
        _make_recovery_sprints,
        _make_oscillating_sprints,
        _make_sparse_sprints,
    ])
    def test_sprint_ids_sorted(self, factory):
        """Sprint IDs are in ascending order."""
        sprints = factory()
        ids = [s["sprint_id"] for s in sprints]
        assert ids == sorted(ids)

    @pytest.mark.parametrize("factory", [
        _make_improving_sprints,
        _make_degrading_sprints,
        _make_recovery_sprints,
        _make_oscillating_sprints,
        _make_sparse_sprints,
    ])
    def test_all_have_full_schema(self, factory):
        """Each sprint has all 11 top-level keys."""
        expected = {
            "sprint_id", "since", "until", "project_dir", "team_size",
            "computed_at", "dora", "ai_dlc", "activity", "economics", "alerts",
        }
        for s in factory():
            assert set(s.keys()) == expected, f"Sprint {s['sprint_id']} missing keys"
