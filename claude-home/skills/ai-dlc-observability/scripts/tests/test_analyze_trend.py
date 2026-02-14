"""Tests for analyze-trend.py (L4 Sprint Trend Analyzer)."""
import json
import os
import subprocess

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
        assert git_log_correlation(None, "2026-02-01_2026-02-07") is None

    def test_returns_none_without_sprint_id(self):
        assert git_log_correlation("/tmp", None) is None

    def test_handles_file_not_found(self, monkeypatch):
        def mock_run(*args, **kwargs):
            raise FileNotFoundError("git not found")
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert git_log_correlation("/tmp", "2026-02-01_2026-02-07") is None


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
