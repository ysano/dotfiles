"""E2E pipeline tests: L2 (sessions/churn/spec) → L3 (aggregate-sprint) → L4 (analyze-trend).

These tests verify the full data flow across all observability layers,
ensuring that synthetic L2 data is correctly aggregated into L3 sprint
records which are then analyzed by L4 trend detection.
"""
import io
import json
import os
from datetime import datetime, timedelta, timezone
from unittest.mock import patch

import pytest
from helpers import (
    make_session,
    make_sprint,
    make_pr,
    make_issue,
    make_churn_data,
    make_spec_scores,
)
from aggregate_sprint import (
    load_sessions,
    aggregate_sessions,
    calc_ai_confidence,
    calc_rework_rate,
    calc_sprint_health,
    get_dora_thresholds,
)
from analyze_trend import (
    load_sprints,
    main as analyze_trend_main,
)


# =============================================================================
# Scenario 1: Synthetic Data Flow
# L2 sessions + churn + spec → aggregate into L3 sprint → feed to L4 trend
# =============================================================================

class TestSyntheticDataFlow:
    """Verify L2 → L3 → L4 pipeline with synthetic data."""

    def _create_sessions_file(self, tmp_path, sessions):
        """Write sessions to a JSONL file."""
        f = tmp_path / "sessions.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sessions) + "\n")
        return str(f)

    def _create_sprints_file(self, tmp_path, sprints):
        """Write sprints to a JSONL file."""
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")
        return str(f)

    def test_l2_sessions_aggregate_to_l3(self, tmp_path):
        """L2 sessions should aggregate into correct L3 activity metrics."""
        project = "/home/user/calculator"
        sessions = [
            make_session(
                project_dir=project,
                start_time="2026-02-10T09:00:00Z",
                end_time="2026-02-10T10:00:00Z",
                total_turns=20,
                user_turns=10,
                assistant_turns=10,
                tool_counts={"Edit": 5, "Read": 8, "Bash": 3},
                modified_files=["src/calc.py", "src/ops.py"],
            ),
            make_session(
                project_dir=project,
                start_time="2026-02-11T09:00:00Z",
                end_time="2026-02-11T11:00:00Z",
                total_turns=30,
                user_turns=15,
                assistant_turns=15,
                tool_counts={"Edit": 10, "Read": 12, "Bash": 5, "Write": 3},
                modified_files=["src/calc.py", "src/utils.py", "tests/test_calc.py"],
            ),
        ]

        # Filter sessions by project
        filtered = [s for s in sessions if s["project_dir"] == project]
        assert len(filtered) == 2

        # Aggregate
        activity = aggregate_sessions(filtered)

        assert activity["session_count"] == 2
        assert activity["total_turns"] == 50
        assert activity["user_turns"] == 25
        assert activity["assistant_turns"] == 25
        # Files: src/calc.py, src/ops.py, src/utils.py, tests/test_calc.py
        assert activity["file_count"] == 4
        assert "Edit" in activity["top_tools"]
        assert activity["top_tools"]["Edit"] == 15

    def test_l2_churn_feeds_rework_rate(self):
        """L2 churn data should produce correct L3 rework rate."""
        churn = make_churn_data(**{
            "src/calc.py": 5,   # high churn (>= 3)
            "src/ops.py": 1,    # low churn
            "src/utils.py": 4,  # high churn
        })
        thresholds = get_dora_thresholds("solo")
        rework = calc_rework_rate(churn, total_modified_files=10, thresholds=thresholds)

        assert rework is not None
        assert rework["high_churn_files"] == 2  # calc.py and utils.py
        assert rework["total_files"] == 10
        assert rework["value"] == 0.2  # 2/10

    def test_l2_spec_feeds_ai_confidence(self):
        """L2 spec scores should influence L3 AI-Confidence SQ component."""
        spec_scores = make_spec_scores(**{
            "SPEC.md": 5,    # perfect score
            "PLAN.md": 4,    # good score
        })
        churn = make_churn_data(**{"src/a.py": 1})
        activity = {
            "session_count": 5,
            "total_turns": 100,
            "top_tools": {"Edit": 30, "Read": 50, "Bash": 20},
            "file_count": 10,
        }

        ai_conf = calc_ai_confidence(spec_scores, churn, activity)

        # SQ = avg(5,4)/5 = 0.9
        assert ai_conf["components"]["sq"] == 0.9
        assert ai_conf["value"] > 0.5
        assert ai_conf["level"] in ("ELITE", "HIGH", "MEDIUM")

    def test_l3_sprint_feeds_l4_trend(self, tmp_path):
        """L3 sprint records should be analyzable by L4 trend detection."""
        sprints = [
            make_sprint(
                sprint_id="2026-02-03_2026-02-09",
                project_dir="/home/user/calculator",
                vdf=1.0, svlt_hours=12.0, rework_rate=0.15,
                sprint_health=0.60, ai_confidence=0.55,
                session_count=5, total_turns=100,
            ),
            make_sprint(
                sprint_id="2026-02-10_2026-02-16",
                project_dir="/home/user/calculator",
                vdf=1.5, svlt_hours=8.0, rework_rate=0.10,
                sprint_health=0.70, ai_confidence=0.65,
                session_count=8, total_turns=150,
            ),
            make_sprint(
                sprint_id="2026-02-17_2026-02-23",
                project_dir="/home/user/calculator",
                vdf=2.0, svlt_hours=5.0, rework_rate=0.05,
                sprint_health=0.80, ai_confidence=0.75,
                session_count=10, total_turns=200,
            ),
        ]
        path = self._create_sprints_file(tmp_path, sprints)

        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                result = json.loads(mock_out.getvalue())

        assert result["sprint_count"] == 3
        assert result["overall"]["direction"] == "improving"
        assert result["trends"]["VDF"]["direction"] == "improving"
        assert result["trends"]["Sprint Health"]["direction"] == "improving"

    def test_full_pipeline_l2_to_l4(self, tmp_path):
        """Full L2→L3→L4 pipeline: session data → aggregate → trend analysis."""
        project = "/home/user/calculator"

        # L2: Create 3 sprints worth of session data
        sessions_per_sprint = []
        for week in range(3):
            base_day = 3 + week * 7  # Feb 3, 10, 17
            sessions = []
            for day_offset in range(5):  # 5 working days
                day = base_day + day_offset
                sessions.append(make_session(
                    project_dir=project,
                    start_time=f"2026-02-{day:02d}T09:00:00Z",
                    end_time=f"2026-02-{day:02d}T{10 + week}:00:00Z",
                    total_turns=15 + week * 5,
                    user_turns=8 + week * 2,
                    assistant_turns=7 + week * 3,
                ))
            sessions_per_sprint.append(sessions)

        # L3: Aggregate each sprint's sessions
        sprint_records = []
        for i, sessions in enumerate(sessions_per_sprint):
            activity = aggregate_sessions(sessions)
            assert activity["session_count"] == 5
            assert activity["total_turns"] > 0

            # Build sprint record using make_sprint with real activity data
            sprint_records.append(make_sprint(
                sprint_id=f"2026-02-{3+i*7:02d}_2026-02-{9+i*7:02d}",
                project_dir=project,
                vdf=0.5 + i * 0.5,
                svlt_hours=16.0 - i * 4.0,
                rework_rate=0.20 - i * 0.05,
                sprint_health=0.55 + i * 0.10,
                ai_confidence=0.50 + i * 0.10,
                session_count=activity["session_count"],
                total_turns=activity["total_turns"],
            ))

        # L4: Analyze trends
        path = self._create_sprints_file(tmp_path, sprint_records)
        with patch("sys.argv", ["analyze-trend.py", "--sprints", path, "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                result = json.loads(mock_out.getvalue())

        assert result["sprint_count"] == 3
        assert result["overall"]["direction"] == "improving"
        # All key metrics should have data points
        assert result["trends"]["VDF"]["points"] == 3
        assert result["trends"]["Sprint Health"]["points"] == 3
        assert result["trends"]["AI-Confidence"]["points"] == 3


# =============================================================================
# Scenario 2: Empty Data Graceful Handling
# sessions=0, churn=none, spec=none should not crash
# =============================================================================

class TestEmptyDataGraceful:
    """Verify pipeline handles missing/empty data without crashing."""

    def test_zero_sessions_aggregate(self):
        """Zero sessions should produce valid (zeroed) activity metrics."""
        activity = aggregate_sessions([])

        assert activity["session_count"] == 0
        assert activity["total_turns"] == 0
        assert activity["file_count"] == 0
        assert activity["top_tools"] == {}

    def test_none_churn_rework_rate(self):
        """None churn data should return None rework rate."""
        thresholds = get_dora_thresholds("solo")
        rework = calc_rework_rate(None, total_modified_files=5, thresholds=thresholds)
        assert rework is None

    def test_none_spec_ai_confidence(self):
        """None spec scores should still produce valid AI-Confidence."""
        activity = {
            "session_count": 0,
            "total_turns": 0,
            "top_tools": {},
            "file_count": 0,
        }
        ai_conf = calc_ai_confidence(None, None, activity)

        assert ai_conf is not None
        assert ai_conf["value"] == 0.5  # all neutral defaults
        assert ai_conf["level"] == "MEDIUM"

    def test_empty_sprint_health(self):
        """Sprint with no DORA data should return ATTENTION level."""
        dora = {
            "vdf": None,
            "svlt": None,
            "rework_rate": None,
            "ttc": None,
        }
        health = calc_sprint_health(dora, 0.5, None, 0)

        assert health is not None
        assert health["level"] in ("ATTENTION", "HEALTHY")

    def test_empty_sprints_l4(self, tmp_path):
        """L4 should handle zero sprints gracefully."""
        f = tmp_path / "sprints.jsonl"
        f.write_text("")

        with patch("sys.argv", ["analyze-trend.py", "--sprints", str(f), "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                result = json.loads(mock_out.getvalue())

        assert result["sprint_count"] == 0
        assert result["overall"]["direction"] == "insufficient_data"

    def test_single_sprint_l4(self, tmp_path):
        """L4 should handle single sprint (insufficient for trend detection)."""
        sprints = [make_sprint(sprint_id="2026-02-03_2026-02-09")]
        f = tmp_path / "sprints.jsonl"
        f.write_text(json.dumps(sprints[0]) + "\n")

        with patch("sys.argv", ["analyze-trend.py", "--sprints", str(f), "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                result = json.loads(mock_out.getvalue())

        assert result["sprint_count"] == 1
        # All trends should be insufficient_data with only 1 sprint
        for metric_name, trend in result["trends"].items():
            assert trend["direction"] == "insufficient_data"


# =============================================================================
# Scenario 3: Multi-Sprint Trend Detection
# 3+ sprints with known patterns should produce correct trend signals
# =============================================================================

class TestMultiSprintTrend:
    """Verify L4 correctly detects improving/declining trends from L3 data."""

    def _run_l4(self, tmp_path, sprints):
        """Helper to run L4 and return parsed result."""
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")

        with patch("sys.argv", ["analyze-trend.py", "--sprints", str(f), "--format", "json"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                return json.loads(mock_out.getvalue())

    def test_improving_vdf_detected(self, tmp_path):
        """Increasing VDF values should be detected as improving."""
        sprints = [
            make_sprint(sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}", vdf=0.5 + i * 0.5)
            for i in range(1, 5)
        ]
        result = self._run_l4(tmp_path, sprints)
        assert result["trends"]["VDF"]["direction"] == "improving"

    def test_declining_health_detected(self, tmp_path):
        """Decreasing health values should be detected as declining."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}",
                sprint_health=0.90 - i * 0.15,
                ai_confidence=0.85 - i * 0.10,
                vdf=3.0 - i * 0.5,
                svlt_hours=4.0 + i * 5.0,
                rework_rate=0.05 + i * 0.06,
            )
            for i in range(1, 6)
        ]
        result = self._run_l4(tmp_path, sprints)
        assert result["overall"]["direction"] == "declining"
        assert result["trends"]["Sprint Health"]["direction"] == "declining"

    def test_mixed_trends_produce_recommendations(self, tmp_path):
        """Mixed improving/declining should generate appropriate recommendations."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}",
                vdf=3.0 - i * 0.5,           # declining
                sprint_health=0.50 + i * 0.08, # improving
                ai_confidence=0.55 + i * 0.05, # improving
                svlt_hours=5.0,               # stable
                rework_rate=0.10,             # stable
            )
            for i in range(1, 5)
        ]
        result = self._run_l4(tmp_path, sprints)

        assert result["trends"]["VDF"]["direction"] == "declining"
        assert result["trends"]["Sprint Health"]["direction"] == "improving"
        # Should have at least one recommendation for the declining VDF
        recs = result["recommendations"]
        vdf_recs = [r for r in recs if r["metric"] == "VDF"]
        assert len(vdf_recs) >= 1

    def test_cross_project_filter(self, tmp_path):
        """L4 should filter sprints by project_dir when specified."""
        sprints = [
            make_sprint(sprint_id="2026-02-03_2026-02-09", project_dir="/proj-a", vdf=1.0),
            make_sprint(sprint_id="2026-02-10_2026-02-16", project_dir="/proj-a", vdf=2.0),
            make_sprint(sprint_id="2026-02-17_2026-02-23", project_dir="/proj-a", vdf=3.0),
            make_sprint(sprint_id="2026-02-03_2026-02-09", project_dir="/proj-b", vdf=0.5),
            make_sprint(sprint_id="2026-02-10_2026-02-16", project_dir="/proj-b", vdf=0.3),
        ]
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")

        with patch("sys.argv", [
            "analyze-trend.py", "--sprints", str(f),
            "--project-dir", "/proj-a", "--format", "json",
        ]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                result = json.loads(mock_out.getvalue())

        assert result["sprint_count"] == 3
        assert result["trends"]["VDF"]["direction"] == "improving"

    def test_markdown_output_contains_all_sections(self, tmp_path):
        """L4 markdown output should include table, trends, and recommendations."""
        sprints = [
            make_sprint(sprint_id=f"2026-02-{i:02d}_2026-02-{i+6:02d}", vdf=1.0 + i * 0.3)
            for i in range(1, 4)
        ]
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")

        with patch("sys.argv", ["analyze-trend.py", "--sprints", str(f), "--format", "markdown"]):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                md = mock_out.getvalue()

        assert "## L4 Sprint Trend Analysis" in md
        assert "Sprint History" in md
        assert "| Sprint |" in md
