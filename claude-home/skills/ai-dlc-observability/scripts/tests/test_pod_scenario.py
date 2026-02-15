"""Pod scenario tests: team_size="pod" threshold classification, multi-session
aggregation, PR review cycles, AI-Confidence at Pod scale, and multi-sprint
trend detection.

Validates that Pod (2-5 ARE) DORA thresholds differ from Solo where expected
and that multi-session data aggregates correctly for team workflows.
"""
import io
import json
from unittest.mock import patch

import pytest
from helpers import (
    make_session,
    make_sprint,
    make_pr,
    make_issue,
    make_churn_data,
    make_spec_scores,
    make_pod_sessions,
    make_reviewed_pr,
)
from aggregate_sprint import (
    classify_level_higher_better,
    classify_level_lower_better,
    get_dora_thresholds,
    aggregate_sessions,
    calc_ai_confidence,
    calc_rework_rate,
    calc_dora,
)
from analyze_trend import main as analyze_trend_main


# =============================================================================
# Class 1: Pod Threshold Classification
# Same metric values produce different DORA levels under Solo vs Pod thresholds
# =============================================================================

class TestPodThresholdClassification:
    """Verify Solo vs Pod threshold divergence for DORA levels."""

    def test_vdf_pod_elite(self, solo_thresholds, pod_thresholds):
        """VDF=5.0 → ELITE under both Solo and Pod."""
        assert classify_level_higher_better(5.0, solo_thresholds["vdf"]) == "ELITE"
        assert classify_level_higher_better(5.0, pod_thresholds["vdf"]) == "ELITE"

    def test_vdf_pod_high(self, solo_thresholds, pod_thresholds):
        """VDF=2.0 → Solo ELITE, Pod HIGH."""
        assert classify_level_higher_better(2.0, solo_thresholds["vdf"]) == "ELITE"
        assert classify_level_higher_better(2.0, pod_thresholds["vdf"]) == "HIGH"

    def test_vdf_pod_medium(self, solo_thresholds, pod_thresholds):
        """VDF=1.0 → Solo HIGH, Pod MEDIUM."""
        assert classify_level_higher_better(1.0, solo_thresholds["vdf"]) == "HIGH"
        assert classify_level_higher_better(1.0, pod_thresholds["vdf"]) == "MEDIUM"

    def test_vdf_pod_low(self, solo_thresholds, pod_thresholds):
        """VDF=0.5 → Solo MEDIUM, Pod LOW."""
        assert classify_level_higher_better(0.5, solo_thresholds["vdf"]) == "MEDIUM"
        assert classify_level_higher_better(0.5, pod_thresholds["vdf"]) == "LOW"

    def test_svlt_pod_elite(self, solo_thresholds, pod_thresholds):
        """SVLT=7h → Solo HIGH (7 < 8), Pod ELITE (7 < 8)."""
        # Solo: 7 >= 4 (not ELITE), 7 < 8 → HIGH
        # Pod:  7 < 8 → ELITE
        assert classify_level_lower_better(7, solo_thresholds["svlt"]) == "HIGH"
        assert classify_level_lower_better(7, pod_thresholds["svlt"]) == "ELITE"

    def test_svlt_pod_high(self, solo_thresholds, pod_thresholds):
        """SVLT=20h → Solo MEDIUM (20 < 24), Pod HIGH (20 < 24)."""
        # Solo: 20 >= 4, >= 8, 20 < 24 → MEDIUM
        # Pod:  20 >= 8, 20 < 24 → HIGH
        assert classify_level_lower_better(20, solo_thresholds["svlt"]) == "MEDIUM"
        assert classify_level_lower_better(20, pod_thresholds["svlt"]) == "HIGH"

    def test_ttc_pod_elite(self, solo_thresholds, pod_thresholds):
        """TTC=3h → Solo HIGH (3 < 4), Pod ELITE (3 < 4)."""
        # Solo: 3 >= 1 (not ELITE), 3 < 4 → HIGH
        # Pod:  3 < 4 → ELITE
        assert classify_level_lower_better(3, solo_thresholds["ttc"]) == "HIGH"
        assert classify_level_lower_better(3, pod_thresholds["ttc"]) == "ELITE"

    def test_rework_same_across_scales(self, solo_thresholds, pod_thresholds):
        """Rework=0.10 → HIGH under both Solo and Pod (scale-independent)."""
        assert classify_level_lower_better(0.10, solo_thresholds["rework"]) == "HIGH"
        assert classify_level_lower_better(0.10, pod_thresholds["rework"]) == "HIGH"

    def test_make_sprint_pod_levels(self):
        """make_sprint(team_size='pod', vdf=2.0) → VDF level=HIGH under Pod thresholds."""
        sprint = make_sprint(team_size="pod", vdf=2.0)
        assert sprint["team_size"] == "pod"
        assert sprint["dora"]["vdf"]["level"] == "HIGH"


# =============================================================================
# Class 2: Multi-Session Aggregation
# 3-5 sessions (Pod members) merge correctly via aggregate_sessions
# =============================================================================

class TestMultiSessionAggregation:
    """Verify aggregate_sessions handles multiple Pod member sessions."""

    def test_three_member_sessions_merge(self):
        """3 member sessions → session_count=3, total_turns=sum."""
        sessions = make_pod_sessions(member_count=3, turns_per_member=20)
        activity = aggregate_sessions(sessions)

        assert activity["session_count"] == 3
        assert activity["total_turns"] == 60  # 3 * 20

    def test_overlapping_files_deduplicated(self):
        """2 members modifying same file → file_count with deduplication."""
        sessions = [
            make_session(
                modified_files=["src/shared.py", "src/module_a.py"],
            ),
            make_session(
                modified_files=["src/shared.py", "src/module_b.py"],
            ),
        ]
        activity = aggregate_sessions(sessions)

        # Unique: src/shared.py, src/module_a.py, src/module_b.py
        assert activity["file_count"] == 3

    def test_tool_counts_accumulate(self):
        """Tool counts from all Pod members accumulate correctly."""
        sessions = make_pod_sessions(member_count=3)
        activity = aggregate_sessions(sessions)

        # Each session: Edit=5, Read=8, Bash=3, Write=2
        assert activity["top_tools"]["Edit"] == 15   # 5 * 3
        assert activity["top_tools"]["Read"] == 24   # 8 * 3
        assert activity["top_tools"]["Bash"] == 9    # 3 * 3
        assert activity["top_tools"]["Write"] == 6   # 2 * 3

    def test_session_durations_independent(self):
        """Overlapping sessions (same time range) have independent durations."""
        # Two members working at the same time
        sessions = [
            make_session(
                start_time="2026-02-10T09:00:00Z",
                end_time="2026-02-10T10:00:00Z",
            ),
            make_session(
                start_time="2026-02-10T09:00:00Z",
                end_time="2026-02-10T11:00:00Z",
            ),
        ]
        activity = aggregate_sessions(sessions)

        # Each session contributes its own duration independently
        assert len(activity["session_durations"]) == 2
        assert activity["session_durations"][0] == 3600   # 1 hour
        assert activity["session_durations"][1] == 7200   # 2 hours

    def test_pod_activity_feeds_ai_confidence(self):
        """3 Pod sessions with avg 15 turns → correct TPR and SE."""
        sessions = make_pod_sessions(member_count=3, turns_per_member=15)
        activity = aggregate_sessions(sessions)

        ai_conf = calc_ai_confidence(None, None, activity)

        # TPR: avg_turns = 45/3 = 15, TPR = 1 - (15-10)/40 = 0.875
        assert ai_conf["components"]["tpr"] == 0.88  # round(0.875, 2) = 0.88

        # SE: Edit=15, Write=6, MultiEdit=0 → edit_tools=21
        #     total_tools = 15+24+9+6 = 54
        #     SE = 21/54 ≈ 0.3889 → round to 0.39
        assert ai_conf["components"]["se"] == 0.39


# =============================================================================
# Class 3: PR Review Cycle
# PR review lifecycle affects DORA metrics (SVLT, Rework, TTC, VDF)
# =============================================================================

class TestPodPrReviewCycle:
    """Verify PR review cycle impact on DORA metrics."""

    def test_multi_reviewer_svlt(self, monkeypatch, pod_thresholds):
        """PR with 2 reviewers → SVLT = issue createdAt to PR mergedAt."""
        import aggregate_sprint as mod

        def mock_run_gh(args, project_dir):
            if "issue" in args and "view" in args:
                return {"createdAt": "2026-02-10T08:00:00Z"}
            return None

        monkeypatch.setattr(mod, "run_gh_command", mock_run_gh)

        prs = [make_reviewed_pr(
            number=1, body="Closes #1", reviewers=2,
            createdAt="2026-02-10T10:00:00Z",
            mergedAt="2026-02-10T16:00:00Z",
        )]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", pod_thresholds)

        # SVLT = 16:00 - 08:00 = 8h
        assert dora["svlt"] is not None
        assert dora["svlt"]["value_hours"] == 8.0
        # Pod ELITE < 8h → 8h is exactly at threshold, strict < means HIGH
        assert dora["svlt"]["level"] == "HIGH"

    def test_review_rejection_extends_svlt(self, monkeypatch, pod_thresholds):
        """changes_requested extends PR merge time → higher SVLT."""
        import aggregate_sprint as mod

        def mock_run_gh(args, project_dir):
            if "issue" in args and "view" in args:
                return {"createdAt": "2026-02-10T08:00:00Z"}
            return None

        monkeypatch.setattr(mod, "run_gh_command", mock_run_gh)

        # Quick approval: merged 4h after issue
        fast_pr = make_reviewed_pr(
            number=1, body="Closes #1",
            createdAt="2026-02-10T09:00:00Z",
            mergedAt="2026-02-10T12:00:00Z",
        )
        # Rejection + re-review: merged 20h after issue
        slow_pr = make_reviewed_pr(
            number=2, body="Closes #2", rejected=True, rounds=2,
            createdAt="2026-02-10T09:00:00Z",
            mergedAt="2026-02-11T04:00:00Z",
        )

        fast_dora = calc_dora([fast_pr], [], [], 7, "/tmp/test", pod_thresholds)
        slow_dora = calc_dora([slow_pr], [], [], 7, "/tmp/test", pod_thresholds)

        assert fast_dora["svlt"]["value_hours"] == 4.0   # 12:00 - 08:00
        assert slow_dora["svlt"]["value_hours"] == 20.0  # 04:00+1d - 08:00
        # Slower SVLT → worse level
        assert fast_dora["svlt"]["level"] == "ELITE"  # 4 < 8
        assert slow_dora["svlt"]["level"] == "HIGH"   # 20 < 24

    def test_churn_from_review_feedback(self, pod_thresholds):
        """Review-driven file churn → Rework Rate via high-churn files."""
        # 3 files with high churn (review feedback caused repeated edits)
        churn = make_churn_data(**{
            "src/api.py": 6,       # high churn from review
            "src/handler.py": 4,   # high churn from review
            "src/model.py": 3,     # high churn from review
            "src/utils.py": 1,     # stable
            "tests/test_api.py": 1,
        })
        rework = calc_rework_rate(churn, total_modified_files=20, thresholds=pod_thresholds)

        assert rework["high_churn_files"] == 3  # api, handler, model
        assert rework["value"] == 0.15          # 3/20
        # Rework 0.15 → HIGH threshold is < 0.15 → 0.15 is not < 0.15 → MEDIUM
        assert rework["level"] == "MEDIUM"

    def test_bug_fix_pr_feeds_ttc(self, pod_thresholds):
        """Bug issue with close time → TTC calculation."""
        bugs = [make_issue(
            number=5,
            title="fix: null pointer in parser",
            createdAt="2026-02-10T08:00:00Z",
            closedAt="2026-02-10T11:00:00Z",
            labels=[{"name": "bug"}],
        )]
        dora = calc_dora([], [], bugs, 7, "/tmp/test", pod_thresholds)

        assert dora["ttc"] is not None
        assert dora["ttc"]["value_hours"] == 3.0  # 11:00 - 08:00
        # Pod TTC ELITE < 4h → 3 < 4 → ELITE
        assert dora["ttc"]["level"] == "ELITE"

    def test_qualified_pr_ratio(self, pod_thresholds):
        """Issue-linked PRs / sprint days = VDF."""
        prs = [
            make_reviewed_pr(number=1, body="Closes #1"),
            make_reviewed_pr(number=2, body="Closes #2"),
            make_reviewed_pr(number=3, body="Closes #3"),
            make_pr(number=4, body="no issue link", headRefName="refactor"),
            make_pr(number=5, body="no issue link", headRefName="chore"),
        ]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", pod_thresholds)

        # 3 qualified PRs / 7 days ≈ 0.43
        assert dora["vdf"]["qualified_prs"] == 3
        assert dora["vdf"]["total_prs"] == 5
        assert dora["vdf"]["value"] == pytest.approx(0.43, abs=0.01)
        # Pod VDF: 0.43 < 1.0 → LOW
        assert dora["vdf"]["level"] == "LOW"


# =============================================================================
# Class 4: Pod AI-Confidence
# Verify 4 components at Pod scale
# =============================================================================

class TestPodAiConfidence:
    """Verify AI-Confidence components with Pod-scale data."""

    def test_pod_spec_quality_multiple_specs(self):
        """3 specs (score 3,4,5) → SQ = avg(3,4,5)/5 = 0.8."""
        spec = make_spec_scores(**{"spec-a.md": 3, "spec-b.md": 4, "spec-c.md": 5})
        activity = {"session_count": 1, "total_turns": 10, "file_count": 10, "top_tools": {"Edit": 5}}
        result = calc_ai_confidence(spec, None, activity)

        assert result["components"]["sq"] == 0.8

    def test_pod_churn_inverse(self):
        """20 files, 4 high-churn → CI = 1 - 4/20 = 0.80."""
        churn = make_churn_data(**{
            f"src/file_{i}.py": (5 if i < 4 else 1)
            for i in range(20)
        })
        activity = {"session_count": 1, "total_turns": 10, "file_count": 20, "top_tools": {"Edit": 5}}
        result = calc_ai_confidence(None, churn, activity)

        assert result["components"]["ci"] == 0.8

    def test_pod_tpr_multiple_sessions(self):
        """3 sessions avg 15 turns → TPR = 1 - (15-10)/40 = 0.875."""
        activity = {
            "session_count": 3,
            "total_turns": 45,  # avg = 15
            "file_count": 10,
            "top_tools": {"Edit": 5},
        }
        result = calc_ai_confidence(None, None, activity)

        # TPR = 1 - (15-10)/40 = 1 - 0.125 = 0.875 → round(0.875, 2) = 0.88
        assert result["components"]["tpr"] == 0.88

    def test_pod_session_efficiency(self):
        """Edit tools = 60% of total → SE = 0.6."""
        activity = {
            "session_count": 3,
            "total_turns": 45,
            "file_count": 10,
            "top_tools": {"Edit": 40, "Write": 15, "MultiEdit": 5, "Read": 30, "Bash": 10},
        }
        result = calc_ai_confidence(None, None, activity)

        # edit_tools = Edit(40) + Write(15) + MultiEdit(5) = 60
        # total = 40+15+5+30+10 = 100
        # SE = 60/100 = 0.6
        assert result["components"]["se"] == 0.6

    def test_pod_composite_high(self):
        """All 4 components → composite ≈ 0.78 (HIGH)."""
        spec = make_spec_scores(**{"spec-a.md": 3, "spec-b.md": 4, "spec-c.md": 5})
        churn = make_churn_data(**{
            f"src/file_{i}.py": (5 if i < 4 else 1)
            for i in range(20)
        })
        activity = {
            "session_count": 3,
            "total_turns": 45,  # avg=15
            "file_count": 20,
            "top_tools": {"Edit": 40, "Write": 15, "MultiEdit": 5, "Read": 30, "Bash": 10},
        }
        result = calc_ai_confidence(spec, churn, activity)

        # SQ=0.8, CI=0.80, TPR=0.88, SE=0.6
        # Composite = 0.8*0.30 + 0.80*0.25 + 0.88*0.25 + 0.6*0.20
        #           = 0.24 + 0.20 + 0.22 + 0.12 = 0.78
        assert result["value"] == 0.78
        assert result["level"] == "HIGH"

    def test_pod_composite_with_missing_spec(self):
        """spec=None → SQ=0.5, composite lower."""
        churn = make_churn_data(**{
            f"src/file_{i}.py": (5 if i < 4 else 1)
            for i in range(20)
        })
        activity = {
            "session_count": 3,
            "total_turns": 45,
            "file_count": 20,
            "top_tools": {"Edit": 40, "Write": 15, "MultiEdit": 5, "Read": 30, "Bash": 10},
        }
        result = calc_ai_confidence(None, churn, activity)

        assert result["components"]["sq"] == 0.5
        # Composite = 0.5*0.30 + 0.80*0.25 + 0.88*0.25 + 0.6*0.20
        #           = 0.15 + 0.20 + 0.22 + 0.12 = 0.69
        assert result["value"] == 0.69
        assert result["level"] == "HIGH"  # 0.69 >= 0.65


# =============================================================================
# Class 5: Pod Multi-Sprint Trend
# 4+ sprint trend detection with Pod thresholds
# =============================================================================

class TestPodMultiSprintTrend:
    """Verify L4 trend detection with Pod-scale sprint data."""

    def _run_l4(self, tmp_path, sprints, project_dir=None):
        """Helper to run L4 trend analysis and return parsed JSON result."""
        f = tmp_path / "sprints.jsonl"
        f.write_text("\n".join(json.dumps(s) for s in sprints) + "\n")

        argv = ["analyze-trend.py", "--sprints", str(f), "--format", "json"]
        if project_dir:
            argv += ["--project-dir", project_dir]

        with patch("sys.argv", argv):
            with patch("sys.stdout", new_callable=io.StringIO) as mock_out:
                analyze_trend_main()
                return json.loads(mock_out.getvalue())

    def test_pod_improving_sprints(self, tmp_path):
        """VDF 1→2→3→5 under Pod thresholds → improving trend."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{3+i*7:02d}_2026-02-{9+i*7:02d}",
                team_size="pod", vdf=vdf,
            )
            for i, vdf in enumerate([1.0, 2.0, 3.0, 5.0])
        ]
        result = self._run_l4(tmp_path, sprints)

        assert result["trends"]["VDF"]["direction"] == "improving"
        assert result["trends"]["VDF"]["points"] == 4

    def test_pod_declining_sprints(self, tmp_path):
        """Sprint Health 0.85→0.70→0.55→0.40 → declining."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{3+i*7:02d}_2026-02-{9+i*7:02d}",
                team_size="pod",
                sprint_health=health,
                ai_confidence=health - 0.10,
                vdf=5.0 - i * 1.0,
                svlt_hours=8.0 + i * 8.0,
                rework_rate=0.05 + i * 0.05,
            )
            for i, health in enumerate([0.85, 0.70, 0.55, 0.40])
        ]
        result = self._run_l4(tmp_path, sprints)

        assert result["overall"]["direction"] == "declining"
        assert result["trends"]["Sprint Health"]["direction"] == "declining"

    def test_pod_stable_at_high(self, tmp_path):
        """All metrics stable at HIGH level → stable trend."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{3+i*7:02d}_2026-02-{9+i*7:02d}",
                team_size="pod",
                vdf=3.0,          # Pod HIGH
                svlt_hours=20.0,  # Pod HIGH
                rework_rate=0.10, # HIGH
                sprint_health=0.75,
                ai_confidence=0.70,
            )
            for i in range(4)
        ]
        result = self._run_l4(tmp_path, sprints)

        assert result["trends"]["VDF"]["direction"] == "stable"
        assert result["trends"]["Sprint Health"]["direction"] == "stable"

    def test_pod_scale_transition(self, tmp_path):
        """Solo→Pod transition → mixed team_size in result."""
        sprints = [
            make_sprint(
                sprint_id="2026-02-03_2026-02-09",
                team_size="solo", vdf=1.5,
            ),
            make_sprint(
                sprint_id="2026-02-10_2026-02-16",
                team_size="pod", vdf=2.0,
            ),
            make_sprint(
                sprint_id="2026-02-17_2026-02-23",
                team_size="pod", vdf=3.0,
            ),
        ]
        result = self._run_l4(tmp_path, sprints)

        # Mixed team_size warning
        assert len(result["team_sizes"]) > 1
        assert "solo" in result["team_sizes"]
        assert "pod" in result["team_sizes"]

    def test_pod_recommendations_p0(self, tmp_path):
        """Health declining over 4 sprints → P0 recommendation."""
        sprints = [
            make_sprint(
                sprint_id=f"2026-02-{3+i*7:02d}_2026-02-{9+i*7:02d}",
                team_size="pod",
                sprint_health=0.80 - i * 0.15,
                ai_confidence=0.70 - i * 0.10,
                vdf=4.0 - i * 0.8,
                svlt_hours=10.0 + i * 6.0,
                rework_rate=0.05 + i * 0.08,
            )
            for i in range(4)
        ]
        result = self._run_l4(tmp_path, sprints)

        # Health: 0.80 → 0.65 → 0.50 → 0.35 (declining)
        assert result["trends"]["Sprint Health"]["direction"] == "declining"
        # P0 recommendation for declining 3+ sprint metrics
        p0_recs = [r for r in result["recommendations"] if r["priority"] == "P0"]
        assert len(p0_recs) >= 1
        # At least one P0 should target Sprint Health (latest < 0.50)
        health_p0 = [r for r in p0_recs if r["metric"] == "Sprint Health"]
        assert len(health_p0) >= 1

    def test_pod_cross_project_isolation(self, tmp_path):
        """Two Pod projects → independent trends when filtered."""
        sprints = [
            # Project A: improving VDF
            make_sprint(sprint_id="2026-02-03_2026-02-09", project_dir="/proj-a",
                        team_size="pod", vdf=1.0),
            make_sprint(sprint_id="2026-02-10_2026-02-16", project_dir="/proj-a",
                        team_size="pod", vdf=2.0),
            make_sprint(sprint_id="2026-02-17_2026-02-23", project_dir="/proj-a",
                        team_size="pod", vdf=3.0),
            # Project B: declining VDF
            make_sprint(sprint_id="2026-02-03_2026-02-09", project_dir="/proj-b",
                        team_size="pod", vdf=5.0),
            make_sprint(sprint_id="2026-02-10_2026-02-16", project_dir="/proj-b",
                        team_size="pod", vdf=3.0),
            make_sprint(sprint_id="2026-02-17_2026-02-23", project_dir="/proj-b",
                        team_size="pod", vdf=1.0),
        ]

        result_a = self._run_l4(tmp_path, sprints, project_dir="/proj-a")
        assert result_a["sprint_count"] == 3
        assert result_a["trends"]["VDF"]["direction"] == "improving"

        result_b = self._run_l4(tmp_path, sprints, project_dir="/proj-b")
        assert result_b["sprint_count"] == 3
        assert result_b["trends"]["VDF"]["direction"] == "declining"
