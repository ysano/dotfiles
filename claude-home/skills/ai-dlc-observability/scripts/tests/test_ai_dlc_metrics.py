"""Tests for AI-DLC metrics: ai_confidence, mttv, sprint_health."""
import pytest
from helpers import make_session, make_pr, make_spec_scores, make_churn_data
from aggregate_sprint import (
    calc_ai_confidence,
    calc_mttv_macro,
    calc_mttv_micro,
    calc_sprint_health,
    aggregate_sessions,
    AI_CONFIDENCE_WEIGHTS,
    LEVEL_TO_SCORE,
)


# --- AI-Confidence ---

class TestAiConfidence:
    def test_all_data(self):
        spec = make_spec_scores(**{"a.md": 4, "b.md": 5})
        churn = make_churn_data(**{"x.py": 1, "y.py": 2})
        activity = {
            "session_count": 5,
            "total_turns": 50,
            "file_count": 10,
            "top_tools": {"Edit": 10, "Read": 20, "Bash": 5},
        }
        result = calc_ai_confidence(spec, churn, activity)
        assert "value" in result
        assert "level" in result
        assert "components" in result
        assert 0 <= result["value"] <= 1.0

    def test_missing_spec(self):
        churn = make_churn_data(**{"x.py": 1})
        activity = {"session_count": 1, "total_turns": 10, "file_count": 5, "top_tools": {"Edit": 5}}
        result = calc_ai_confidence(None, churn, activity)
        assert result["components"]["sq"] == 0.5

    def test_missing_churn(self):
        spec = make_spec_scores(**{"a.md": 4})
        activity = {"session_count": 1, "total_turns": 10, "file_count": 5, "top_tools": {"Edit": 5}}
        result = calc_ai_confidence(spec, None, activity)
        assert result["components"]["ci"] == 0.5

    def test_missing_activity(self):
        spec = make_spec_scores(**{"a.md": 4})
        churn = make_churn_data(**{"x.py": 1})
        activity = {"session_count": 0, "total_turns": 0, "file_count": 0, "top_tools": {}}
        result = calc_ai_confidence(spec, churn, activity)
        assert result["components"]["tpr"] == 0.5
        assert result["components"]["se"] == 0.5

    def test_weights_sum(self):
        total = sum(AI_CONFIDENCE_WEIGHTS.values())
        assert abs(total - 1.0) < 1e-9

    def test_elite(self):
        spec = make_spec_scores(**{"a.md": 5, "b.md": 5})
        churn = make_churn_data(**{"x.py": 1})
        activity = {
            "session_count": 10,
            "total_turns": 100,  # avg=10, TPR=1.0
            "file_count": 100,
            "top_tools": {"Edit": 50, "Write": 20, "MultiEdit": 10, "Read": 20},
        }
        result = calc_ai_confidence(spec, churn, activity)
        assert result["level"] == "ELITE"
        assert result["value"] >= 0.85

    def test_low(self):
        spec = make_spec_scores(**{"a.md": 0, "b.md": 0})
        churn = make_churn_data(**{f"f{i}.py": 10 for i in range(50)})
        activity = {
            "session_count": 1,
            "total_turns": 60,  # avg=60, TPR=0.0
            "file_count": 5,
            "top_tools": {"Read": 100},
        }
        result = calc_ai_confidence(spec, churn, activity)
        assert result["level"] == "LOW"
        assert result["value"] < 0.45


# --- TPR clamping ---

class TestTprClamping:
    def test_low_turns(self):
        activity = {"session_count": 2, "total_turns": 10, "file_count": 0, "top_tools": {}}
        result = calc_ai_confidence(None, None, activity)
        assert result["components"]["tpr"] == 1.0  # avg=5, (5-10)/40=-0.125, 1-(-0.125)=1.125→clamped to 1.0

    def test_high_turns(self):
        activity = {"session_count": 1, "total_turns": 60, "file_count": 0, "top_tools": {}}
        result = calc_ai_confidence(None, None, activity)
        assert result["components"]["tpr"] == 0.0  # avg=60, (60-10)/40=1.25, 1-1.25=-0.25→clamped to 0.0


# --- SE ---

class TestSessionEfficiency:
    def test_no_tools(self):
        activity = {"session_count": 1, "total_turns": 10, "file_count": 0, "top_tools": {}}
        result = calc_ai_confidence(None, None, activity)
        assert result["components"]["se"] == 0.5

    def test_tools_with_zero_total(self):
        """top_tools is truthy but all values sum to 0 → SE=0.5."""
        activity = {"session_count": 1, "total_turns": 10, "file_count": 0, "top_tools": {"Read": 0}}
        result = calc_ai_confidence(None, None, activity)
        assert result["components"]["se"] == 0.5


# --- MTTV Macro ---

class TestMttvMacro:
    def test_basic(self):
        prs = [
            make_pr(createdAt="2026-02-10T10:00:00Z", mergedAt="2026-02-10T12:00:00Z"),
            make_pr(createdAt="2026-02-10T10:00:00Z", mergedAt="2026-02-10T14:00:00Z"),
            make_pr(createdAt="2026-02-10T10:00:00Z", mergedAt="2026-02-10T16:00:00Z"),
        ]
        result = calc_mttv_macro(prs)
        assert result == 4.0  # median of [2, 4, 6] = 4

    def test_empty(self):
        assert calc_mttv_macro([]) is None

    def test_none(self):
        assert calc_mttv_macro(None) is None

    def test_prs_with_no_dates(self):
        """PRs exist but all have None dates → no durations → None."""
        prs = [
            make_pr(createdAt=None, mergedAt=None),
            make_pr(createdAt=None, mergedAt="2026-02-10T12:00:00Z"),
        ]
        assert calc_mttv_macro(prs) is None


# --- MTTV Micro ---

class TestMttvMicro:
    def test_basic(self):
        sessions = [
            make_session(
                start_time="2026-02-10T09:00:00Z",
                end_time="2026-02-10T10:00:00Z",
                total_turns=20,
            ),
        ]
        result = calc_mttv_micro(sessions)
        assert result == 180.0  # 3600s / 20 turns = 180

    def test_no_turns(self):
        sessions = [
            make_session(
                start_time="2026-02-10T09:00:00Z",
                end_time="2026-02-10T10:00:00Z",
                total_turns=0,
            ),
        ]
        result = calc_mttv_micro(sessions)
        assert result is None


# --- Sprint Health ---

class TestSprintHealth:
    def _make_dora(self, vdf_level, svlt_level, ttc_level, rework_level):
        """Helper to build minimal dora dict."""
        def _entry(level):
            return {"level": level} if level else None
        return {
            "vdf": _entry(vdf_level),
            "svlt": _entry(svlt_level),
            "ttc": _entry(ttc_level),
            "rework_rate": _entry(rework_level),
        }

    def test_all_elite(self):
        dora = self._make_dora("ELITE", "ELITE", "ELITE", "ELITE")
        result = calc_sprint_health(dora, 1.0, 1.0, 0)
        assert result["value"] == 1.0
        assert result["level"] == "HEALTHY"

    def test_all_low(self):
        dora = self._make_dora("LOW", "LOW", "LOW", "LOW")
        result = calc_sprint_health(dora, 0.25, 0.25, 5)
        assert result["value"] == 0.25
        assert result["level"] == "CRITICAL"

    def test_mixed(self):
        dora = self._make_dora("ELITE", "HIGH", "MEDIUM", "LOW")
        # scores: VDF=1.0, SVLT=0.75, TTC=0.5, Rework=0.25, AI-Conf=0.7, Spec=0.8
        result = calc_sprint_health(dora, 0.7, 0.8, 2)
        expected = round((1.0 + 0.75 + 0.5 + 0.25 + 0.7 + 0.8) / 6, 2)
        assert result["value"] == expected

    def test_ttc_null_no_bugs(self):
        dora = self._make_dora("ELITE", "ELITE", None, "ELITE")
        result = calc_sprint_health(dora, 0.9, 0.9, 0)
        # TTC → 1.0 (no bugs = best)
        expected = round((1.0 + 1.0 + 1.0 + 1.0 + 0.9 + 0.9) / 6, 2)
        assert result["value"] == expected

    def test_ttc_null_with_bugs(self):
        dora = self._make_dora("ELITE", "ELITE", None, "ELITE")
        result = calc_sprint_health(dora, 0.9, 0.9, 3)
        # TTC → 0.5 (bugs exist but TTC data missing)
        expected = round((1.0 + 1.0 + 0.5 + 1.0 + 0.9 + 0.9) / 6, 2)
        assert result["value"] == expected

    def test_six_elements(self):
        """Sprint health is average of exactly 6 elements."""
        dora = self._make_dora("HIGH", "HIGH", "HIGH", "HIGH")
        result = calc_sprint_health(dora, 0.75, 0.75, 1)
        # All 6 elements = 0.75
        assert result["value"] == 0.75

    def test_thresholds(self):
        # HEALTHY ≥ 0.75
        dora = self._make_dora("ELITE", "ELITE", "ELITE", "ELITE")
        assert calc_sprint_health(dora, 0.9, 0.9, 0)["level"] == "HEALTHY"

        # ATTENTION ≥ 0.50
        dora2 = self._make_dora("MEDIUM", "MEDIUM", "MEDIUM", "MEDIUM")
        result2 = calc_sprint_health(dora2, 0.5, 0.5, 1)
        assert result2["level"] == "ATTENTION"

        # CRITICAL < 0.50
        dora3 = self._make_dora("LOW", "LOW", "LOW", "LOW")
        result3 = calc_sprint_health(dora3, 0.2, 0.2, 5)
        assert result3["level"] == "CRITICAL"
