"""Tests for DORA metrics: extract_issue_number, calc_dora, calc_rework_rate."""
import json
import pytest
from helpers import make_pr, make_issue, make_churn_data
from aggregate_sprint import (
    extract_issue_number,
    calc_dora,
    calc_rework_rate,
    classify_level_lower_better,
    get_dora_thresholds,
)


# --- extract_issue_number ---

class TestExtractIssueNumber:
    def test_closes(self):
        pr = make_pr(body="Closes #42")
        assert extract_issue_number(pr) == 42

    def test_fixes(self):
        pr = make_pr(body="Fixes #7")
        assert extract_issue_number(pr) == 7

    def test_branch(self):
        pr = make_pr(body="no link", headRefName="issue-15")
        assert extract_issue_number(pr) == 15

    def test_none(self):
        pr = make_pr(body="no link here", headRefName="feature-branch")
        assert extract_issue_number(pr) is None

    def test_resolves(self):
        pr = make_pr(body="Resolves #99")
        assert extract_issue_number(pr) == 99


# --- calc_dora (VDF) ---

class TestCalcDoraVdf:
    def test_vdf_linked_prs(self, solo_thresholds):
        prs = [
            make_pr(number=1, body="Closes #1"),
            make_pr(number=2, body="Closes #2"),
            make_pr(number=3, body="Closes #3"),
        ]
        dora = calc_dora(prs, [], [], 1, "/tmp/test", solo_thresholds)
        assert dora["vdf"]["value"] == 3.0
        assert dora["vdf"]["level"] == "ELITE"
        assert dora["vdf"]["qualified_prs"] == 3

    def test_vdf_no_links(self, solo_thresholds):
        prs = [make_pr(body="no link", headRefName="feature")]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["vdf"]["value"] == 0.0
        assert dora["vdf"]["level"] == "LOW"

    def test_vdf_pod_threshold(self, pod_thresholds):
        # 3 PRs / 1 day = 3.0 → HIGH for Pod (ELITE needs ≥5.0)
        prs = [make_pr(number=i, body=f"Closes #{i}") for i in range(1, 4)]
        dora = calc_dora(prs, [], [], 1, "/tmp/test", pod_thresholds)
        assert dora["vdf"]["level"] == "HIGH"

    def test_vdf_squad_threshold(self, squad_thresholds):
        # 3 PRs / 1 day = 3.0 → MEDIUM for Squad (HIGH needs ≥5.0)
        prs = [make_pr(number=i, body=f"Closes #{i}") for i in range(1, 4)]
        dora = calc_dora(prs, [], [], 1, "/tmp/test", squad_thresholds)
        assert dora["vdf"]["level"] == "MEDIUM"

    def test_vdf_none_when_no_prs(self, solo_thresholds):
        dora = calc_dora(None, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["vdf"] is None


# --- calc_dora (SVLT) — requires monkeypatched gh CLI ---

class TestCalcDoraSvlt:
    def test_svlt_basic(self, monkeypatch, solo_thresholds):
        """SVLT with monkeypatched run_gh_command."""
        import aggregate_sprint as mod

        def mock_run_gh(args, project_dir):
            if "issue" in args and "view" in args:
                return {"createdAt": "2026-02-10T08:00:00Z"}
            return None

        monkeypatch.setattr(mod, "run_gh_command", mock_run_gh)

        prs = [make_pr(
            number=1, body="Closes #1",
            createdAt="2026-02-10T10:00:00Z",
            mergedAt="2026-02-10T12:00:00Z",
        )]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["svlt"] is not None
        assert dora["svlt"]["value_hours"] == 4.0  # 12:00 - 08:00 = 4h
        assert dora["svlt"]["level"] == "HIGH"  # 4h == ELITE threshold, but < uses strict less-than → HIGH

    def test_svlt_no_qualified(self, solo_thresholds):
        prs = [make_pr(body="no link", headRefName="feature")]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["svlt"] is None

    def test_svlt_missing_pr_dates(self, monkeypatch, solo_thresholds):
        """PR with missing createdAt/mergedAt should be skipped in SVLT."""
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "run_gh_command", lambda a, p: {"createdAt": "2026-02-10T08:00:00Z"})

        prs = [make_pr(number=1, body="Closes #1", createdAt=None, mergedAt=None)]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["svlt"] is None  # skipped due to missing dates

    def test_svlt_issue_created_none(self, monkeypatch, solo_thresholds):
        """gh returns issue data but createdAt is None → skip."""
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "run_gh_command", lambda a, p: {"createdAt": None})

        prs = [make_pr(
            number=1, body="Closes #1",
            createdAt="2026-02-10T10:00:00Z",
            mergedAt="2026-02-10T12:00:00Z",
        )]
        dora = calc_dora(prs, [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["svlt"] is None


# --- calc_dora (TTC) ---

class TestCalcDoraTtc:
    def test_ttc_with_bugs(self, solo_thresholds):
        bugs = [make_issue(
            createdAt="2026-02-10T08:00:00Z",
            closedAt="2026-02-10T09:00:00Z",
            labels=[{"name": "bug"}],
        )]
        dora = calc_dora([], [], bugs, 7, "/tmp/test", solo_thresholds)
        assert dora["ttc"] is not None
        assert dora["ttc"]["value_hours"] == 1.0
        assert dora["ttc"]["level"] == "HIGH"  # 1h == ELITE threshold, strict less-than → HIGH

    def test_ttc_no_bugs(self, solo_thresholds):
        dora = calc_dora([], [], [], 7, "/tmp/test", solo_thresholds)
        assert dora["ttc"] is None


# --- calc_rework_rate ---

class TestCalcReworkRate:
    def test_basic(self, solo_thresholds):
        churn = make_churn_data(**{"a.py": 5, "b.py": 4, "c.py": 1})
        result = calc_rework_rate(churn, 10, solo_thresholds)
        assert result["value"] == 0.2  # 2 high-churn / 10 total
        assert result["level"] == "MEDIUM"

    def test_zero_files(self, solo_thresholds):
        churn = make_churn_data(**{"a.py": 5})
        result = calc_rework_rate(churn, 0, solo_thresholds)
        assert result["value"] == 0.0
        assert result["level"] == "ELITE"

    def test_none_churn(self, solo_thresholds):
        result = calc_rework_rate(None, 10, solo_thresholds)
        assert result is None

    def test_elite(self, solo_thresholds):
        churn = make_churn_data(**{"a.py": 1, "b.py": 2})
        result = calc_rework_rate(churn, 100, solo_thresholds)
        assert result["value"] == 0.0
        assert result["level"] == "ELITE"

    def test_universal_rework_threshold(self):
        """Rework Rate thresholds are identical across all scales."""
        solo = get_dora_thresholds("solo")["rework"]
        pod = get_dora_thresholds("pod")["rework"]
        squad = get_dora_thresholds("squad")["rework"]
        assert solo == pod == squad
