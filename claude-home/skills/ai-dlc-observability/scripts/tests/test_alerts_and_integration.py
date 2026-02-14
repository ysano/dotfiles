"""Tests for alerts generation and integration (dry-run, team_size output, schema)."""
import json
import os
import tempfile
import pytest
from helpers import make_churn_data, make_spec_scores
from aggregate_sprint import generate_alerts


# --- Alerts ---

class TestAlerts:
    def test_high_churn_error(self):
        churn = make_churn_data(**{"hot.py": 5})
        alerts = generate_alerts(churn, None, {})
        error_alerts = [a for a in alerts if a["level"] == "error"]
        assert len(error_alerts) == 1
        assert "hot.py" in error_alerts[0]["message"]

    def test_high_churn_warn(self):
        churn = make_churn_data(**{"warm.py": 3})
        alerts = generate_alerts(churn, None, {})
        warn_alerts = [a for a in alerts if a["level"] == "warn"]
        assert len(warn_alerts) == 1

    def test_low_spec(self):
        specs = make_spec_scores(**{"bad-spec.md": 2})
        alerts = generate_alerts(None, specs, {})
        warn_alerts = [a for a in alerts if a["level"] == "warn"]
        assert len(warn_alerts) == 1
        assert "bad-spec" in warn_alerts[0]["message"]

    def test_dora_low(self):
        dora = {"vdf": {"level": "LOW"}, "svlt": None, "rework_rate": None, "ttc": None}
        alerts = generate_alerts(None, None, dora)
        error_alerts = [a for a in alerts if a["level"] == "error"]
        assert len(error_alerts) == 1
        assert "VDF" in error_alerts[0]["message"]

    def test_empty(self):
        alerts = generate_alerts(None, None, {})
        assert alerts == []


# --- Integration: dry-run ---

class TestDryRun:
    def test_dry_run_no_write(self, monkeypatch, tmp_path):
        """--dry-run should not write to sprints.jsonl."""
        import aggregate_sprint as mod
        import sys

        sprints_file = str(tmp_path / "sprints.jsonl")
        monkeypatch.setattr(mod, "SPRINTS_FILE", sprints_file)
        monkeypatch.setattr(mod, "METRICS_DIR", str(tmp_path))

        # Mock run_gh_command to return empty
        monkeypatch.setattr(mod, "run_gh_command", lambda *a, **kw: None)

        # Run main with --dry-run
        monkeypatch.setattr(sys, "argv", [
            "aggregate-sprint.py", "--dry-run", "--project-dir", str(tmp_path),
        ])
        mod.main()

        assert not os.path.exists(sprints_file)


# --- Integration: team_size in output ---

class TestTeamSizeOutput:
    def test_team_size_in_output(self, monkeypatch, tmp_path, capsys):
        """--team-size pod should appear in output JSON."""
        import aggregate_sprint as mod
        import sys

        monkeypatch.setattr(mod, "SPRINTS_FILE", str(tmp_path / "sprints.jsonl"))
        monkeypatch.setattr(mod, "METRICS_DIR", str(tmp_path))
        monkeypatch.setattr(mod, "run_gh_command", lambda *a, **kw: None)
        monkeypatch.setattr(sys, "argv", [
            "aggregate-sprint.py", "--dry-run", "--team-size", "pod",
            "--project-dir", str(tmp_path),
        ])
        mod.main()

        output = json.loads(capsys.readouterr().out)
        assert output["team_size"] == "pod"


# --- Integration: output schema ---

class TestOutputSchema:
    REQUIRED_FIELDS = [
        "sprint_id", "since", "until", "project_dir", "team_size",
        "computed_at", "dora", "ai_dlc", "activity", "economics", "alerts",
    ]

    def test_output_schema_fields(self, monkeypatch, tmp_path, capsys):
        import aggregate_sprint as mod
        import sys

        monkeypatch.setattr(mod, "SPRINTS_FILE", str(tmp_path / "sprints.jsonl"))
        monkeypatch.setattr(mod, "METRICS_DIR", str(tmp_path))
        monkeypatch.setattr(mod, "run_gh_command", lambda *a, **kw: None)
        monkeypatch.setattr(sys, "argv", [
            "aggregate-sprint.py", "--dry-run", "--project-dir", str(tmp_path),
        ])
        mod.main()

        output = json.loads(capsys.readouterr().out)
        for field in self.REQUIRED_FIELDS:
            assert field in output, f"Missing field: {field}"
