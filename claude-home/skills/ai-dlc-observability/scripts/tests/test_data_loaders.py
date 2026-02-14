"""Tests for L2 data loaders: load_sessions, aggregate_sessions, load_churn_data, load_spec_scores."""
import hashlib
import json
import os
import pytest
from helpers import make_session
from aggregate_sprint import (
    load_sessions,
    aggregate_sessions,
    load_churn_data,
    load_spec_scores,
    log,
    parse_iso,
)


# --- log() ---

class TestLog:
    def test_verbose_true(self, capsys):
        log("test message", verbose=True)
        assert "test message" in capsys.readouterr().err

    def test_verbose_false(self, capsys):
        log("test message", verbose=False)
        assert capsys.readouterr().err == ""


# --- load_sessions ---

class TestLoadSessions:
    def test_loads_matching_entries(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        s1 = make_session(start_time="2026-02-11T10:00:00Z")
        s2 = make_session(start_time="2026-02-12T10:00:00Z")
        sessions_file.write_text(
            json.dumps(s1) + "\n" + json.dumps(s2) + "\n"
        )
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(sessions_file))

        since = parse_iso("2026-02-11T00:00:00Z")
        until = parse_iso("2026-02-12T23:59:59Z")
        result = load_sessions(since, until, "/home/user/project")
        assert len(result) == 2

    def test_filters_by_project(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        s1 = make_session(project_dir="/home/user/project-a")
        s2 = make_session(project_dir="/home/user/project-b")
        sessions_file.write_text(
            json.dumps(s1) + "\n" + json.dumps(s2) + "\n"
        )
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(sessions_file))

        since = parse_iso("2026-02-01T00:00:00Z")
        until = parse_iso("2026-02-28T23:59:59Z")
        result = load_sessions(since, until, "/home/user/project-a")
        assert len(result) == 1
        assert result[0]["project_dir"] == "/home/user/project-a"

    def test_filters_by_date_range(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        s_before = make_session(start_time="2026-02-01T10:00:00Z")
        s_in = make_session(start_time="2026-02-11T10:00:00Z")
        s_after = make_session(start_time="2026-02-25T10:00:00Z")
        sessions_file.write_text(
            json.dumps(s_before) + "\n"
            + json.dumps(s_in) + "\n"
            + json.dumps(s_after) + "\n"
        )
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(sessions_file))

        since = parse_iso("2026-02-10T00:00:00Z")
        until = parse_iso("2026-02-14T23:59:59Z")
        result = load_sessions(since, until, "/home/user/project")
        assert len(result) == 1

    def test_missing_file(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(tmp_path / "missing.jsonl"))
        result = load_sessions(None, None, "/home/user/project")
        assert result == []

    def test_malformed_json_skipped(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        s1 = make_session()
        sessions_file.write_text(
            "not valid json\n" + json.dumps(s1) + "\n"
        )
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(sessions_file))

        result = load_sessions(None, None, "/home/user/project")
        assert len(result) == 1

    def test_empty_lines_skipped(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        s1 = make_session()
        sessions_file.write_text(
            "\n" + json.dumps(s1) + "\n\n"
        )
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SESSIONS_FILE", str(sessions_file))

        result = load_sessions(None, None, "/home/user/project")
        assert len(result) == 1


# --- aggregate_sessions ---

class TestAggregateSessions:
    def test_basic_aggregation(self):
        sessions = [
            make_session(total_turns=20, user_turns=10, assistant_turns=10,
                         tool_counts={"Edit": 5, "Read": 10},
                         modified_files=["a.py", "b.py"]),
            make_session(total_turns=10, user_turns=5, assistant_turns=5,
                         tool_counts={"Edit": 3, "Read": 7, "Bash": 2},
                         modified_files=["b.py", "c.py"]),
        ]
        result = aggregate_sessions(sessions)
        assert result["session_count"] == 2
        assert result["total_turns"] == 30
        assert result["user_turns"] == 15
        assert result["assistant_turns"] == 15
        assert result["file_count"] == 3  # a.py, b.py, c.py (deduplicated)
        assert "Edit" in result["top_tools"]
        assert result["top_tools"]["Edit"] == 8

    def test_empty_sessions(self):
        result = aggregate_sessions([])
        assert result["session_count"] == 0
        assert result["total_turns"] == 0
        assert result["file_count"] == 0

    def test_session_durations(self):
        sessions = [
            make_session(start_time="2026-02-10T09:00:00Z",
                         end_time="2026-02-10T10:00:00Z"),
        ]
        result = aggregate_sessions(sessions)
        assert len(result["session_durations"]) == 1
        assert result["session_durations"][0] == 3600.0  # 1 hour

    def test_no_duration_when_missing_times(self):
        sessions = [make_session(start_time=None, end_time=None)]
        result = aggregate_sessions(sessions)
        assert result["session_durations"] == []


# --- load_churn_data ---

class TestLoadChurnData:
    def test_loads_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "CHURN_CACHE_DIR", str(tmp_path))

        project_dir = "/home/user/project"
        parts = project_dir.rstrip("/").split("/")
        project_name = "_".join(parts[-2:])
        cache_file = tmp_path / f"{project_name}_churn.json"
        churn = {"src/main.py": {"count": 5}}
        cache_file.write_text(json.dumps(churn))

        result = load_churn_data(project_dir)
        assert result is not None
        assert result["src/main.py"]["count"] == 5

    def test_missing_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "CHURN_CACHE_DIR", str(tmp_path))
        result = load_churn_data("/home/user/project")
        assert result is None

    def test_corrupt_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "CHURN_CACHE_DIR", str(tmp_path))

        project_dir = "/home/user/project"
        parts = project_dir.rstrip("/").split("/")
        project_name = "_".join(parts[-2:])
        cache_file = tmp_path / f"{project_name}_churn.json"
        cache_file.write_text("not json")

        result = load_churn_data(project_dir)
        assert result is None


# --- load_spec_scores ---

class TestLoadSpecScores:
    def test_loads_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SPEC_CACHE_DIR", str(tmp_path))

        project_dir = "/home/user/project"
        project_hash = hashlib.md5(project_dir.encode()).hexdigest()[:12]
        cache_file = tmp_path / f"{project_hash}_scores.json"
        scores = {"spec.md": {"score": 4}}
        cache_file.write_text(json.dumps(scores))

        result = load_spec_scores(project_dir)
        assert result is not None
        assert result["spec.md"]["score"] == 4

    def test_missing_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SPEC_CACHE_DIR", str(tmp_path))
        result = load_spec_scores("/home/user/project")
        assert result is None

    def test_corrupt_cache(self, tmp_path, monkeypatch):
        import aggregate_sprint as mod
        monkeypatch.setattr(mod, "SPEC_CACHE_DIR", str(tmp_path))

        project_dir = "/home/user/project"
        project_hash = hashlib.md5(project_dir.encode()).hexdigest()[:12]
        cache_file = tmp_path / f"{project_hash}_scores.json"
        cache_file.write_text("{invalid")

        result = load_spec_scores(project_dir)
        assert result is None
