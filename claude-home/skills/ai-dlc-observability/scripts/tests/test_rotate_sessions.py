"""Tests for rotate-sessions.py."""
import importlib.util
import json
import os
import sys
from datetime import datetime, timezone, timedelta

import pytest

# Import rotate-sessions.py via importlib (hyphenated filename)
_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_SCRIPT_DIR = os.path.dirname(_TEST_DIR)
_SCRIPT_PATH = os.path.join(_SCRIPT_DIR, "rotate-sessions.py")

spec = importlib.util.spec_from_file_location("rotate_sessions", _SCRIPT_PATH)
rotate_sessions = importlib.util.module_from_spec(spec)
sys.modules["rotate_sessions"] = rotate_sessions
spec.loader.exec_module(rotate_sessions)


class TestParseIso:
    def test_z_suffix(self):
        result = rotate_sessions.parse_iso("2026-02-10T12:00:00Z")
        assert result is not None
        assert result.year == 2026

    def test_none(self):
        assert rotate_sessions.parse_iso(None) is None

    def test_empty(self):
        assert rotate_sessions.parse_iso("") is None

    def test_invalid(self):
        assert rotate_sessions.parse_iso("not-a-date") is None


class TestLoadAndPartition:
    def test_partitions_by_cutoff(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        old_entry = {"start_time": "2026-01-01T10:00:00Z", "turns": 5}
        new_entry = {"start_time": "2026-02-10T10:00:00Z", "turns": 10}
        sessions_file.write_text(
            json.dumps(old_entry) + "\n" + json.dumps(new_entry) + "\n"
        )
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))

        cutoff = datetime(2026, 2, 1, tzinfo=timezone.utc)
        keep, archive = rotate_sessions.load_and_partition(cutoff)
        assert len(archive) == 1
        assert len(keep) == 1
        assert archive[0]["turns"] == 5

    def test_empty_file(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        sessions_file.write_text("")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))

        cutoff = datetime(2026, 2, 1, tzinfo=timezone.utc)
        keep, archive = rotate_sessions.load_and_partition(cutoff)
        assert keep == []
        assert archive == []

    def test_missing_file(self, tmp_path, monkeypatch):
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(tmp_path / "missing.jsonl"))

        cutoff = datetime(2026, 2, 1, tzinfo=timezone.utc)
        keep, archive = rotate_sessions.load_and_partition(cutoff)
        assert keep == []
        assert archive == []

    def test_malformed_lines_kept(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        sessions_file.write_text("not json\n")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))

        cutoff = datetime(2026, 2, 1, tzinfo=timezone.utc)
        keep, archive = rotate_sessions.load_and_partition(cutoff)
        assert len(keep) == 1  # malformed lines preserved in keep
        assert archive == []

    def test_no_start_time_kept(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        entry = {"turns": 5}  # no start_time
        sessions_file.write_text(json.dumps(entry) + "\n")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))

        cutoff = datetime(2026, 2, 1, tzinfo=timezone.utc)
        keep, archive = rotate_sessions.load_and_partition(cutoff)
        assert len(keep) == 1
        assert archive == []


class TestWriteArchive:
    def test_writes_monthly_files(self, tmp_path, monkeypatch):
        monkeypatch.setattr(rotate_sessions, "METRICS_DIR", str(tmp_path))

        entries = [
            {"start_time": "2026-01-15T10:00:00Z", "turns": 5},
            {"start_time": "2026-01-20T10:00:00Z", "turns": 8},
            {"start_time": "2025-12-10T10:00:00Z", "turns": 3},
        ]
        rotate_sessions.write_archive(entries)

        jan_file = tmp_path / "sessions-archive-2026-01.jsonl"
        dec_file = tmp_path / "sessions-archive-2025-12.jsonl"
        assert jan_file.exists()
        assert dec_file.exists()

        jan_lines = jan_file.read_text().strip().split("\n")
        assert len(jan_lines) == 2

        dec_lines = dec_file.read_text().strip().split("\n")
        assert len(dec_lines) == 1


class TestWriteSessions:
    def test_atomic_replacement(self, tmp_path, monkeypatch):
        sessions_file = tmp_path / "sessions.jsonl"
        sessions_file.write_text("old content\n")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))

        keep_lines = ['{"start_time":"2026-02-10T10:00:00Z","turns":10}']
        rotate_sessions.write_sessions(keep_lines)

        content = sessions_file.read_text()
        assert "old content" not in content
        assert "turns" in content
        assert not (tmp_path / "sessions.jsonl.tmp").exists()


class TestMain:
    def test_nothing_to_archive(self, tmp_path, monkeypatch, capsys):
        sessions_file = tmp_path / "sessions.jsonl"
        new_entry = {"start_time": datetime.now(timezone.utc).isoformat(), "turns": 10}
        sessions_file.write_text(json.dumps(new_entry) + "\n")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))
        monkeypatch.setattr(rotate_sessions, "METRICS_DIR", str(tmp_path))
        monkeypatch.setattr(sys, "argv", ["rotate-sessions.py"])

        rotate_sessions.main()
        output = capsys.readouterr().out
        assert "Nothing to archive" in output

    def test_dry_run(self, tmp_path, monkeypatch, capsys):
        sessions_file = tmp_path / "sessions.jsonl"
        old_entry = {"start_time": "2025-01-01T10:00:00Z", "turns": 5}
        new_entry = {"start_time": datetime.now(timezone.utc).isoformat(), "turns": 10}
        sessions_file.write_text(
            json.dumps(old_entry) + "\n" + json.dumps(new_entry) + "\n"
        )
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))
        monkeypatch.setattr(rotate_sessions, "METRICS_DIR", str(tmp_path))
        monkeypatch.setattr(sys, "argv", ["rotate-sessions.py", "--dry-run"])

        rotate_sessions.main()
        output = capsys.readouterr().out
        assert "[dry-run]" in output
        # Verify original file unchanged
        lines = sessions_file.read_text().strip().split("\n")
        assert len(lines) == 2

    def test_actual_rotation(self, tmp_path, monkeypatch, capsys):
        sessions_file = tmp_path / "sessions.jsonl"
        old_entry = {"start_time": "2025-01-01T10:00:00Z", "turns": 5}
        new_entry = {"start_time": datetime.now(timezone.utc).isoformat(), "turns": 10}
        sessions_file.write_text(
            json.dumps(old_entry) + "\n" + json.dumps(new_entry) + "\n"
        )
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))
        monkeypatch.setattr(rotate_sessions, "METRICS_DIR", str(tmp_path))
        monkeypatch.setattr(sys, "argv", ["rotate-sessions.py"])

        rotate_sessions.main()
        output = capsys.readouterr().out
        assert "Archived 1" in output
        assert "kept 1" in output

        # Verify sessions.jsonl now has only the new entry
        remaining = sessions_file.read_text().strip().split("\n")
        assert len(remaining) == 1

        # Verify archive file created
        archive_file = tmp_path / "sessions-archive-2025-01.jsonl"
        assert archive_file.exists()

    def test_custom_days(self, tmp_path, monkeypatch, capsys):
        sessions_file = tmp_path / "sessions.jsonl"
        # Entry from 45 days ago
        ts = (datetime.now(timezone.utc) - timedelta(days=45)).isoformat()
        entry = {"start_time": ts, "turns": 5}
        sessions_file.write_text(json.dumps(entry) + "\n")
        monkeypatch.setattr(rotate_sessions, "SESSIONS_FILE", str(sessions_file))
        monkeypatch.setattr(rotate_sessions, "METRICS_DIR", str(tmp_path))

        # With --days 60, 45-day-old entry should NOT be archived
        monkeypatch.setattr(sys, "argv", ["rotate-sessions.py", "--days", "60"])
        rotate_sessions.main()
        output = capsys.readouterr().out
        assert "Nothing to archive" in output

        # With --days 30, 45-day-old entry SHOULD be archived
        monkeypatch.setattr(sys, "argv", ["rotate-sessions.py", "--days", "30"])
        rotate_sessions.main()
        output = capsys.readouterr().out
        assert "Archived 1" in output
