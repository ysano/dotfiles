"""Tests for utility functions: parse_iso, classify_level_*, get_dora_thresholds, run_gh_command, TZ."""
import subprocess
from datetime import datetime, timezone, timedelta
import pytest
from aggregate_sprint import (
    parse_iso,
    classify_level_higher_better,
    classify_level_lower_better,
    get_dora_thresholds,
    run_gh_command,
    resolve_local_tz,
    parse_date_arg,
    VALID_TEAM_SIZES,
    DORA_THRESHOLDS_BY_SCALE,
)


# --- parse_iso ---

class TestParseIso:
    def test_z_suffix(self):
        dt = parse_iso("2026-02-10T12:00:00Z")
        assert dt is not None
        assert dt.year == 2026
        assert dt.month == 2
        assert dt.day == 10
        assert dt.hour == 12

    def test_offset(self):
        dt = parse_iso("2026-02-10T21:00:00+09:00")
        assert dt is not None
        assert dt.hour == 21

    def test_none(self):
        assert parse_iso(None) is None

    def test_empty(self):
        assert parse_iso("") is None

    def test_invalid(self):
        assert parse_iso("not-a-date") is None


# --- classify_level_higher_better ---

class TestClassifyHigherBetter:
    @pytest.fixture
    def thresholds(self):
        return {"ELITE": 2.0, "HIGH": 1.0, "MEDIUM": 0.5}

    def test_elite(self, thresholds):
        assert classify_level_higher_better(3.0, thresholds) == "ELITE"

    def test_high(self, thresholds):
        assert classify_level_higher_better(1.5, thresholds) == "HIGH"

    def test_medium(self, thresholds):
        assert classify_level_higher_better(0.7, thresholds) == "MEDIUM"

    def test_low(self, thresholds):
        assert classify_level_higher_better(0.3, thresholds) == "LOW"

    def test_none_value(self, thresholds):
        assert classify_level_higher_better(None, thresholds) is None


# --- classify_level_lower_better ---

class TestClassifyLowerBetter:
    @pytest.fixture
    def thresholds(self):
        return {"ELITE": 4, "HIGH": 8, "MEDIUM": 24}

    def test_elite(self, thresholds):
        assert classify_level_lower_better(2, thresholds) == "ELITE"

    def test_high(self, thresholds):
        assert classify_level_lower_better(6, thresholds) == "HIGH"

    def test_medium(self, thresholds):
        assert classify_level_lower_better(12, thresholds) == "MEDIUM"

    def test_low(self, thresholds):
        assert classify_level_lower_better(30, thresholds) == "LOW"

    def test_none_value(self, thresholds):
        assert classify_level_lower_better(None, thresholds) is None


# --- get_dora_thresholds ---

class TestGetDoraThresholds:
    @pytest.mark.parametrize("size", ["solo", "pod", "squad"])
    def test_valid(self, size):
        t = get_dora_thresholds(size)
        assert "vdf" in t
        assert "svlt" in t
        assert "rework" in t
        assert "ttc" in t

    def test_invalid(self):
        with pytest.raises(ValueError, match="Invalid team_size"):
            get_dora_thresholds("enterprise")

    def test_valid_team_sizes_tuple(self):
        assert VALID_TEAM_SIZES == ("solo", "pod", "squad")

    def test_all_scales_in_table(self):
        for size in VALID_TEAM_SIZES:
            assert size in DORA_THRESHOLDS_BY_SCALE


# --- run_gh_command ---

class TestRunGhCommand:
    def test_success(self, monkeypatch):
        def mock_run(*args, **kwargs):
            return subprocess.CompletedProcess(
                args=args, returncode=0, stdout='{"number": 1}', stderr=""
            )
        monkeypatch.setattr(subprocess, "run", mock_run)
        result = run_gh_command(["pr", "list"], "/tmp")
        assert result == {"number": 1}

    def test_nonzero_returncode(self, monkeypatch):
        def mock_run(*args, **kwargs):
            return subprocess.CompletedProcess(
                args=args, returncode=1, stdout="", stderr="error"
            )
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert run_gh_command(["pr", "list"], "/tmp") is None

    def test_empty_stdout(self, monkeypatch):
        def mock_run(*args, **kwargs):
            return subprocess.CompletedProcess(
                args=args, returncode=0, stdout="   ", stderr=""
            )
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert run_gh_command(["pr", "list"], "/tmp") is None

    def test_invalid_json(self, monkeypatch):
        def mock_run(*args, **kwargs):
            return subprocess.CompletedProcess(
                args=args, returncode=0, stdout="not json", stderr=""
            )
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert run_gh_command(["pr", "list"], "/tmp") is None

    def test_file_not_found(self, monkeypatch):
        def mock_run(*args, **kwargs):
            raise FileNotFoundError("gh not found")
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert run_gh_command(["pr", "list"], "/tmp") is None

    def test_timeout(self, monkeypatch):
        def mock_run(*args, **kwargs):
            raise subprocess.TimeoutExpired(cmd="gh", timeout=30)
        monkeypatch.setattr(subprocess, "run", mock_run)
        assert run_gh_command(["pr", "list"], "/tmp") is None


# --- resolve_local_tz ---

class TestResolveLocalTz:
    def test_none_returns_system_tz(self):
        tz = resolve_local_tz(None)
        assert tz is not None
        # Should be a valid timezone object
        now = datetime.now(tz)
        assert now.tzinfo is not None

    def test_explicit_tz_name(self):
        tz = resolve_local_tz("Asia/Tokyo")
        assert tz is not None
        dt = datetime(2026, 2, 10, 0, 0, 0, tzinfo=tz)
        # JST is UTC+9
        utc_dt = dt.astimezone(timezone.utc)
        assert utc_dt.hour == 15  # 00:00 JST = 15:00 UTC previous day
        assert utc_dt.day == 9

    def test_utc(self):
        tz = resolve_local_tz("UTC")
        assert tz is not None


# --- parse_date_arg ---

class TestParseDateArg:
    def test_start_of_day_utc(self):
        utc = timezone.utc
        dt = parse_date_arg("2026-02-10", utc, end_of_day=False)
        assert dt.hour == 0
        assert dt.minute == 0
        assert dt.tzinfo == utc

    def test_end_of_day_utc(self):
        utc = timezone.utc
        dt = parse_date_arg("2026-02-10", utc, end_of_day=True)
        assert dt.hour == 23
        assert dt.minute == 59
        assert dt.second == 59

    def test_jst_start_of_day_converted_to_utc(self):
        jst = timezone(timedelta(hours=9))
        dt = parse_date_arg("2026-02-10", jst, end_of_day=False)
        # 2026-02-10 00:00 JST = 2026-02-09 15:00 UTC
        assert dt.tzinfo == timezone.utc
        assert dt.day == 9
        assert dt.hour == 15

    def test_jst_end_of_day_converted_to_utc(self):
        jst = timezone(timedelta(hours=9))
        dt = parse_date_arg("2026-02-10", jst, end_of_day=True)
        # 2026-02-10 23:59:59 JST = 2026-02-10 14:59:59 UTC
        assert dt.tzinfo == timezone.utc
        assert dt.day == 10
        assert dt.hour == 14
        assert dt.minute == 59

    def test_backward_compat_utc(self):
        """When TZ is UTC, behavior matches original (UTC midnight)."""
        utc = timezone.utc
        dt = parse_date_arg("2026-02-10", utc, end_of_day=False)
        assert dt == datetime(2026, 2, 10, 0, 0, 0, tzinfo=utc)
