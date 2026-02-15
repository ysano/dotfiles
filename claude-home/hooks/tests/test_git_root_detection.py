"""Tests for file_path → git root detection in hooks (#17 fix)."""
import json
import os
import subprocess
import sys
from unittest.mock import patch, MagicMock

import pytest

# Add hooks directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from churn_counter import detect_git_root, get_project_name
from check_spec_existence import find_project_root


# --- detect_git_root (churn-counter.py) ---


class TestDetectGitRoot:
    def test_detects_root_from_file_in_repo(self, tmp_path):
        """A file inside a git repo should resolve to the repo root."""
        # Create a git repo in tmp_path
        subprocess.run(["git", "init"], cwd=str(tmp_path), capture_output=True)
        sub = tmp_path / "src"
        sub.mkdir()
        f = sub / "main.py"
        f.write_text("print('hello')")

        root = detect_git_root(str(f))
        assert root == str(tmp_path)

    def test_returns_none_for_non_repo(self, tmp_path):
        """A file outside any git repo should return None."""
        f = tmp_path / "orphan.py"
        f.write_text("x = 1")

        root = detect_git_root(str(f))
        assert root is None

    def test_returns_none_for_nonexistent_file(self):
        """A nonexistent file path should return None."""
        root = detect_git_root("/nonexistent/path/file.py")
        assert root is None

    def test_handles_subprocess_timeout(self, monkeypatch):
        """Should return None on subprocess timeout."""

        def mock_run(*args, **kwargs):
            raise subprocess.TimeoutExpired(cmd="git", timeout=3)

        monkeypatch.setattr(subprocess, "run", mock_run)
        root = detect_git_root("/some/file.py")
        assert root is None

    def test_nested_repo(self, tmp_path):
        """A file in a nested git repo should resolve to the inner repo."""
        # Outer repo
        subprocess.run(["git", "init"], cwd=str(tmp_path), capture_output=True)

        # Inner repo (subproject)
        inner = tmp_path / "subproject"
        inner.mkdir()
        subprocess.run(["git", "init"], cwd=str(inner), capture_output=True)

        f = inner / "app.py"
        f.write_text("y = 2")

        root = detect_git_root(str(f))
        assert root == str(inner)


# --- get_project_name (churn-counter.py) ---


class TestGetProjectName:
    def test_from_explicit_dir(self):
        name = get_project_name("/home/user/calculator-experiment")
        assert name == "user_calculator-experiment"

    def test_from_none_uses_env(self, monkeypatch):
        monkeypatch.setenv("CLAUDE_PROJECT_DIR", "/home/user/dotfiles")
        name = get_project_name(None)
        assert name == "user_dotfiles"

    def test_from_none_no_env(self, monkeypatch):
        monkeypatch.delenv("CLAUDE_PROJECT_DIR", raising=False)
        # Falls back to cwd
        name = get_project_name(None)
        assert isinstance(name, str)
        assert len(name) > 0

    def test_single_component(self):
        name = get_project_name("/root")
        # split("/") on "/root" gives ["", "root"], so "_".join(["", "root"]) = "_root"
        assert name == "_root"

    def test_trailing_slash(self):
        name = get_project_name("/home/user/project/")
        assert name == "user_project"


# --- find_project_root (check-spec-existence.py) ---


class TestFindProjectRoot:
    def test_file_path_git_root_takes_priority(self, tmp_path, monkeypatch):
        """file_path's git root should override CLAUDE_PROJECT_DIR."""
        # Create a git repo
        subprocess.run(["git", "init"], cwd=str(tmp_path), capture_output=True)
        f = tmp_path / "main.py"
        f.write_text("code")

        # Set CLAUDE_PROJECT_DIR to a different directory
        monkeypatch.setenv("CLAUDE_PROJECT_DIR", "/home/user/dotfiles")

        root = find_project_root(str(f))
        assert root == str(tmp_path)

    def test_falls_back_to_claude_project_dir(self, tmp_path, monkeypatch):
        """Without file_path, should use CLAUDE_PROJECT_DIR."""
        monkeypatch.setenv("CLAUDE_PROJECT_DIR", str(tmp_path))
        root = find_project_root(None)
        assert root == str(tmp_path)

    def test_falls_back_to_claude_project_dir_when_file_not_in_repo(self, tmp_path, monkeypatch):
        """When file_path is not in a git repo, should use CLAUDE_PROJECT_DIR."""
        f = tmp_path / "orphan.py"
        f.write_text("code")
        monkeypatch.setenv("CLAUDE_PROJECT_DIR", str(tmp_path))

        root = find_project_root(str(f))
        assert root == str(tmp_path)


# --- Integration: churn-counter main with file_path from different repo ---


class TestChurnCounterIntegration:
    def test_cache_key_uses_file_git_root(self, tmp_path, monkeypatch):
        """Churn cache should be keyed by the file's git root, not CLAUDE_PROJECT_DIR."""
        import io
        import churn_counter

        # Setup: create a git repo for "calculator-experiment"
        calc_dir = tmp_path / "calculator-experiment"
        calc_dir.mkdir()
        subprocess.run(["git", "init"], cwd=str(calc_dir), capture_output=True)
        src = calc_dir / "src"
        src.mkdir()
        f = src / "calc.py"
        f.write_text("def add(a, b): return a + b")

        # Set CLAUDE_PROJECT_DIR to dotfiles (simulating the issue)
        monkeypatch.setenv("CLAUDE_PROJECT_DIR", "/home/user/dotfiles")

        # Prepare hook input as JSON on stdin
        hook_input = {
            "tool_name": "Edit",
            "tool_input": {"file_path": str(f)},
        }

        cache_dir = tmp_path / "churn-cache"
        monkeypatch.setattr(churn_counter, "CACHE_DIR", str(cache_dir))

        # Disable should_skip (tmp_path contains "/test" which triggers skip)
        monkeypatch.setattr(churn_counter, "should_skip", lambda _: False)

        stdin_data = io.StringIO(json.dumps(hook_input))
        monkeypatch.setattr("sys.stdin", stdin_data)

        with pytest.raises(SystemExit) as exc_info:
            churn_counter.main()
        assert exc_info.value.code == 0

        # Verify cache file is keyed by calculator-experiment, not dotfiles
        cache_files = list(cache_dir.iterdir())
        assert len(cache_files) == 1
        cache_name = cache_files[0].name
        # Should contain calculator-experiment in the name
        assert "calculator-experiment" in cache_name
        assert "dotfiles" not in cache_name
