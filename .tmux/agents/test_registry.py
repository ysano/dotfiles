import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("registry", HERE / "registry.py")
registry = importlib.util.module_from_spec(spec)
spec.loader.exec_module(registry)


class SnapshotTests(unittest.TestCase):
    def test_unregistered_fallback_status_is_counted(self):
        panes = [{"pane_id": "%1", "session_id": "$1", "cwd": "/repo", "command": "claude",
                  "claude_status": "Busy"},
                 {"pane_id": "%2", "session_id": "$1", "cwd": "/repo", "command": "claude",
                  "claude_status": "Permission"}]
        result = registry.build_snapshot("$1", "%1", panes, {}, [], False)
        self.assertEqual(result["summary"], {"busy": 1, "waiting": 1, "errors": 0})

    def test_parent_child_counts_and_worktree_location(self):
        pane = {"pane_id": "%3", "session_id": "$1", "cwd": "/repo", "command": "claude.exe",
                "claude_status": "Permission", "claude_state": {
                    "session_id": "conversation", "cwd": "/repo", "provider": "claude",
                    "actors": {"root": {"active": False, "pending": [], "name": "親"},
                               "child": {"active": True, "pending": ["Bash"], "name": "検証", "cwd": "/wt"}}}}
        repos = {"/repo": {"id": "/repo/.git", "path": "/repo", "name": "repo"}}
        value = registry.build_snapshot("$1", "%3", [pane], repos, [], False)
        self.assertEqual(len(value["agents"]), 2)
        root, child = value["agents"]
        self.assertEqual(child["parent_id"], root["id"])
        self.assertEqual(child["cwd"], "/wt")
        self.assertEqual(child["pane_id"], "")
        self.assertEqual(child["parent_pane"], "%3")
        self.assertEqual(value["summary"]["waiting"], 1)
        self.assertEqual(value["summary"]["busy"], 0)

    def test_unregistered_and_shell_worktrees_are_visible(self):
        panes = [{"pane_id": "%1", "session_id": "$1", "cwd": "/repo", "command": "node", "codex_status": "Unknown"},
                 {"pane_id": "%2", "session_id": "$1", "cwd": "/repo", "command": "zsh"}]
        repos = {"/repo": {"id": "/repo/.git", "path": "/repo", "name": "repo"}}
        rows = [{"id": "/repo", "path": "/repo", "repo": "/repo/.git", "panes": ["%2"]}]
        value = registry.build_snapshot("$1", "%1", panes, repos, rows, False)
        self.assertEqual(len(value["agents"]), 1)
        self.assertEqual(value["agents"][0]["status"], "Unknown")
        self.assertEqual(value["worktrees"], rows)

    def test_snapshot_filters_other_tmux_sessions(self):
        panes = [{"pane_id": "%1", "session_id": "$2", "cwd": "/repo", "command": "claude", "claude_status": "Busy"}]
        result = registry.build_snapshot("$1", "%0", panes, {}, [], False)
        self.assertEqual(result["agents"], [])


@unittest.skipUnless(shutil.which("tmux"), "tmux required")
class RepositoryTests(unittest.TestCase):
    def test_linked_worktree_and_main_share_repository_id(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder) / "repo with spaces"
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            subprocess.run(["git", "-C", str(root), "-c", "user.name=Test", "-c", "user.email=t@example.invalid", "commit", "--allow-empty", "-qm", "initial"], check=True)
            linked = Path(folder) / "linked worktree"
            subprocess.run(["git", "-C", str(root), "worktree", "add", "-qb", "test", str(linked)], check=True)
            self.assertEqual(registry.repository(str(root))["id"], registry.repository(str(linked))["id"])


if __name__ == "__main__":
    unittest.main()
