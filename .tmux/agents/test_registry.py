import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("registry", HERE / "registry.py")
registry = importlib.util.module_from_spec(spec)
spec.loader.exec_module(registry)


class SnapshotTests(unittest.TestCase):
    def test_disabled_provider_is_hidden_even_before_poll_clears_metadata(self):
        pane = {"pane_id": "%1", "session_id": "$1", "cwd": "/repo", "command": "codex",
                "codex_status": "Busy", "codex_enabled": False}
        value = registry.build_snapshot("$1", "%1", [pane], {}, [], False)
        self.assertEqual(value["agents"], [])
        self.assertEqual(value["summary"]["busy"], 0)

    def test_snapshot_detects_unregistered_node_wrapper_before_first_poll(self):
        columns = ["%1", "$1", "/repo", "node", "100", "30", "1", "title", "", "", "", "", "7"]
        def tmux(*args):
            return "\t".join(columns) if args[0] == "list-panes" else ""
        processes = {7: (1, "node"), 8: (7, "/bin/codex")}
        with mock.patch.object(registry, "tmux", tmux), mock.patch.object(registry, "process_table", return_value=processes):
            panes = registry.read_panes("$1")
        value = registry.build_snapshot("$1", "%1", panes, {}, [], False)
        self.assertEqual([a["provider"] for a in value["agents"]], ["codex"])
        self.assertEqual(value["agents"][0]["status"], "Unknown")

    def test_snapshot_maps_panes_to_window_and_pane_index(self):
        columns = ["%1", "$1", "/repo", "zsh", "100", "30", "1", "title",
                   "", "", "", "", "7", "2", "editor", "3"]
        def tmux(*args):
            return "\t".join(columns) if args[0] == "list-panes" else ""
        with mock.patch.object(registry, "tmux", tmux), \
                mock.patch.object(registry, "process_table", return_value={}):
            panes = registry.read_panes("$1")
        value = registry.build_snapshot("$1", "%1", panes, {}, [], False)
        self.assertEqual(value["pane_locations"], {"%1": {"window": "2:editor", "pane": "3"}})

    def test_pane_line_shifted_by_tab_in_a_field_is_skipped_not_fatal(self):
        good = ["%1", "$1", "/repo", "zsh", "100", "30", "1", "title",
                "", "", "", "", "7", "2", "editor", "3"]
        shifted = ["%2", "$1", "/re", "po", "zsh", "100", "30", "1", "title",
                   "", "", "", "", "8", "2", "editor", "4"]
        def tmux(*args):
            if args[0] != "list-panes":
                return ""
            return "\n".join("\t".join(line) for line in (shifted, good))
        with mock.patch.object(registry, "tmux", tmux), \
                mock.patch.object(registry, "process_table", return_value={}):
            panes = registry.read_panes("$1")
        self.assertEqual([pane["pane_id"] for pane in panes], ["%1"])

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

    def test_independent_team_conversations_are_not_linked_by_repository_or_actor_id(self):
        panes = []
        for index in (1, 2):
            panes.append({"pane_id": "%" + str(index), "session_id": "$1", "cwd": "/repo",
                          "command": "claude", "claude_state": {
                              "session_id": "conversation-" + str(index), "cwd": "/repo",
                              "actors": {"root": {"active": True}, "same-child-id": {"active": True}}}})
        repo = {"id": "/repo/.git", "path": "/repo", "name": "repo"}
        value = registry.build_snapshot("$1", "%1", panes, {"/repo": repo}, [], False)
        first, child_one, second, child_two = value["agents"]
        self.assertEqual(first["parent_id"], repo["id"])
        self.assertEqual(second["parent_id"], repo["id"])
        self.assertEqual(child_one["parent_id"], first["id"])
        self.assertEqual(child_two["parent_id"], second["id"])
        self.assertNotEqual(child_one["id"], child_two["id"])


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
