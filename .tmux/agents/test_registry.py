import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

import worktrees

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


class SnapshotGitTests(unittest.TestCase):
    """1 回の snapshot 内で git 問い合わせを registry と worktrees が共有する。"""

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="registry git test ")
        base = Path(self.temp.name)
        self.root = base / "repo"
        subprocess.run(["git", "init", "-q", "-b", "main", str(self.root)], check=True)
        subprocess.run(["git", "-C", str(self.root), "-c", "user.name=Test",
                        "-c", "user.email=t@example.invalid", "commit", "--allow-empty",
                        "-qm", "initial"], check=True, capture_output=True)
        self.sub = self.root / "src" / "deep"
        self.sub.mkdir(parents=True)
        self.linked = base / "linked"
        subprocess.run(["git", "-C", str(self.root), "worktree", "add", "-qb", "topic",
                        str(self.linked)], check=True, capture_output=True)
        self.plain = base / "not a repo"
        self.plain.mkdir()

    def tearDown(self):
        self.temp.cleanup()

    def snapshot(self, cwds, calls):
        panes = [{"pane_id": "%" + str(i), "session_id": "$1", "cwd": cwd, "command": "zsh",
                  "width": 100, "height": 30, "active": i == 0, "title": "",
                  "window_index": "1", "window_name": "w", "pane_index": str(i),
                  "claude_enabled": True, "codex_enabled": True,
                  "claude_detected": False, "codex_detected": False,
                  "claude_state": {}, "codex_state": {}, "claude_status": "", "codex_status": ""}
                 for i, cwd in enumerate(cwds)]
        real_run = subprocess.run

        def counting_run(argv, *args, **kwargs):
            if argv and argv[0] == "git":
                calls.append(tuple(str(x) for x in argv))
            return real_run(argv, *args, **kwargs)

        with mock.patch.object(registry, "read_panes", return_value=panes), \
                mock.patch.object(registry, "tmux", return_value=""), \
                mock.patch.object(worktrees, "_states_for_panes", return_value=[]), \
                mock.patch.object(subprocess, "run", counting_run):
            return registry.snapshot("$1", "%0")

    def test_worktree_list_runs_once_per_repository_and_rev_parse_once_per_cwd(self):
        calls = []
        cwds = [str(self.root), str(self.sub), str(self.linked)]

        value = self.snapshot(cwds, calls)

        listing = [c for c in calls if "worktree" in c and "list" in c]
        rev_parse = [c for c in calls if "rev-parse" in c]
        self.assertEqual(len(listing), 1, calls)
        self.assertLessEqual(len(rev_parse), len(cwds), calls)
        self.assertEqual(len(value["repos"]), 1)
        self.assertEqual({Path(row["path"]).name for row in value["worktrees"]},
                         {"repo", "linked"})

    def test_repository_result_is_unchanged_by_sharing(self):
        expected = {"id": str((self.root / ".git").resolve()),
                    "name": "repo", "path": str(self.root.resolve())}
        self.assertEqual(registry.repository(str(self.sub)), expected)
        self.assertEqual(registry.repository(str(self.linked)), expected)
        inventory = worktrees.GitInventory()
        self.assertEqual(registry.repository(str(self.sub), inventory), expected)
        self.assertEqual(registry.repository(str(self.linked), inventory), expected)
        self.assertIsNone(registry.repository(str(self.plain), inventory))

    def test_repository_keeps_undecodable_path_bytes_like_before(self):
        porcelain = "worktree /srv/caf\udce9\0HEAD abc\0branch refs/heads/main\0\0"

        def fake_git(root, *args, **kwargs):
            return "/srv/caf\udce9/.git\n" if args[0] == "rev-parse" else porcelain

        with mock.patch.object(worktrees, "_git", side_effect=fake_git):
            value = registry.repository("/srv/caf\udce9")
        self.assertIsNotNone(value)
        self.assertEqual(value["name"], "caf\udce9")

    def test_one_failing_directory_does_not_hide_other_repositories(self):
        calls = []
        missing = str(Path(self.temp.name) / "gone")

        value = self.snapshot([str(self.plain), missing, str(self.root)], calls)

        self.assertEqual([repo["name"] for repo in value["repos"]], ["repo"])
        self.assertEqual(len(value["worktrees"]), 2)

    def test_git_timeout_in_one_repository_is_skipped_not_fatal(self):
        real_git = worktrees._git

        def flaky(root, *args, **kwargs):
            if str(root).endswith("linked"):
                raise subprocess.TimeoutExpired(["git"], 2)
            return real_git(root, *args, **kwargs)

        with mock.patch.object(worktrees, "_git", flaky):
            value = self.snapshot([str(self.linked), str(self.root)], [])
        self.assertEqual([repo["name"] for repo in value["repos"]], ["repo"])


if __name__ == "__main__":
    unittest.main()
