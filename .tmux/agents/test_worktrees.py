"""Real git/tmux coverage for the worktree inventory and auto-pane policy."""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock


HERE = Path(__file__).resolve().parent
MODULE_PATH = HERE / "worktrees.py"


def load_module():
    if not MODULE_PATH.exists():
        return None
    spec = importlib.util.spec_from_file_location("worktrees", MODULE_PATH)
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


module = load_module()


@unittest.skipUnless(shutil.which("git"), "git required")
class GitWorktreeTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="worktrees git test ")
        self.base = Path(self.temp.name)
        self.repo = self.base / "repo with space"
        self.repo.mkdir()
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "test@example.invalid")
        self.git("config", "user.name", "Test User")
        (self.repo / "tracked.txt").write_text("initial\n")
        self.git("add", "tracked.txt")
        self.git("commit", "-qm", "initial")

    def tearDown(self):
        self.temp.cleanup()

    def git(self, *args, cwd=None, check=True):
        return subprocess.run(
            ["git", *args], cwd=cwd or self.repo, check=check,
            text=True, capture_output=True,
        )

    def require_module(self):
        self.assertIsNotNone(module, "worktrees.py has not been implemented")

    def test_list_uses_canonical_paths_common_git_dir_and_pane_ids(self):
        self.require_module()
        linked = self.base / "linked tree with space"
        self.git("worktree", "add", "-qb", "topic", str(linked), "HEAD")
        nested = linked / "nested"
        nested.mkdir()
        common_value = Path(self.git("rev-parse", "--git-common-dir").stdout.strip())
        common = (self.repo / common_value).resolve() if not common_value.is_absolute() else common_value.resolve()
        panes = [{
            "pane_id": "%7", "session_id": "$3", "cwd": str(nested),
            "command": "zsh", "width": 120, "height": 40, "active": True,
        }]

        rows = module.list_worktrees([str(self.repo), str(nested)], panes)

        self.assertEqual([row["id"] for row in rows], [str(self.repo.resolve()), str(linked.resolve())])
        linked_row = rows[1]
        self.assertEqual(linked_row["repo"], str(common))
        self.assertEqual(linked_row["branch"], "topic")
        self.assertEqual(linked_row["panes"], ["%7"])
        self.assertEqual(linked_row["status"], "open")

    def test_temporary_agent_worktree_is_listed_with_exclusion_reason(self):
        self.require_module()
        linked = self.base / ".claude" / "worktrees" / "child task"
        linked.parent.mkdir(parents=True)
        self.git("worktree", "add", "-qb", "agent-child", str(linked), "HEAD")

        row = next(row for row in module.list_worktrees([str(self.repo)], []) if row["path"] == str(linked.resolve()))

        self.assertTrue(row["temporary"])
        self.assertEqual(row["status"], "temporary")
        self.assertIn("temporary", row["auto_reason"].lower())

    def test_create_uses_external_path_and_remove_refuses_dirty_tree(self):
        self.require_module()
        created = Path(module.create_worktree(str(self.repo), "safe-name", "HEAD", "$none", "%none"))
        expected = self.base / "worktrees" / f"{self.repo.name}-safe-name"
        self.assertEqual(created, expected.resolve())
        self.assertTrue((created / ".git").exists())
        (created / "untracked.txt").write_text("dirty\n")

        with self.assertRaisesRegex(RuntimeError, "dirty|uncommitted"):
            module.remove_worktree(str(created))

        self.assertTrue(created.exists())

    def test_create_rejects_option_like_and_unknown_base_before_creation(self):
        self.require_module()
        for name, base in (("option-base", "--force"), ("unknown-base", "missing-ref")):
            with self.subTest(base=base):
                with self.assertRaisesRegex(ValueError, "base"):
                    module.create_worktree(str(self.repo), name, base, "$none", "%none")
                expected = self.base / "worktrees" / f"{self.repo.name}-{name}"
                self.assertFalse(expected.exists())


@unittest.skipUnless(shutil.which("git") and shutil.which("tmux"), "git and tmux required")
class TmuxAutoPaneTests(unittest.TestCase):
    def setUp(self):
        self.require_module()
        self.temp = tempfile.TemporaryDirectory(prefix="worktrees tmux test ")
        self.base = Path(self.temp.name)
        self.repo = self.base / "repo with space"
        self.repo.mkdir()
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "test@example.invalid")
        self.git("config", "user.name", "Test User")
        (self.repo / "tracked.txt").write_text("initial\n")
        self.git("add", "tracked.txt")
        self.git("commit", "-qm", "initial")
        self.socket = str(self.base / "tmux socket")
        self.tmux("-f", "/dev/null", "new-session", "-d", "-s", "probe", "-x", "180", "-y", "45", "-c", str(self.repo), "sleep 300")
        self.session = self.tmux("display-message", "-p", "-t", "probe", "#{session_id}")
        self.origin = self.tmux("display-message", "-p", "-t", "probe", "#{pane_id}")
        self.env = dict(os.environ, TMUX=self.socket + ",0,0", TMUX_PANE=self.origin)
        self.env_patch = mock.patch.dict(os.environ, self.env, clear=False)
        self.env_patch.start()

    def tearDown(self):
        self.env_patch.stop()
        self.tmux("kill-server", check=False)
        self.temp.cleanup()

    def require_module(self):
        self.assertIsNotNone(module, "worktrees.py has not been implemented")

    def git(self, *args, cwd=None, check=True):
        return subprocess.run(
            ["git", *args], cwd=cwd or self.repo, check=check,
            text=True, capture_output=True,
        )

    def tmux(self, *args, check=True):
        result = subprocess.run(
            ["tmux", "-S", self.socket, *args], check=check,
            text=True, capture_output=True,
        )
        return result.stdout.strip()

    def panes(self):
        output = self.tmux(
            "list-panes", "-s", "-t", self.session, "-F",
            "#{pane_id}\t#{session_id}\t#{pane_current_path}\t#{pane_current_command}\t#{pane_width}\t#{pane_height}\t#{pane_active}",
        )
        rows = []
        for line in output.splitlines():
            pane_id, session_id, cwd, command, width, height, active = line.split("\t")
            rows.append({
                "pane_id": pane_id, "session_id": session_id, "cwd": cwd,
                "command": command, "width": int(width), "height": int(height),
                "active": active == "1",
            })
        return rows

    def add_manual(self, name, path=None):
        linked = path or (self.base / f"manual {name}")
        self.git("worktree", "add", "-qb", name, str(linked), "HEAD")
        return Path(linked).resolve()

    def test_auto_defaults_off_and_enabling_baselines_existing_worktrees(self):
        old = self.add_manual("old")

        module.poll_session(self.session, self.panes())
        self.assertEqual(len(self.panes()), 1)
        module.set_auto(self.session, True, self.panes())
        module.poll_session(self.session, self.panes())

        self.assertEqual(len(self.panes()), 1)
        rows = module.list_worktrees([str(self.repo)], self.panes())
        old_row = next(row for row in rows if row["path"] == str(old))
        self.assertIn("baseline", old_row["auto_reason"].lower())

    def test_created_worktree_auto_opens_one_detached_pane_and_keeps_focus(self):
        module.set_auto(self.session, True, self.panes())
        first = module.create_worktree(str(self.repo), "first", "HEAD", self.session, self.origin)
        module.poll_session(self.session, self.panes())

        after_first = self.panes()
        self.assertEqual(len(after_first), 2)
        self.assertEqual(next(row for row in after_first if row["active"])["pane_id"], self.origin)
        self.assertTrue(any(Path(row["cwd"]).resolve() == Path(first).resolve() for row in after_first))

        second = module.create_worktree(str(self.repo), "second", "HEAD", self.session, self.origin)
        module.poll_session(self.session, self.panes())

        self.assertEqual(len(self.panes()), 2)
        second_row = next(row for row in module.list_worktrees([str(self.repo)], self.panes()) if row["path"] == second)
        self.assertIn("already", second_row["auto_reason"].lower())

    def test_manually_closed_auto_pane_is_not_recreated(self):
        module.set_auto(self.session, True, self.panes())
        created = module.create_worktree(str(self.repo), "close-me", "HEAD", self.session, self.origin)
        module.poll_session(self.session, self.panes())
        auto_pane = next(row["pane_id"] for row in self.panes() if Path(row["cwd"]).resolve() == Path(created).resolve())
        self.tmux("kill-pane", "-t", auto_pane)

        module.poll_session(self.session, self.panes())
        module.poll_session(self.session, self.panes())

        self.assertEqual(len(self.panes()), 1)
        row = next(row for row in module.list_worktrees([str(self.repo)], self.panes()) if row["path"] == created)
        self.assertIn("closed", row["auto_reason"].lower())

    def test_unknown_and_temporary_worktrees_never_auto_open(self):
        module.set_auto(self.session, True, self.panes())
        unknown = self.add_manual("unknown")
        temporary = self.add_manual("temporary", self.base / ".codex" / "worktrees" / "temporary")

        module.poll_session(self.session, self.panes())

        self.assertEqual(len(self.panes()), 1)
        rows = {row["path"]: row for row in module.list_worktrees([str(self.repo)], self.panes())}
        self.assertIn("unknown", rows[str(unknown)]["auto_reason"].lower())
        self.assertTrue(rows[str(temporary)]["temporary"])

    def test_successful_hook_candidate_auto_opens_but_failed_hook_does_not(self):
        module.set_auto(self.session, True, self.panes())
        good = self.add_manual("hook-good")
        command = "git worktree add -b hook-good " + __import__("shlex").quote(str(good)) + " HEAD"
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": command}, "tool_response": {"exit_code": 0},
        }, self.origin)
        module.poll_session(self.session, self.panes())
        self.assertTrue(any(Path(row["cwd"]).resolve() == good for row in self.panes()))

        existing_auto = next(row["pane_id"] for row in self.panes() if row["pane_id"] != self.origin)
        self.tmux("kill-pane", "-t", existing_auto)
        module.poll_session(self.session, self.panes())
        bad = self.add_manual("hook-bad")
        bad_command = "git worktree add -b hook-bad " + __import__("shlex").quote(str(bad)) + " HEAD"
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": bad_command}, "tool_response": {"exit_code": 1},
        }, self.origin)
        module.poll_session(self.session, self.panes())
        self.assertFalse(any(Path(row["cwd"]).resolve() == bad for row in self.panes()))

    def test_observe_hook_only_queues_and_does_not_scan_git(self):
        module.set_auto(self.session, True, self.panes())
        candidate = self.base / "not created yet"
        command = "git worktree add " + __import__("shlex").quote(str(candidate))

        with mock.patch.object(module, "_git", side_effect=AssertionError("git scan in hook")):
            module.observe_hook({
                "hook_event_name": "PostToolUse", "tool_name": "Bash",
                "tool_input": {"command": command}, "tool_response": {"exit_code": 0},
            }, self.origin)

        raw = self.tmux("show-options", "-qv", "-t", self.session, module.STATE_OPTION)
        self.assertIn(str(candidate.resolve()), json.loads(raw)["candidates"])

    def test_hook_requires_root_event_explicit_success_and_actual_git_command(self):
        module.set_auto(self.session, True, self.panes())
        ignored = [
            ({"hook_event_name": "PostToolUse", "tool_input": {"command": "git worktree add /tmp/unknown"}}, "/tmp/unknown"),
            ({"hook_event_name": "PostToolUse", "agent_id": "child", "tool_input": {"command": "git worktree add /tmp/child"}, "tool_response": {"exit_code": 0}}, "/tmp/child"),
            ({"hook_event_name": "PostToolUse", "tool_input": {"command": "echo git worktree add /tmp/echoed"}, "tool_response": {"exit_code": 0}}, "/tmp/echoed"),
            ({"hook_event_name": "PostToolUse", "tool_input": {"command": "git worktree add /tmp/masked || true"}, "tool_response": {"exit_code": 0}}, "/tmp/masked"),
            ({"hook_event_name": "PostToolUse", "tool_name": "Bash", "tool_input": {"command": "git worktree add /tmp/background & true"}, "tool_response": {"exit_code": 0}}, "/tmp/background"),
            ({"hook_event_name": "PostToolUse", "tool_name": "Bash", "tool_input": {"command": "git worktree add /tmp/newline\ntrue"}, "tool_response": {"exit_code": 0}}, "/tmp/newline"),
        ]
        for event, _ in ignored:
            module.observe_hook(event, self.origin)

        accepted = self.base / "codex cmd"
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "exec_command",
            "tool_input": {"cmd": "git worktree add " + __import__("shlex").quote(str(accepted))},
            "tool_response": {"exit_code": 0},
        }, self.origin)

        raw = self.tmux("show-options", "-qv", "-t", self.session, module.STATE_OPTION)
        candidates = json.loads(raw)["candidates"]
        self.assertEqual(list(candidates), [str(accepted.resolve())])

    def test_hook_accepts_official_claude_bash_success_shape_only_when_not_interrupted(self):
        module.set_auto(self.session, True, self.panes())
        accepted = self.base / "claude success"
        interrupted = self.base / "claude interrupted"
        response = {"stdout": "", "stderr": "", "interrupted": False, "isImage": False}
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": "git worktree add " + __import__("shlex").quote(str(accepted))},
            "tool_response": response,
        }, self.origin)
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": "git worktree add " + __import__("shlex").quote(str(interrupted))},
            "tool_response": {**response, "interrupted": True},
        }, self.origin)

        raw = self.tmux("show-options", "-qv", "-t", self.session, module.STATE_OPTION)
        self.assertEqual(list(json.loads(raw)["candidates"]), [str(accepted.resolve())])

    def test_hook_resolves_relative_path_from_git_dash_c_directory(self):
        hook_pane = self.tmux(
            "new-window", "-d", "-t", self.session, "-c", str(self.base),
            "-P", "-F", "#{pane_id}", "sleep 300",
        )
        module.set_auto(self.session, True, self.panes())
        linked = self.base / "relative hook"
        self.git("worktree", "add", "-qb", "hook-relative", "../relative hook", "HEAD")
        command = __import__("shlex").join([
            "git", "-C", str(self.repo), "worktree", "add", "-b",
            "hook-relative", "../relative hook", "HEAD",
        ])

        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": command}, "tool_response": {"exit_code": 0},
        }, hook_pane)
        module.poll_session(self.session, self.panes())

        self.assertTrue(any(Path(row["cwd"]).resolve() == linked.resolve() for row in self.panes()))

    def test_hook_candidate_path_discovers_repo_not_shown_in_any_pane(self):
        hook_pane = self.tmux(
            "new-window", "-d", "-t", self.session, "-c", str(self.base),
            "-P", "-F", "#{pane_id}", "sleep 300",
        )
        self.tmux("kill-pane", "-t", self.origin)
        module.set_auto(self.session, True, self.panes())
        linked = self.base / "other repo hook"
        self.git("worktree", "add", "-qb", "hook-other", str(linked), "HEAD")
        command = __import__("shlex").join([
            "git", "-C", str(self.repo), "worktree", "add", "-b",
            "hook-other", str(linked), "HEAD",
        ])

        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "exec_command",
            "tool_input": {"cmd": command},
            "tool_response": {"exit_code": 0},
        }, hook_pane)
        module.poll_session(self.session, self.panes())

        self.assertTrue(any(Path(row["cwd"]).resolve() == linked.resolve() for row in self.panes()))

    def test_list_keeps_disabled_candidate_from_repo_absent_in_panes(self):
        hook_pane = self.tmux(
            "new-window", "-d", "-t", self.session, "-c", str(self.base),
            "-P", "-F", "#{pane_id}", "sleep 300",
        )
        self.tmux("kill-pane", "-t", self.origin)
        linked = self.base / "disabled other repo"
        self.git("worktree", "add", "-qb", "disabled-other", str(linked), "HEAD")
        module.observe_hook({
            "hook_event_name": "PostToolUse", "tool_name": "Bash",
            "tool_input": {"command": "git worktree add " + __import__("shlex").quote(str(linked))},
            "tool_response": {"stdout": "", "stderr": "", "interrupted": False, "isImage": False},
        }, hook_pane)

        rows = module.list_worktrees([str(self.base)], self.panes())

        by_path = {item["path"]: item for item in rows}
        self.assertIn(str(linked.resolve()), by_path)
        row = by_path[str(linked.resolve())]
        self.assertIn("disabled", row["auto_reason"].lower())

        module.set_auto(self.session, True, self.panes())
        module.poll_session(self.session, self.panes())
        enabled_rows = {item["path"]: item for item in module.list_worktrees([str(self.base)], self.panes())}
        self.assertIn(str(linked.resolve()), enabled_rows)
        self.assertIn("baseline", enabled_rows[str(linked.resolve())]["auto_reason"].lower())
        self.assertEqual(len(self.panes()), 1)

    def test_small_origin_records_reason_instead_of_splitting(self):
        self.tmux("resize-window", "-t", self.session, "-x", "80", "-y", "32")
        module.set_auto(self.session, True, self.panes())
        created = module.create_worktree(str(self.repo), "too-small", "HEAD", self.session, self.origin)

        module.poll_session(self.session, self.panes())

        self.assertEqual(len(self.panes()), 1)
        row = next(row for row in module.list_worktrees([str(self.repo)], self.panes()) if row["path"] == created)
        self.assertIn("80x16", row["auto_reason"])

    def test_auto_vertical_split_leaves_each_pane_at_least_80x16(self):
        self.tmux("resize-window", "-t", self.session, "-x", "80", "-y", "33")
        module.set_auto(self.session, True, self.panes())
        module.create_worktree(str(self.repo), "vertical", "HEAD", self.session, self.origin)

        module.poll_session(self.session, self.panes())

        panes = self.panes()
        self.assertEqual(len(panes), 2)
        self.assertTrue(all(row["width"] >= 80 and row["height"] >= 16 for row in panes))

    def test_auto_horizontal_fallback_leaves_each_pane_at_least_80x16(self):
        self.tmux("resize-window", "-t", self.session, "-x", "161", "-y", "16")
        module.set_auto(self.session, True, self.panes())
        module.create_worktree(str(self.repo), "horizontal", "HEAD", self.session, self.origin)

        module.poll_session(self.session, self.panes())

        panes = self.panes()
        self.assertEqual(len(panes), 2)
        self.assertTrue(all(row["width"] >= 80 and row["height"] >= 16 for row in panes))

    def test_open_reuses_existing_worktree_pane_in_another_window(self):
        linked = self.add_manual("already-open")
        existing = self.tmux("new-window", "-d", "-t", self.session, "-c", str(linked), "-P", "-F", "#{pane_id}", "sleep 300")

        opened = module.open_worktree(str(linked), self.session, self.origin)

        self.assertEqual(opened, existing)
        self.assertEqual(len(self.panes()), 2)

    def test_open_launches_provider_by_safe_resolved_executable(self):
        linked = self.add_manual("provider")
        fake_bin = self.base / "fake commands with space"
        fake_bin.mkdir()
        executable = fake_bin / "claude"
        executable.write_text("#!/bin/sh\nsleep 300\n")
        executable.chmod(0o755)
        # Keep the server unable to resolve `claude`; the launch command must
        # carry the safely quoted absolute executable resolved by the caller.
        self.tmux("set-environment", "-g", "PATH", "/usr/bin:/bin")
        self.tmux("set-option", "-g", "default-shell", "/bin/sh")
        caller_path = str(fake_bin) + os.pathsep + os.environ.get("PATH", "")

        with mock.patch.dict(os.environ, {"PATH": caller_path}):
            opened = module.open_worktree(str(linked), self.session, self.origin, "claude")

        pane = next((row for row in self.panes() if row["pane_id"] == opened), None)
        self.assertIsNotNone(pane)
        self.assertEqual(Path(pane["cwd"]).resolve(), linked)

    def test_open_reports_missing_provider_before_creating_pane(self):
        linked = self.add_manual("missing-provider")
        before = len(self.panes())

        tool_path = str(Path(shutil.which("tmux")).parent) + os.pathsep + "/usr/bin:/bin"
        with mock.patch.dict(os.environ, {"PATH": tool_path}):
            with self.assertRaisesRegex(RuntimeError, "claude.*not found"):
                module.open_worktree(str(linked), self.session, self.origin, "claude")

        self.assertEqual(len(self.panes()), before)

    def test_explicit_provider_launch_does_not_reuse_existing_shell_pane(self):
        linked = self.add_manual("provider-with-shell")
        existing = self.tmux(
            "new-window", "-d", "-t", self.session, "-c", str(linked),
            "-P", "-F", "#{pane_id}", "sleep 300",
        )
        fake_bin = self.base / "provider bin"
        fake_bin.mkdir()
        executable = fake_bin / "claude"
        executable.write_text("#!/bin/sh\nsleep 300\n")
        executable.chmod(0o755)
        caller_path = str(fake_bin) + os.pathsep + os.environ.get("PATH", "")

        with mock.patch.dict(os.environ, {"PATH": caller_path}):
            opened = module.open_worktree(str(linked), self.session, self.origin, "claude")

        self.assertNotEqual(opened, existing)
        self.assertEqual(len(self.panes()), 3)

    def test_remove_refuses_worktree_used_by_tmux_pane(self):
        linked = self.add_manual("in-use")
        self.tmux("new-window", "-d", "-t", self.session, "-c", str(linked), "sleep 300")

        with self.assertRaisesRegex(RuntimeError, "in use.*pane"):
            module.remove_worktree(str(linked))

        self.assertTrue(linked.exists())

    def test_remove_refuses_active_actor_cwd_even_when_pane_cwd_is_elsewhere(self):
        linked = self.add_manual("child-in-use")
        state = json.dumps({
            "session_id": "claude-session",
            "cwd": str(self.repo),
            "provider": "claude",
            "actors": {"child": {"active": True, "cwd": str(linked)}},
        })
        self.tmux("set-option", "-p", "-t", self.origin, "@claude_state", state)

        with self.assertRaisesRegex(RuntimeError, "in use.*pane"):
            module.remove_worktree(str(linked))

        self.assertTrue(linked.exists())


if __name__ == "__main__":
    unittest.main()
