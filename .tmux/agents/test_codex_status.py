"""Observed hook sequences and isolated tmux integration; no API calls."""
from contextlib import contextmanager
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
spec = importlib.util.spec_from_file_location("codex_status", HERE / "codex_status.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


def event(name, session="session-a", turn="turn-a", **extra):
    return dict(hook_event_name=name, session_id=session, turn_id=turn, **extra)


class StateTests(unittest.TestCase):
    def setUp(self):
        self.state = {}

    def send(self, name, **extra):
        self.state = module.reduce_event(self.state, event(name, **extra))
        return module.status(self.state)

    def test_actor_metadata_preserves_child_cwd(self):
        self.send("UserPromptSubmit", cwd="/repo")
        self.send("SubagentStart", agent_id="child", turn="child-turn", cwd="/child", agent_name="review")
        self.send("Stop", cwd="/repo")
        self.assertEqual(self.state.get("provider"), "codex")
        self.assertEqual(self.state.get("cwd"), "/repo")
        self.assertEqual(self.state["actors"]["child"].get("cwd"), "/child")
        self.assertEqual(self.state["actors"]["child"].get("name"), "review")

    def test_normal_turn_and_session_end(self):
        self.assertEqual(self.send("SessionStart"), "Idle")
        self.assertEqual(self.send("UserPromptSubmit"), "Busy")
        self.assertEqual(self.send("PostToolUse"), "Busy")
        self.assertEqual(self.send("Stop"), "Idle")
        self.assertEqual(self.send("SessionEnd"), "")

    def test_parent_stop_does_not_finish_child(self):
        self.send("UserPromptSubmit")
        self.send("SubagentStart", turn="child-turn", agent_id="child")
        self.assertEqual(self.send("Stop"), "Busy")
        self.assertEqual(self.send("SubagentStop", turn="child-turn", agent_id="child"), "Idle")

    def test_child_permission_does_not_get_cleared_by_parent_tool(self):
        self.send("UserPromptSubmit")
        self.send("SubagentStart", turn="child-turn", agent_id="child")
        self.assertEqual(self.send("PermissionRequest", turn="child-turn", agent_id="child"), "Permission")
        self.assertEqual(self.send("PostToolUse"), "Permission")
        self.assertEqual(self.send("PostToolUse", turn="child-turn", agent_id="child"), "Busy")

    def test_interrupt_retains_background_child(self):
        self.send("UserPromptSubmit")
        self.send("SubagentStart", turn="child-turn", agent_id="child")
        self.assertEqual(self.send("Interrupt"), "Busy")
        self.assertEqual(self.send("SubagentStop", turn="child-turn", agent_id="child"), "Idle")

    def test_late_events_do_not_resurrect_completed_turn(self):
        self.send("UserPromptSubmit")
        self.send("Stop")
        self.assertEqual(self.send("PostToolUse"), "Idle")
        self.send("UserPromptSubmit", turn="turn-b")
        self.assertEqual(self.send("Stop"), "Busy")

    def test_late_old_session_cannot_clear_new_session(self):
        self.send("UserPromptSubmit")
        self.send("SessionStart", session="session-b")
        self.send("UserPromptSubmit", session="session-b")
        self.assertEqual(self.send("SessionEnd"), "Busy")

    def test_compaction_keeps_busy(self):
        self.send("UserPromptSubmit")
        self.assertEqual(self.send("SessionStart", source="compact"), "Busy")

    def test_stop_hook_continuation_reactivates_same_turn(self):
        self.send("UserPromptSubmit")
        self.send("Stop")
        self.assertEqual(self.send("UserPromptSubmit"), "Busy")
        self.send("Stop")
        self.assertEqual(self.send("PreToolUse"), "Busy")

    def test_child_stop_hook_continuation_and_interrupt(self):
        self.send("UserPromptSubmit")
        self.send("SubagentStart", turn="child-turn", agent_id="child")
        self.send("Stop")
        self.send("SubagentStop", turn="child-turn", agent_id="child")
        self.assertEqual(self.send("PreToolUse", turn="child-turn", agent_id="child"), "Busy")
        self.send("SubagentStop", turn="child-turn", agent_id="child")
        self.send("UserPromptSubmit")
        self.send("Interrupt")
        self.assertEqual(self.send("PreToolUse"), "Idle")

    def test_unknown_or_invalid_events_do_not_register(self):
        self.assertEqual(module.reduce_event({}, {}), {})
        self.assertEqual(self.send("Unexpected"), "")

    def test_no_completion_sound_on_startup_or_interrupt(self):
        self.assertEqual(module.sound_for("", "Idle", "SessionStart"), "")
        self.assertEqual(module.sound_for("Busy", "Idle", "Interrupt"), "")
        self.assertEqual(module.sound_for("Busy", "Idle", "Stop"), "complete")
        self.assertEqual(module.sound_for("Busy", "Busy", "SubagentStop"), "")

    def test_npm_wrapper_detected_without_matching_unrelated_node(self):
        processes = {10: (1, "zsh"), 11: (10, "node"), 12: (11, "/opt/bin/codex"),
                     20: (1, "node"), 21: (20, "webpack"), 30: (1, "codex-helper")}
        self.assertTrue(module.is_codex_process(10, processes))
        self.assertTrue(module.is_codex_process(11, processes))
        self.assertFalse(module.is_codex_process(20, processes))
        self.assertFalse(module.is_codex_process(30, processes))

    def test_agent_in_another_pane_subtree_is_not_attributed(self):
        processes = {10: (1, "zsh"), 20: (1, "zsh"), 21: (20, "/opt/bin/claude.exe")}
        self.assertFalse(module.is_agent_process(10, processes, "claude"))
        self.assertTrue(module.is_agent_process(20, processes, "claude"))
        self.assertTrue(module.is_agent_process(21, processes, "claude"))
        self.assertFalse(module.is_agent_process(20, processes, "codex"))

    def test_cyclic_parent_links_terminate(self):
        processes = {10: (12, "zsh"), 11: (10, "node"), 12: (11, "codex"),
                     20: (21, "zsh"), 21: (20, "node")}
        self.assertTrue(module.is_agent_process(10, processes, "codex"))
        self.assertFalse(module.is_agent_process(20, processes, "codex"))
        self.assertFalse(module.is_agent_process(99, processes, "codex"))

    def test_ancestor_index_is_built_once_per_process_table(self):
        processes = {10: (1, "zsh"), 11: (10, "node"), 12: (11, "/opt/bin/codex")}
        with mock.patch.object(module, "_agent_ancestors",
                               wraps=module._agent_ancestors) as build:
            for _ in range(16):
                self.assertTrue(module.is_agent_process(10, processes, "codex"))
                self.assertFalse(module.is_agent_process(10, processes, "claude"))
        self.assertEqual(build.call_count, 2)  # provider ごとに 1 回

    def test_new_process_table_does_not_reuse_a_stale_index(self):
        before = {10: (1, "zsh"), 11: (10, "codex")}
        after = {10: (1, "zsh")}
        self.assertTrue(module.is_agent_process(10, before, "codex"))
        self.assertFalse(module.is_agent_process(10, after, "codex"))
        after[11] = (10, "codex")  # 同じ表への追記も古い索引で判定しない
        self.assertTrue(module.is_agent_process(10, after, "codex"))

    def test_results_match_the_per_call_scan_on_random_process_tables(self):
        def scan(pane_pid, processes, provider):  # 索引化前の実装
            for pid, (_, executable) in processes.items():
                if Path(executable).name not in {provider, provider + ".exe"}:
                    continue
                seen = set()
                while pid in processes and pid not in seen:
                    if pid == pane_pid:
                        return True
                    seen.add(pid)
                    pid = processes[pid][0]
            return False

        import random
        rng = random.Random(61)
        names = ["zsh", "node", "codex", "/opt/bin/codex", "claude.exe",
                 "/usr/local/bin/claude", "codex-helper", "bin/", ""]
        for _ in range(200):
            size = rng.randint(1, 12)
            # 親は表の外 (0, 99) や自分自身・循環も含む
            processes = {pid: (rng.choice([0, 99] + list(range(1, size + 1))),
                               rng.choice(names)) for pid in range(1, size + 1)}
            for pane_pid in range(0, size + 2):
                for provider in ("claude", "codex"):
                    self.assertEqual(
                        module.is_agent_process(pane_pid, processes, provider),
                        scan(pane_pid, processes, provider),
                        (pane_pid, provider, processes))

    def test_same_length_in_place_change_is_not_answered_from_a_stale_index(self):
        processes = {10: (1, "zsh"), 11: (10, "node")}
        self.assertFalse(module.is_agent_process(10, processes, "codex"))
        processes[11] = (10, "codex")
        self.assertTrue(module.is_agent_process(10, processes, "codex"))
        processes[11] = (1, "codex")
        self.assertFalse(module.is_agent_process(10, processes, "codex"))


@unittest.skipUnless(shutil.which("tmux"), "tmux required for isolated server tests")
class TmuxTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="codex-status-test-")
        self.socket = str(Path(self.temp.name) / "socket")
        self.tmux("-f", "/dev/null", "new-session", "-d", "-s", "probe", "-x", "120", "-y", "40", "sleep 300")
        self.env = dict(os.environ, TMUX=self.socket + ",0,0", TMUX_PANE="%0")

    def tearDown(self):
        self.tmux("kill-server")
        self.temp.cleanup()

    def tmux(self, *args):
        return subprocess.check_output(["tmux", "-S", self.socket, *args], text=True).strip()

    def hook(self, name, **extra):
        result = subprocess.run(["python3", str(HERE / "codex_status.py")], input=json.dumps(event(name, **extra)), text=True, capture_output=True, env=self.env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), {})

    def test_hook_updates_pane_and_window_and_session_end_clears(self):
        self.hook("SessionStart")
        self.hook("UserPromptSubmit")
        self.assertEqual(self.tmux("show-option", "-pqv", "-t", "%0", "@codex_status"), "Busy")
        self.assertEqual(self.tmux("show-option", "-wqv", "-t", "%0", "@codex_icon"), "⚡")
        self.hook("Stop")
        self.assertEqual(self.tmux("show-option", "-wqv", "-t", "%0", "@codex_icon"), "✅")
        self.hook("SessionEnd")
        self.assertEqual(self.tmux("show-option", "-wqv", "-t", "%0", "@codex_icon"), "")

    def test_unchanged_tool_event_is_observed_outside_state_lock(self):
        self.hook("UserPromptSubmit")
        before = self.tmux("show-option", "-pqv", "-t", "%0", "@codex_state")
        locked, observed = [], []
        original_lock = module.server_lock

        @contextmanager
        def tracked_lock():
            with original_lock():
                locked.append(True)
                try:
                    yield
                finally:
                    locked.pop()

        def observe(payload, pane):
            self.assertFalse(locked)
            observed.append((payload["hook_event_name"], pane))

        with mock.patch.dict(os.environ, self.env), mock.patch.object(module, "server_lock", tracked_lock), \
                mock.patch.object(module, "observe_hook", observe):
            module.process_hook(event("PostToolUse", tool_name="exec_command"))
        self.assertEqual(self.tmux("show-option", "-pqv", "-t", "%0", "@codex_state"), before)
        self.assertEqual(observed, [("PostToolUse", "%0")])

    def test_codex_notification_forwards_origin_pane(self):
        self.tmux("set-option", "-g", "@claude_voice_sound_enabled", "true")
        calls = []
        original_popen = subprocess.Popen

        def popen(argv, *args, **kwargs):
            if len(argv) > 1 and str(argv[1]).endswith("/sound_utils.sh"):
                calls.append(argv)
                return None  # Stub notification backend: never play real audio.
            return original_popen(argv, *args, **kwargs)

        with mock.patch.dict(os.environ, self.env), mock.patch.object(module.subprocess, "Popen", popen):
            module.process_hook(event("UserPromptSubmit"))
        self.assertEqual(calls[0][-3:], ["play", "start", "%0"])

    def test_legacy_hook_integration_fixture_contract(self):
        self.tmux("set-option", "-g", "@claude_voice_sound_enabled", "false")
        self.tmux("set-option", "-g", "@claude_voice_summary_enabled", "false")
        result = subprocess.run(["bash", "-c", 'source "$1"; test_hooks_status_update; [[ "${#FAILED_TESTS[@]}" -eq 0 ]]',
                                 "bash", str(HERE.parent / "claude/integration_test.sh")],
                                env=self.env, text=True, capture_output=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("UserPromptSubmit → Busy: 成功", result.stdout)
        self.assertIn("SessionEnd → クリア: 成功", result.stdout)

    def test_poll_clears_exited_process(self):
        self.hook("UserPromptSubmit")
        subprocess.run(["python3", str(HERE / "codex_status.py"), "poll"], env=self.env, check=True)
        self.assertEqual(self.tmux("show-option", "-pqv", "-t", "%0", "@codex_status"), "")

    def test_moving_pane_does_not_leave_old_window_icon(self):
        self.tmux("new-window", "-d", "-n", "other", "sleep 300")
        self.tmux("split-window", "-d", "-t", "%0", "sleep 300")
        self.hook("UserPromptSubmit")
        self.tmux("join-pane", "-s", "%0", "-t", "%1")
        self.hook("Stop")
        self.assertEqual(self.tmux("show-option", "-wqv", "-t", "%2", "@codex_icon"), "")
        self.assertEqual(self.tmux("show-option", "-wqv", "-t", "%0", "@codex_icon"), "✅")

    def test_disabled_hook_does_not_write(self):
        self.tmux("set-option", "-g", "@codex_enabled", "false")
        self.hook("UserPromptSubmit")
        self.assertEqual(self.tmux("show-option", "-pqv", "-t", "%0", "@codex_status"), "")


if __name__ == "__main__":
    unittest.main()
