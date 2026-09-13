"""Observed hook sequences and isolated tmux integration; no API calls."""
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

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
