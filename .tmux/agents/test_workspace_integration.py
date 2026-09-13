"""Provider hook -> real tmux -> registry -> dashboard contract regression."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

import dashboard
import registry

HERE = Path(__file__).resolve().parent


@unittest.skipUnless(shutil.which("tmux"), "tmux required")
class WorkspaceIntegrationTests(unittest.TestCase):
    def test_real_hooks_show_both_providers_children_and_other_windows(self):
        with tempfile.TemporaryDirectory(prefix="workspace-contract-") as directory:
            folder = Path(directory)
            socket = str(folder / "socket")
            def tmux(*args):
                return subprocess.check_output(["tmux", "-S", socket, *args], text=True).strip()
            subprocess.run(["git", "init", "-q", str(folder / "repo")], check=True)
            subprocess.run(["git", "-C", str(folder / "repo"), "-c", "user.name=Test",
                            "-c", "user.email=test@example.invalid", "commit", "--allow-empty",
                            "-qm", "initial"], check=True)
            try:
                pane = tmux("-f", "/dev/null", "new-session", "-d", "-s", "contract",
                            "-x", "160", "-y", "45", "-c", str(folder / "repo"),
                            "-P", "-F", "#{pane_id}", "sleep 300")
                session = tmux("display-message", "-p", "-t", pane, "#{session_id}")
                other = tmux("new-window", "-d", "-t", session, "-c", str(folder / "repo"),
                             "-P", "-F", "#{pane_id}", "sleep 300")
                tmux("set-option", "-g", "@claude_voice_sound_enabled", "false")
                tmux("set-option", "-g", "@claude_voice_summary_enabled", "false")
                env = dict(os.environ, TMUX=socket + ",0,0")
                for provider, target in (("claude", pane), ("codex", other)):
                    for name, extra in (("UserPromptSubmit", {}),
                                        ("SubagentStart", {"agent_id": "child", "agent_name": "検証担当"}),
                                        ("PermissionRequest", {"agent_id": "child", "tool_name": "Bash"}),
                                        ("Stop", {})):
                        event = dict(hook_event_name=name, session_id=provider + "-conversation",
                                     cwd=str(folder / "repo"), turn_id="turn", **extra)
                        subprocess.run(["python3", str(HERE / (provider + "_status.py")), "hook"],
                                       input=json.dumps(event), text=True, capture_output=True, check=True,
                                       env=dict(env, TMUX_PANE=target))
                with mock.patch.dict(os.environ, env):
                    snapshot = registry.snapshot(session, pane)
                    model = dashboard.DashboardModel(snapshot)
                    self.assertEqual(snapshot["summary"]["waiting"], 2)
                    self.assertEqual(snapshot["summary"]["busy"], 0)
                    self.assertEqual(len(snapshot["repos"]), 1)
                    roots = [row for row in model.rows if row.kind == "agent" and row.data["pane_id"]]
                    self.assertEqual(len(roots), 2)
                    self.assertEqual(model.enter_action().target, pane)
                    model.selected_id = next(row.id for row in roots if row.data["pane_id"] == other)
                    self.assertTrue(dashboard.focus_pane(model.enter_action().target, session))
                    self.assertEqual(tmux("display-message", "-p", "-t", session, "#{pane_id}"), other)
                    model.toggle_view()
                    self.assertTrue(any(row.kind == "worktree" for row in model.rows))
            finally:
                subprocess.run(["tmux", "-S", socket, "kill-server"], capture_output=True)


if __name__ == "__main__":
    unittest.main()
