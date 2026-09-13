"""Prefix+w helper のorigin/session引き渡しを検証する。"""
import os
from pathlib import Path
import stat
import subprocess
import tempfile
import unittest


HERE = Path(__file__).resolve().parent


class PopupHelperTests(unittest.TestCase):
    def run_helper(self, path):
        with tempfile.TemporaryDirectory(prefix="dashboard-popup-test-") as directory:
            root = Path(directory)
            log = root / "tmux.log"
            fake_tmux = root / "tmux"
            fake_tmux.write_text(
                "#!/bin/sh\n"
                "command=$1\n"
                "printf '%s' \"$1\" >> \"$TMUX_POPUP_LOG\"\n"
                "shift\n"
                "for value in \"$@\"; do printf '\\t%s' \"$value\" >> \"$TMUX_POPUP_LOG\"; done\n"
                "printf '\\n' >> \"$TMUX_POPUP_LOG\"\n"
                "if [ \"$command\" = display-message ]; then printf '/tmp/repo with spaces\\n'; fi\n"
            )
            fake_tmux.chmod(fake_tmux.stat().st_mode | stat.S_IXUSR)
            env = dict(os.environ,
                       PATH=path(root),
                       TMUX_POPUP_LOG=str(log))
            subprocess.run(
                [str(HERE / "dashboard_popup.sh"), "$1", "%2", "/dev/ttys000"],
                env=env, check=True, capture_output=True, text=True,
            )
            return [line.split("\t") for line in log.read_text().splitlines()]

    def test_helper_targets_explicit_origin_and_keeps_session_id_literal(self):
        calls = self.run_helper(lambda root: str(root) + os.pathsep + os.environ.get("PATH", ""))
        message, popup = calls
        self.assertEqual(message[:4], ["display-message", "-p", "-t", "%2"])
        self.assertEqual(popup[:5], ["display-popup", "-c", "/dev/ttys000", "-t", "%2"])
        self.assertIn("-d", popup)
        self.assertIn("/tmp/repo with spaces", popup)
        command = popup[-1]
        self.assertIn("--session '$1'", command)
        self.assertIn("--pane '%2'", command)

    def test_helper_passes_origin_to_legacy_fallback_without_python(self):
        calls = self.run_helper(lambda root: str(root))
        message, popup = calls
        self.assertEqual(message[:4], ["display-message", "-p", "-t", "%2"])
        self.assertEqual(popup[:7], ["display-popup", "-c", "/dev/ttys000", "-t", "%2",
                                     "-e", "TMUX_WORKTREE_ORIGIN=%2"])
        self.assertEqual(popup[-1], "~/.tmux/claude/worktree_launch.sh popup")


if __name__ == "__main__":
    unittest.main()
