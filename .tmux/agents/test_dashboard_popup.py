"""Prefix+w helper のorigin/session引き渡しを検証する。"""
import os
from pathlib import Path
import stat
import subprocess
import tempfile
import unittest


HERE = Path(__file__).resolve().parent


class PopupHelperTests(unittest.TestCase):
    def test_helper_targets_explicit_origin_and_keeps_session_id_literal(self):
        with tempfile.TemporaryDirectory(prefix="dashboard-popup-test-") as directory:
            root = Path(directory)
            log = root / "tmux.log"
            fake_tmux = root / "tmux"
            fake_tmux.write_text(
                "#!/bin/sh\n"
                "printf '%s\\n' \"$@\" >> \"$TMUX_POPUP_LOG\"\n"
                "if [ \"$1\" = display-message ]; then printf '/tmp/repo with spaces\\n'; fi\n"
            )
            fake_tmux.chmod(fake_tmux.stat().st_mode | stat.S_IXUSR)
            env = dict(os.environ,
                       PATH=str(root) + os.pathsep + os.environ.get("PATH", ""),
                       TMUX_POPUP_LOG=str(log))
            subprocess.run(
                [str(HERE / "dashboard_popup.sh"), "$1", "%2"],
                env=env, check=True, capture_output=True, text=True,
            )
            values = log.read_text().splitlines()
            self.assertIn("-t", values)
            self.assertIn("%2", values)
            self.assertIn("-d", values)
            self.assertIn("/tmp/repo with spaces", values)
            command = values[-1]
            self.assertIn("--session '$1'", command)
            self.assertIn("--pane '%2'", command)


if __name__ == "__main__":
    unittest.main()
