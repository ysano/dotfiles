import json
from pathlib import Path
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


class SetupTests(unittest.TestCase):
    def test_merge_preserves_existing_hooks_and_is_idempotent(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "hooks.json"
            existing = {"description": "user config", "hooks": {"Stop": [{"hooks": [{"type": "command", "command": "echo existing"}]}]}}
            target.write_text(json.dumps(existing))
            target.chmod(0o640)
            command = ["python3", str(HERE / "setup_codex_hooks.py"), "--output", str(target)]
            subprocess.run(command, check=True, capture_output=True)
            first = target.read_text()
            subprocess.run(command, check=True, capture_output=True)
            self.assertEqual(target.read_text(), first)
            result = json.loads(first)
            self.assertEqual(result["description"], "user config")
            self.assertEqual(result["hooks"]["Stop"][0], existing["hooks"]["Stop"][0])
            self.assertEqual(len(result["hooks"]["Stop"]), 2)
            self.assertIn("Interrupt", result["hooks"])
            self.assertEqual(len(list(Path(tmp).glob("*.backup.*"))), 1)
            self.assertEqual(target.stat().st_mode & 0o777, 0o640)
            for name in ["Interrupt", "SessionEnd"]:
                self.assertLessEqual(result["hooks"][name][-1]["hooks"][0]["timeout"], 3)

    def test_invalid_input_is_not_overwritten(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "hooks.json"
            target.write_text("invalid json")
            result = subprocess.run(["python3", str(HERE / "setup_codex_hooks.py"), "--output", str(target)], capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(target.read_text(), "invalid json")


if __name__ == "__main__":
    unittest.main()
