import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


class SetupClaudeHooksTests(unittest.TestCase):
    def test_merge_removes_owned_duplicates_preserves_other_hooks_and_is_idempotent(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            settings = home / '.claude/settings.json'
            settings.parent.mkdir()
            own = str(home / '.tmux/claude/hooks/status-update.sh')
            other = {'type': 'command', 'command': 'echo user-custom-hook'}
            settings.write_text(json.dumps({'permissions': {'allow': ['Read']}, 'hooks': {
                'Stop': [{'hooks': [{'type': 'command', 'command': own, 'async': True}, other]},
                         {'hooks': [{'type': 'command', 'command': own, 'async': True}]}]}}))
            env = dict(os.environ, HOME=directory)
            script = HERE.parent / 'claude/hooks/setup-hooks.sh'
            subprocess.run(['bash', str(script)], env=env, check=True, capture_output=True)
            result = json.loads(settings.read_text())
            for event in ['PreToolUse', 'PostToolUse', 'PostToolUseFailure', 'SubagentStart', 'SubagentStop', 'PermissionRequest', 'Stop']:
                entries = [hook for entry in result['hooks'].get(event, []) for hook in entry['hooks'] if hook['command'] == own]
                self.assertEqual(len(entries), 1, event)
                self.assertFalse(entries[0].get('async', False))
            self.assertIn(other, [h for e in result['hooks']['Stop'] for h in e['hooks']])
            self.assertEqual(result['permissions']['allow'], ['Read'])
            first = settings.read_text()
            subprocess.run(['bash', str(script)], env=env, check=True, capture_output=True)
            self.assertEqual(settings.read_text(), first)
            self.assertEqual(len(list(settings.parent.glob('settings.json.backup.*'))), 1)
