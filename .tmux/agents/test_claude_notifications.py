"""Exercise notification routes with executable stubs: no audio or network."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


class ClaudeNotificationTests(unittest.TestCase):
    def run_route(self, route, enabled):
        with tempfile.TemporaryDirectory(prefix='claude-notification-test-') as directory:
            root = Path(directory)
            claude = root / 'claude'
            claude.mkdir()
            (claude / 'hooks').mkdir()
            for name in ('functions.sh', 'polling_monitor.sh', 'dialog_detector.sh', 'error_detector.sh'):
                shutil.copy(HERE.parent / 'claude' / name, claude / name)
            shutil.copy(HERE.parent / 'claude/hooks/notify.sh', claude / 'hooks/notify.sh')
            sound = claude / 'sound_utils.sh'
            sound.write_text('''#!/bin/bash
speak_text() { echo tts >> "$NOTIFICATIONS"; }
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then echo sound >> "$NOTIFICATIONS"; fi
''')
            sound.chmod(0o755)
            (claude / 'ollama_utils.sh').write_text('summarize_with_ollama() { echo summary; }\n')
            env = dict(os.environ, PROBE=str(claude), ENABLED=enabled,
                       NOTIFICATIONS=str(root / 'notifications'), EVIDENCE=str(root / 'evidence'))
            setup = '''
trap 'wait' EXIT
source "$PROBE/polling_monitor.sh"
tmux() {
    case "$*" in
        *'@claude_voice_enabled') printf '%s' "$ENABLED" ;;
        *'@claude_voice_sound_enabled'|*'@claude_voice_summary_enabled') echo true ;;
        *'@claude_status') echo Idle ;;
        *'@claude_state'|*'@claude_dialog_active') : ;;
        capture-pane*) printf '☐ Choice\\nPick one\\nEnter to select\\n' ;;
        display-message*) echo '⠋ Claude Code' ;;
        *) : ;;
    esac
}
detect_claude_panes() { echo '%1'; }
update_claude_evidence() { printf '%s:%s\\n' "$2" "$3" >> "$EVIDENCE"; echo changed; }
check_anthropic_status() { echo network >> "$NOTIFICATIONS"; echo ok; }
'''
            actions = {
                'hook': 'source "$PROBE/hooks/notify.sh" %1 Permission waiting test',
                'dialog': 'detect_dialogs',
                'error': 'notify_error %1 api',
                'title': 'correct_status_from_title',
            }
            result = subprocess.run(['bash', '-c', setup + actions[route]], env=env,
                                    text=True, capture_output=True, timeout=5)
            self.assertEqual(result.returncode, 0, result.stderr)
            notifications = (root / 'notifications').read_text().splitlines() if (root / 'notifications').exists() else []
            evidence = (root / 'evidence').read_text() if (root / 'evidence').exists() else ''
            return notifications, evidence

    def test_global_false_suppresses_every_notification_route(self):
        for route in ('hook', 'dialog', 'error', 'title'):
            with self.subTest(route=route):
                notifications, evidence = self.run_route(route, 'false')
                self.assertEqual(notifications, [], route)
                if route == 'dialog':
                    self.assertIn('dialog:Enter to select', evidence)
                if route == 'title':
                    self.assertIn('title:Busy', evidence)

    def test_unset_global_switch_preserves_notification_defaults(self):
        for route in ('hook', 'dialog', 'error', 'title'):
            with self.subTest(route=route):
                notifications, _ = self.run_route(route, '')
                self.assertIn('sound', notifications, route)
                if route in {'hook', 'dialog', 'error'}:
                    self.assertIn('tts', notifications, route)
