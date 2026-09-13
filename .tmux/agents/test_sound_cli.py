"""Verify sound CLI target routing with fake audio backends, never real audio."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent


class SoundCliTests(unittest.TestCase):
    def test_speech_notification_fallback_preserves_pane(self):
        result = subprocess.run(['bash', '-c', '''
source "$1"
get_os_type() { echo Linux; }
get_powershell_path() { return 1; }
tmux() { :; }
play_notification_sound() { printf "%s %s" "$1" "${2:-}"; }
speak_text hello %9
''', 'bash', str(HERE.parent / 'claude/sound_utils.sh')], text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, 'complete %9')

    def test_all_notification_types_preserve_explicit_pane(self):
        with tempfile.TemporaryDirectory(prefix='sound-cli-test-') as directory:
            root = Path(directory)
            bindir = root / 'bin'; bindir.mkdir()
            sounds = root / 'Library/Sounds'; sounds.mkdir(parents=True)
            (sounds / 'Probe.aiff').write_text('stub audio')
            (root / 'core').mkdir()
            (root / 'core/logging_utils.sh').write_text('log_debug() { :; }; log_error() { :; }\n')
            (root / 'panning_engine.sh').write_text('apply_panning() { printf "%s %s\\n" "$3" "$2" >> "$AUDIO_LOG"; }\n')
            stubs = {
                'uname': 'echo Darwin',
                'tmux': '''case "$*" in
                    *'@claude_voice_panning_enabled') echo true ;;
                    *'@claude_voice_sound_'*) echo Probe ;;
                    *) echo 0.8 ;;
                esac''',
                'afplay': 'echo unpositioned >> "$AUDIO_LOG"',
                'bc': 'echo 1',
            }
            for name, body in stubs.items():
                path = bindir / name
                path.write_text('#!/bin/bash\n' + body + '\n'); path.chmod(0o755)
            env = dict(os.environ, HOME=directory, SCRIPT_DIR=directory,
                       PATH=str(bindir) + os.pathsep + os.environ['PATH'], AUDIO_LOG=str(root / 'audio.log'))
            for kind in ('start', 'complete', 'waiting', 'error'):
                with self.subTest(kind=kind):
                    (root / 'audio.log').write_text('')
                    result = subprocess.run(['bash', str(HERE.parent / 'claude/sound_utils.sh'), 'play', kind, '%23'],
                                            env=env, text=True, capture_output=True, timeout=5)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual((root / 'audio.log').read_text().strip(), kind + ' %23')
