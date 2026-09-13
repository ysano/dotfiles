"""Claude state regressions, including the real Bash hook and isolated tmux."""
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


def load_module():
    spec = importlib.util.spec_from_file_location('claude_status', HERE / 'claude_status.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def event(name, **extra):
    return dict({'hook_event_name': name, 'session_id': 'claude-session'}, **extra)


class ClaudeStateTests(unittest.TestCase):
    def test_actor_metadata_and_root_stop(self):
        self.assertTrue((HERE / 'claude_status.py').exists(), 'pane-local Claude reducer required')
        module = load_module()
        state = module.reduce_event({}, event('UserPromptSubmit', cwd='/repo'))
        state = module.reduce_event(state, event('SubagentStart', agent_id='child', agent_type='reviewer', cwd='/child'))
        state = module.reduce_event(state, event('Stop', cwd='/repo'))
        self.assertEqual(module.status(state), 'Busy')
        self.assertEqual(state['provider'], 'claude')
        self.assertEqual(state['cwd'], '/repo')
        self.assertEqual(state['actors']['child']['cwd'], '/child')
        self.assertEqual(state['actors']['child']['name'], 'reviewer')
        state = module.reduce_event(state, event('SubagentStop', agent_id='child'))
        self.assertEqual(module.status(state), 'Idle')

    def test_parent_cwd_on_child_completion_does_not_replace_child_location(self):
        module = load_module()
        state = module.reduce_event({}, event('UserPromptSubmit', cwd='/repo'))
        state = module.reduce_event(state, event('SubagentStart', agent_id='child', cwd='/child'))
        state = module.reduce_event(state, event('SubagentStop', agent_id='child', cwd='/repo'))
        self.assertEqual(state['actors']['child']['cwd'], '/child')

    def test_permission_resumes_on_tool_without_turn_ids(self):
        self.assertTrue((HERE / 'claude_status.py').exists())
        module = load_module()
        state = module.reduce_event({}, event('UserPromptSubmit'))
        state = module.reduce_event(state, event('Notification', notification_type='permission_prompt'))
        self.assertEqual(module.status(state), 'Permission')
        state = module.reduce_event(state, event('PostToolUse', tool_name='Bash'))
        self.assertEqual(module.status(state), 'Busy')

    def test_native_executable_descendant_detection(self):
        self.assertTrue((HERE / 'claude_status.py').exists())
        module = load_module()
        processes = {10: (1, 'zsh'), 11: (10, 'node'), 12: (11, '/bin/claude.exe'), 20: (1, 'node')}
        self.assertTrue(module.is_claude_process(10, processes))
        self.assertFalse(module.is_claude_process(20, processes))


@unittest.skipUnless(shutil.which('tmux') and shutil.which('cc'), 'tmux and C compiler required')
class ClaudeTmuxTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='claude-state-test-')
        self.socket = str(Path(self.temp.name) / 'socket')
        # Actual native named executable, never a real Claude API session.
        executable = Path(self.temp.name) / 'claude.exe'
        source = Path(self.temp.name) / 'sleep.c'
        source.write_text('#include <unistd.h>\nint main(void) { sleep(300); return 0; }\n')
        subprocess.run(['cc', str(source), '-o', str(executable)], check=True, capture_output=True)
        self.tmux('-f', '/dev/null', 'new-session', '-d', '-s', 'probe', '-x', '120', '-y', '40', str(executable) + ' 300')
        self.env = dict(os.environ, TMUX=self.socket + ',0,0', TMUX_PANE='%0')
        self.tmux('set-option', '-g', '@claude_voice_sound_enabled', 'false')
        self.tmux('set-option', '-g', '@claude_voice_summary_enabled', 'false')

    def tearDown(self):
        self.tmux('kill-server')
        self.temp.cleanup()

    def tmux(self, *args):
        return subprocess.check_output(['tmux', '-S', self.socket, *args], text=True).strip()

    def hook(self, name, **extra):
        result = subprocess.run(['bash', str(HERE.parent / 'claude/hooks/status-update.sh')], input=json.dumps(event(name, **extra)), text=True, capture_output=True, env=self.env)
        self.assertEqual(result.returncode, 0, result.stderr)

    def shell(self, code):
        return subprocess.check_output(['bash', '-c', 'source "$1/polling_monitor.sh"; ' + code, 'bash', str(HERE.parent / 'claude')], env=self.env, text=True).strip()

    def test_parent_stop_retains_child_and_permission_resume(self):
        self.hook('UserPromptSubmit', cwd=self.temp.name)
        self.hook('SubagentStart', agent_id='child')
        self.hook('Stop')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_pane_status_probe__0_0'), 'Busy')
        self.hook('SubagentStop', agent_id='child')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Idle')
        self.hook('UserPromptSubmit')
        self.hook('Notification', notification_type='permission_prompt')
        self.hook('PostToolUse', tool_name='Bash')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Busy')

    def test_pane_move_and_cli_exit_clear_legacy_positions(self):
        self.hook('UserPromptSubmit')
        self.tmux('new-window', '-d', '-n', 'other', 'sleep 300')
        self.tmux('split-window', '-d', '-t', '%0', 'sleep 300')
        self.tmux('join-pane', '-s', '%0', '-t', '%1')
        self.hook('Stop')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_icon_probe_0'), '')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_icon_probe_1'), '✅')
        self.tmux('respawn-pane', '-k', '-t', '%0', 'sleep 300')
        subprocess.run(['python3', str(HERE / 'claude_status.py'), 'poll'], check=True, env=self.env)
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), '')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_icon_probe_1'), '')

    def test_observation_is_outside_lock_and_notifications_are_deduplicated(self):
        module = load_module()
        locked = []
        original_lock = module.server_lock
        observations = []

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
            observations.append((payload['hook_event_name'], pane))

        launches = []
        background = []
        original_popen = subprocess.Popen

        def popen(argv, *args, **kwargs):
            if len(argv) > 1 and str(argv[1]).endswith('/hooks/notify.sh'):
                launches.append(argv)
                child = original_popen(argv, *args, **kwargs)
                background.append(child)
                return child
            return original_popen(argv, *args, **kwargs)

        with mock.patch.dict(os.environ, self.env), mock.patch.object(module, 'server_lock', tracked_lock), \
                mock.patch.object(module.base, 'observe_hook', observe), mock.patch.object(module.subprocess, 'Popen', popen):
            module.process_hook(event('UserPromptSubmit'))
            module.process_hook(event('SubagentStart', agent_id='child'))
            module.process_hook(event('Stop'))
            module.process_hook(event('SubagentStop', agent_id='child'))
            module.process_hook(event('SubagentStop', agent_id='child'))
        for child in background:
            child.wait(timeout=5)
        self.assertEqual([argv[3] for argv in launches], ['Busy', 'Idle'])
        self.assertEqual(len(observations), 5)

    def test_new_session_discards_previous_screen_evidence(self):
        self.hook('UserPromptSubmit')
        self.shell('update_claude_evidence probe:0.0 error api')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Error')
        self.hook('SessionStart', session_id='new-session')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Idle')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_evidence'), '')

    def test_claude_exe_is_detected(self):
        self.assertEqual(self.shell('detect_claude_panes'), 'probe:0.0')

    def test_title_idle_cannot_override_hook_busy(self):
        self.hook('UserPromptSubmit')
        self.tmux('select-pane', '-t', '%0', '-T', '✳ Claude Code')
        self.shell('correct_status_from_title')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_pane_status_probe__0_0'), 'Busy')

    def test_dialog_without_notifications_restores_hook_state(self):
        self.tmux('set-option', '-g', '@claude_voice_enabled', 'false')
        self.hook('UserPromptSubmit')
        # Inject terminal output using a tmux display job on the same pane.
        self.tmux('respawn-pane', '-k', '-t', '%0', "printf '☐ Question\\nPick one\\nEnter to select\\n'; exec '" + str(Path(self.temp.name) / 'claude.exe') + "' 300")
        self.shell('detect_dialogs')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_pane_status_probe__0_0'), 'Question')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Question')
        self.assertEqual(self.tmux('show-option', '-gqv', '@claude_voice_enabled'), 'false')
        self.tmux('respawn-pane', '-k', '-t', '%0', "printf '\\033[2J\\033[H'; exec '" + str(Path(self.temp.name) / 'claude.exe') + "' 300")
        self.shell('detect_dialogs')
        self.assertEqual(self.tmux('show-option', '-pqv', '-t', '%0', '@claude_status'), 'Busy')


if __name__ == '__main__':
    unittest.main()
