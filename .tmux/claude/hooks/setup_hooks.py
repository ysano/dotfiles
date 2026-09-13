#!/usr/bin/env python3
"""Install only our state hooks. No jq dependency and no global settings reset."""
import datetime
import json
import os
from pathlib import Path
import shlex
import shutil
import tempfile

EVENTS = ('SessionStart', 'SessionEnd', 'UserPromptSubmit', 'Notification',
          'PermissionRequest', 'PreToolUse', 'PostToolUse', 'PostToolUseFailure',
          'Stop', 'SubagentStart', 'SubagentStop')


def main():
    path = Path.home() / '.claude/settings.json'
    command = str(Path.home() / '.tmux/claude/hooks/status-update.sh')
    original = path.read_text() if path.exists() else ''
    data = json.loads(original) if original else {}
    hooks = data.setdefault('hooks', {})
    for event, entries in hooks.items():
        retained = []
        for entry in entries:
            others = []
            for hook in entry.get('hooks', []):
                value = hook.get('command', '')
                try:
                    managed = value == command or shlex.split(value) == [command]
                except ValueError:
                    managed = False
                if not managed:
                    others.append(hook)
            if others or 'hooks' not in entry:
                retained.append(dict(entry, hooks=others) if 'hooks' in entry else entry)
        hooks[event] = retained
    for event in EVENTS:
        entry = {'hooks': [{'type': 'command', 'command': shlex.quote(command), 'timeout': 10}]}
        if event == 'Notification':
            entry['matcher'] = 'idle_prompt|permission_prompt'
        hooks.setdefault(event, []).append(entry)
    text = json.dumps(data, ensure_ascii=False, indent=2) + '\n'
    if text == original:
        print('Claude hooks は最新です。')
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    if original:
        stamp = datetime.datetime.now().strftime('%Y%m%d_%H%M%S_%f')
        shutil.copy2(path, str(path) + '.backup.' + stamp)
    fd, temporary = tempfile.mkstemp(prefix='.settings-', dir=path.parent)
    try:
        with os.fdopen(fd, 'w') as stream:
            stream.write(text)
        if path.exists():
            os.chmod(temporary, path.stat().st_mode & 0o777)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
    print('Claude hooks を更新しました: ' + str(path))
    print('次の Claude Code セッションから有効になります。')


if __name__ == '__main__':
    main()
