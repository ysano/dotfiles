#!/usr/bin/env python3
"""Claude hooks are authoritative; terminal evidence is a reversible overlay."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
# Also supports loading this module by file path in unit tests.
_spec = importlib.util.spec_from_file_location('tmux_agent_state', HERE / 'codex_status.py')
base = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(base)
tmux, pane_option, set_pane = base.tmux, base.pane_option, base.set_pane
server_lock = base.server_lock
ICONS = {'Error': '⚠', 'Permission': '⌛', 'Question': '❓', 'Busy': '⚡', 'Unknown': '?', 'Idle': '✅'}


def reduce_event(previous, event):
    event = dict(event)
    name = event.get('hook_event_name')
    if name == 'Notification':
        notification = event.get('notification_type')
        if notification == 'permission_prompt':
            event['hook_event_name'] = 'PermissionRequest'
        elif notification == 'idle_prompt':
            # Notification is not an actor completion signal. Stop owns that.
            return previous
        else:
            return previous
    elif name == 'PostToolUseFailure':
        event['hook_event_name'] = 'PostToolUse'
    actor = previous.get('actors', {}).get(event.get('agent_id') or 'root', {})
    if not event.get('turn_id'):
        turn = actor.get('turn_id', '')
        if name in {'UserPromptSubmit', 'SubagentStart'}:
            turn = str(previous.get('generation', 0) + 1)
        event['turn_id'] = turn
    state = base._reduce_event(previous, event)
    if not state or state is previous:
        return state
    if name in {'UserPromptSubmit', 'SubagentStart'}:
        state['generation'] = previous.get('generation', 0) + 1
    state = base.actor_metadata(state, event, 'claude')
    actor = state['actors'].get(event.get('agent_id') or 'root')
    if actor and actor['active'] and name in {'PreToolUse', 'PostToolUse', 'PostToolUseFailure'}:
        # Notification has no tool identity; any resumed tool proves its dismissal.
        key = event.get('tool_name', 'unknown')
        actor['pending'] = [p for p in actor['pending'] if p not in {key, 'unknown'}]
    return state


def status(state, evidence=None):
    evidence = evidence or {}
    if evidence.get('error'):
        return 'Error'
    value = base.status(state)
    if value == 'Permission':
        return value
    if evidence.get('dialog'):
        return 'Question'
    return value or evidence.get('title', '')


def read_json(pane, key):
    raw = pane_option(pane, key)
    return json.loads(raw) if raw else {}


def aggregate():
    """Rebuild legacy position-keyed options from stable native pane options."""
    desired = {}
    window_values = {}
    for line in tmux('list-panes', '-a', '-F', '#{session_name}\t#{window_index}\t#{pane_index}\t#{@claude_status}\t#{@claude_updated}').splitlines():
        fields = line.split('\t')
        fields += [''] * (5 - len(fields))
        session, window, pane, value, updated = fields
        icon_key = '@claude_voice_icon_' + session + '_' + window
        window_values.setdefault(icon_key, []).append(value)
        if value:
            suffix = session + '__' + window + '_' + pane
            for prefix in ('@claude_voice_pane_status_', '@claude_voice_status_'):
                desired[prefix + suffix] = value
            if updated:
                desired['@claude_voice_hooks_ts_' + suffix] = updated
    for key, values in window_values.items():
        desired[key] = next((icon for value, icon in ICONS.items() if value in values), '')
    prefixes = ('@claude_voice_pane_status_', '@claude_voice_status_', '@claude_voice_hooks_ts_', '@claude_voice_icon_')
    existing = {}
    for line in tmux('show-options', '-g').splitlines():
        key, _, value = line.partition(' ')
        if key.startswith(prefixes):
            existing[key] = value.strip('"')
    for key in existing.keys() - desired.keys():
        tmux('set-option', '-gu', key)
    for key, value in desired.items():
        if existing.get(key) != value:
            tmux('set-option', '-g', key, value)


def process_hook(event):
    pane = os.environ.get('TMUX_PANE', '')
    if not os.environ.get('TMUX') or not re.fullmatch(r'%[0-9]+', pane):
        return
    notification = None
    with server_lock():
        before = pane_option(pane, '@claude_status')
        previous = read_json(pane, '@claude_state')
        state = reduce_event(previous, event)
        if state != previous:
            evidence = read_json(pane, '@claude_evidence')
            # A fresh root prompt invalidates previous terminal observations.
            if (state.get('session_id') != previous.get('session_id') or
                    (event.get('hook_event_name') in {'UserPromptSubmit', 'SessionEnd'} and not event.get('agent_id'))):
                evidence = {}
                set_pane(pane, '@claude_dialog_active', '')
            after = status(state, evidence)
            set_pane(pane, '@claude_state', json.dumps(state, separators=(',', ':')) if state else '')
            set_pane(pane, '@claude_evidence', json.dumps(evidence, separators=(',', ':')) if evidence else '')
            set_pane(pane, '@claude_status', after)
            set_pane(pane, '@claude_updated', str(int(time.time())) if state else '')
            aggregate()
            sound = base.sound_for(before, after, event.get('hook_event_name'))
            if sound:
                notification = (after, sound)
    # Never perform git inspection or slow notification work inside the lock.
    base.observe_hook(event, pane)
    if notification:
        script = HERE.parent / 'claude/hooks/notify.sh'
        subprocess.Popen(['bash', str(script), pane, notification[0], notification[1], event.get('message', '')],
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                         stderr=subprocess.DEVNULL, start_new_session=True)


def evidence_update(pane, kind, value):
    """Return whether display changed, so only one detector sends feedback."""
    if kind not in {'dialog', 'error', 'title'}:
        raise ValueError('unknown evidence kind')
    with server_lock():
        state = read_json(pane, '@claude_state')
        if kind == 'title' and state:
            return False
        evidence = read_json(pane, '@claude_evidence')
        before = pane_option(pane, '@claude_status')
        if value:
            evidence[kind] = value
        else:
            evidence.pop(kind, None)
        after = status(state, evidence) or 'Unknown'
        set_pane(pane, '@claude_evidence', json.dumps(evidence, separators=(',', ':')) if evidence else '')
        set_pane(pane, '@claude_status', after)
        aggregate()
        return before != after


def is_claude_process(pane_pid, processes):
    return base.is_agent_process(pane_pid, processes, 'claude')


def detected_panes():
    processes = base.process_table()
    for line in tmux('list-panes', '-a', '-F', '#{pane_id}\t#{pane_pid}\t#{session_name}:#{window_index}.#{pane_index}').splitlines():
        pane, pid, target = line.split('\t', 2)
        if is_claude_process(int(pid), processes):
            yield pane, target


def poll():
    with server_lock():
        alive = {pane for pane, _ in detected_panes()}
        for line in tmux('list-panes', '-a', '-F', '#{pane_id}\t#{@claude_status}').splitlines():
            pane, _, current = line.partition('\t')
            if pane not in alive:
                for key in ('@claude_state', '@claude_status', '@claude_updated', '@claude_evidence', '@claude_dialog_active'):
                    if pane_option(pane, key):
                        set_pane(pane, key, '')
            elif not current:
                set_pane(pane, '@claude_status', 'Unknown')
        aggregate()


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else 'hook'
    try:
        if mode == 'poll':
            poll()
        elif mode == 'detect':
            print('\n'.join(target for _, target in detected_panes()))
        elif mode == 'evidence':
            print('changed' if evidence_update(*sys.argv[2:5]) else '')
        elif mode == 'status':
            print(tmux('list-panes', '-a', '-F', '#{pane_id} #{@claude_status} #{@claude_state}'))
        else:
            event = json.load(sys.stdin)
            if isinstance(event, dict):
                process_hook(event)
    except (OSError, ValueError, KeyError, TypeError, RuntimeError, subprocess.SubprocessError) as exc:
        print('tmux Claude status: ' + str(exc), file=sys.stderr)
    finally:
        if mode == 'hook':
            print('{}')


if __name__ == '__main__':
    main()
