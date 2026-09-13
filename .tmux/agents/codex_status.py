#!/usr/bin/env python3
"""Codex hooks -> pane-local state. stdout is reserved for hook JSON."""
import copy
from contextlib import contextmanager
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time

EVENTS = {"SessionStart", "SessionEnd", "UserPromptSubmit", "PreToolUse",
          "PostToolUse", "PermissionRequest", "Stop", "Interrupt",
          "SubagentStart", "SubagentStop"}
ICONS = {"Permission": "⌛", "Busy": "⚡", "Unknown": "?", "Idle": "✅"}
HERE = Path(__file__).resolve().parent


def reduce_event(previous, event):
    """Keep separate turns for root and each child; ignore obsolete completions."""
    name, sid = event.get("hook_event_name"), event.get("session_id")
    if name not in EVENTS or not isinstance(sid, str) or not sid:
        return previous
    state = copy.deepcopy(previous)
    child = event.get("agent_id")
    if state.get("session_id") != sid:
        if state and name not in {"SessionStart", "UserPromptSubmit"}:
            return previous
        if child or name in {"SessionEnd", "Stop", "Interrupt", "SubagentStop"}:
            return previous
        state = {"session_id": sid, "actors": {}}
    if name == "SessionEnd":
        return {}
    if name == "SessionStart":
        return state  # compact/resume must not turn ongoing work into Idle
    actors = state["actors"]
    actor_key = child or "root"
    turn = event.get("turn_id", "")
    actor = actors.get(actor_key)
    if name in {"UserPromptSubmit", "SubagentStart"}:
        if actor and actor["turn_id"] == turn and actor["active"]:
            return state
        actors[actor_key] = {"turn_id": turn, "active": True, "pending": []}
        return state
    if actor is None:
        if name not in {"PreToolUse", "PermissionRequest", "PostToolUse"}:
            return state
        actor = actors[actor_key] = {"turn_id": turn, "active": True, "pending": []}
    if actor["turn_id"] != turn:
        return state
    if not actor["active"]:
        # Another Stop hook may request continuation. A new tool invocation is
        # evidence of resumed work; a delayed tool result alone is not.
        if name == "PreToolUse" and actor.get("closed_by") in {"Stop", "SubagentStop"}:
            actor["active"] = True
        else:
            return state
    if name in {"Stop", "SubagentStop", "Interrupt"}:
        actor["active"] = False
        actor["pending"] = []
        actor["closed_by"] = name
    elif name == "PermissionRequest":
        key = event.get("tool_name", "unknown")
        if key not in actor["pending"]:
            actor["pending"].append(key)
    elif name == "PostToolUse":
        key = event.get("tool_name", "unknown")
        actor["pending"] = [x for x in actor["pending"] if x != key]
    return state


def status(state):
    if not state:
        return ""
    actors = list(state.get("actors", {}).values())
    if any(x["active"] and x["pending"] for x in actors):
        return "Permission"
    return "Busy" if any(x["active"] for x in actors) else "Idle"


def sound_for(before, after, event):
    if before == after:
        return ""
    if after == "Busy":
        return "start" if before in {"", "Idle", "Unknown"} else ""
    if after == "Permission":
        return "waiting"
    if after == "Idle" and before in {"Busy", "Permission"} and event in {"Stop", "SubagentStop"}:
        return "complete"
    return ""


def tmux(*args):
    result = subprocess.run(["tmux", *args], text=True, capture_output=True, timeout=3)
    if result.returncode:
        raise RuntimeError(result.stderr.strip())
    return result.stdout.strip()


def pane_option(pane, key):
    return tmux("show-option", "-pqv", "-t", pane, key)


def set_pane(pane, key, value):
    tmux("set-option", "-p", "-t", pane, key, value)


def aggregate():
    previous = {}
    for line in tmux("list-windows", "-a", "-F", "#{window_id}\t#{@codex_icon}").splitlines():
        window, _, icon = line.partition("\t")
        previous[window] = icon
    windows = {w: [] for w in previous}
    for line in tmux("list-panes", "-a", "-F", "#{window_id}\t#{@codex_status}").splitlines():
        window, _, value = line.partition("\t")
        windows.setdefault(window, []).append(value)
    for window, values in windows.items():
        value = next((ICONS[x] for x in ICONS if x in values), "")
        if previous.get(window) != value:
            tmux("set-option", "-w", "-t", window, "@codex_icon", value)


@contextmanager
def server_lock():
    # flock is released even if the hook process dies. Lock is scoped to server.
    root = Path(tempfile.gettempdir()) / ("tmux-codex-" + str(os.getuid()))
    root.mkdir(mode=0o700, exist_ok=True)
    if root.is_symlink() or root.stat().st_uid != os.getuid():
        raise RuntimeError("invalid lock directory")
    socket = tmux("display-message", "-p", "#{socket_path}")
    digest = hashlib.sha256(socket.encode()).hexdigest()
    with (root / (digest + ".lock")).open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        yield


def process_hook(event):
    pane = os.environ.get("TMUX_PANE", "")
    if not os.environ.get("TMUX") or not re.fullmatch(r"%[0-9]+", pane):
        return
    if tmux("show-option", "-gqv", "@codex_enabled") == "false":
        return
    with server_lock():
        raw = pane_option(pane, "@codex_state")
        state = json.loads(raw) if raw else {}
        before = pane_option(pane, "@codex_status")
        updated = reduce_event(state, event)
        if updated == state:
            return
        after = status(updated)
        set_pane(pane, "@codex_state", json.dumps(updated, separators=(",", ":")) if updated else "")
        set_pane(pane, "@codex_status", after)
        set_pane(pane, "@codex_updated", str(int(time.time())))
        aggregate()
        sound = sound_for(before, after, event.get("hook_event_name"))
        script = HERE.parent / "claude" / "sound_utils.sh"
        if sound and script.exists() and tmux("show-option", "-gqv", "@claude_voice_sound_enabled") == "true":
            subprocess.Popen(["bash", str(script), "play", sound],
                             stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                             stderr=subprocess.DEVNULL, start_new_session=True)


def is_codex_process(pane_pid, processes):
    """npm's node wrapper may be foreground; inspect descendants by executable."""
    for pid, (_, executable) in processes.items():
        if Path(executable).name not in {"codex", "codex.exe"}:
            continue
        seen = set()
        while pid in processes and pid not in seen:
            if pid == pane_pid:
                return True
            seen.add(pid)
            pid = processes[pid][0]
    return False


def process_table():
    result = subprocess.run(["ps", "-axo", "pid=,ppid=,comm="], text=True,
                            capture_output=True, check=True, timeout=3)
    processes = {}
    for line in result.stdout.splitlines():
        fields = line.strip().split(None, 2)
        if len(fields) == 3:
            processes[int(fields[0])] = (int(fields[1]), fields[2])
    return processes


def poll_locked():
    """Clear dead CLIs; never infer completion from a title or a timeout."""
    disabled = tmux("show-option", "-gqv", "@codex_enabled") == "false"
    processes = process_table()
    for line in tmux("list-panes", "-a", "-F", "#{pane_id}\t#{pane_pid}\t#{@codex_status}").splitlines():
        fields = line.split("\t")
        pane, pid = fields[:2]
        current = fields[2] if len(fields) > 2 else ""
        alive = is_codex_process(int(pid), processes)
        if current and (disabled or not alive):
            for key in ("@codex_state", "@codex_status", "@codex_updated"):
                set_pane(pane, key, "")
        elif alive and not current and not disabled:
            set_pane(pane, "@codex_status", "Unknown")
    aggregate()


def poll():
    with server_lock():
        poll_locked()


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "hook"
    try:
        if mode == "poll":
            poll()
        elif mode == "status":
            print(tmux("list-panes", "-a", "-F", "#{pane_id} #{pane_current_command} #{@codex_status} #{@codex_state}"))
        else:
            event = json.load(sys.stdin)
            if isinstance(event, dict):
                process_hook(event)
    except (OSError, ValueError, KeyError, TypeError, RuntimeError, subprocess.SubprocessError) as exc:
        # Monitoring must never block the agent; no prompt or tool text in logs.
        print("tmux Codex status: " + str(exc), file=sys.stderr)
    finally:
        if mode == "hook":
            print("{}")


if __name__ == "__main__":
    main()
