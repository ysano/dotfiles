#!/usr/bin/env python3
"""Merge tmux status hooks into Codex config without changing permissions."""
import argparse
import json
import os
from pathlib import Path
import shlex
import shutil
import tempfile
import time

from codex_status import EVENTS

MARKER = "tmux Codex status"


def install(target, script):
    data = json.loads(target.read_text()) if target.exists() else {}
    if not isinstance(data, dict) or not isinstance(data.get("hooks", {}), dict):
        raise ValueError("hooks.json must contain an object of hook events")
    hooks = data.setdefault("hooks", {})
    for event in sorted(EVENTS):
        groups = []
        for group in hooks.get(event, []):
            retained = [hook for hook in group.get("hooks", []) if hook.get("statusMessage") != MARKER]
            if retained:
                groups.append(dict(group, hooks=retained))
        groups.append({"hooks": [{"type": "command", "command": shlex.join(["python3", str(script)]),
                                  "timeout": 3, "statusMessage": MARKER}]})
        hooks[event] = groups
    content = json.dumps(data, ensure_ascii=False, indent=2) + "\n"
    if target.exists() and target.read_text() == content:
        return False
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.exists():
        shutil.copy2(target, str(target) + ".backup." + str(time.time_ns()))
    fd, temporary = tempfile.mkstemp(prefix=".tmux-hooks-", dir=target.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            stream.write(content)
        if target.exists():
            shutil.copymode(target, temporary)
        os.replace(temporary, target)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
    return True


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path(os.environ.get("CODEX_HOME", str(Path.home() / ".codex"))) / "hooks.json")
    parser.add_argument("--script", type=Path, default=Path(__file__).absolute().with_name("codex_status.py"))
    args = parser.parse_args()
    if not args.script.is_file():
        parser.error("status script does not exist")
    try:
        changed = install(args.output, args.script.absolute())
    except (OSError, ValueError, TypeError, AttributeError) as exc:
        parser.exit(1, "設定を更新できません: " + str(exc) + "\n")
    print(("更新: " if changed else "変更なし: ") + str(args.output))
    print("次回の Codex 起動時に hooks を確認・信頼してください。権限設定は変更していません。")
