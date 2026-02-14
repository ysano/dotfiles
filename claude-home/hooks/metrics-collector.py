#!/usr/bin/env python3
"""
AI metrics collector for AI-DLC workflow (G5).
Stop hook: collects session metrics and appends to sessions.jsonl.
Data source for `/ai-dlc:diagnose` and `/ai-dlc:status`.
"""
import json
import os
import re
import sys
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path


METRICS_DIR = os.path.expanduser("~/.claude/metrics")
METRICS_FILE = os.path.join(METRICS_DIR, "sessions.jsonl")


def get_session_jsonl_path():
    """Derive session JSONL path from environment variables."""
    session_id = os.environ.get("CLAUDE_SESSION_ID")
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR")

    if not session_id or not project_dir:
        return None

    # Encode project dir: /home/user/dotfiles -> -home-user-dotfiles
    encoded = project_dir.replace("/", "-")
    if encoded.startswith("-"):
        pass  # keep leading dash

    sessions_dir = os.path.expanduser(f"~/.claude/projects/{encoded}")
    jsonl_path = os.path.join(sessions_dir, f"{session_id}.jsonl")

    if os.path.isfile(jsonl_path):
        return jsonl_path

    return None


def parse_branch_issue(branch):
    """Extract issue number from branch name."""
    if not branch:
        return None
    match = re.search(r'(?:GH-|issue-|#)?(\d{2,})', branch)
    return match.group(1) if match else None


def get_git_branch():
    """Get current git branch name."""
    import subprocess
    try:
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True, text=True, timeout=3,
            cwd=os.environ.get("CLAUDE_PROJECT_DIR", os.getcwd()),
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass
    return None


def parse_session_jsonl(jsonl_path):
    """Parse session JSONL and extract metrics."""
    user_turns = 0
    assistant_turns = 0
    tool_counts = Counter()
    modified_files = set()
    start_time = None
    end_time = None

    try:
        with open(jsonl_path, "r") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    entry = json.loads(line)
                except json.JSONDecodeError:
                    continue

                # Track timestamps
                ts = entry.get("timestamp")
                if ts:
                    if start_time is None:
                        start_time = ts
                    end_time = ts

                msg_type = entry.get("type")
                if msg_type == "human":
                    user_turns += 1
                elif msg_type == "assistant":
                    assistant_turns += 1

                    # Count tool uses from assistant messages
                    content = entry.get("message", {}).get("content", [])
                    if isinstance(content, list):
                        for block in content:
                            if isinstance(block, dict) and block.get("type") == "tool_use":
                                tool_name = block.get("name", "unknown")
                                tool_counts[tool_name] += 1

                                # Track modified files
                                tool_input = block.get("input", {})
                                if tool_name in ("Write", "Edit", "MultiEdit"):
                                    fp = tool_input.get("file_path", "")
                                    if fp:
                                        modified_files.add(fp)
    except OSError:
        pass

    return {
        "start_time": start_time,
        "end_time": end_time,
        "user_turns": user_turns,
        "assistant_turns": assistant_turns,
        "tool_counts": dict(tool_counts),
        "modified_files": sorted(modified_files),
    }


def main():
    # Read stdin (hook input) but we don't need it for Stop hooks
    try:
        json.load(sys.stdin)
    except (json.JSONDecodeError, EOFError):
        pass

    session_id = os.environ.get("CLAUDE_SESSION_ID")
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR")

    if not session_id:
        sys.exit(0)

    # Get session JSONL
    jsonl_path = get_session_jsonl_path()

    # Parse metrics from session JSONL
    if jsonl_path:
        metrics = parse_session_jsonl(jsonl_path)
    else:
        metrics = {
            "start_time": None,
            "end_time": None,
            "user_turns": 0,
            "assistant_turns": 0,
            "tool_counts": {},
            "modified_files": [],
        }

    # Get git context
    branch = get_git_branch()
    linked_issue = parse_branch_issue(branch)

    # Build output record
    record = {
        "session_id": session_id,
        "project_dir": project_dir,
        "git_branch": branch,
        "linked_issue": linked_issue,
        "start_time": metrics["start_time"],
        "end_time": metrics["end_time"],
        "user_turns": metrics["user_turns"],
        "assistant_turns": metrics["assistant_turns"],
        "total_turns": metrics["user_turns"] + metrics["assistant_turns"],
        "tool_counts": metrics["tool_counts"],
        "modified_files": metrics["modified_files"],
        "file_count": len(metrics["modified_files"]),
    }

    # Write to sessions.jsonl
    try:
        os.makedirs(METRICS_DIR, exist_ok=True)
        with open(METRICS_FILE, "a") as f:
            f.write(json.dumps(record, ensure_ascii=False) + "\n")
    except OSError as e:
        print(f"metrics-collector: failed to write metrics: {e}", file=sys.stderr)

    sys.exit(0)


if __name__ == "__main__":
    main()
