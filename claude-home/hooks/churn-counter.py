#!/usr/bin/env python3
"""
Churn counter for AI-DLC workflow (G3).
PostToolUse hook: tracks repeated modifications to the same file.
Warns at 3 edits (churn alert) and 7 edits (spec revert recommendation).
Never blocks - always exits 0.
"""
import json
import os
import sys
import time

# Reuse SKIP_PATTERNS from check-spec-existence.py
SKIP_PATTERNS = [
    "/test", "/tests", "/__test__", "/__tests__",
    "/spec/", "/specs/",
    ".test.", ".spec.", "_test.", "_spec.",
    ".config.", ".conf",
    ".md", ".mdx", ".txt", ".rst",
    ".json", ".yaml", ".yml", ".toml", ".ini", ".env",
    ".lock", ".sum",
    ".d.ts",
    "/node_modules/", "/dist/", "/build/", "/.next/",
    "/generated/", "/__generated__/",
    "CLAUDE.md", "SKILL.md", "README.md",
]

CACHE_DIR = "/tmp/claude-churn-cache"
STALE_HOURS = 24
WARN_THRESHOLD = 3
ALERT_THRESHOLD = 7


def should_skip(file_path):
    """Check if the file path matches any skip pattern."""
    lower_path = file_path.lower()
    return any(pattern in lower_path for pattern in SKIP_PATTERNS)


def get_project_name():
    """Derive a safe project name from CLAUDE_PROJECT_DIR."""
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR", os.getcwd())
    # Use last two path components for readability
    parts = project_dir.rstrip("/").split("/")
    return "_".join(parts[-2:]) if len(parts) >= 2 else parts[-1]


def load_churn_data(cache_path):
    """Load churn data from cache, cleaning stale entries."""
    if not os.path.isfile(cache_path):
        return {}

    try:
        with open(cache_path, "r") as f:
            data = json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}

    # Clean stale entries (older than STALE_HOURS)
    cutoff = time.time() - (STALE_HOURS * 3600)
    cleaned = {}
    for path, entry in data.items():
        last_ts = entry.get("last_ts", 0)
        if isinstance(last_ts, str):
            try:
                from datetime import datetime, timezone
                last_ts = datetime.fromisoformat(last_ts).timestamp()
            except (ValueError, ImportError):
                last_ts = 0
        if last_ts > cutoff:
            cleaned[path] = entry

    return cleaned


def save_churn_data(cache_path, data):
    """Save churn data to cache."""
    try:
        os.makedirs(os.path.dirname(cache_path), exist_ok=True)
        with open(cache_path, "w") as f:
            json.dump(data, f)
    except OSError:
        pass


def main():
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError:
        sys.exit(0)

    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")

    if not file_path:
        print(json.dumps({"suppressOutput": True}))
        sys.exit(0)

    # Skip test/config/doc files
    if should_skip(file_path):
        print(json.dumps({"suppressOutput": True}))
        sys.exit(0)

    # Load and update churn data
    project_name = get_project_name()
    cache_path = os.path.join(CACHE_DIR, f"{project_name}_churn.json")
    data = load_churn_data(cache_path)

    now = time.time()
    if file_path in data:
        data[file_path]["count"] += 1
        data[file_path]["last_ts"] = now
    else:
        data[file_path] = {"count": 1, "last_ts": now}

    count = data[file_path]["count"]
    save_churn_data(cache_path, data)

    # Check thresholds
    if count == ALERT_THRESHOLD:
        # Collect all high-churn files
        high_churn = sorted(
            [(p, e["count"]) for p, e in data.items() if e["count"] >= WARN_THRESHOLD],
            key=lambda x: -x[1]
        )
        churn_list = "\n".join(f"  {p} ({c} edits)" for p, c in high_churn)
        print(
            f"\n🚨 Churn 異常: {file_path} が {count} 回修正されました\n"
            f"高 Churn ファイル一覧:\n{churn_list}\n"
            f"推奨: Spec Definition に差し戻し、根本原因を再分析してください。\n"
            f"`/ai-dlc:diagnose` でスプリント診断を実行できます\n",
            file=sys.stderr,
        )
    elif count == WARN_THRESHOLD:
        high_churn = sorted(
            [(p, e["count"]) for p, e in data.items() if e["count"] >= WARN_THRESHOLD],
            key=lambda x: -x[1]
        )
        churn_list = "\n".join(f"  {p} ({c} edits)" for p, c in high_churn)
        print(
            f"\n⚠️ Churn 警告: {file_path} が {count} 回修正されました\n"
            f"高 Churn ファイル一覧:\n{churn_list}\n"
            f"同一ファイルへの繰り返し修正は Spec 不足の兆候です\n",
            file=sys.stderr,
        )

    # Never block
    print(json.dumps({"suppressOutput": True}))
    sys.exit(0)


if __name__ == "__main__":
    main()
