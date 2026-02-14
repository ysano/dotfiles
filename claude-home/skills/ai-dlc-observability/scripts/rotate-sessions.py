#!/usr/bin/env python3
"""
Rotate sessions.jsonl entries older than a configurable threshold.

Archives old entries to sessions-archive-YYYY-MM.jsonl and atomically
replaces sessions.jsonl with only recent entries.

Usage:
    python3 rotate-sessions.py              # 30日超をアーカイブ
    python3 rotate-sessions.py --days 60    # 60日超
    python3 rotate-sessions.py --dry-run    # 確認のみ
"""
import argparse
import json
import os
import sys
from datetime import datetime, timezone, timedelta


METRICS_DIR = os.path.expanduser("~/.claude/metrics")
SESSIONS_FILE = os.path.join(METRICS_DIR, "sessions.jsonl")


def parse_iso(ts_str):
    """Parse ISO 8601 timestamp string to datetime (UTC)."""
    if not ts_str:
        return None
    try:
        ts_str = ts_str.replace("Z", "+00:00")
        if "+" not in ts_str and ts_str.count("-") <= 2:
            ts_str += "+00:00"
        return datetime.fromisoformat(ts_str)
    except (ValueError, TypeError):
        return None


def load_and_partition(cutoff_dt):
    """Load sessions.jsonl and partition into (keep, archive) lists."""
    keep = []
    archive = []

    if not os.path.isfile(SESSIONS_FILE):
        return keep, archive

    with open(SESSIONS_FILE, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                keep.append(line)  # preserve malformed lines
                continue

            start_time = parse_iso(entry.get("start_time"))
            if start_time and start_time < cutoff_dt:
                archive.append(entry)
            else:
                keep.append(line)

    return keep, archive


def write_archive(archive_entries):
    """Append archived entries to monthly archive files."""
    by_month = {}
    for entry in archive_entries:
        start = parse_iso(entry.get("start_time"))
        if start:
            key = start.strftime("%Y-%m")
        else:
            key = "unknown"
        by_month.setdefault(key, []).append(entry)

    for month, entries in sorted(by_month.items()):
        archive_path = os.path.join(METRICS_DIR, f"sessions-archive-{month}.jsonl")
        with open(archive_path, "a") as f:
            for entry in entries:
                f.write(json.dumps(entry, ensure_ascii=False) + "\n")


def write_sessions(keep_lines):
    """Atomically replace sessions.jsonl with kept entries."""
    tmp_path = SESSIONS_FILE + ".tmp"
    with open(tmp_path, "w") as f:
        for line in keep_lines:
            f.write(line + "\n")
    os.replace(tmp_path, SESSIONS_FILE)


def main():
    parser = argparse.ArgumentParser(description="Rotate sessions.jsonl")
    parser.add_argument("--days", type=int, default=30,
                        help="Archive entries older than N days (default: 30)")
    parser.add_argument("--dry-run", action="store_true",
                        help="Show what would be archived without writing")
    args = parser.parse_args()

    cutoff_dt = datetime.now(timezone.utc) - timedelta(days=args.days)
    keep, archive = load_and_partition(cutoff_dt)

    if not archive:
        print(f"No entries older than {args.days} days. Nothing to archive.")
        return

    oldest = min(
        (parse_iso(e.get("start_time")) for e in archive if parse_iso(e.get("start_time"))),
        default=None,
    )
    oldest_str = oldest.strftime("%Y-%m-%d") if oldest else "unknown"
    cutoff_str = cutoff_dt.strftime("%Y-%m-%d")

    if args.dry_run:
        print(f"[dry-run] Would archive {len(archive)} entries "
              f"(oldest: {oldest_str}), keep {len(keep)} entries "
              f"(cutoff: {cutoff_str})")
        return

    os.makedirs(METRICS_DIR, exist_ok=True)
    write_archive(archive)
    write_sessions(keep)
    print(f"Archived {len(archive)} entries (oldest: {oldest_str}), "
          f"kept {len(keep)} entries (cutoff: {cutoff_str})")


if __name__ == "__main__":
    main()
