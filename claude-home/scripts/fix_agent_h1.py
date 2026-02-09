#!/usr/bin/env python3
"""Remove H1 headers from agent files that already have frontmatter."""
import os
import sys
import re

def fix_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    basename = os.path.basename(filepath)
    if basename in ('README.md', 'WORKFLOW_EXAMPLES.md', 'TASK-STATUS-PROTOCOL.md'):
        return f"SKIP: {filepath}"

    if not content.startswith('---'):
        return f"SKIP_NO_FM: {filepath}"

    parts = content.split('---', 2)
    if len(parts) < 3:
        return f"SKIP_BAD_FM: {filepath}"

    frontmatter = parts[1]
    body = parts[2]

    # Remove H1 from body
    body_lines = body.split('\n')
    new_lines = []
    h1_removed = False
    skip_blank_after_h1 = False

    for line in body_lines:
        if not h1_removed and line.startswith('# '):
            h1_removed = True
            skip_blank_after_h1 = True
            continue
        if skip_blank_after_h1 and line.strip() == '':
            skip_blank_after_h1 = False
            continue
        skip_blank_after_h1 = False
        new_lines.append(line)

    if not h1_removed:
        return f"NO_H1: {filepath}"

    new_content = f'---{frontmatter}---\n' + '\n'.join(new_lines)

    with open(filepath, 'w') as f:
        f.write(new_content)
    return f"FIXED: {filepath}"

def main():
    agents_dir = sys.argv[1] if len(sys.argv) > 1 else 'claude-home/agents'
    results = {}

    for fname in sorted(os.listdir(agents_dir)):
        if not fname.endswith('.md'):
            continue
        filepath = os.path.join(agents_dir, fname)
        if not os.path.isfile(filepath):
            continue
        result = fix_file(filepath)
        key = result.split(':')[0]
        results[key] = results.get(key, 0) + 1
        print(result)

    print(f"\n=== Summary ===")
    for k, v in results.items():
        print(f"  {k}: {v}")

if __name__ == '__main__':
    main()
