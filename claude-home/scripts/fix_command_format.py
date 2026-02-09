#!/usr/bin/env python3
"""Fix command file format: add frontmatter description from H1, remove H1."""
import os
import sys
import re

def fix_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Skip if already has frontmatter
    if content.startswith('---'):
        # Check if H1 exists after frontmatter and remove it
        parts = content.split('---', 2)
        if len(parts) >= 3:
            frontmatter = parts[1]
            body = parts[2]
            # Remove leading H1 from body if present
            body_stripped = body.lstrip('\n')
            h1_match = re.match(r'^# (.+)\n', body_stripped)
            if h1_match:
                body_stripped = body_stripped[h1_match.end():]
                new_content = f'---{frontmatter}---\n{body_stripped}'
                if new_content != content:
                    with open(filepath, 'w') as f:
                        f.write(new_content)
                    return f"REMOVED_H1: {filepath}"
        return f"SKIP_HAS_FM: {filepath}"

    # Skip README files
    basename = os.path.basename(filepath)
    if basename == 'README.md' or basename.startswith('SIMULATION_') or basename.startswith('ORCHESTRATION-'):
        return f"SKIP_README: {filepath}"

    lines = content.split('\n')

    # Pattern: # Title\n\nDescription or # Title\n\nDescription\n\n## ...
    if lines and lines[0].startswith('# '):
        h1_title = lines[0][2:].strip()

        # Find description: next non-empty line after H1
        desc = ''
        desc_end_idx = 1
        for i in range(1, len(lines)):
            line = lines[i].strip()
            if line == '':
                continue
            if line.startswith('#'):
                break
            desc = line
            desc_end_idx = i + 1
            break

        if not desc:
            desc = h1_title

        # Build new content: frontmatter + body (skip H1 and desc line)
        # Find where the real body starts (after H1, optional blank, desc, optional blank)
        body_start = 1  # after H1
        # Skip blank lines after H1
        while body_start < len(lines) and lines[body_start].strip() == '':
            body_start += 1
        # Skip the description line if it exists and is not a heading
        if body_start < len(lines) and not lines[body_start].startswith('#'):
            body_start += 1
        # Skip blank lines after description
        while body_start < len(lines) and lines[body_start].strip() == '':
            body_start += 1

        body = '\n'.join(lines[body_start:])
        new_content = f'---\ndescription: "{desc}"\n---\n\n{body}\n'

        with open(filepath, 'w') as f:
            f.write(new_content)
        return f"FIXED: {filepath}"

    return f"SKIP_NO_H1: {filepath}"


def main():
    commands_dir = sys.argv[1] if len(sys.argv) > 1 else 'claude-home/commands'

    results = {'FIXED': 0, 'REMOVED_H1': 0, 'SKIP_HAS_FM': 0, 'SKIP_README': 0, 'SKIP_NO_H1': 0}

    for root, dirs, files in os.walk(commands_dir):
        for fname in sorted(files):
            if not fname.endswith('.md'):
                continue
            filepath = os.path.join(root, fname)
            result = fix_file(filepath)
            key = result.split(':')[0]
            results[key] = results.get(key, 0) + 1
            print(result)

    print(f"\n=== Summary ===")
    for k, v in results.items():
        print(f"  {k}: {v}")

if __name__ == '__main__':
    main()
