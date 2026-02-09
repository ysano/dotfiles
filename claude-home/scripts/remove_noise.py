#!/usr/bin/env python3
"""Remove prompt noise patterns from command and agent files.

Targets:
- "Remember to..." trailing paragraphs
- "## Best Practices" sections
- "## Tips" / "## Tips for Success" sections
- "Remember: ..." trailing lines
- Trailing "Remember to:" + bullet list blocks
"""
import os
import sys
import re


def remove_noise(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()

    original_len = len(lines)
    changes = []

    # Pass 1: Remove "## Best Practices" and "## Tips" sections (until next ##)
    new_lines = []
    skip_section = False
    section_name = None
    for line in lines:
        if re.match(r'^## (Best Practices|Tips|Tips for Success)\s*$', line.strip()):
            skip_section = True
            section_name = line.strip()
            changes.append(f"REMOVED_SECTION: {section_name}")
            continue
        if skip_section:
            if line.startswith('## ') and not re.match(r'^## (Best Practices|Tips)', line.strip()):
                skip_section = False
                new_lines.append(line)
            continue
        new_lines.append(line)
    lines = new_lines

    # Pass 2: Remove trailing "Remember to..." paragraphs and "Remember:" blocks
    # Work backwards from end of file
    while lines:
        last = lines[-1].strip()
        # Skip trailing blank lines for analysis
        if last == '':
            lines.pop()
            continue
        # Remove "Remember to ..." single-line endings
        if re.match(r'^Remember to[:\s]', last, re.IGNORECASE):
            changes.append(f"REMOVED_REMEMBER: {last[:60]}")
            lines.pop()
            continue
        # Remove "Remember: ..." single-line endings
        if re.match(r'^Remember:', last, re.IGNORECASE):
            changes.append(f"REMOVED_REMEMBER: {last[:60]}")
            lines.pop()
            continue
        break

    # Pass 3: Remove "Remember to:" + following bullet list at end
    # Check if file ends with a "Remember to:" followed by bullets
    if lines:
        # Find last non-blank content block
        i = len(lines) - 1
        while i >= 0 and lines[i].strip() == '':
            i -= 1
        # Check for bullet list ending
        bullet_end = i
        while i >= 0 and re.match(r'^[-*]\s', lines[i].strip()):
            i -= 1
        # Check if preceded by "Remember to:"
        if i >= 0 and re.match(r'^Remember to:\s*$', lines[i].strip(), re.IGNORECASE):
            removed = [l.strip() for l in lines[i:bullet_end+1]]
            changes.append(f"REMOVED_REMEMBER_BLOCK: {len(removed)} lines")
            lines = lines[:i]

    # Ensure file ends with single newline
    while lines and lines[-1].strip() == '':
        lines.pop()
    if lines:
        if not lines[-1].endswith('\n'):
            lines[-1] += '\n'

    if not changes:
        return None, 0

    with open(filepath, 'w') as f:
        f.writelines(lines)

    removed = original_len - len(lines)
    return changes, removed


def main():
    base_dirs = sys.argv[1:] if len(sys.argv) > 1 else [
        'claude-home/commands',
        'claude-home/agents'
    ]

    total_files = 0
    total_removed = 0
    total_changes = 0

    for base_dir in base_dirs:
        for root, dirs, files in sorted(os.walk(base_dir)):
            for fname in sorted(files):
                if not fname.endswith('.md'):
                    continue
                filepath = os.path.join(root, fname)
                changes, removed = remove_noise(filepath)
                if changes:
                    total_files += 1
                    total_removed += removed
                    total_changes += len(changes)
                    print(f"FIXED ({removed:+d} lines): {filepath}")
                    for c in changes:
                        print(f"  {c}")

    print(f"\n=== Summary ===")
    print(f"  Files modified: {total_files}")
    print(f"  Lines removed: {total_removed}")
    print(f"  Changes applied: {total_changes}")


if __name__ == '__main__':
    main()
