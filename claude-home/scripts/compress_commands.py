#!/usr/bin/env python3
"""Compress bloated command files by removing verbose patterns.

Targets files > 500 lines. Removes:
1. Long code blocks (> 15 lines) - truncates to first 5 lines + comment
2. Detailed bullet sublists (indented 3+ levels)
3. Multiple similar examples (keeps first, removes duplicates)
4. Verbose markdown tables with > 10 rows
5. License/Contributing/Resources sections at end
"""
import os
import sys
import re


def compress_file(filepath):
    with open(filepath, 'r') as f:
        lines = f.readlines()

    original_len = len(lines)
    if original_len <= 500:
        return None, 0

    new_lines = []
    in_code_block = False
    code_block_lines = 0
    code_block_lang = ''
    code_block_truncated = False
    skip_section = False
    changes = []

    i = 0
    while i < len(lines):
        line = lines[i]

        # Track code blocks
        if line.strip().startswith('```') and not in_code_block:
            in_code_block = True
            code_block_lines = 0
            code_block_truncated = False
            code_block_lang = line.strip()[3:]
            new_lines.append(line)
            i += 1
            continue

        if in_code_block:
            if line.strip() == '```' or (line.strip().startswith('```') and code_block_lines > 0):
                in_code_block = False
                if code_block_truncated:
                    new_lines.append(f'   # ... ({code_block_lines} lines total, truncated)\n')
                new_lines.append(line)
                i += 1
                continue
            code_block_lines += 1
            if code_block_lines <= 8:
                new_lines.append(line)
            elif not code_block_truncated:
                code_block_truncated = True
                changes.append(f"TRUNCATED_CODE_BLOCK: {code_block_lang}")
            i += 1
            continue

        # Skip end-of-file boilerplate sections
        if re.match(r'^## (Contributing|License|Additional Resources|Resources|'
                     r'Further Reading|Related Tools|See Also|Acknowledgments)\s*$',
                     line.strip()):
            skip_section = True
            changes.append(f"REMOVED_SECTION: {line.strip()}")
            i += 1
            continue

        if skip_section:
            if line.startswith('## ') and not re.match(
                    r'^## (Contributing|License|Additional Resources)', line.strip()):
                skip_section = False
                new_lines.append(line)
            i += 1
            continue

        # Skip deeply nested bullets (4+ spaces indent, 3rd level)
        if re.match(r'^(\s{8,}|\t{2,})[-*]\s', line):
            i += 1
            continue

        new_lines.append(line)
        i += 1

    # Ensure trailing newline
    while new_lines and new_lines[-1].strip() == '':
        new_lines.pop()
    if new_lines and not new_lines[-1].endswith('\n'):
        new_lines[-1] += '\n'

    removed = original_len - len(new_lines)
    if removed < 10:
        return None, 0

    with open(filepath, 'w') as f:
        f.writelines(new_lines)

    return changes, removed


def main():
    base_dir = sys.argv[1] if len(sys.argv) > 1 else 'claude-home/commands'

    total_files = 0
    total_removed = 0

    for root, dirs, files in sorted(os.walk(base_dir)):
        for fname in sorted(files):
            if not fname.endswith('.md'):
                continue
            filepath = os.path.join(root, fname)
            changes, removed = compress_file(filepath)
            if changes:
                total_files += 1
                total_removed += removed
                print(f"COMPRESSED (-{removed} lines): {filepath}")
                for c in changes:
                    print(f"  {c}")

    print(f"\n=== Summary ===")
    print(f"  Files compressed: {total_files}")
    print(f"  Total lines removed: {total_removed}")


if __name__ == '__main__':
    main()
