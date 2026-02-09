#!/usr/bin/env python3
"""Comprehensive format fixer for commands and agents.

Fixes:
1. Missing frontmatter (extracts from H1 or filename)
2. H1 in body after frontmatter
3. Remaining noise patterns (Best Practices, Tips, Remember)
"""
import os
import sys
import re


def extract_description_from_h1(h1_line):
    """Extract description text from H1 line."""
    text = h1_line.lstrip('# ').strip()
    # Remove emoji prefixes
    text = re.sub(r'^[\U0001F300-\U0001F9FF\u2600-\u26FF\u2700-\u27BF]+\s*', '', text)
    return text


def filename_to_description(filename):
    """Generate description from filename."""
    name = os.path.splitext(filename)[0]
    # Handle prefixed names like svelte:component
    name = name.replace(':', ' ')
    words = re.split(r'[-_]', name)
    return ' '.join(w.capitalize() for w in words)


def remove_h1_from_body(body):
    """Remove first H1 line and following blank line from body."""
    lines = body.split('\n')
    new_lines = []
    h1_found = False
    skip_blank = False
    for line in lines:
        if not h1_found and re.match(r'^# [^#]', line):
            h1_found = True
            skip_blank = True
            continue
        if skip_blank and line.strip() == '':
            skip_blank = False
            continue
        skip_blank = False
        new_lines.append(line)
    return '\n'.join(new_lines), h1_found


def remove_noise_from_content(content):
    """Remove Best Practices, Tips, Remember patterns."""
    lines = content.split('\n')
    new_lines = []
    skip_section = False
    changes = []

    for line in lines:
        # Skip Best Practices / Tips sections
        if re.match(r'^## (Best Practices|Tips|Tips for Success)\s*$', line.strip()):
            skip_section = True
            changes.append(f"REMOVED: {line.strip()}")
            continue
        if skip_section:
            if line.startswith('## ') and not re.match(r'^## (Best Practices|Tips)', line.strip()):
                skip_section = False
                new_lines.append(line)
            continue
        new_lines.append(line)

    # Remove trailing Remember blocks
    while new_lines and new_lines[-1].strip() == '':
        new_lines.pop()
    while new_lines:
        last = new_lines[-1].strip()
        if re.match(r'^Remember[: ]', last, re.IGNORECASE):
            changes.append(f"REMOVED: {last[:50]}")
            new_lines.pop()
            while new_lines and new_lines[-1].strip() == '':
                new_lines.pop()
            continue
        # Remove trailing bullet list preceded by "Remember to:"
        if re.match(r'^[-*]\s', last):
            # Check if this is part of a Remember block
            idx = len(new_lines) - 1
            while idx >= 0 and re.match(r'^[-*]\s', new_lines[idx].strip()):
                idx -= 1
            if idx >= 0 and re.match(r'^Remember to:\s*$', new_lines[idx].strip(), re.IGNORECASE):
                removed_count = len(new_lines) - idx
                changes.append(f"REMOVED: Remember block ({removed_count} lines)")
                new_lines = new_lines[:idx]
                while new_lines and new_lines[-1].strip() == '':
                    new_lines.pop()
                continue
        break

    return '\n'.join(new_lines), changes


def fix_file(filepath, is_agent=False):
    with open(filepath, 'r') as f:
        content = f.read()

    basename = os.path.basename(filepath)
    changes = []

    # Skip certain files
    if basename in ('README.md',) and not is_agent:
        # README files in commands/ don't need frontmatter
        return f"SKIP_README: {filepath}", changes

    has_frontmatter = content.startswith('---')

    if has_frontmatter:
        parts = content.split('---', 2)
        if len(parts) < 3:
            return f"SKIP_BAD_FM: {filepath}", changes

        frontmatter = parts[1]
        body = parts[2]

        has_description = bool(re.search(r'^description\s*:', frontmatter, re.MULTILINE))

        # Add description if missing
        if not has_description:
            # Try to find H1 in body
            h1_match = re.search(r'^# (.+)$', body, re.MULTILINE)
            if h1_match:
                desc = extract_description_from_h1(h1_match.group(0))
            else:
                desc = filename_to_description(basename)
            frontmatter = frontmatter.rstrip('\n') + f'\ndescription: "{desc}"\n'
            changes.append(f"ADDED_DESC: {desc}")

        # Remove H1 from body
        new_body, h1_removed = remove_h1_from_body(body)
        if h1_removed:
            changes.append("REMOVED_H1")
            body = new_body

        # Remove noise
        body, noise_changes = remove_noise_from_content(body)
        changes.extend(noise_changes)

        if not changes:
            return f"OK: {filepath}", changes

        new_content = f'---{frontmatter}---{body}'

    else:
        # No frontmatter - need to create it
        # Find H1
        h1_match = re.match(r'^# (.+)\n', content)
        if h1_match:
            desc = extract_description_from_h1(h1_match.group(0))
            body = content[h1_match.end():]
            # Skip blank line after H1
            if body.startswith('\n'):
                body = body[1:]
        else:
            desc = filename_to_description(basename)
            body = content

        frontmatter = f'description: "{desc}"'
        changes.append(f"CREATED_FM: {desc}")

        # Remove any remaining H1 in body
        body, h1_removed = remove_h1_from_body(body)
        if h1_removed:
            changes.append("REMOVED_H1")

        # Remove noise
        body, noise_changes = remove_noise_from_content(body)
        changes.extend(noise_changes)

        new_content = f'---\n{frontmatter}\n---\n{body}'

    # Ensure single trailing newline
    new_content = new_content.rstrip('\n') + '\n'

    with open(filepath, 'w') as f:
        f.write(new_content)

    return f"FIXED: {filepath}", changes


def main():
    targets = sys.argv[1:] if len(sys.argv) > 1 else [
        'claude-home/commands',
        'claude-home/agents'
    ]

    stats = {'FIXED': 0, 'OK': 0, 'SKIP_README': 0, 'SKIP_BAD_FM': 0}

    for base_dir in targets:
        is_agent = 'agents' in base_dir
        for root, dirs, files in sorted(os.walk(base_dir)):
            for fname in sorted(files):
                if not fname.endswith('.md'):
                    continue
                filepath = os.path.join(root, fname)
                result, changes = fix_file(filepath, is_agent)
                key = result.split(':')[0]
                stats[key] = stats.get(key, 0) + 1
                if changes:
                    print(f"{result}")
                    for c in changes:
                        print(f"  {c}")

    print(f"\n=== Summary ===")
    for k, v in sorted(stats.items()):
        print(f"  {k}: {v}")


if __name__ == '__main__':
    main()
