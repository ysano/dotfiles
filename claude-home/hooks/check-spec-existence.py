#!/usr/bin/env python3
"""
Spec/Plan existence checker for AI-DLC workflow.
PreToolUse hook: verifies that a planning document exists before code modifications.
Prompts for confirmation (ask) rather than blocking, to allow spikes and prototypes.
"""
import json
import os
import subprocess
import sys

# Code file extensions that require spec verification
CODE_EXTENSIONS = {
    ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs",
    ".py", ".rs", ".go", ".java", ".kt", ".swift",
    ".svelte", ".vue", ".rb", ".php", ".cs", ".cpp", ".c", ".h",
}

# Paths that are always skipped (tests, config, docs, generated)
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

# Plan/spec file candidates to search for
PLAN_FILES = [
    "PLAN.md",
    "plan.md",
    "SPEC.md",
    "spec.md",
]

# Directories to check for planning artifacts
PLAN_DIRS = [
    ".claude",
    ".steering",
    "docs",
]


def is_code_file(file_path):
    """Check if the file is a code file that requires spec verification."""
    _, ext = os.path.splitext(file_path)
    return ext.lower() in CODE_EXTENSIONS


def should_skip(file_path):
    """Check if the file path matches any skip pattern."""
    lower_path = file_path.lower()
    return any(pattern in lower_path for pattern in SKIP_PATTERNS)


def find_project_root():
    """Find the project root (git root or CLAUDE_PROJECT_DIR)."""
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR")
    if project_dir and os.path.isdir(project_dir):
        return project_dir

    try:
        result = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True, text=True, timeout=3
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    return os.getcwd()


def has_plan_file(root):
    """Check if any plan/spec file exists in the project."""
    for name in PLAN_FILES:
        if os.path.isfile(os.path.join(root, name)):
            return True

    for dir_name in PLAN_DIRS:
        dir_path = os.path.join(root, dir_name)
        if not os.path.isdir(dir_path):
            continue
        try:
            for entry in os.listdir(dir_path):
                lower = entry.lower()
                if "plan" in lower or "spec" in lower:
                    if os.path.isfile(os.path.join(dir_path, entry)):
                        return True
        except OSError:
            continue

    return False


def has_issue_branch():
    """Check if the current branch name suggests an associated issue."""
    try:
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True, text=True, timeout=3
        )
        if result.returncode == 0:
            branch = result.stdout.strip()
            # Patterns: feature/123-xxx, fix/GH-123, issue-123, etc.
            import re
            if re.search(r'(\d{2,}|GH-\d+|issue-\d+)', branch):
                return True
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    return False


def main():
    try:
        input_data = json.load(sys.stdin)
    except json.JSONDecodeError:
        sys.exit(0)

    tool_input = input_data.get("tool_input", {})
    file_path = tool_input.get("file_path", "")

    if not file_path:
        sys.exit(0)

    # Skip non-code files
    if not is_code_file(file_path):
        print(json.dumps({"suppressOutput": True}))
        sys.exit(0)

    # Skip test/config/doc files
    if should_skip(file_path):
        print(json.dumps({"suppressOutput": True}))
        sys.exit(0)

    # Check for plan/spec existence
    root = find_project_root()

    if has_plan_file(root) or has_issue_branch():
        print(json.dumps({"suppressOutput": True}))
        sys.exit(0)

    # No plan/spec found - ask for confirmation
    output = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "ask",
            "permissionDecisionReason": (
                "Spec/Plan が見つかりません。"
                "Atomic Spec なしでコード変更を続けますか？\n"
                f"対象: {file_path}\n"
                "ヒント: PLAN.md を作成するか、Issue 番号を含むブランチ名に切り替えてください"
            ),
        }
    }
    print(json.dumps(output))
    sys.exit(0)


if __name__ == "__main__":
    main()
