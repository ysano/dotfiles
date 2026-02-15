#!/usr/bin/env python3
"""
Spec/Plan existence and quality checker for AI-DLC workflow.
PreToolUse hook: verifies that a planning document exists before code modifications,
and scores its quality against Atomic Spec 5 elements (G4).
Prompts for confirmation (ask) rather than blocking, to allow spikes and prototypes.
"""
import hashlib
import json
import os
import re
import subprocess
import sys
import time

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

# Atomic Spec 5 elements with regex patterns
ATOMIC_SPEC_ELEMENTS = {
    "Context": re.compile(
        r"(\*\*Context\*\*\s*:|#{2,4}\s*Context|####\s*Context)", re.IGNORECASE
    ),
    "Current Behavior": re.compile(
        r"(\*\*Current\s+Behavior\*\*\s*:|#{2,4}\s*Current\s+Behavior)", re.IGNORECASE
    ),
    "Expected Behavior": re.compile(
        r"(\*\*Expected\s+Behavior\*\*\s*:|#{2,4}\s*Expected\s+Behavior)", re.IGNORECASE
    ),
    "Constraints": re.compile(
        r"(\*\*Constraints?\*\*\s*:|#{2,4}\s*Constraints?)", re.IGNORECASE
    ),
    "Verification": re.compile(
        r"(\*\*Verification\*\*\s*:|#{2,4}\s*Verification)", re.IGNORECASE
    ),
}

CACHE_DIR = "/tmp/claude-spec-quality-cache"


def is_code_file(file_path):
    """Check if the file is a code file that requires spec verification."""
    _, ext = os.path.splitext(file_path)
    return ext.lower() in CODE_EXTENSIONS


def should_skip(file_path):
    """Check if the file path matches any skip pattern."""
    lower_path = file_path.lower()
    return any(pattern in lower_path for pattern in SKIP_PATTERNS)


def find_project_root(file_path=None):
    """Find the project root from file_path's git root, CLAUDE_PROJECT_DIR, or cwd.

    Priority: file_path git root > CLAUDE_PROJECT_DIR > cwd git root > cwd.
    The file_path-based detection fixes #17: when editing files in a different
    repo than the Claude Code session directory.
    """
    # First: detect git root from file_path (most accurate)
    if file_path:
        try:
            dir_path = os.path.dirname(os.path.abspath(file_path))
            result = subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True, text=True, timeout=3,
                cwd=dir_path,
            )
            if result.returncode == 0:
                return result.stdout.strip()
        except (subprocess.TimeoutExpired, FileNotFoundError, OSError):
            pass

    # Fallback: CLAUDE_PROJECT_DIR
    project_dir = os.environ.get("CLAUDE_PROJECT_DIR")
    if project_dir and os.path.isdir(project_dir):
        return project_dir

    # Fallback: cwd git root
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
    """Check if any plan/spec file exists in the project. Returns (found, path|None)."""
    for name in PLAN_FILES:
        full_path = os.path.join(root, name)
        if os.path.isfile(full_path):
            return True, full_path

    for dir_name in PLAN_DIRS:
        dir_path = os.path.join(root, dir_name)
        if not os.path.isdir(dir_path):
            continue
        try:
            for entry in os.listdir(dir_path):
                lower = entry.lower()
                if "plan" in lower or "spec" in lower:
                    full_path = os.path.join(dir_path, entry)
                    if os.path.isfile(full_path):
                        return True, full_path
        except OSError:
            continue

    return False, None


def has_issue_branch():
    """Check if the current branch name suggests an associated issue."""
    try:
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True, text=True, timeout=3
        )
        if result.returncode == 0:
            branch = result.stdout.strip()
            if re.search(r'(\d{2,}|GH-\d+|issue-\d+)', branch):
                return True
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    return False


def get_project_name(root):
    """Derive a safe project name from the root path for cache keying."""
    return hashlib.md5(root.encode()).hexdigest()[:12]


def get_cached_score(cache_path, spec_path):
    """Return cached (score, missing) if spec mtime unchanged, else None."""
    try:
        if not os.path.isfile(cache_path):
            return None
        with open(cache_path, "r") as f:
            cache = json.load(f)
        entry = cache.get(spec_path)
        if not entry:
            return None
        cached_mtime = entry.get("mtime", 0)
        current_mtime = os.path.getmtime(spec_path)
        if abs(cached_mtime - current_mtime) < 0.01:
            return entry["score"], entry["missing"]
    except (OSError, json.JSONDecodeError, KeyError):
        pass
    return None


def update_cache_score(cache_path, spec_path, score, missing):
    """Write score to cache file."""
    try:
        os.makedirs(os.path.dirname(cache_path), exist_ok=True)
        cache = {}
        if os.path.isfile(cache_path):
            with open(cache_path, "r") as f:
                cache = json.load(f)
        cache[spec_path] = {
            "score": score,
            "missing": missing,
            "mtime": os.path.getmtime(spec_path),
            "scored_at": time.time(),
        }
        with open(cache_path, "w") as f:
            json.dump(cache, f)
    except (OSError, json.JSONDecodeError):
        pass


def score_spec_quality(spec_path):
    """Score a spec file against Atomic Spec 5 elements. Returns (score: 0-5, missing: list)."""
    try:
        with open(spec_path, "r") as f:
            content = f.read()
    except OSError:
        return 0, list(ATOMIC_SPEC_ELEMENTS.keys())

    found = []
    missing = []
    for element, pattern in ATOMIC_SPEC_ELEMENTS.items():
        if pattern.search(content):
            found.append(element)
        else:
            missing.append(element)

    return len(found), missing


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

    # Check for plan/spec existence (file_path-based detection for #17)
    root = find_project_root(file_path)
    found, spec_path = has_plan_file(root)

    if found and spec_path:
        # Spec exists - score its quality (G4)
        project_name = get_project_name(root)
        cache_path = os.path.join(CACHE_DIR, f"{project_name}_scores.json")

        cached = get_cached_score(cache_path, spec_path)
        if cached:
            score, missing = cached
        else:
            score, missing = score_spec_quality(spec_path)
            update_cache_score(cache_path, spec_path, score, missing)

        if score >= 3:
            print(json.dumps({"suppressOutput": True}))
            sys.exit(0)

        # Low quality spec - warn with missing elements
        missing_str = ", ".join(missing)
        output = {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "ask",
                "permissionDecisionReason": (
                    f"Spec 品質スコア: {score}/5 (不足: {missing_str})\n"
                    f"ファイル: {os.path.basename(spec_path)}\n"
                    f"対象: {file_path}\n"
                    "Atomic Spec 5 要素を補完してください。"
                    " `/ai-dlc:refine` でバックログ精査を実行できます"
                ),
            }
        }
        print(json.dumps(output))
        sys.exit(0)

    if has_issue_branch():
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
