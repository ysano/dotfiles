#!/usr/bin/env python3
"""Git worktree inventory and session-scoped tmux pane management."""
from concurrent.futures import ThreadPoolExecutor
from contextlib import contextmanager
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile
import threading

try:
    import fcntl
except ImportError:  # tmux is not native on Windows; keep inventory importable.
    fcntl = None


STATE_OPTION = "@agent_worktree_state"
MIN_WIDTH = 80
MIN_HEIGHT = 16
PROVIDERS = {"shell", "claude", "codex"}
SUCCESS_EVENTS = {"posttooluse", "aftertooluse", "toolcompleted"}
SHELL_TOOLS = {"bash", "execcommand"}
MAX_GIT_WORKERS = 8
GIT_ERRORS = (OSError, RuntimeError, subprocess.SubprocessError, ValueError)


def _run(argv, *, cwd=None, timeout=10):
    result = subprocess.run(
        argv, cwd=cwd, text=True, capture_output=True, timeout=timeout,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip() or "command failed"
        raise RuntimeError(f"{argv[0]}: {detail}")
    return result.stdout


def _git(root, *args, timeout=15):
    return _run(["git", "-C", str(root), *args], timeout=timeout)


def _tmux(*args):
    return _run(["tmux", *args], timeout=5).strip()


def _canonical(path, *, base=None):
    value = Path(path).expanduser()
    if not value.is_absolute():
        value = Path(base or os.getcwd()) / value
    return str(value.resolve(strict=False))


def _inside(child, parent):
    try:
        Path(child).resolve(strict=False).relative_to(Path(parent).resolve(strict=False))
        return True
    except ValueError:
        return False


def _common_git_dir(root):
    root_path = Path(root).expanduser().resolve(strict=False)
    output = _git(root_path, "rev-parse", "--git-common-dir").strip()
    common = Path(output)
    if not common.is_absolute():
        common = root_path / common
    return str(common.resolve(strict=False))


def _worker_count(jobs):
    return max(1, min(MAX_GIT_WORKERS, jobs))


class GitInventory:
    """1 回の snapshot 内で git の問い合わせ結果を registry と共有するメモ。

    寿命は呼び出し 1 回分に限る（モジュールに持ち越さない）。失敗も記憶し、
    どの呼び出し元にも同じ例外を返す。"""

    def __init__(self):
        self._common = {}
        self._records = {}
        self._lock = threading.Lock()

    @staticmethod
    def _key(root):
        return str(Path(root).expanduser().resolve(strict=False))

    def _memo(self, table, key, compute):
        with self._lock:
            known = key in table
        if not known:
            try:
                value = compute()
            except GIT_ERRORS as exc:
                value = exc
            with self._lock:
                table.setdefault(key, value)
        with self._lock:
            value = table[key]
        if isinstance(value, BaseException):
            raise value
        return value

    def common_git_dir(self, root):
        return self._memo(self._common, self._key(root), lambda: _common_git_dir(root))

    def worktree_records(self, root):
        common = self.common_git_dir(root)
        return self._memo(self._records, common, lambda: _parse_porcelain_z(
            _git(root, "worktree", "list", "--porcelain", "-z")))

    def _quiet(self, method, root):
        try:
            method(root)
        except GIT_ERRORS:
            pass

    def prefetch(self, roots):
        """未知の root の問い合わせを並列で先に済ませる。"""
        roots = [r for r in dict.fromkeys(roots) if isinstance(r, str) and r]
        with self._lock:
            pending = [r for r in roots if self._key(r) not in self._common]
        if pending:
            with ThreadPoolExecutor(max_workers=_worker_count(len(pending))) as pool:
                list(pool.map(self._quiet, [self.common_git_dir] * len(pending), pending))
        by_common = {}
        for root in roots:
            try:
                by_common.setdefault(self.common_git_dir(root), root)
            except GIT_ERRORS:
                continue
        with self._lock:
            pending = [root for common, root in by_common.items() if common not in self._records]
        if pending:
            with ThreadPoolExecutor(max_workers=_worker_count(len(pending))) as pool:
                list(pool.map(self._quiet, [self.worktree_records] * len(pending), pending))


def _parse_porcelain_z(output):
    records = []
    current = {}
    for field in output.split("\0"):
        if not field:
            if current:
                records.append(current)
                current = {}
            continue
        key, separator, value = field.partition(" ")
        current[key] = value if separator else True
    if current:
        records.append(current)
    return records


def _is_temporary(path):
    parts = Path(path).parts
    return any(
        parts[index] in {".claude", ".codex", ".agents"}
        and parts[index + 1] == "worktrees"
        for index in range(len(parts) - 1)
    )


def _state_actor_locations(value):
    if isinstance(value, str):
        try:
            value = json.loads(value) if value else {}
        except json.JSONDecodeError:
            return []
    if not isinstance(value, dict):
        return []
    actors = value.get("actors", {})
    if not isinstance(actors, dict):
        return []
    locations = []
    for actor in actors.values():
        if not isinstance(actor, dict) or actor.get("active") is not True:
            continue
        cwd = actor.get("cwd") or value.get("cwd")
        if isinstance(cwd, str) and cwd:
            locations.append(cwd)
    return locations


def _pane_locations(pane):
    locations = []
    cwd = pane.get("cwd")
    if isinstance(cwd, str) and cwd:
        locations.append(cwd)
    for provider in ("claude", "codex"):
        locations.extend(_state_actor_locations(pane.get(provider + "_state")))
    return list(dict.fromkeys(locations))


def _inventory(roots, panes, inventory=None):
    inventory = inventory or GitInventory()
    inventory.prefetch(roots)
    repos = []
    seen_repos = set()
    for root in roots:
        if not isinstance(root, str) or not root:
            continue
        try:
            common = inventory.common_git_dir(root)
            if common in seen_repos:
                continue
            records = inventory.worktree_records(root)
        except GIT_ERRORS:
            continue
        seen_repos.add(common)
        repo_rows = []
        for record in records:
            path = record.get("worktree")
            if not isinstance(path, str) or not path:
                continue
            canonical = _canonical(path)
            branch_ref = record.get("branch")
            if isinstance(branch_ref, str) and branch_ref.startswith("refs/heads/"):
                branch = branch_ref[len("refs/heads/"):]
            elif record.get("detached"):
                branch = "(detached)"
            elif record.get("bare"):
                branch = "(bare)"
            else:
                branch = ""
            repo_rows.append({
                "id": canonical,
                "path": canonical,
                "repo": common,
                "branch": branch,
                "temporary": _is_temporary(canonical),
                "panes": [],
                "status": "available",
                "auto_reason": "",
                "_locked": bool(record.get("locked")),
                "_prunable": bool(record.get("prunable")),
            })
        repos.extend(repo_rows)

    # Match the deepest registered worktree, so a nested worktree never gets
    # attributed to a parent checkout that happens to contain it.
    for pane in panes:
        pane_id = pane.get("pane_id")
        if not isinstance(pane_id, str):
            continue
        for cwd in _pane_locations(pane):
            matches = [row for row in repos if _inside(cwd, row["path"])]
            if matches:
                match = max(matches, key=lambda row: len(Path(row["path"]).parts))
                if pane_id not in match["panes"]:
                    match["panes"].append(pane_id)

    for row in repos:
        row["panes"].sort()
        if row["temporary"]:
            row["status"] = "temporary"
        elif row["_prunable"]:
            row["status"] = "prunable"
        elif row["_locked"]:
            row["status"] = "locked"
        elif row["panes"]:
            row["status"] = "open"
    return repos


def _empty_state():
    return {
        "version": 1,
        "auto": False,
        "baseline": [],
        "candidates": {},
        "managed": {},
        "dismissed": [],
        "reasons": {},
    }


def _normalize_state(value):
    state = _empty_state()
    if not isinstance(value, dict):
        return state
    state["auto"] = bool(value.get("auto", False))
    for key in ("baseline", "dismissed"):
        items = value.get(key, [])
        if isinstance(items, list):
            state[key] = [item for item in items if isinstance(item, str)]
    for key in ("candidates", "managed", "reasons"):
        items = value.get(key, {})
        if isinstance(items, dict):
            state[key] = items
    return state


def _state(session):
    raw = _tmux("show-options", "-qv", "-t", session, STATE_OPTION)
    if not raw:
        return _empty_state()
    try:
        return _normalize_state(json.loads(raw))
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"invalid {STATE_OPTION} for session {session}") from exc


def _save_state(session, state):
    payload = json.dumps(state, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    _tmux("set-option", "-t", session, STATE_OPTION, payload)


def _server_identity():
    try:
        return _tmux("display-message", "-p", "#{socket_path}")
    except (OSError, RuntimeError, subprocess.TimeoutExpired):
        return os.environ.get("TMUX", "no-tmux").split(",", 1)[0]


@contextmanager
def _session_lock(session):
    identity = f"{_server_identity()}\0{session}"
    digest = hashlib.sha256(identity.encode()).hexdigest()
    uid = getattr(os, "getuid", lambda: 0)()
    lock_root = Path(tempfile.gettempdir()) / f"tmux-agent-worktrees-{uid}"
    lock_root.mkdir(mode=0o700, exist_ok=True)
    if lock_root.is_symlink() or (hasattr(lock_root.stat(), "st_uid") and lock_root.stat().st_uid != uid):
        raise RuntimeError("unsafe worktree state lock directory")
    with (lock_root / digest).open("a+") as handle:
        if fcntl is not None:
            fcntl.flock(handle, fcntl.LOCK_EX)
        try:
            yield
        finally:
            if fcntl is not None:
                fcntl.flock(handle, fcntl.LOCK_UN)


def _states_for_panes(panes):
    states = []
    sessions = {pane.get("session_id") for pane in panes if pane.get("session_id")}
    for session in sessions:
        try:
            states.append(_state(session))
        except (OSError, RuntimeError, subprocess.TimeoutExpired):
            continue
    return states


def _state_roots(state):
    roots = []
    for key in ("baseline", "dismissed"):
        roots.extend(state[key])
    for key in ("candidates", "managed", "reasons"):
        roots.extend(state[key])
    for candidate in state["candidates"].values():
        if isinstance(candidate, dict) and isinstance(candidate.get("root"), str):
            roots.append(candidate["root"])
    return roots


def list_worktrees(roots: list[str], panes: list[dict], inventory=None) -> list[dict]:
    """List unique repositories' worktrees using canonical, stable paths."""
    states = _states_for_panes(panes)
    discovery_roots = list(roots)
    for state in states:
        discovery_roots.extend(_state_roots(state))
    rows = _inventory(discovery_roots, panes, inventory)
    for row in rows:
        path = row["path"]
        if row["temporary"]:
            row["auto_reason"] = "temporary agent worktree; automatic opening excluded"
        else:
            for state in states:
                reason = state["reasons"].get(path)
                if isinstance(reason, str) and reason:
                    row["auto_reason"] = reason
                    break
                if path in state["dismissed"]:
                    row["auto_reason"] = "closed manually; automatic reopening suppressed"
                    break
                if path in state["baseline"]:
                    row["auto_reason"] = "baseline: existed when automatic opening was enabled"
                    break
                if path in state["candidates"] and not state["auto"]:
                    row["auto_reason"] = "automatic opening is disabled"
                    break
                if state["auto"] and path not in state["candidates"]:
                    row["auto_reason"] = "unknown ownership; automatic opening excluded"
                    break
        row.pop("_locked", None)
        row.pop("_prunable", None)
    return rows


def set_auto(session: str, enabled: bool, panes: list[dict]) -> None:
    """Set automatic opening for one immutable tmux session ID."""
    with _session_lock(session):
        state = _state(session)
        if enabled and not state["auto"]:
            roots = [pane["cwd"] for pane in panes if pane.get("session_id") == session and pane.get("cwd")]
            roots.extend(_state_roots(state))
            state["baseline"] = [row["path"] for row in _inventory(roots, panes)]
            state["candidates"] = {}
            state["reasons"] = {
                path: reason for path, reason in state["reasons"].items()
                if path in state["dismissed"]
            }
        state["auto"] = bool(enabled)
        _save_state(session, state)


def _candidate_roots(state, session_panes):
    roots = [pane["cwd"] for pane in session_panes if pane.get("cwd")]
    # Candidate existence is intentionally checked by poll, never by the hook.
    # State paths keep repositories discoverable after their last pane closes.
    roots.extend(_state_roots(state))
    return roots


def poll_session(session: str, panes: list[dict]) -> None:
    """Verify attributed candidates and open at most one background shell pane."""
    session_panes = [pane for pane in panes if pane.get("session_id") == session]
    pane_ids = {pane.get("pane_id") for pane in session_panes}
    with _session_lock(session):
        state = _state(session)
        for path, pane_id in list(state["managed"].items()):
            if pane_id not in pane_ids:
                state["managed"].pop(path, None)
                if path not in state["dismissed"]:
                    state["dismissed"].append(path)
                state["reasons"][path] = "closed manually; automatic reopening suppressed"

        if not state["auto"]:
            _save_state(session, state)
            return

        inventory = _inventory(_candidate_roots(state, session_panes), panes)
        by_path = {row["path"]: row for row in inventory}
        managed_open = bool(state["managed"])
        opened_this_poll = False
        for path, candidate in list(state["candidates"].items()):
            row = by_path.get(path)
            if row is None:
                # Attribution is for one concrete creation. Once that
                # registration disappears, the same canonical path must not
                # inherit authority if it is reused by a later operation.
                state["candidates"].pop(path, None)
                state["reasons"].pop(path, None)
                continue
            candidate_root = candidate.get("root") if isinstance(candidate, dict) else None
            try:
                candidate_repo = _common_git_dir(candidate_root) if candidate_root else ""
            except (OSError, RuntimeError, subprocess.TimeoutExpired):
                candidate_repo = ""
            if candidate_repo != row["repo"]:
                state["candidates"].pop(path, None)
                state["reasons"].pop(path, None)
                continue
            if row["temporary"]:
                state["reasons"][path] = "temporary agent worktree; automatic opening excluded"
                state["candidates"].pop(path, None)
                continue
            if path in state["baseline"]:
                state["reasons"][path] = "baseline: existed when automatic opening was enabled"
                state["candidates"].pop(path, None)
                continue
            if path in state["dismissed"]:
                state["reasons"][path] = "closed manually; automatic reopening suppressed"
                state["candidates"].pop(path, None)
                continue
            if row["panes"]:
                state["reasons"][path] = "existing pane reused; duplicate automatic pane suppressed"
                state["candidates"].pop(path, None)
                continue
            if managed_open or opened_this_poll:
                state["reasons"][path] = "automatic pane already open in this session"
                continue

            origin = candidate.get("origin") if isinstance(candidate, dict) else None
            origin_pane = next((pane for pane in session_panes if pane.get("pane_id") == origin), None)
            if origin_pane is None:
                state["reasons"][path] = "origin pane is no longer available"
                continue
            width = int(origin_pane.get("width", 0))
            height = int(origin_pane.get("height", 0))
            if width >= MIN_WIDTH and height >= (MIN_HEIGHT * 2 + 1):
                orientation = "-v"
            elif width >= (MIN_WIDTH * 2 + 1) and height >= MIN_HEIGHT:
                orientation = "-h"
            else:
                state["reasons"][path] = (
                    f"origin pane too small; automatic split requires {MIN_WIDTH}x{MIN_HEIGHT} "
                    "for each resulting pane"
                )
                continue

            new_pane = _tmux(
                "split-window", "-d", orientation, "-t", origin, "-c", path,
                "-P", "-F", "#{pane_id}",
            )
            state["managed"][path] = new_pane
            state["candidates"].pop(path, None)
            state["reasons"][path] = "opened automatically"
            managed_open = True
            opened_this_poll = True
        _save_state(session, state)


def _event_succeeded(event):
    name = re.sub(r"[^a-z]", "", str(event.get("hook_event_name", "")).lower())
    if name not in SUCCESS_EVENTS:
        return False
    tool = re.sub(r"[^a-z]", "", str(event.get("tool_name", "")).lower())
    if tool not in SHELL_TOOLS:
        return False
    explicit_success = False
    for response_key in ("tool_response", "response", "result"):
        response = event.get(response_key)
        if isinstance(response, dict):
            if (response.get("is_error") is True or response.get("success") is False
                    or response.get("interrupted") is True):
                return False
            if response.get("success") is True:
                explicit_success = True
            # Claude Bash PostToolUse is itself the success event. Its official
            # response has no exit code, but does report interruption status.
            if tool == "bash" and response.get("interrupted") is False:
                explicit_success = True
            code = response.get("exit_code", response.get("exitCode"))
            if code is not None:
                if str(code) != "0":
                    return False
                explicit_success = True
    code = event.get("exit_code", event.get("exitCode"))
    if code is not None:
        return str(code) == "0"
    return explicit_success


def _event_command(event):
    for container_key in ("tool_input", "input", "arguments"):
        container = event.get(container_key)
        if isinstance(container, dict):
            for command_key in ("command", "cmd"):
                if isinstance(container.get(command_key), str):
                    return container[command_key]
    for command_key in ("command", "cmd"):
        if isinstance(event.get(command_key), str):
            return event[command_key]
    return ""


def _worktree_add_path(command, cwd):
    if "\n" in command or "\r" in command:
        return None
    try:
        lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|")
        lexer.whitespace_split = True
        lexer.commenters = ""
        tokens = list(lexer)
    except ValueError:
        return None
    operators = {"&", "&&", "||", ";", "|"}
    if not tokens or Path(tokens[0]).name != "git" or any(token in operators for token in tokens):
        return None
    position = 1
    git_cwd = cwd
    while position < len(tokens) and tokens[position] != "worktree":
        if tokens[position] in operators:
            return None
        if tokens[position] == "-C" and position + 1 < len(tokens):
            git_cwd = _canonical(tokens[position + 1], base=git_cwd)
            position += 2
        elif tokens[position].startswith("-C") and len(tokens[position]) > 2:
            git_cwd = _canonical(tokens[position][2:], base=git_cwd)
            position += 1
        elif tokens[position] in {"-c", "--git-dir", "--work-tree"}:
            position += 2
        else:
            position += 1
    if position + 1 >= len(tokens) or tokens[position:position + 2] != ["worktree", "add"]:
        return None
    position += 2
    consumes = {"-b", "-B", "--reason"}
    while position < len(tokens):
        token = tokens[position]
        if token in operators:
            break
        if token == "--":
            position += 1
            break
        if token in consumes:
            position += 2
            continue
        if token.startswith("--reason="):
            position += 1
            continue
        if token.startswith("-"):
            position += 1
            continue
        return _canonical(token, base=git_cwd), _canonical(git_cwd)
    if position < len(tokens) and tokens[position] not in operators:
        return _canonical(tokens[position], base=git_cwd), _canonical(git_cwd)
    return None


def _record_candidate(session, path, origin, root, source):
    with _session_lock(session):
        state = _state(session)
        canonical = _canonical(path, base=root)
        state["candidates"][canonical] = {
            "origin": origin,
            "root": _canonical(root),
            "source": source,
        }
        state["reasons"].pop(canonical, None)
        _save_state(session, state)


def observe_hook(event: dict, pane: str) -> None:
    """Queue an explicit successful `git worktree add` without scanning git."""
    if not isinstance(event, dict) or event.get("agent_id") or not _event_succeeded(event):
        return
    command = _event_command(event)
    if not command:
        return
    try:
        identity = _tmux(
            "display-message", "-p", "-t", pane,
            "#{session_id}\t#{pane_current_path}",
        )
        session, cwd = identity.split("\t", 1)
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired):
        return
    parsed = _worktree_add_path(command, cwd)
    if parsed is None:
        return
    path, git_cwd = parsed
    _record_candidate(session, path, pane, git_cwd, "hook")


def create_worktree(root: str, name: str, base: str, session: str, origin: str) -> str:
    """Create an external linked worktree and attribute it to the requesting UI."""
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]*", name or ""):
        raise ValueError("worktree name must contain only letters, digits, dot, underscore, or hyphen")
    if not isinstance(base, str) or not base:
        raise ValueError("base revision is required")
    rows = _inventory([root], [])
    if not rows:
        raise RuntimeError(f"not a git worktree: {root}")
    try:
        commit = _git(
            root, "rev-parse", "--verify", "--end-of-options",
            f"{base}^{{commit}}",
        ).strip()
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as exc:
        raise ValueError(f"invalid base revision: {base}") from exc
    if not re.fullmatch(r"[0-9a-fA-F]{40,64}", commit):
        raise ValueError(f"invalid base revision: {base}")
    main = Path(rows[0]["path"])
    destination = (main.parent / "worktrees" / f"{main.name}-{name}").resolve(strict=False)
    if destination.exists():
        raise RuntimeError(f"worktree path already exists: {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    _git(root, "worktree", "add", "-b", f"worktree-{name}", str(destination), commit, timeout=60)
    try:
        _record_candidate(session, str(destination), origin, str(Path(root).resolve()), "ui")
    except (OSError, RuntimeError, subprocess.TimeoutExpired):
        # Git creation has succeeded and must not be reported as an atomic failure.
        # A caller without a live tmux server can still use this API safely.
        pass
    return str(destination)


def _all_tmux_locations():
    output = _tmux(
        "list-panes", "-a", "-F",
        "#{pane_id}\t#{pane_current_path}\t#{@claude_state}\t#{@codex_state}",
    )
    locations = []
    for line in output.splitlines():
        values = line.split("\t", 3)
        values += [""] * (4 - len(values))
        pane_id, cwd, claude_raw, codex_raw = values
        pane = {"cwd": cwd, "claude_state": claude_raw, "codex_state": codex_raw}
        locations.extend((pane_id, path) for path in _pane_locations(pane))
    return locations


def remove_worktree(path: str) -> None:
    """Remove a clean, unused linked worktree without force or branch deletion."""
    canonical = _canonical(path)
    rows = _inventory([canonical], [])
    row = next((item for item in rows if item["path"] == canonical), None)
    if row is None:
        raise RuntimeError(f"not a registered worktree: {canonical}")
    if rows and rows[0]["path"] == canonical:
        raise RuntimeError("refusing to remove the main worktree")
    dirty = _git(canonical, "status", "--porcelain", "-z", "--untracked-files=all")
    if dirty:
        raise RuntimeError("worktree is dirty or has uncommitted files")
    users = sorted({
        pane_id for pane_id, cwd in _all_tmux_locations()
        if _inside(cwd, canonical)
    })
    if users:
        raise RuntimeError(f"worktree is in use by tmux pane(s): {', '.join(users)}")
    _git(rows[0]["path"], "worktree", "remove", canonical, timeout=60)


def _session_panes(session):
    output = _tmux(
        "list-panes", "-s", "-t", session, "-F",
        "#{pane_id}\t#{session_id}\t#{pane_current_path}\t#{@claude_state}\t#{@codex_state}",
    )
    panes = []
    for line in output.splitlines():
        parts = line.split("\t", 4)
        parts += [""] * (5 - len(parts))
        panes.append({
            "pane_id": parts[0], "session_id": parts[1], "cwd": parts[2],
            "claude_state": parts[3], "codex_state": parts[4],
        })
    return panes


def open_worktree(path: str, session: str, origin: str, provider: str = "shell") -> str:
    """Focus an existing pane or explicitly launch a provider in a new pane."""
    if provider not in PROVIDERS:
        raise ValueError(f"unknown provider: {provider}")
    canonical = _canonical(path)
    panes = _session_panes(session)
    rows = _inventory([canonical], panes)
    target = next((row for row in rows if row["path"] == canonical), None)
    if target is None:
        raise RuntimeError(f"not a registered worktree: {canonical}")
    existing_ids = set(target["panes"])
    existing = next((pane for pane in panes if pane["pane_id"] in existing_ids), None)
    if existing and provider == "shell":
        _tmux("select-pane", "-t", existing["pane_id"])
        return existing["pane_id"]
    origin_session = _tmux("display-message", "-p", "-t", origin, "#{session_id}")
    if origin_session != session:
        raise RuntimeError("origin pane does not belong to the requested session")
    shell = os.environ.get("SHELL") or "/bin/sh"
    executable = shell if provider == "shell" else provider
    resolved = shutil.which(executable)
    if resolved is None:
        raise RuntimeError(f"{provider} executable not found: {executable}")
    command = "exec " + shlex.join([resolved])
    return _tmux(
        "split-window", "-t", origin, "-c", canonical,
        "-P", "-F", "#{pane_id}", command,
    )
