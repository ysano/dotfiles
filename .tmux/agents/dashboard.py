#!/usr/bin/env python3
"""現在の tmux session に属する agent/worktree を表示する popup UI。"""
import argparse
import os
import queue
import sys
import threading
import time
import unicodedata
from pathlib import Path
import subprocess
from typing import NamedTuple


HERE = Path(__file__).resolve().parent


class Row(NamedTuple):
    id: str
    kind: str
    depth: int
    data: dict
    waiting: int = 0
    expandable: bool = False


class Decision(NamedTuple):
    kind: str
    target: str


WAITING_STATES = {"permission", "waiting", "question", "approval", "承認待ち", "質問待ち"}
COMPLETED_STATES = {"idle", "complete", "completed", "done", "終了", "完了"}


def _is_waiting(agent):
    value = agent.get("own_status", agent.get("status", ""))
    return str(value).strip().lower() in WAITING_STATES


def _is_completed(agent):
    return str(agent.get("status", "")).strip().lower() in COMPLETED_STATES


class DashboardModel:
    """snapshot から安定した可視行を作る、curses 非依存の状態モデル。"""

    def __init__(self, snapshot):
        self.snapshot = snapshot
        self.view = "agents"
        self._expanded = {"agents": set(), "worktrees": set()}
        self._known = {"agents": set(), "worktrees": set()}
        self._order = {"agents": {}, "worktrees": {}}
        self._next_order = {"agents": 0, "worktrees": 0}
        self._selected = {"agents": "", "worktrees": ""}
        self.selected_id = ""
        self.rows = []
        self._rebuild(initial=True)

    @property
    def expanded(self):
        return self._expanded[self.view]

    def _remember_order(self, ids):
        order = self._order[self.view]
        for row_id in ids:
            if row_id not in order:
                order[row_id] = self._next_order[self.view]
                self._next_order[self.view] += 1

    def _agent_structure(self):
        repos = list(self.snapshot.get("repos", []))
        # registry.snapshot が tmux session で scope 済み。agent の session_id は
        # Claude/Codex の会話IDなので tmux session ID と比較してはならない。
        agents = list(self.snapshot.get("agents", []))
        repo_ids = {repo.get("id") for repo in repos}
        for agent in agents:
            repo_id = agent.get("repo_id")
            if repo_id and repo_id not in repo_ids:
                repos.append({"id": repo_id, "name": repo_id, "path": agent.get("cwd", "")})
                repo_ids.add(repo_id)
        agent_by_id = {agent.get("id"): agent for agent in agents if agent.get("id")}
        children = {}
        parent_of = {}
        for agent in agents:
            agent_id = agent.get("id")
            parent = agent.get("parent_id", "")
            if parent not in agent_by_id or agent_by_id[parent].get("repo_id") != agent.get("repo_id"):
                parent = ""
            children.setdefault((agent.get("repo_id"), parent), []).append(agent)
            parent_of["agent:" + agent_id] = ("agent:" + parent if parent else
                                               "repo:" + str(agent.get("repo_id", "")))
        return repos, agents, children, parent_of

    def _all_agent_ids(self, repos, agents):
        return (["repo:" + str(repo.get("id", "")) for repo in repos] +
                ["agent:" + str(agent.get("id", "")) for agent in agents])

    def _waiting_counts(self, agents, children):
        memo = {}

        def total(agent):
            agent_id = agent.get("id")
            if agent_id in memo:
                return memo[agent_id]
            count = int(_is_waiting(agent))
            for child in children.get((agent.get("repo_id"), agent_id), []):
                count += total(child)
            memo[agent_id] = count
            return count

        for agent in agents:
            total(agent)
        return memo

    def _agent_rows(self):
        repos, agents, children, _ = self._agent_structure()
        order = self._order["agents"]
        waiting = self._waiting_counts(agents, children)
        rows = []
        for repo in sorted(repos, key=lambda item: order["repo:" + str(item.get("id", ""))]):
            repo_id = repo.get("id", "")
            roots = children.get((repo_id, ""), [])
            repo_waiting = sum(waiting.get(agent.get("id"), 0) for agent in roots)
            repo_row_id = "repo:" + str(repo_id)
            rows.append(Row(repo_row_id, "repo", 0, repo, repo_waiting, bool(roots)))
            if repo_row_id not in self.expanded:
                continue

            def append_agent(agent, depth):
                row_id = "agent:" + agent["id"]
                descendant_waiting = waiting.get(agent["id"], 0) - int(_is_waiting(agent))
                nested = children.get((agent.get("repo_id"), agent["id"]), [])
                rows.append(Row(row_id, "agent", depth, agent,
                                descendant_waiting, bool(nested)))
                if row_id not in self.expanded:
                    return
                for child in sorted(nested, key=lambda item: order["agent:" + item["id"]]):
                    append_agent(child, depth + 1)

            for agent in sorted(roots, key=lambda item: order["agent:" + item["id"]]):
                append_agent(agent, 1)
        return rows

    def _worktree_structure(self):
        repos = list(self.snapshot.get("repos", []))
        worktrees = list(self.snapshot.get("worktrees", []))
        repo_ids = {repo.get("id") for repo in repos}
        for worktree in worktrees:
            repo_id = worktree.get("repo", "")
            if repo_id not in repo_ids:
                repos.append({"id": repo_id, "name": repo_id, "path": ""})
                repo_ids.add(repo_id)
        parent_of = {"worktree:" + str(item.get("id", item.get("path", ""))):
                     "repo:" + str(item.get("repo", "")) for item in worktrees}
        return repos, worktrees, parent_of

    def _worktree_rows(self):
        repos, worktrees, _ = self._worktree_structure()
        order = self._order["worktrees"]
        grouped = {}
        for worktree in worktrees:
            grouped.setdefault(worktree.get("repo", ""), []).append(worktree)
        rows = []
        for repo in sorted(repos, key=lambda item: order["repo:" + str(item.get("id", ""))]):
            repo_id = repo.get("id", "")
            repo_row_id = "repo:" + str(repo_id)
            items = grouped.get(repo_id, [])
            rows.append(Row(repo_row_id, "repo", 0, repo, 0, bool(items)))
            if repo_row_id not in self.expanded:
                continue
            for item in sorted(items, key=lambda value: order[
                    "worktree:" + str(value.get("id", value.get("path", "")))]):
                item_id = str(item.get("id", item.get("path", "")))
                rows.append(Row("worktree:" + item_id, "worktree", 1, item))
        return rows

    def _structure(self):
        if self.view == "agents":
            repos, agents, _, parents = self._agent_structure()
            ids = self._all_agent_ids(repos, agents)
        else:
            repos, worktrees, parents = self._worktree_structure()
            ids = (["repo:" + str(repo.get("id", "")) for repo in repos] +
                   ["worktree:" + str(item.get("id", item.get("path", "")))
                    for item in worktrees])
        return ids, parents

    def _rebuild(self, initial=False):
        ids, parents = self._structure()
        self._remember_order(ids)
        new_ids = set(ids) - self._known[self.view]
        if initial or new_ids:
            if self.view == "agents":
                agents = {"agent:" + str(a.get("id", "")): a
                          for a in self.snapshot.get("agents", [])}
                for row_id in new_ids:
                    if (row_id.startswith("repo:") or
                            (row_id in parents.values() and
                             not _is_completed(agents.get(row_id, {})))):
                        self.expanded.add(row_id)
            else:
                self.expanded.update(row_id for row_id in new_ids if row_id.startswith("repo:"))
        self._known[self.view].update(ids)
        self.rows = self._agent_rows() if self.view == "agents" else self._worktree_rows()
        visible = {row.id for row in self.rows}
        wanted = self._selected[self.view] or self.selected_id
        while wanted and wanted not in visible:
            wanted = parents.get(wanted, "")
        if not wanted:
            wanted = self._initial_selection()
        self.selected_id = wanted
        self._selected[self.view] = wanted

    def _initial_selection(self):
        origin = self.snapshot.get("origin_pane", "")
        for row in self.rows:
            if row.kind == "agent" and row.data.get("pane_id") == origin:
                return row.id
            if row.kind == "worktree" and origin in _pane_ids(row.data.get("panes", [])):
                return row.id
        return self.rows[0].id if self.rows else ""

    def refresh(self, snapshot):
        self._selected[self.view] = self.selected_id
        self.snapshot = snapshot
        self._rebuild()

    def toggle_view(self):
        self._selected[self.view] = self.selected_id
        self.view = "worktrees" if self.view == "agents" else "agents"
        self.selected_id = self._selected[self.view]
        self._rebuild(initial=not self._known[self.view])

    def selected_row(self):
        return next((row for row in self.rows if row.id == self.selected_id), None)

    def _parent_map(self):
        return self._structure()[1]

    def _has_children(self, row_id):
        return row_id in self._parent_map().values()

    def move(self, delta):
        if not self.rows:
            return
        index = next((i for i, row in enumerate(self.rows) if row.id == self.selected_id), 0)
        index = max(0, min(len(self.rows) - 1, index + delta))
        self.selected_id = self.rows[index].id
        self._selected[self.view] = self.selected_id

    def left(self):
        if self._has_children(self.selected_id) and self.selected_id in self.expanded:
            self.expanded.remove(self.selected_id)
            self._rebuild()
            return
        parent = self._parent_map().get(self.selected_id)
        if parent:
            self.selected_id = parent
            self._selected[self.view] = parent

    def right(self):
        if self._has_children(self.selected_id) and self.selected_id not in self.expanded:
            self.expanded.add(self.selected_id)
            self._rebuild()
            return
        parent_map = self._parent_map()
        child = next((row.id for row in self.rows if parent_map.get(row.id) == self.selected_id), "")
        if child:
            self.selected_id = child
            self._selected[self.view] = child

    def enter_action(self):
        row = self.selected_row()
        if not row:
            return Decision("none", "")
        pane = _row_pane(row, self.snapshot.get("session_id", ""))
        return Decision("focus", pane) if pane else Decision("details", row.id)

    def parent_action(self):
        row = self.selected_row()
        if (not row or row.kind != "agent" or
                not self._parent_map().get(row.id, "").startswith("agent:")):
            return Decision("none", "")
        pane = row.data.get("parent_pane", "")
        return Decision("focus", pane) if pane else Decision("details", row.id)


def _pane_ids(panes):
    result = []
    for pane in panes or []:
        result.append(pane.get("pane_id", "") if isinstance(pane, dict) else str(pane))
    return result


def _row_pane(row, session):
    if row.kind == "agent":
        return row.data.get("pane_id", "")
    if row.kind == "worktree":
        for pane in row.data.get("panes", []):
            if isinstance(pane, dict):
                if pane.get("session_id") in {None, "", session}:
                    return pane.get("pane_id", "")
            elif pane:
                return str(pane)
    return ""


def cell_width(text):
    """端末セル幅を外部依存なしで概算する。"""
    width = 0
    for char in str(text):
        if unicodedata.combining(char):
            continue
        if unicodedata.category(char).startswith("C"):
            continue
        width += 2 if unicodedata.east_asian_width(char) in {"W", "F"} else 1
    return width


def clip_text(text, width):
    text = str(text)
    if width <= 0:
        return ""
    if cell_width(text) <= width:
        return text
    if width == 1:
        return "…"
    result = []
    used = 0
    for char in text:
        char_width = cell_width(char)
        if used + char_width > width - 1:
            break
        result.append(char)
        used += char_width
    return "".join(result) + "…"


def _status_label(value):
    labels = {
        "busy": "作業中", "permission": "要対応", "waiting": "要対応",
        "question": "要対応", "approval": "要対応", "idle": "待機",
        "complete": "完了", "completed": "完了", "done": "完了",
        "unknown": "不明", "open": "表示中", "closed": "未表示",
        "available": "利用可", "temporary": "一時領域",
        "prunable": "要整理", "locked": "ロック中", "error": "エラー",
    }
    text = str(value or "-")
    return labels.get(text.lower(), text)


def _row_name(row):
    if row.kind == "repo":
        return row.data.get("name") or row.data.get("path") or row.data.get("id", "-")
    if row.kind == "agent":
        return row.data.get("name") or row.data.get("title") or row.data.get("id", "-")
    return row.data.get("branch") or row.data.get("path") or row.data.get("id", "-")


def _row_cwd(row):
    if row.kind == "repo":
        return row.data.get("path", "-")
    return row.data.get("cwd") or row.data.get("path", "-")


def _row_location(row):
    if row.kind == "agent":
        return row.data.get("pane_id") or row.data.get("parent_pane") or "詳細"
    if row.kind == "worktree":
        panes = _pane_ids(row.data.get("panes", []))
        return (",".join(value for value in panes if value) or
                _reason_label(row.data.get("auto_reason")) or "-")
    return "-"


def _repository_name(repo_id, snapshot):
    for repo in snapshot.get("repos", []):
        if repo.get("id") == repo_id:
            return repo.get("name") or repo.get("path") or repo_id
    return repo_id or "-"


def _worktree_for_path(path, snapshot):
    """path を含む最も深い worktree を返す。"""
    if not path:
        return None
    matches = []
    for worktree in snapshot.get("worktrees", []):
        worktree_path = worktree.get("path")
        if not worktree_path:
            continue
        try:
            Path(path).resolve(strict=False).relative_to(Path(worktree_path).resolve(strict=False))
            matches.append(worktree)
        except ValueError:
            continue
    return max(matches, key=lambda item: len(item.get("path", "")), default=None)


def row_columns(row, snapshot):
    """一覧に出すリポジトリ、branch、worktreeの構造化値。"""
    if row.kind == "repo":
        repo_id = row.data.get("id", "")
        worktree = _worktree_for_path(row.data.get("path", ""), snapshot)
    elif row.kind == "worktree":
        repo_id = row.data.get("repo", "")
        worktree = row.data
    else:
        repo_id = row.data.get("repo_id", "")
        worktree = _worktree_for_path(row.data.get("cwd", ""), snapshot)
    return {
        "repo": _repository_name(repo_id, snapshot),
        "branch": (worktree or {}).get("branch", "-") or "-",
        "worktree": (Path((worktree or {}).get("path", "")).name or "-"),
    }


def row_marker(row, snapshot=None):
    if row.kind == "repo":
        return "◆"
    if row.kind == "agent":
        return "●"
    if row.data.get("auto_reason") == "opened automatically":
        return "⚙"
    if snapshot:
        for repo in snapshot.get("repos", []):
            if repo.get("path") == row.data.get("path"):
                return "⌂"
    return "⌘"


def status_color_key(value):
    status = str(value or "").strip().lower()
    if status in {"permission", "waiting", "question", "approval", "承認待ち", "質問待ち"}:
        return "waiting"
    if status in {"busy", "open", "available", "prunable", "locked"}:
        return "busy"
    if status == "error":
        return "error"
    if status in COMPLETED_STATES:
        return "idle"
    return "default"


def branch_color_key(branch):
    value = str(branch or "").lower()
    if value in {"main", "master", "trunk", "develop", "development"}:
        return "base"
    if value.startswith(("feature/", "feat/")):
        return "feature"
    if value.startswith(("fix/", "bugfix/")):
        return "fix"
    if value.startswith("hotfix/"):
        return "hotfix"
    if value.startswith("release/"):
        return "release"
    return "default"


def _reason_label(value):
    reasons = {
        "temporary agent worktree; automatic opening excluded": "一時領域（自動対象外）",
        "closed manually; automatic reopening suppressed": "手動終了済み",
        "baseline: existed when automatic opening was enabled": "既存領域（自動対象外）",
        "automatic opening is disabled": "自動表示OFF",
        "unknown ownership; automatic opening excluded": "作成元不明（自動対象外）",
        "candidate is not a registered worktree": "未登録worktree",
        "existing pane reused; duplicate automatic pane suppressed": "既存paneあり",
        "automatic pane already open in this session": "自動paneは作成済み",
        "origin pane is no longer available": "作成元paneなし",
        "opened automatically": "自動表示済み",
    }
    text = str(value or "")
    if text.startswith("origin pane too small"):
        return "paneが小さいため未表示"
    return reasons.get(text, text)


def _pad_text(text, width):
    value = clip_text(text, width)
    return value + " " * max(0, width - cell_width(value))


def row_segments(row, width, expanded, snapshot=None):
    """幅に応じた一覧の列（値、セル幅、色カテゴリ）を返す。"""
    if width <= 0:
        return []
    tree = (("▾ " if row.id in expanded else "▸ ")
            if row.expandable else "• ")
    target = "  " * row.depth + tree + row_marker(row, snapshot) + " " + _row_name(row)
    waiting = " 待ち{}".format(row.waiting) if row.waiting else ""
    status = "● " + _status_label(row.data.get("status", "")) + waiting
    if width < 36:
        return [(target, width, "default")]
    if width < 55:
        return [(target, width - 10, "repo"), (status, 8, status_color_key(row.data.get("status", "")))]
    columns = row_columns(row, snapshot or {})
    if width < 71:
        target_width = max(14, width - 42)
        return [
            (target, target_width, "repo"),
            (status, 8, status_color_key(row.data.get("status", ""))),
            (columns["repo"], 11, "repo"),
            (columns["branch"], 17, branch_color_key(columns["branch"])),
        ]
    include_location = width >= 94
    fixed = 8 + 11 + 16 + 14 + (12 if include_location else 0)
    target_width = max(14, width - fixed - (10 if include_location else 8))
    segments = [
        (target, target_width, "repo"),
        (status, 8, status_color_key(row.data.get("status", ""))),
        (columns["repo"], 11, "repo"),
        (columns["branch"], 16, branch_color_key(columns["branch"])),
        (columns["worktree"], 14, "worktree"),
    ]
    if include_location:
        segments.append((_row_location(row), 12, "default"))
    return segments


def format_row(row, width, expanded, snapshot=None):
    """1行を端末幅内に収める。cwdではなく構造化した列を表示する。"""
    segments = row_segments(row, width, expanded, snapshot)
    if not segments:
        return ""
    return clip_text("  ".join(_pad_text(text, field_width)
                               for text, field_width, _ in segments), width)


def render_lines(model, width):
    return [format_row(row, width, model.expanded, model.snapshot) for row in model.rows]


def dump_text(snapshot, width=120):
    model = DashboardModel(snapshot)
    return "\n".join(render_lines(model, max(1, width)))


def footer_text(row, auto):
    actions = ["C-n/p:選択", "C-f/b:開閉", "Tab:表示", "Enter:移動/詳細"]
    if row:
        actions.extend(["s:シェル", "c:Claude", "x:Codex"])
        if row.kind == "agent" and row.data.get("agent_id"):
            actions.append("p:親pane")
        if row.kind == "worktree":
            actions.append("d:削除")
    actions.extend(["n:作成", "a:自動{}".format("ON" if auto else "OFF"),
                    "l:旧ランチャー", "C-g:閉じる"])
    return "  ".join(actions)


def tmux(*args):
    result = subprocess.run(["tmux", *args], text=True, capture_output=True, timeout=5)
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or "tmux command failed")
    return result.stdout.strip()


def focus_pane(pane, session, tmux_fn=tmux):
    """指定 session 内に存在する pane だけを前面化する。"""
    if not pane or not session:
        return False
    available = set(tmux_fn("list-panes", "-s", "-t", session,
                            "-F", "#{pane_id}").splitlines())
    if pane not in available:
        return False
    tmux_fn("select-window", "-t", pane)
    tmux_fn("select-pane", "-t", pane)
    return True


def is_close_key(key):
    return key in {27, "\x1b", "q"}


def navigation_action(key):
    return {
        "\x0e": "down",  # C-n
        "\x10": "up",    # C-p
        "\x06": "right", # C-f
        "\x02": "left",  # C-b
        "\x07": "close", # C-g
    }.get(key, "")


def tmux_panes(session, tmux_fn=tmux):
    fields = ("#{pane_id}\t#{session_id}\t#{pane_current_path}\t"
              "#{pane_current_command}\t#{pane_width}\t#{pane_height}\t#{pane_active}")
    output = tmux_fn("list-panes", "-s", "-t", session, "-F", fields)
    panes = []
    for line in output.splitlines():
        values = line.split("\t")
        if len(values) != 7:
            continue
        try:
            width, height = int(values[4]), int(values[5])
        except ValueError:
            continue
        panes.append({"pane_id": values[0], "session_id": values[1],
                      "cwd": values[2], "command": values[3],
                      "width": width, "height": height,
                      "active": values[6] == "1"})
    return panes


def worktree_path_for_row(row, snapshot):
    if not row:
        return ""
    if row.kind == "worktree":
        return row.data.get("path", "")
    candidate = row.data.get("cwd") if row.kind == "agent" else row.data.get("path")
    if not candidate:
        return ""
    matches = []
    for worktree in snapshot.get("worktrees", []):
        path = worktree.get("path")
        if not path:
            continue
        try:
            Path(candidate).resolve(strict=False).relative_to(Path(path).resolve(strict=False))
            matches.append(path)
        except ValueError:
            continue
    return max(matches, key=lambda path: len(Path(path).parts)) if matches else ""


def repository_root_for_row(row, snapshot):
    if not row:
        return ""
    if row.kind == "repo":
        repo_id = row.data.get("id")
    elif row.kind == "agent":
        repo_id = row.data.get("repo_id")
    else:
        repo_id = row.data.get("repo")
    for repo in snapshot.get("repos", []):
        if repo.get("id") == repo_id:
            return repo.get("path", "")
    return ""


class SnapshotLoader:
    """重い registry scan をキー入力スレッドから分離する。"""

    def __init__(self, fetch):
        self.fetch = fetch
        self.results = queue.Queue()
        self.worker = None

    def request(self):
        if self.worker is not None and self.worker.is_alive():
            return

        def load():
            try:
                self.results.put((self.fetch(), None))
            except Exception as exc:  # UI に表示し、次の更新を継続する。
                self.results.put((None, exc))

        self.worker = threading.Thread(target=load, daemon=True)
        self.worker.start()

    def poll(self):
        latest = None
        while True:
            try:
                latest = self.results.get_nowait()
            except queue.Empty:
                return latest


def _safe_addstr(screen, y, x, value, attribute=0):
    """右下セルや極小popupでも curses.error を外へ出さない。"""
    try:
        height, width = screen.getmaxyx()
        if y < 0 or y >= height or x < 0 or x >= width:
            return
        limit = max(0, width - x - (1 if y == height - 1 else 0))
        value = clip_text(value, limit)
        if value:
            screen.addstr(y, x, value, attribute)
    except Exception as exc:
        # curses.error は環境依存で import 時に利用できない場合がある。
        if exc.__class__.__module__ != "_curses":
            raise


def _color_styles(curses):
    """端末の既定背景を保ったまま、意味ごとの色属性を作る。"""
    try:
        if not curses.has_colors():
            return {}
        curses.start_color()
        curses.use_default_colors()
        palette = {
            "busy": curses.COLOR_YELLOW, "waiting": curses.COLOR_YELLOW,
            "error": curses.COLOR_RED, "idle": curses.COLOR_WHITE,
            "repo": curses.COLOR_BLUE, "worktree": curses.COLOR_CYAN,
            "base": curses.COLOR_GREEN, "feature": curses.COLOR_CYAN,
            "fix": curses.COLOR_YELLOW, "hotfix": curses.COLOR_RED,
            "release": curses.COLOR_MAGENTA,
        }
        styles = {}
        for index, (name, color) in enumerate(palette.items(), start=1):
            curses.init_pair(index, color, -1)
            styles[name] = curses.color_pair(index)
        styles["waiting"] |= curses.A_BOLD
        styles["error"] |= curses.A_BOLD
        return styles
    except curses.error:
        return {}


def _draw(screen, model, message, offset, styles=None):
    import curses
    try:
        screen.erase()
    except curses.error:
        pass
    height, width = screen.getmaxyx()
    summary = model.snapshot.get("summary", {})
    view = "エージェント" if model.view == "agents" else "worktree"
    title = "AI 作業一覧 [{}] 作業中{} 待ち{} 自動{}".format(
        view, summary.get("busy", 0), summary.get("waiting", 0),
        "ON" if model.snapshot.get("auto") else "OFF")
    _safe_addstr(screen, 0, 0, title, curses.A_BOLD)
    if height >= 3:
        _safe_addstr(screen, 1, 0, "対象 / 状態 / リポジトリ / ブランチ / worktree / pane", curses.A_DIM)
    top = 2 if height >= 4 else 1
    bottom = max(top, height - 2)
    capacity = max(0, bottom - top)
    selected = next((index for index, row in enumerate(model.rows)
                     if row.id == model.selected_id), 0)
    if selected < offset:
        offset = selected
    elif capacity and selected >= offset + capacity:
        offset = selected - capacity + 1
    offset = max(0, min(offset, max(0, len(model.rows) - capacity)))
    for y, row in enumerate(model.rows[offset:offset + capacity], start=top):
        attribute = curses.A_REVERSE if row.id == model.selected_id else curses.A_NORMAL
        x = 0
        for value, field_width, color_key in row_segments(
                row, max(1, width - 1), model.expanded, model.snapshot):
            color = (styles or {}).get(color_key, 0)
            _safe_addstr(screen, y, x, _pad_text(value, field_width), attribute | color)
            x += field_width + 2
    if height >= 2:
        footer = message or footer_text(model.selected_row(), bool(model.snapshot.get("auto")))
        _safe_addstr(screen, height - 1, 0, footer, curses.A_BOLD if message else curses.A_DIM)
    try:
        screen.refresh()
    except curses.error:
        pass
    return offset


def _prompt(screen, label, default=""):
    import curses
    height, width = screen.getmaxyx()
    if height < 2 or width < 4:
        return None
    prompt = label + (" [{}]".format(default) if default else "") + ": "
    try:
        screen.move(height - 1, 0)
        screen.clrtoeol()
    except curses.error:
        pass
    _safe_addstr(screen, height - 1, 0, prompt)
    try:
        screen.timeout(-1)
        curses.echo()
        curses.curs_set(1)
    except curses.error:
        pass
    try:
        raw = screen.getstr(height - 1, min(cell_width(prompt), width - 1),
                            max(1, width - cell_width(prompt) - 1))
        value = raw.decode("utf-8", errors="replace").strip()
    except (curses.error, UnicodeError):
        return None
    finally:
        curses.noecho()
        screen.timeout(100)
        try:
            curses.curs_set(0)
        except curses.error:
            pass
    return value or default


def _confirm(screen, text):
    import curses
    height, _ = screen.getmaxyx()
    _safe_addstr(screen, height - 1, 0, text + " [y/N]", curses.A_BOLD)
    try:
        screen.timeout(-1)
        screen.refresh()
        key = screen.get_wch()
    except curses.error:
        return False
    finally:
        screen.timeout(100)
    return key in {"y", "Y"}


def _details(screen, row):
    import curses
    height, _ = screen.getmaxyx()
    try:
        screen.erase()
    except curses.error:
        pass
    _safe_addstr(screen, 0, 0, "詳細: " + _row_name(row), curses.A_BOLD)
    details = [("種類", row.kind), ("状態", _status_label(row.data.get("status", ""))),
               ("cwd", _row_cwd(row)), ("表示先", _row_location(row)),
               ("待ち", str(row.waiting))]
    if row.data.get("auto_reason"):
        details.append(("自動表示", _reason_label(row.data["auto_reason"])))
    for y, (key, value) in enumerate(details, start=2):
        if y >= height - 1:
            break
        _safe_addstr(screen, y, 0, "{}: {}".format(key, value))
    if height:
        _safe_addstr(screen, height - 1, 0, "何かキーを押すと戻ります", curses.A_DIM)
    try:
        screen.timeout(-1)
        screen.refresh()
        screen.get_wch()
    except curses.error:
        pass
    finally:
        screen.timeout(100)


def _suspend_for_legacy(screen, cwd):
    import curses
    try:
        curses.def_prog_mode()
        curses.endwin()
        subprocess.run(["bash", str(HERE.parent / "claude" / "worktree_launch.sh"), "popup"],
                       cwd=cwd or None, check=False)
    finally:
        try:
            curses.reset_prog_mode()
            screen.refresh()
        except curses.error:
            pass


def _run_dashboard(screen, initial, registry, worktrees, session, origin):
    import curses
    if hasattr(curses, "set_escdelay"):
        curses.set_escdelay(25)
    model = DashboardModel(initial)
    loader = SnapshotLoader(lambda: registry.snapshot(session, origin))
    styles = _color_styles(curses)
    screen.keypad(True)
    screen.timeout(100)
    try:
        curses.curs_set(0)
    except curses.error:
        pass
    offset = 0
    message = ""
    message_until = 0.0
    next_refresh = time.monotonic() + 1.0

    while True:
        loaded = loader.poll()
        if loaded:
            value, error = loaded
            if value is not None:
                model.refresh(value)
            if error is not None:
                message = "更新失敗: " + str(error)
                message_until = time.monotonic() + 4
        now = time.monotonic()
        if now >= next_refresh:
            loader.request()
            next_refresh = now + 1.0
        if message and now >= message_until:
            message = ""
        offset = _draw(screen, model, message, offset, styles)
        try:
            key = screen.get_wch()
        except curses.error:
            continue

        action = navigation_action(key)
        if is_close_key(key) or action == "close":
            try:
                focus_pane(origin, session)
            except (OSError, RuntimeError, subprocess.SubprocessError):
                pass
            return
        if key == curses.KEY_UP or action == "up":
            model.move(-1)
        elif key == curses.KEY_DOWN or action == "down":
            model.move(1)
        elif key == curses.KEY_LEFT or action == "left":
            model.left()
        elif key == curses.KEY_RIGHT or action == "right":
            model.right()
        elif key == "\t":
            model.toggle_view()
            offset = 0
        elif key in {"\n", "\r", curses.KEY_ENTER}:
            decision = model.enter_action()
            if decision.kind == "focus":
                try:
                    if focus_pane(decision.target, session):
                        return
                except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
                    message = "移動失敗: " + str(exc)
                    message_until = now + 4
            elif decision.kind == "details" and model.selected_row():
                _details(screen, model.selected_row())
        elif key == "p":
            decision = model.parent_action()
            if decision.kind == "focus":
                try:
                    if focus_pane(decision.target, session):
                        return
                except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
                    message = "親paneへの移動失敗: " + str(exc)
                    message_until = now + 4
        elif key in {"s", "c", "x"}:
            provider = {"s": "shell", "c": "claude", "x": "codex"}[key]
            path = worktree_path_for_row(model.selected_row(), model.snapshot)
            if not path:
                message = "選択行に対応する worktree がありません"
                message_until = now + 4
                continue
            try:
                pane = worktrees.open_worktree(path, session, origin, provider)
                if pane and focus_pane(pane, session):
                    return
            except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
                message = "起動失敗: " + str(exc)
                message_until = now + 5
        elif key == "n":
            root = repository_root_for_row(model.selected_row(), model.snapshot)
            if not root:
                message = "作成先リポジトリを選択してください"
                message_until = now + 4
                continue
            name = _prompt(screen, "worktree名")
            if not name:
                continue
            base = _prompt(screen, "起点", "HEAD")
            if not base:
                continue
            try:
                created = worktrees.create_worktree(root, name, base, session, origin)
                message = "作成しました: " + created
                message_until = time.monotonic() + 5
                loader.request()
            except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
                message = "作成失敗: " + str(exc)
                message_until = time.monotonic() + 5
        elif key == "d":
            row = model.selected_row()
            if not row or row.kind != "worktree":
                continue
            path = row.data.get("path", "")
            if not _confirm(screen, "{} を安全に削除しますか？".format(path)):
                message = "削除を中止しました"
                message_until = time.monotonic() + 2
                continue
            try:
                worktrees.remove_worktree(path)
                message = "削除しました: " + path
                message_until = time.monotonic() + 4
                loader.request()
            except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
                message = "削除失敗: " + str(exc)
                message_until = time.monotonic() + 6
        elif key == "a":
            enabled = not bool(model.snapshot.get("auto"))
            try:
                worktrees.set_auto(session, enabled, tmux_panes(session))
                model.snapshot["auto"] = enabled
                message = "自動表示を{}にしました".format("ON" if enabled else "OFF")
                message_until = time.monotonic() + 3
                loader.request()
            except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
                message = "自動表示の変更失敗: " + str(exc)
                message_until = time.monotonic() + 5
        elif key == "l":
            cwd = _row_cwd(model.selected_row()) if model.selected_row() else os.getcwd()
            _suspend_for_legacy(screen, cwd if Path(cwd).is_dir() else os.getcwd())
            loader.request()


def _load_dependencies():
    import registry
    import worktrees
    return registry, worktrees


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--session", help="対象 tmux session ID")
    parser.add_argument("--pane", help="popup を開いた pane ID")
    parser.add_argument("--dump", action="store_true", help="cursesを使わず同じ行を表示")
    args = parser.parse_args(argv)
    try:
        session = args.session or tmux("display-message", "-p", "#{session_id}")
        origin = args.pane or tmux("display-message", "-p", "#{pane_id}")
        registry, worktrees = _load_dependencies()
        initial = registry.snapshot(session, origin)
        if args.dump:
            width = int(os.environ.get("COLUMNS", "120"))
            print(dump_text(initial, max(1, width)))
            return 0
        if not sys.stdin.isatty() or not sys.stdout.isatty():
            print("対話表示にはTTYが必要です（非対話では --dump を使用）", file=sys.stderr)
            return 2
        import curses
        curses.wrapper(_run_dashboard, initial, registry, worktrees, session, origin)
        return 0
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
        print("tmux agent dashboard: " + str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
