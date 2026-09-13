#!/usr/bin/env python3
"""Read tmux metadata into a session-scoped, stable agent/worktree snapshot."""
import argparse
import json
from pathlib import Path
import subprocess
import sys

from codex_status import tmux

HERE = Path(__file__).resolve().parent
PROVIDERS = {"claude": "Claude Code", "codex": "Codex"}
WAITING = {"Permission", "Question"}


def repository(cwd):
    try:
        result = subprocess.run(["git", "-C", cwd, "rev-parse", "--git-common-dir"],
                                text=True, capture_output=True, check=True, timeout=2)
        common = Path(result.stdout.strip())
        if not common.is_absolute():
            common = Path(cwd) / common
        common = common.resolve()
        listing = subprocess.run(["git", "-C", cwd, "worktree", "list", "--porcelain", "-z"],
                                 capture_output=True, check=True, timeout=2).stdout
        first = next(x[9:] for x in listing.split(b"\0") if x.startswith(b"worktree "))
        root = Path(first.decode("utf-8", errors="surrogateescape")).resolve()
        return {"id": str(common), "name": root.name, "path": str(root)}
    except (OSError, StopIteration, subprocess.SubprocessError):
        return None


def _json(value):
    try:
        data = json.loads(value)
        return data if isinstance(data, dict) else {}
    except (ValueError, TypeError):
        return {}


def read_panes(session=None):
    columns = ["pane_id", "session_id", "pane_current_path", "pane_current_command",
               "pane_width", "pane_height", "pane_active", "pane_title",
               "@claude_state", "@claude_status", "@codex_state", "@codex_status"]
    args = ["list-panes", "-s", "-t", session] if session else ["list-panes", "-a"]
    result = tmux(*args, "-F", "\t".join("#{" + x + "}" for x in columns))
    rows = []
    for line in result.splitlines():
        values = line.split("\t")
        values += [""] * (len(columns) - len(values))
        row = dict(zip(columns, values))
        rows.append({"pane_id": row["pane_id"], "session_id": row["session_id"],
                     "cwd": row["pane_current_path"], "command": row["pane_current_command"],
                     "width": int(row["pane_width"]), "height": int(row["pane_height"]),
                     "active": row["pane_active"] == "1", "title": row["pane_title"],
                     **{p + "_state": _json(row["@" + p + "_state"]) for p in PROVIDERS},
                     **{p + "_status": row["@" + p + "_status"] for p in PROVIDERS}})
    return rows


def actor_status(actor):
    if not actor:
        return "Unknown"
    if not actor.get("active"):
        return "Idle"
    if actor.get("pending"):
        return "Permission"
    return "Busy"


def build_snapshot(session_id, origin_pane, panes, repo_map, worktree_rows, auto):
    repos, agents = {repo["id"]: repo for repo in repo_map.values()}, []
    for pane in panes:
        if pane["session_id"] != session_id:
            continue
        cwd = pane["cwd"]
        repo = repo_map.get(cwd)
        if repo:
            repos[repo["id"]] = repo
        for provider, label in PROVIDERS.items():
            state = pane.get(provider + "_state", {})
            visible = pane.get(provider + "_status", "")
            native = pane["command"] in {provider, provider + ".exe"}
            if not state and not visible and not native:
                continue
            agent_cwd = state.get("cwd") or cwd
            agent_repo = repo_map.get(agent_cwd) or repo or {
                "id": "directory:" + str(Path(agent_cwd).resolve()),
                "name": Path(agent_cwd).name + " (Git管理外)", "path": agent_cwd}
            repos[agent_repo["id"]] = agent_repo
            session = state.get("session_id", "unregistered")
            root_id = provider + ":" + pane["pane_id"] + ":" + session
            actors = state.get("actors", {})
            root = actors.get("root", {})
            own = actor_status(root) if root else visible or "Unknown"
            if visible in {"Question", "Error"}:
                own = visible
            root_row = {"id": root_id, "parent_id": agent_repo["id"], "repo_id": agent_repo["id"],
                        "name": root.get("name") or state.get("name") or label,
                        "provider": provider, "status": visible or actor_status(root), "own_status": own,
                        "cwd": agent_cwd, "pane_id": pane["pane_id"], "parent_pane": pane["pane_id"],
                        "session_id": session, "agent_id": "", "title": pane.get("title", "")}
            agents.append(root_row)
            for actor_id, actor in actors.items():
                if actor_id == "root":
                    continue
                value = actor_status(actor)
                agents.append({"id": root_id + ":" + actor_id, "parent_id": root_id,
                               "repo_id": agent_repo["id"], "name": actor.get("name") or actor.get("agent_type") or actor_id,
                               "provider": provider, "status": value, "own_status": value,
                               "cwd": actor.get("cwd") or agent_cwd, "pane_id": "", "parent_pane": pane["pane_id"],
                               "session_id": session, "agent_id": actor_id})
    summary = {"busy": sum(x["own_status"] == "Busy" for x in agents),
               "waiting": sum(x["own_status"] in WAITING for x in agents),
               "errors": sum(x["own_status"] == "Error" for x in agents)}
    return {"session_id": session_id, "origin_pane": origin_pane, "repos": list(repos.values()),
            "agents": agents, "worktrees": worktree_rows, "summary": summary, "auto": auto}


def snapshot(session_id, origin_pane):
    import worktrees
    panes = read_panes(session_id)
    paths = {p["cwd"] for p in panes}
    for pane in panes:
        for provider in PROVIDERS:
            state = pane[provider + "_state"]
            if state.get("cwd"):
                paths.add(state["cwd"])
            for actor in state.get("actors", {}).values():
                if actor.get("cwd"):
                    paths.add(actor["cwd"])
    repo_map = {cwd: repository(cwd) for cwd in sorted(paths)}
    repo_map = {k: v for k, v in repo_map.items() if v}
    rows = worktrees.list_worktrees(sorted(paths), panes)
    known = {repo["id"] for repo in repo_map.values()}
    for row in rows:
        if row["repo"] not in known:
            repo = repository(row["path"])
            if repo:
                repo_map[row["path"]] = repo
                known.add(repo["id"])
    setting = _json(tmux("show-option", "-qv", "-t", session_id, "@agent_worktree_state"))
    return build_snapshot(session_id, origin_pane, panes, repo_map, rows, bool(setting.get("auto", False)))


def poll():
    import codex_status
    import worktrees
    codex_status.poll()
    # Claude's entrypoint also handles title/dialog/error evidence and config drift.
    subprocess.run(["bash", str(HERE.parent / "claude" / "polling_monitor.sh")],
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=15)
    sessions = {}
    for pane in read_panes():
        sessions.setdefault(pane["session_id"], []).append(pane)
    for session, panes in sessions.items():
        worktrees.poll_session(session, panes)
        origin = next((p["pane_id"] for p in panes if p["active"]), panes[0]["pane_id"])
        # Counts only need actor metadata; avoid another tmux/Git scan per session.
        value = build_snapshot(session, origin, panes, {}, [], False)
        counts = value["summary"]
        text = ""
        if value["agents"]:
            text = "AI 作業中" + str(counts["busy"]) + "・待ち" + str(counts["waiting"])
            if counts["errors"]:
                text += "・エラー" + str(counts["errors"])
        if tmux("show-option", "-qv", "-t", session, "@agent_summary") != text:
            tmux("set-option", "-t", session, "@agent_summary", text)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["snapshot", "poll"], nargs="?", default="snapshot")
    parser.add_argument("--session")
    parser.add_argument("--pane")
    args = parser.parse_args()
    try:
        if args.mode == "poll":
            poll()
        else:
            session = args.session or tmux("display-message", "-p", "#{session_id}")
            pane = args.pane or tmux("display-message", "-p", "#{pane_id}")
            print(json.dumps(snapshot(session, pane), ensure_ascii=False))
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as exc:
        print("tmux agent registry: " + str(exc), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
