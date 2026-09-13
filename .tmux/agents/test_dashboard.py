"""dashboard の行構築と操作判断を、tmux/curses なしで検証する。"""
import importlib.util
from pathlib import Path
import unittest


HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("dashboard", HERE / "dashboard.py")
dashboard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(dashboard)


class PopupBindingTests(unittest.TestCase):
    def test_dashboard_command_resolves_session_from_popup_context(self):
        """run-shell expands origin formats before invoking the popup helper."""
        config = (HERE.parent / "claude-worktree.conf").read_text()
        binding = next(line for line in config.splitlines()
                       if line.startswith("bind-key w run-shell"))
        self.assertIn("~/.tmux/agents/dashboard_popup.sh", binding)
        self.assertIn("'#{session_id}'", binding)
        self.assertIn("'#{pane_id}'", binding)
        self.assertIn("'#{client_tty}'", binding)


def snapshot():
    return {
        "session_id": "$1",
        "origin_pane": "%1",
        "repos": [
            {"id": "/repo/a/.git", "name": "repo-a", "path": "/repo/a"},
            {"id": "/repo/b/.git", "name": "repo-b", "path": "/repo/b"},
        ],
        "agents": [
            {"id": "root-a", "parent_id": "/repo/a/.git", "repo_id": "/repo/a/.git",
             "name": "Claude", "provider": "claude", "status": "Busy",
             "own_status": "Busy", "cwd": "/repo/a", "pane_id": "%1",
             "parent_pane": "", "session_id": "claude-session-a", "agent_id": ""},
            {"id": "child-a", "parent_id": "root-a", "repo_id": "/repo/a/.git",
             "name": "調査", "provider": "claude", "status": "Permission",
             "own_status": "Permission", "cwd": "/repo/a/wt", "pane_id": "",
             "parent_pane": "%1", "session_id": "claude-session-a", "agent_id": "agent-a"},
            {"id": "root-b", "parent_id": "/repo/b/.git", "repo_id": "/repo/b/.git",
             "name": "Codex", "provider": "codex", "status": "Idle",
             "own_status": "Idle", "cwd": "/repo/b", "pane_id": "%2",
             "parent_pane": "", "session_id": "codex-session-b", "agent_id": ""},
        ],
        "worktrees": [],
        "summary": {"busy": 1, "waiting": 1},
        "auto": False,
    }


class RowConstructionTests(unittest.TestCase):
    def test_agent_rows_are_repository_parent_child_tree(self):
        model = dashboard.DashboardModel(snapshot())

        self.assertEqual(
            [(row.id, row.depth) for row in model.rows],
            [("repo:/repo/a/.git", 0), ("agent:root-a", 1),
             ("agent:child-a", 2), ("repo:/repo/b/.git", 0),
             ("agent:root-b", 1)],
        )

    def test_waiting_counts_use_own_status_without_double_counting(self):
        data = snapshot()
        data["agents"][0]["status"] = "Permission"
        model = dashboard.DashboardModel(data)

        waiting = {row.id: row.waiting for row in model.rows}
        self.assertEqual(waiting["repo:/repo/a/.git"], 1)
        self.assertEqual(waiting["agent:root-a"], 1)
        self.assertEqual(waiting["agent:child-a"], 0)

    def test_completed_child_is_initially_collapsed_but_remains_visible(self):
        data = snapshot()
        data["agents"].extend([
            {"id": "done", "parent_id": "root-a", "repo_id": "/repo/a/.git",
             "name": "完了", "provider": "claude", "status": "Idle",
             "own_status": "Idle", "cwd": "/repo/a/done", "pane_id": "",
             "parent_pane": "%1", "session_id": "claude-session-a", "agent_id": "done"},
            {"id": "nested", "parent_id": "done", "repo_id": "/repo/a/.git",
             "name": "孫", "provider": "claude", "status": "Busy",
             "own_status": "Busy", "cwd": "/repo/a/done", "pane_id": "",
             "parent_pane": "%1", "session_id": "claude-session-a", "agent_id": "nested"},
        ])

        model = dashboard.DashboardModel(data)

        self.assertIn("agent:done", [row.id for row in model.rows])
        self.assertNotIn("agent:nested", [row.id for row in model.rows])
        self.assertNotIn("agent:done", model.expanded)


class UpdateTests(unittest.TestCase):
    def test_refresh_preserves_selection_expansion_and_existing_order(self):
        model = dashboard.DashboardModel(snapshot())
        model.selected_id = "agent:child-a"
        model.expanded.remove("agent:root-a")
        updated = snapshot()
        updated["repos"].reverse()
        updated["agents"].reverse()
        updated["agents"].append(
            {"id": "new", "parent_id": "root-a", "repo_id": "/repo/a/.git",
             "name": "新規", "provider": "codex", "status": "Busy",
             "own_status": "Busy", "cwd": "/repo/a/new", "pane_id": "",
             "parent_pane": "%1", "session_id": "codex-session-new", "agent_id": "new"})

        model.refresh(updated)

        self.assertEqual(model.selected_id, "agent:root-a")
        self.assertNotIn("agent:root-a", model.expanded)
        self.assertEqual([row.id for row in model.rows],
                         ["repo:/repo/a/.git", "agent:root-a",
                          "repo:/repo/b/.git", "agent:root-b"])

    def test_refresh_keeps_selected_row_when_it_still_exists(self):
        model = dashboard.DashboardModel(snapshot())
        model.selected_id = "agent:child-a"

        model.refresh(snapshot())

        self.assertEqual(model.selected_id, "agent:child-a")


class WorktreeViewTests(unittest.TestCase):
    def test_tab_view_includes_worktrees_without_agents(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a", "path": "/repo/a", "repo": "/repo/a/.git",
             "branch": "main", "temporary": False, "panes": ["%1"],
             "status": "open", "auto_reason": ""},
            {"id": "/repo/b/quiet", "path": "/repo/b/quiet", "repo": "/repo/b/.git",
             "branch": "quiet", "temporary": False, "panes": [],
             "status": "closed", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)

        model.toggle_view()

        self.assertEqual(model.view, "worktrees")
        self.assertIn("worktree:/repo/b/quiet", [row.id for row in model.rows])

    def test_each_tab_preserves_its_own_expansion_state(self):
        model = dashboard.DashboardModel(snapshot())
        model.selected_id = "repo:/repo/a/.git"
        model.left()
        self.assertNotIn("agent:root-a", [row.id for row in model.rows])

        model.toggle_view()
        model.toggle_view()

        self.assertNotIn("agent:root-a", [row.id for row in model.rows])


class NavigationTests(unittest.TestCase):
    def test_escape_from_get_wch_closes_for_string_and_integer_forms(self):
        self.assertTrue(dashboard.is_close_key("\x1b"))
        self.assertTrue(dashboard.is_close_key(27))
        self.assertTrue(dashboard.is_close_key("q"))
        self.assertFalse(dashboard.is_close_key("Q"))

    def test_initial_focus_and_enter_only_focus_an_existing_session_pane(self):
        model = dashboard.DashboardModel(snapshot())
        self.assertEqual(model.selected_id, "agent:root-a")
        self.assertEqual(model.enter_action(), dashboard.Decision("focus", "%1"))

        model.selected_id = "agent:child-a"
        self.assertEqual(model.enter_action(),
                         dashboard.Decision("details", "agent:child-a"))

    def test_parent_action_uses_recorded_parent_pane(self):
        model = dashboard.DashboardModel(snapshot())
        model.selected_id = "agent:child-a"

        self.assertEqual(model.parent_action(), dashboard.Decision("focus", "%1"))
        model.selected_id = "agent:root-a"
        self.assertEqual(model.parent_action(), dashboard.Decision("none", ""))

    def test_left_right_and_vertical_movement_operate_on_visible_rows(self):
        model = dashboard.DashboardModel(snapshot())
        model.selected_id = "agent:root-a"

        model.left()
        self.assertEqual([row.id for row in model.rows],
                         ["repo:/repo/a/.git", "agent:root-a",
                          "repo:/repo/b/.git", "agent:root-b"])
        model.right()
        self.assertEqual(model.selected_id, "agent:root-a")
        model.right()
        self.assertEqual(model.selected_id, "agent:child-a")
        model.move(1)
        self.assertEqual(model.selected_id, "repo:/repo/b/.git")

    def test_focus_checks_only_the_requested_session(self):
        calls = []

        def tmux(*args):
            calls.append(args)
            if args[0] == "list-panes":
                return "%1\n%2"
            return ""

        self.assertFalse(dashboard.focus_pane("%9", "$1", tmux))
        self.assertEqual(calls, [("list-panes", "-s", "-t", "$1", "-F", "#{pane_id}")])
        calls.clear()
        self.assertTrue(dashboard.focus_pane("%2", "$1", tmux))
        self.assertEqual(calls[-2:], [
            ("select-window", "-t", "%2"),
            ("select-pane", "-t", "%2"),
        ])

    def test_action_path_uses_deepest_worktree_containing_agent_cwd(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a", "path": "/repo/a", "repo": "/repo/a/.git",
             "branch": "main", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "topic", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        model.selected_id = "agent:child-a"

        self.assertEqual(dashboard.worktree_path_for_row(model.selected_row(), data),
                         "/repo/a/wt")

    def test_pane_inventory_is_limited_to_requested_session(self):
        calls = []

        def tmux(*args):
            calls.append(args)
            return "%1\t$1\t/repo/a dir\tzsh\t100\t40\t1"

        panes = dashboard.tmux_panes("$1", tmux)

        self.assertEqual(panes, [{"pane_id": "%1", "session_id": "$1",
                                  "cwd": "/repo/a dir", "command": "zsh",
                                  "width": 100, "height": 40, "active": True}])
        self.assertEqual(calls[0][:4], ("list-panes", "-s", "-t", "$1"))
        self.assertNotIn("-a", calls[0])


class RenderingTests(unittest.TestCase):
    def test_tree_markers_distinguish_collapsed_nodes_and_leaves(self):
        model = dashboard.DashboardModel(snapshot())
        repo = model.rows[0]
        leaf = next(row for row in model.rows if row.id == "agent:child-a")
        model.expanded.remove(repo.id)

        self.assertTrue(dashboard.format_row(repo, 20, model.expanded).startswith("▸ "))
        self.assertTrue(dashboard.format_row(leaf, 20, model.expanded).startswith("    • "))

    def test_unicode_text_is_clipped_by_terminal_cell_width(self):
        self.assertEqual(dashboard.cell_width("調査abc"), 7)
        self.assertEqual(dashboard.clip_text("調査abc", 5), "調査…")
        self.assertLessEqual(dashboard.cell_width(dashboard.clip_text("調査", 1)), 1)

    def test_narrow_dump_uses_same_visible_rows_without_raising(self):
        text = dashboard.dump_text(snapshot(), width=12)

        self.assertIn("repo-a", text)
        self.assertIn("Claude", text)
        self.assertNotIn("repo-b/.git", text)
        for line in text.splitlines():
            self.assertLessEqual(dashboard.cell_width(line), 12)

    def test_footer_only_offers_remove_for_a_worktree(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "topic", "temporary": False, "panes": [],
             "status": "closed", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        self.assertNotIn("d:削除", dashboard.footer_text(model.selected_row(), False))

        model.toggle_view()
        model.right()
        self.assertIn("d:削除", dashboard.footer_text(model.selected_row(), False))
        self.assertIn("s:シェル", dashboard.footer_text(model.selected_row(), False))
        self.assertIn("a:自動OFF", dashboard.footer_text(model.selected_row(), False))


if __name__ == "__main__":
    unittest.main()
