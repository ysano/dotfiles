"""dashboard の行構築と操作判断を、tmux/curses なしで検証する。"""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest import mock


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
    def test_emacs_navigation_keys_have_the_same_actions_as_arrow_keys(self):
        self.assertEqual(dashboard.navigation_action("\x0e"), "down")  # C-n
        self.assertEqual(dashboard.navigation_action("\x10"), "up")    # C-p
        self.assertEqual(dashboard.navigation_action("\x06"), "right") # C-f
        self.assertEqual(dashboard.navigation_action("\x02"), "left")  # C-b
        self.assertEqual(dashboard.navigation_action("\x07"), "close") # C-g
        self.assertEqual(dashboard.navigation_action("\x0c"), "redraw") # C-l

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
    def test_agent_columns_use_its_deepest_containing_worktree(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a", "path": "/repo/a", "repo": "/repo/a/.git",
             "branch": "main", "temporary": False, "panes": [],
             "status": "open", "auto_reason": ""},
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "feature/dashboard", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        agent = next(row for row in model.rows if row.id == "agent:child-a")

        self.assertEqual(dashboard.row_columns(agent, data), {
            "repo": "repo-a", "branch": "feature/dashboard", "worktree": "wt",
        })

    def test_worktree_row_has_branch_name_and_worktree_kind_marker(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "fix/popup", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        model.toggle_view()
        row = next(item for item in model.rows if item.kind == "worktree")

        self.assertEqual(dashboard.row_columns(row, data), {
            "repo": "repo-a", "branch": "fix/popup", "worktree": "wt",
        })
        self.assertEqual(dashboard.row_marker(row), "⌘")

    def test_status_and_branch_have_semantic_color_categories(self):
        self.assertEqual(dashboard.status_color_key("Busy"), "busy")
        self.assertEqual(dashboard.status_color_key("Permission"), "waiting")
        self.assertEqual(dashboard.branch_color_key("main"), "base")
        self.assertEqual(dashboard.branch_color_key("feature/dashboard"), "feature")
        self.assertEqual(dashboard.branch_color_key("fix/popup"), "fix")

    def test_medium_width_row_never_overflows_when_worktree_column_is_hidden(self):
        data = snapshot()
        data["worktrees"] = [
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "feature/dashboard", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        row = next(item for item in model.rows if item.id == "agent:child-a")

        rendered = dashboard.format_row(row, 64, model.expanded, data)

        self.assertLessEqual(dashboard.cell_width(rendered), 64)
        self.assertIn("feature/dashboard", rendered)
        self.assertNotIn(" /repo/a/wt", rendered)

    def _wide_snapshot(self):
        data = snapshot()
        data["repos"][0]["name"] = "karin-internal"
        data["worktrees"] = [
            {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
             "branch": "docs/design-information", "temporary": False, "panes": [],
             "status": "available", "auto_reason": ""},
        ]
        return data

    def test_wide_layout_fits_columns_to_content_without_clipping(self):
        text = dashboard.dump_text(self._wide_snapshot(), width=140)

        self.assertIn("karin-internal", text)
        self.assertIn("docs/design-information", text)
        self.assertNotIn("…", text)
        for line in text.splitlines():
            self.assertLessEqual(dashboard.cell_width(line), 140)

    def test_target_column_does_not_absorb_all_slack(self):
        lines = dashboard.dump_text(snapshot(), width=200).splitlines()

        self.assertTrue(lines[0].startswith("▾ ◆ repo-a"))
        self.assertLess(lines[0].index("●"), 40)

    def test_columns_align_across_rows(self):
        model = dashboard.DashboardModel(self._wide_snapshot())
        layout = dashboard.column_layout(model.rows, 120, model.expanded, model.snapshot)
        widths = {tuple(width for _, width, _ in dashboard.row_segments(
            row, 120, model.expanded, model.snapshot, layout)) for row in model.rows}
        self.assertEqual(len(widths), 1)

    def test_waiting_status_is_not_clipped_when_space_allows(self):
        data = snapshot()
        data["agents"].append(dict(data["agents"][1], id="child-b", name="調査2"))
        text = dashboard.dump_text(data, width=100)

        self.assertIn("要対応", text)
        self.assertIn("待ち2", text)

    def test_header_labels_follow_visible_columns(self):
        model = dashboard.DashboardModel(self._wide_snapshot())
        wide = dashboard.column_layout(model.rows, 140, model.expanded, model.snapshot)
        narrow = dashboard.column_layout(model.rows, 30, model.expanded, model.snapshot)

        wide_header = dashboard.header_text(wide)
        self.assertIn("ブランチ", wide_header)
        line = dashboard.format_row(model.rows[0], 140, model.expanded,
                                    model.snapshot, wide)
        self.assertEqual(dashboard.cell_width(wide_header[:wide_header.index("状態")]),
                         dashboard.cell_width(line[:line.index("●", 4)]))
        self.assertNotIn("ブランチ", dashboard.header_text(narrow))

    def test_window_and_pane_columns_show_tmux_location(self):
        data = snapshot()
        data["pane_locations"] = {"%1": {"window": "1:main", "pane": "0"},
                                  "%2": {"window": "3:review", "pane": "2"}}
        model = dashboard.DashboardModel(data)
        layout = dashboard.column_layout(model.rows, 140, model.expanded, data)
        lines = {row.id: dashboard.format_row(row, 140, model.expanded, data, layout)
                 for row in model.rows}

        self.assertIn("window", dashboard.header_text(layout))
        self.assertIn("pane", dashboard.header_text(layout))
        self.assertIn("3:review  2", lines["agent:root-b"])
        # 子agentは親paneの位置を示す。
        self.assertIn("1:main    0", lines["agent:child-a"])

    def test_worktree_row_lists_all_pane_locations(self):
        data = snapshot()
        data["pane_locations"] = {"%1": {"window": "1:main", "pane": "0"},
                                  "%2": {"window": "1:main", "pane": "1"}}
        data["worktrees"] = [
            {"id": "/repo/a", "path": "/repo/a", "repo": "/repo/a/.git",
             "branch": "main", "temporary": False, "panes": ["%1", "%2"],
             "status": "open", "auto_reason": ""},
        ]
        model = dashboard.DashboardModel(data)
        model.toggle_view()
        row = next(item for item in model.rows if item.kind == "worktree")

        self.assertEqual(dashboard.row_location(row, data), ("1:main", "0,1"))

    def test_location_falls_back_to_reason_without_tmux_mapping(self):
        data = snapshot()
        model = dashboard.DashboardModel(data)
        row = next(item for item in model.rows if item.id == "agent:root-b")

        self.assertEqual(dashboard.row_location(row, data), ("-", "%2"))

    def test_narrow_layout_shrinks_long_columns_before_hiding(self):
        data = self._wide_snapshot()
        data["agents"][1]["cwd"] = "/repo/a/wt"
        model = dashboard.DashboardModel(data)
        row = next(item for item in model.rows if item.id == "agent:child-a")

        rendered = dashboard.format_row(row, 64, model.expanded, data)

        self.assertLessEqual(dashboard.cell_width(rendered), 64)
        self.assertIn("docs/design", rendered)

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


def cached_snapshot():
    data = snapshot()
    data["worktrees"] = [
        {"id": "/repo/a", "path": "/repo/a", "repo": "/repo/a/.git",
         "branch": "main", "temporary": False, "panes": ["%1"],
         "status": "open", "auto_reason": ""},
        {"id": "/repo/a/wt", "path": "/repo/a/wt", "repo": "/repo/a/.git",
         "branch": "docs/design-information", "temporary": False, "panes": [],
         "status": "available", "auto_reason": ""},
    ]
    data["pane_locations"] = {"%1": {"window": "1:main", "pane": "0"}}
    return data


class RenderCacheTests(unittest.TestCase):
    """列の値は _rebuild で 1 回だけ求め、描画ではパス解決をしない。"""

    def test_render_after_rebuild_resolves_no_paths(self):
        model = dashboard.DashboardModel(cached_snapshot())
        with mock.patch.object(dashboard.Path, "resolve",
                               side_effect=AssertionError("Path.resolve during render")):
            for _ in range(5):
                lines = dashboard.render_lines(model, 140)
                layout = dashboard.column_layout(model.rows, 140, model.expanded,
                                                 model.snapshot, model.values)
        text = "\n".join(lines)
        self.assertIn("docs/design-information", text)
        self.assertIn("1:main", text)
        self.assertEqual(set(layout.values), {row.id for row in model.rows})

    def test_values_follow_refresh_and_expansion_changes(self):
        model = dashboard.DashboardModel(cached_snapshot())
        self.assertIn("docs/design-information", "\n".join(dashboard.render_lines(model, 140)))

        changed = cached_snapshot()
        changed["worktrees"][1]["branch"] = "feature/renamed"
        model.refresh(changed)
        self.assertIn("feature/renamed", "\n".join(dashboard.render_lines(model, 140)))
        self.assertNotIn("docs/design-information", "\n".join(dashboard.render_lines(model, 140)))

        model.selected_id = "agent:root-a"
        model.left()  # 折りたたみ → ツリー記号が ▸ に変わる
        self.assertTrue(any(line.lstrip().startswith("▸") for line in
                            dashboard.render_lines(model, 140)[1:]))

    def test_frame_key_is_stable_until_something_visible_changes(self):
        model = dashboard.DashboardModel(cached_snapshot())
        base = dashboard.frame_key(model, "", 0, (40, 120))
        self.assertEqual(dashboard.frame_key(model, "", 0, (40, 120)), base)
        self.assertFalse(dashboard.should_redraw(base, base))
        self.assertTrue(dashboard.should_redraw(None, base))
        self.assertTrue(dashboard.should_redraw(base, base, force=True))

        model.refresh(copy.deepcopy(cached_snapshot()))  # 同じ内容の再取得
        self.assertEqual(dashboard.frame_key(model, "", 0, (40, 120)), base)

        self.assertNotEqual(dashboard.frame_key(model, "更新失敗", 0, (40, 120)), base)
        self.assertNotEqual(dashboard.frame_key(model, "", 1, (40, 120)), base)
        self.assertNotEqual(dashboard.frame_key(model, "", 0, (41, 120)), base)

        model.move(1)
        moved = dashboard.frame_key(model, "", 0, (40, 120))
        self.assertNotEqual(moved, base)
        model.left()
        self.assertNotEqual(dashboard.frame_key(model, "", 0, (40, 120)), moved)
        model.toggle_view()
        self.assertNotEqual(dashboard.frame_key(model, "", 0, (40, 120)), moved)

        busy = cached_snapshot()
        busy["agents"][2]["status"] = "Busy"
        busy["summary"] = {"busy": 2, "waiting": 1}
        fresh = dashboard.DashboardModel(cached_snapshot())
        before = dashboard.frame_key(fresh, "", 0, (40, 120))
        fresh.refresh(busy)
        self.assertNotEqual(dashboard.frame_key(fresh, "", 0, (40, 120)), before)


class BenchScriptTests(unittest.TestCase):
    def test_measure_render_reports_milliseconds_without_tmux(self):
        spec_ = importlib.util.spec_from_file_location("bench_dashboard", HERE / "bench_dashboard.py")
        bench = importlib.util.module_from_spec(spec_)
        spec_.loader.exec_module(bench)
        value = bench.measure_render(cached_snapshot(), width=100, repeat=3)
        self.assertIsInstance(value, float)
        self.assertGreaterEqual(value, 0.0)
        self.assertNotIn("/Users/", (HERE / "bench_dashboard.py").read_text())


if __name__ == "__main__":
    unittest.main()
