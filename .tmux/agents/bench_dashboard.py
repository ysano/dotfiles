#!/usr/bin/env python3
"""dashboard の表示速度を同じ手順で測る（起動時間・snapshot・外部コマンド数・描画）。

    python3 bench_dashboard.py            # 現在の tmux session を対象
    python3 bench_dashboard.py --width 100 --runs 5
"""
import argparse
from collections import Counter
from pathlib import Path
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))


def measure_render(snapshot, width=160, repeat=20):
    """同じ snapshot に対する render_lines 1 回の平均 ms。tmux 不要。"""
    import dashboard
    model = dashboard.DashboardModel(snapshot)
    start = time.perf_counter()
    for _ in range(repeat):
        dashboard.render_lines(model, width)
    return (time.perf_counter() - start) / max(1, repeat) * 1000


def measure_snapshot(session, origin):
    """registry.snapshot 1 回の ms と、外部コマンドの実行回数。"""
    import registry
    calls = Counter()
    real_run = subprocess.run

    def counting_run(argv, *args, **kwargs):
        calls[str(argv[0])] += 1
        return real_run(argv, *args, **kwargs)

    subprocess.run = counting_run
    try:
        start = time.perf_counter()
        snapshot = registry.snapshot(session, origin)
        elapsed = (time.perf_counter() - start) * 1000
    finally:
        subprocess.run = real_run
    return snapshot, elapsed, calls


def measure_startup(session, origin, runs=3):
    """--dump が終わるまでの秒数（popup を開いて最初の描画までに相当）。"""
    times = []
    for _ in range(runs):
        start = time.perf_counter()
        subprocess.run([sys.executable, str(HERE / "dashboard.py"), "--session", session,
                        "--pane", origin, "--dump"],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
        times.append(time.perf_counter() - start)
    return times


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--session", help="対象 tmux session ID")
    parser.add_argument("--pane", help="起点 pane ID")
    parser.add_argument("--width", type=int, default=160, help="描画計測の端末幅")
    parser.add_argument("--runs", type=int, default=3, help="起動時間の計測回数")
    args = parser.parse_args(argv)
    import dashboard
    session = args.session or dashboard.tmux("display-message", "-p", "#{session_id}")
    origin = args.pane or dashboard.tmux("display-message", "-p", "#{pane_id}")
    snapshot, snapshot_ms, calls = measure_snapshot(session, origin)
    startup = measure_startup(session, origin, args.runs)
    model = dashboard.DashboardModel(snapshot)
    print("規模: pane {} / リポジトリ {} / worktree {} / 表示行 {}".format(
        len(snapshot.get("pane_locations", {})), len(snapshot.get("repos", [])),
        len(snapshot.get("worktrees", [])), len(model.rows)))
    print("起動時間 (--dump, {} 回): {} 秒".format(
        args.runs, " / ".join("%.2f" % value for value in startup)))
    print("snapshot 1 回: {:.0f} ms / 外部コマンド {} 回 {}".format(
        snapshot_ms, sum(calls.values()), dict(calls)))
    print("描画 1 回 (幅 {}): {:.2f} ms".format(args.width, measure_render(snapshot, args.width)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
