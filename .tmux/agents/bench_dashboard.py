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


def measure_steady(session, origin, seconds=10.0, fast=None, slow=None):
    """dashboard を開いたままにしたときの外部コマンド実行数（回/秒）と取得回数。

    curses を使わず、本番と同じ RefreshSchedule / SnapshotLoader / SnapshotSource を
    実時間で回す。"""
    import dashboard
    import registry
    calls = Counter()
    kinds = Counter()
    real_run = subprocess.run

    def counting_run(argv, *args, **kwargs):
        calls[str(argv[0])] += 1
        return real_run(argv, *args, **kwargs)

    source = registry.SnapshotSource(session, origin)
    source.slow()  # 起動時の取得は定常状態に含めない
    loader = dashboard.SnapshotLoader(
        lambda kind: source.fast() if kind == "fast" else source.slow())
    subprocess.run = counting_run
    try:
        cpu_before = _cpu_seconds()
        start = time.monotonic()
        schedule = dashboard.RefreshSchedule(start, fast or dashboard.FAST_REFRESH_SECONDS,
                                             slow or dashboard.SLOW_REFRESH_SECONDS)
        while time.monotonic() - start < seconds:
            loaded = loader.poll()
            if loaded:
                schedule.finished(time.monotonic(), ok=loaded[1] is None)
            kind = schedule.due(time.monotonic())
            if kind and loader.request(kind):
                schedule.started(kind)
                kinds[kind] += 1
            time.sleep(0.1)  # get_wch の timeout(100) に相当
        if loader.worker is not None:
            loader.worker.join(10)
        cpu = (_cpu_seconds() - cpu_before) / seconds * 100
    finally:
        subprocess.run = real_run
    return sum(calls.values()) / seconds, calls, kinds, cpu


def _cpu_seconds():
    """自プロセスと、終了済みの子プロセス（git / tmux / ps）の CPU 秒。"""
    import resource
    total = 0.0
    for who in (resource.RUSAGE_SELF, resource.RUSAGE_CHILDREN):
        usage = resource.getrusage(who)
        total += usage.ru_utime + usage.ru_stime
    return total


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
    parser.add_argument("--steady", type=float, default=0.0, metavar="SECONDS",
                        help="開いたままにしたときの外部コマンド数を SECONDS 秒計測する")
    parser.add_argument("--fast", type=float, help="--steady の速い周期（既定は本番の定数）")
    parser.add_argument("--slow", type=float, help="--steady の遅い周期（既定は本番の定数）")
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
    if args.steady > 0:
        rate, steady_calls, kinds, cpu = measure_steady(session, origin, args.steady, args.fast, args.slow)
        print("定常 ({:.0f} 秒): {:.1f} 回/秒 {} / 取得 {} / CPU {:.1f}% (1 コア比)".format(
            args.steady, rate, dict(steady_calls), dict(kinds), cpu))
    return 0


if __name__ == "__main__":
    sys.exit(main())
