#!/bin/bash
# test_ci.sh - ci.sh（tmux の検証の入口）のテスト
# 用途: .tmux/ci.sh および手動 (bash test_ci.sh) から実行。
# 偽の .tmux の木に ci.sh を写し、各テストを「名前を記録して待つ・指定なら失敗する」
# 偽物に置き換えて、選んで回す・同時に回す・結果の集計を確かめる。
# ok は常に 0 を返すので「条件 && ok || bad」で足りる
# shellcheck disable=SC2015
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
CI_BASH=${CI_BASH:-bash}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fail=0

ok()  { echo "ok   $1"; }
bad() { echo "FAIL $1"; fail=1; }

# --- 偽の木 -------------------------------------------------------------------
root="$work/dot"; tm="$root/.tmux"
mkdir -p "$tm/claude" "$tm/scripts" "$tm/agents" "$root/.claude/skills/tmux-config/scripts"
cp "$HERE/ci.sh" "$tm/ci.sh"
printf '#!/bin/sh\n:\n' > "$tm/scripts/ok.sh"; chmod +x "$tm/scripts/ok.sh"

# 名前 NAME の偽物: 開始と終了を記録し、STUB_SLEEP 秒待つ。STUB_FAIL に名前があれば失敗
stub_body() {
    cat <<EOF
# STUB_IGNORE: INT / TERM を無視する。STUB_CLEANUP: 止められたら 1 秒かけて後片付けし、記録する
[ -z "\${STUB_IGNORE:-}" ] || trap '' INT TERM
[ -z "\${STUB_CLEANUP:-}" ] || trap 'sleep 1; echo "cleaned $1" >> "\$STUB_LOG"; exit 1' INT TERM
echo "start $1" >> "\$STUB_LOG"
echo "$1: first line"
sleep "\${STUB_SLEEP:-0}"
# STUB_HOLD のファイルができるまで待つ（割り込みのテストで、止める前に終わらないように）
if [ -n "\${STUB_HOLD:-}" ]; then n=0; while [ ! -e "\$STUB_HOLD" ] && [ "\$n" -lt 300 ]; do sleep 0.1; n=\$((n + 1)); done; fi
echo "$1: last line"
echo "end $1" >> "\$STUB_LOG"
case " \${STUB_FAIL:-} " in *" $1 "*) exit 1 ;; esac
exit 0
EOF
}
mkstub() { { echo '#!/bin/bash'; stub_body "$1"; } > "$2"; chmod +x "$2"; }
mkstub worktree-launch "$tm/claude/test_worktree_launch.sh"
mkstub logging-utils   "$tm/claude/test_logging_utils.sh"
mkstub ollama-utils    "$tm/claude/test_ollama_utils.sh"
mkstub functions       "$tm/claude/test_functions.sh"
mkstub open-path       "$tm/test_open_path.sh"
mkstub balance-panes   "$tm/test_balance_panes.sh"
mkstub conflicts       "$root/.claude/skills/tmux-config/scripts/check_conflicts.sh"
# ci.sh 自身のテスト（この test_ci.sh）は、偽の木では偽物にする
mkstub ci              "$tm/test_ci.sh"
# Python のテストは unittest から呼ばれる偽物
cat > "$tm/agents/test_stub.py" <<'EOF'
import os, signal, subprocess, unittest
class Stub(unittest.TestCase):
    def test_stub(self):
        # Ctrl-C は子の bash（偽物の本体）に任せ、後片付けを終えるまで待つ
        # （subprocess.run は KeyboardInterrupt で子を強制終了するため）
        previous = signal.signal(signal.SIGINT, signal.SIG_IGN)
        try:
            result = subprocess.run(["bash", "-c", os.environ["STUB_AGENTS"]],
                                    preexec_fn=lambda: signal.signal(signal.SIGINT, signal.SIG_DFL))
        finally:
            signal.signal(signal.SIGINT, previous)
        self.assertEqual(result.returncode, 0)
EOF
export STUB_AGENTS
STUB_AGENTS=$(stub_body agents)

ALL="worktree-launch logging-utils ollama-utils functions open-path balance-panes agents ci conflicts"
ALL_SORTED=$(echo "$ALL" | tr ' ' '\n' | sort | tr '\n' ' ' | sed 's/ $//')

# run [args...] → $out（出力）・$rc・$secs、$work/log（開始・終了の記録）
run() {
    : > "$work/log"
    local t0=$SECONDS
    out=$(cd "$tm" && STUB_LOG="$work/log" "$CI_BASH" ci.sh "$@" 2>&1); rc=$?
    secs=$((SECONDS - t0))
}
ran() { grep '^start ' "$work/log" | sed 's/^start //' | sort | tr '\n' ' ' | sed 's/ $//'; }

# --- 全部 -----------------------------------------------------------------------
run
[ "$rc" = 0 ] && ok "引数なしは全部を回して成功" || bad "引数なしは全部を回して成功: rc=$rc"
[ "$(ran)" = "$ALL_SORTED" ] && ok "引数なしは全部を回す" || bad "引数なしは全部を回す: got '$(ran)'"
echo "$out" | grep -q '^=== CI Complete (errors: 0) ===$' && ok "集計は errors: 0" || bad "集計は errors: 0"
echo "$out" | grep -q '^\[PASS\] Executable: ok.sh$' && ok "静的チェックも回す" || bad "静的チェックも回す"

# 出力は suite ごとにまとまり、決まった順に並ぶ
order=$(echo "$out" | sed -n 's/^\([a-z-]*\): first line$/\1/p' | tr '\n' ' ' | sed 's/ $//')
[ "$order" = "$ALL" ] && ok "出力は決まった順" || bad "出力は決まった順: got '$order'"
interleaved=0
for n in $ALL; do
    next=$(echo "$out" | grep -A1 -x "$n: first line" | tail -1)
    [ "$next" = "$n: last line" ] || interleaved=1
done
[ "$interleaved" = 0 ] && ok "suite の出力は混ざらない" || bad "suite の出力は混ざらない"

# --- 同時に回す -------------------------------------------------------------------
# 全部が始まってから、最初の 1 つが終わる（所要時間でなく、開始・終了の記録の重なりで見る）
STUB_SLEEP=2 run
overlap=$(awk '$1 == "end" { ended = 1 } $1 == "start" && ended { late = 1 } END { print late ? "no" : "yes" }' "$work/log")
[ "$rc" = 0 ] && [ "$overlap" = yes ] && ok "同時に回す（${secs}s）" || bad "同時に回す: rc=$rc overlap=$overlap ${secs}s"

# --serial は 1 つずつ（開始・終了が交互に並ぶ）。待ちを入れて、同時なら必ず重なるようにする
STUB_SLEEP=0.3 run --serial
pairs=$(awk '{ if ($1 == "start") { if (open != "") bad = 1; open = $2 } else { if (open != $2) bad = 1; open = "" } } END { print bad ? "no" : "yes" }' "$work/log")
[ "$rc" = 0 ] && [ "$pairs" = yes ] && [ "$(ran)" = "$ALL_SORTED" ] && ok "--serial は 1 つずつ回す" || bad "--serial は 1 つずつ回す: rc=$rc pairs=$pairs"

# --- 選んで回す -------------------------------------------------------------------
run balance-panes open-path
[ "$rc" = 0 ] && [ "$(ran)" = "balance-panes open-path" ] && ok "名前で選んで回す" || bad "名前で選んで回す: rc=$rc got '$(ran)'"
echo "$out" | grep -q '^\[PASS\] Executable: ok.sh$' && ok "選んでも静的チェックは回す" || bad "選んでも静的チェックは回す"
run agents
[ "$rc" = 0 ] && [ "$(ran)" = "agents" ] && ok "Python のテストだけを選ぶ" || bad "Python のテストだけを選ぶ: rc=$rc got '$(ran)'"

# 知らない名前は、何も回さずに使い方を出して 2 で終わる
run open-path no-such-suite
[ "$rc" = 2 ] && [ -z "$(ran)" ] && ok "知らない名前は何も回さない" || bad "知らない名前は何も回さない: rc=$rc got '$(ran)'"
echo "$out" | grep -q 'no-such-suite' && echo "$out" | grep -q 'balance-panes' && ok "知らない名前と選べる名前を出す" || bad "知らない名前と選べる名前を出す: $out"

run --list
listed=$(echo "$out" | awk '{print $1}' | tr '\n' ' ' | sed 's/ $//')
[ "$rc" = 0 ] && [ "$listed" = "$ALL" ] && [ -z "$(ran)" ] && ok "--list は名前を並べるだけ" || bad "--list は名前を並べるだけ: rc=$rc got '$listed'"

# --- 失敗の集計 ---------------------------------------------------------------------
STUB_FAIL="open-path agents" run
[ "$rc" = 1 ] && ok "失敗があれば 1 で終わる" || bad "失敗があれば 1 で終わる: rc=$rc"
[ "$(ran)" = "$ALL_SORTED" ] && ok "失敗しても他は最後まで回す" || bad "失敗しても他は最後まで回す: got '$(ran)'"
echo "$out" | grep -q '^=== CI Complete (errors: 2) ===$' && ok "失敗の数を数える" || bad "失敗の数を数える"
echo "$out" | grep -q '^\[FAIL\] open-path' && echo "$out" | grep -q '^\[FAIL\] agents' && ok "失敗した suite を名指しする" || bad "失敗した suite を名指しする"
echo "$out" | grep -q '^open-path: last line$' && ok "失敗した suite の出力も出す" || bad "失敗した suite の出力も出す"

STUB_FAIL="conflicts" run conflicts
[ "$rc" = 1 ] && echo "$out" | grep -q '^\[FAIL\] conflicts' && ok "競合チェックの失敗も数える" || bad "競合チェックの失敗も数える: rc=$rc"

# 静的チェックの失敗は、選んで回したときも数える
printf '#!/bin/sh\n:\n' > "$tm/scripts/noexec.sh"
run functions
[ "$rc" = 1 ] && echo "$out" | grep -q 'errors: 1' && ok "静的チェックの失敗も数える" || bad "静的チェックの失敗も数える: rc=$rc"
rm -f "$tm/scripts/noexec.sh"

# --- 割り込み -----------------------------------------------------------------------
# ci.sh を止めたら、回っている suite（その子プロセスまで）も止める。
# 呼び出し元しだいで SIGINT が無視されたまま引き継がれるので、python で既定に戻して起動する
interrupt() { # interrupt <INT|TERM> → "<ci.sh の終了コード> <止まるまでの秒数> <止めた後に終わった suite の数> <後片付けを終えた数>"
    : > "$work/log"
    rm -f "$work/release"
    STUB_LOG="$work/log" STUB_HOLD="$work/release" python3 - "$1" "$CI_BASH" "$tm" "$work/log" "$work/release" <<'EOF'
import signal, subprocess, sys, time
sig, shell, cwd, log, release = sys.argv[1:]
signal.signal(signal.SIGINT, signal.SIG_DFL)
proc = subprocess.Popen([shell, "ci.sh"], cwd=cwd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
for _ in range(100):  # 全部が始まるまで待つ
    if open(log).read().count("start ") >= 9:
        break
    time.sleep(0.05)
proc.send_signal(getattr(signal, "SIG" + sig))
t0 = time.monotonic()
try:
    rc = proc.wait(timeout=10)
except subprocess.TimeoutExpired:
    proc.kill(); rc = "timeout"
stopped = time.monotonic() - t0
open(release, "w").close()  # 止まっていない suite は、これで終わって end を記録する
time.sleep(1)
print(rc, int(stopped), open(log).read().count("end "), open(log).read().count("cleaned "))
EOF
}
for sig in INT TERM; do
    read -r irc isecs iends _ <<< "$(interrupt "$sig")"
    [ "$irc" != 0 ] && [ "$irc" != timeout ] && [ "$isecs" -le 2 ] && [ "$iends" = 0 ] &&
        ok "$sig で止めると回っている suite も止める" ||
        bad "$sig で止めると回っている suite も止める: rc=$irc ${isecs}s ends=$iends"
done
# 信号を無視する suite も、期限を過ぎたら KILL で止める
read -r irc isecs iends _ <<< "$(STUB_IGNORE=1 interrupt INT)"
[ "$irc" != 0 ] && [ "$irc" != timeout ] && [ "$isecs" -le 8 ] && [ "$iends" = 0 ] &&
    ok "信号を無視する suite も期限で止める（${isecs}s）" ||
    bad "信号を無視する suite も期限で止める: rc=$irc ${isecs}s ends=$iends"
# 止める前に、テストの後片付けが終わるのを待つ（TERM で起動役が先に終わっても）
for sig in INT TERM; do
    read -r irc isecs iends icleaned <<< "$(STUB_CLEANUP=1 interrupt "$sig")"
    [ "$irc" != 0 ] && [ "$irc" != timeout ] && [ "$iends" = 0 ] && [ "$icleaned" = 9 ] &&
        ok "$sig: テストの後片付けを待ってから終わる" ||
        bad "$sig: テストの後片付けを待ってから終わる: rc=$irc ${isecs}s ends=$iends cleaned=$icleaned"
done

if [ "$fail" = 0 ]; then echo "test_ci: all ok"; else echo "test_ci: FAILED"; fi
exit "$fail"
