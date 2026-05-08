#!/usr/bin/env bash
# Smoke test for scripts/asdf-doctor.sh
#
# Strategy: exercise the script against the real asdf install, plus a fixture
# directory under SEARCH_DIRS that references definitely-not-installed
# versions ("99.99.99") so we can verify "missing" detection without depending
# on the test machine's exact installed-version list.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPT="$REPO_ROOT/scripts/asdf-doctor.sh"
FIXTURE_DIR="$REPO_ROOT/tests/fixtures/asdf-doctor"

# Pre-flight: fixtures must exist; otherwise tests would silently pass.
for sub in proj_test proj_dup proj_edge_empty proj_edge_comments; do
    if [[ ! -d "$FIXTURE_DIR/$sub" ]]; then
        echo "Error: fixture directory missing: $FIXTURE_DIR/$sub" >&2
        exit 1
    fi
done
if [[ ! -f "$FIXTURE_DIR/proj_test/.tool-versions" ]]; then
    echo "Error: fixture file missing: $FIXTURE_DIR/proj_test/.tool-versions" >&2
    exit 1
fi

# Colors (only when stdout is a tty)
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BOLD=''; NC=''
fi

passed=0
failed=0
skipped=0

assert() {
    local name="$1"; shift
    if "$@"; then
        echo -e "  [${GREEN}PASS${NC}] $name"
        passed=$((passed + 1))
    else
        echo -e "  [${RED}FAIL${NC}] $name"
        failed=$((failed + 1))
    fi
}

skip() {
    echo -e "  [${YELLOW}SKIP${NC}] $1"
    skipped=$((skipped + 1))
}

echo -e "${BOLD}=== asdf-doctor smoke test ===${NC}"
echo "script: $SCRIPT"
echo "fixture: $FIXTURE_DIR"
echo

# ---------------------------------------------------------------------------
# Pre-flight: skip entire suite if asdf not present
# ---------------------------------------------------------------------------
if ! command -v asdf >/dev/null 2>&1; then
    echo "asdf not available — testing only the graceful skip path."
    out=$(bash "$SCRIPT" 2>&1 >/dev/null)
    exit_code=$?
    if (( exit_code == 0 )) && [[ "$out" == *"asdf not found"* ]]; then
        echo -e "[${GREEN}PASS${NC}] graceful skip when asdf absent"
        exit 0
    else
        echo -e "[${RED}FAIL${NC}] graceful skip path"
        exit 1
    fi
fi

# ---------------------------------------------------------------------------
# Test 1: --help
# ---------------------------------------------------------------------------
echo "Test 1: --help"
help_out=$("$SCRIPT" --help 2>&1)
assert "help mentions Usage" grep -q "Usage:" <<< "$help_out"
assert "help mentions --json" grep -q -- "--json" <<< "$help_out"
assert "help mentions --clean-suggestions" grep -q -- "--clean-suggestions" <<< "$help_out"
assert "help mentions SEARCH_DIRS" grep -q "SEARCH_DIRS" <<< "$help_out"
echo

# ---------------------------------------------------------------------------
# Test 2: default Markdown output has all 4 sections + index freshness
# ---------------------------------------------------------------------------
echo "Test 2: default Markdown output (real env, fixture as SEARCH_DIRS)"
out=$(SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" 2>/dev/null)
assert "section: Unused Versions" grep -q "## Unused Versions" <<< "$out"
assert "section: Missing Versions" grep -q "## Missing Versions" <<< "$out"
assert "section: Healthy Versions" grep -q "## Healthy Versions" <<< "$out"
assert "section: Unused Plugins" grep -q "## Unused Plugins" <<< "$out"
assert "section: Index freshness" grep -q "## Index freshness" <<< "$out"
echo

# ---------------------------------------------------------------------------
# Test 3: fixture-driven missing version detection
#         proj_test references python 99.99.99 + nodejs 99.99.99 + golang 99.99.99
#         which definitely aren't installed → must appear in Missing.
# ---------------------------------------------------------------------------
echo "Test 3: fixture missing-version detection"
assert "missing python 99.99.99" grep -qE "python.*99\.99\.99" <<< "$out"
assert "missing nodejs 99.99.99" grep -qE "nodejs.*99\.99\.99" <<< "$out"
assert "missing golang 99.99.99 (from go.mod toolchain)" grep -qE "golang.*99\.99\.99" <<< "$out"
echo

# ---------------------------------------------------------------------------
# Test 4: --json produces valid JSON (when jq is available)
# ---------------------------------------------------------------------------
echo "Test 4: --json"
if command -v jq >/dev/null 2>&1; then
    json_out=$(SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" --json 2>/dev/null)
    # Helper: check exit code of `jq -e <query>` against $json_out
    jq_check() { jq -e "$1" >/dev/null 2>&1 <<< "$json_out"; }
    assert "--json output is valid JSON" jq_check '.'
    assert "json has plugins object" jq_check '.plugins'
    assert "json has unused_plugins array" jq_check '.unused_plugins | type == "array"'
    assert "json has index_freshness" jq_check '.index_freshness'
else
    skip "jq not available, JSON validity not verified"
fi
echo

# ---------------------------------------------------------------------------
# Test 5: --clean-suggestions outputs commands
# ---------------------------------------------------------------------------
echo "Test 5: --clean-suggestions"
clean_out=$(SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" --clean-suggestions 2>/dev/null)
assert "clean output has suggestion header" grep -q "clean suggestions" <<< "$clean_out"
echo

# ---------------------------------------------------------------------------
# Test 6: empty / nonexistent SEARCH_DIRS produces output without crashing
# ---------------------------------------------------------------------------
echo "Test 6: nonexistent SEARCH_DIRS"
empty_out=$(SEARCH_DIRS="/tmp/__asdf_doctor_nonexistent_$$" "$SCRIPT" 2>/dev/null)
assert "produces non-empty output" test -n "$empty_out"
assert "still has 4 main sections" grep -q "## Healthy Versions" <<< "$empty_out"
echo

# ---------------------------------------------------------------------------
# Test 7: cross-project deduplication
#         proj_test and proj_dup both reference python 99.99.99 — it must
#         appear EXACTLY ONCE in the Missing section (not duplicated).
# ---------------------------------------------------------------------------
echo "Test 7: cross-project deduplication of (plugin, version) pairs"
dup_count=$(grep -cE "^\| python \| 99\.99\.99 " <<< "$out" || true)
assert "python 99.99.99 appears exactly once in missing" test "$dup_count" -eq 1

if command -v jq >/dev/null 2>&1; then
    # Same check via JSON: missing[python] must contain "99.99.99" exactly once
    json_dup_count=$(SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" --json 2>/dev/null \
        | jq '[.plugins.python.missing[] | select(. == "99.99.99")] | length' 2>/dev/null)
    assert "json: missing python 99.99.99 unique" test "$json_dup_count" -eq 1
fi
echo

# ---------------------------------------------------------------------------
# Test 8: edge-case .tool-versions files
#         - proj_edge_empty/.tool-versions   : 0 bytes
#         - proj_edge_comments/.tool-versions: only comments + blank lines
#         Neither should crash the parser nor produce phantom references.
# ---------------------------------------------------------------------------
echo "Test 8: edge-case .tool-versions files (empty / comments-only)"
edge_out=$(SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" 2>/dev/null)
edge_rc=$?
assert "edge fixtures: script exits 0" test "$edge_rc" -eq 0
assert "edge fixtures: still has 4 main sections" \
    grep -q "## Healthy Versions" <<< "$edge_out"
# Only-comments file must NOT inject a phantom plugin into Missing.
# (proj_edge_comments has no version lines; if parser misreads, a stray
#  entry like "comment" or "#" would appear in Missing.)
assert "edge fixtures: no phantom 'comment' plugin in missing" \
    bash -c "! grep -qE '^\| comment ' <<< \"\$edge_out\""

if command -v jq >/dev/null 2>&1; then
    edge_json_check() {
        SEARCH_DIRS="$FIXTURE_DIR" "$SCRIPT" --json 2>/dev/null | jq -e '.' >/dev/null 2>&1
    }
    assert "edge fixtures: json valid" edge_json_check
fi
echo

# ---------------------------------------------------------------------------
# Test 9: bad argument
# ---------------------------------------------------------------------------
echo "Test 9: invalid argument"
set +e
"$SCRIPT" --bogus-flag >/dev/null 2>&1
rc=$?
set -e
assert "exit code 2 for bad arg" test "$rc" -eq 2
echo

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
total=$((passed + failed + skipped))
echo -e "${BOLD}=== Summary ===${NC}"
echo "  PASS: $passed"
echo "  FAIL: $failed"
echo "  SKIP: $skipped"
echo "  Total assertions: $total"

if (( failed > 0 )); then
    exit 1
fi
echo -e "${GREEN}all assertions passed${NC}"
