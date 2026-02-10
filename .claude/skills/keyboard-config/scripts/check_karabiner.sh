#!/usr/bin/env bash
# =============================================================================
# Karabiner-Elements 設定検証スクリプト
# Usage: check_karabiner.sh
# Checks: JSON 構文、除外アプリ一貫性、ルール重複
# Output: PASS/WARN/FAIL per check with details
# =============================================================================
set -euo pipefail

DOTFILES_ROOT="$(cd "$(dirname "$0")/../../../.." && pwd)"
KARABINER_JSON="$DOTFILES_ROOT/karabiner/karabiner.json"

# --- Colors ---
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BOLD=''; NC=''
fi

pass=0; warn=0; fail=0

result() {
    local level="$1" msg="$2"
    case "$level" in
        PASS) echo -e "[${GREEN}PASS${NC}] $msg"; ((pass++)) || true ;;
        WARN) echo -e "[${YELLOW}WARN${NC}] $msg"; ((warn++)) || true ;;
        FAIL) echo -e "[${RED}FAIL${NC}] $msg"; ((fail++)) || true ;;
    esac
}

echo -e "${BOLD}=== Karabiner-Elements Config Check ===${NC}"
echo ""

# =============================================================================
# Check 0: ファイル存在確認
# =============================================================================
if [[ ! -f "$KARABINER_JSON" ]]; then
    result FAIL "karabiner.json not found at $KARABINER_JSON"
    echo ""
    echo -e "${BOLD}=== Summary ===${NC}"
    echo -e "  ${GREEN}PASS${NC}: $pass"
    echo -e "  ${YELLOW}WARN${NC}: $warn"
    echo -e "  ${RED}FAIL${NC}: $fail"
    exit 1
fi

# =============================================================================
# Check 1: JSON 構文
# =============================================================================
echo -e "${BOLD}--- JSON Syntax ---${NC}"

if python3 -c "import json; json.load(open('$KARABINER_JSON'))" 2>/dev/null; then
    result PASS "karabiner.json: valid JSON"
else
    err=$(python3 -c "import json; json.load(open('$KARABINER_JSON'))" 2>&1 || true)
    result FAIL "karabiner.json: invalid JSON — $err"
fi
echo ""

# =============================================================================
# Check 2: bundle_identifiers 除外アプリ一貫性
# =============================================================================
echo -e "${BOLD}--- Bundle Identifier Consistency ---${NC}"

# 各ルールの bundle_identifiers を抽出して比較
# Python で JSON を構造的に解析（正規表現では不十分なため）
consistency_result=$(KARABINER_JSON="$KARABINER_JSON" python3 << 'PYEOF'
import json, sys, os
from collections import Counter

path = os.environ["KARABINER_JSON"]
with open(path) as f:
    data = json.load(f)

profiles = data.get("profiles", [])
if not profiles:
    print("WARN|No profiles found")
    sys.exit(0)

profile = profiles[0]
rules = profile.get("complex_modifications", {}).get("rules", [])
if not rules:
    print("WARN|No rules found")
    sys.exit(0)

rule_bundles = {}
for i, rule in enumerate(rules):
    desc = rule.get("description", f"rule_{i}")
    bundles = set()
    for manip in rule.get("manipulators", []):
        for cond in manip.get("conditions", []):
            if cond.get("type") == "frontmost_application_unless":
                for bid in cond.get("bundle_identifiers", []):
                    bundles.add(bid)
    if bundles:
        rule_bundles[desc] = frozenset(bundles)

if not rule_bundles:
    print("WARN|No bundle_identifiers exclusions found")
    sys.exit(0)

unique_sets = set(rule_bundles.values())
if len(unique_sets) == 1:
    count = len(list(rule_bundles.values())[0])
    print(f"PASS|All {len(rule_bundles)} rules share identical exclusion list ({count} patterns)")
else:
    set_counts = Counter(rule_bundles.values())
    majority_set = set_counts.most_common(1)[0][0]
    diffs = []
    for desc, bset in rule_bundles.items():
        if bset != majority_set:
            missing = majority_set - bset
            extra = bset - majority_set
            parts = []
            if missing:
                parts.append(f"missing {len(missing)}")
            if extra:
                parts.append(f"extra {len(extra)}")
            diffs.append(f"{desc} ({', '.join(parts)})")
    msg = f"{len(diffs)} rule(s) have inconsistent exclusion lists"
    print(f"FAIL|{msg}")
    for d in diffs:
        print(f"DETAIL|  {d}")
PYEOF
)

level=$(echo "$consistency_result" | head -1 | cut -d'|' -f1)
msg=$(echo "$consistency_result" | head -1 | cut -d'|' -f2-)
result "$level" "$msg"

# 詳細行があれば出力
echo "$consistency_result" | grep '^DETAIL|' | sed 's/^DETAIL|//' || true
echo ""

# =============================================================================
# Check 3: excluded_apps.json との同期チェック
# =============================================================================
echo -e "${BOLD}--- Excluded Apps Sync ---${NC}"

EXCLUDED_APPS="$DOTFILES_ROOT/karabiner/excluded_apps.json"
if [[ -f "$EXCLUDED_APPS" ]]; then
    sync_result=$(KARABINER_JSON="$KARABINER_JSON" EXCLUDED_APPS="$EXCLUDED_APPS" python3 << 'PYEOF'
import json, os, sys

karabiner_path = os.environ["KARABINER_JSON"]
excluded_path = os.environ["EXCLUDED_APPS"]

with open(excluded_path) as f:
    expected = json.load(f)["bundle_identifiers"]

with open(karabiner_path) as f:
    data = json.load(f)

for profile in data.get("profiles", []):
    for rule in profile.get("complex_modifications", {}).get("rules", []):
        if "[Emacs Mode" not in rule.get("description", ""):
            continue
        for manip in rule.get("manipulators", []):
            for cond in manip.get("conditions", []):
                if cond.get("type") == "frontmost_application_unless":
                    actual = cond.get("bundle_identifiers", [])
                    if actual != expected:
                        missing = set(expected) - set(actual)
                        extra = set(actual) - set(expected)
                        parts = []
                        if missing:
                            parts.append(f"missing: {missing}")
                        if extra:
                            parts.append(f"extra: {extra}")
                        if not parts:
                            parts.append("order differs")
                        desc = rule.get("description", "unknown")
                        print(f"FAIL|karabiner.json out of sync with excluded_apps.json ({desc}: {', '.join(parts)}). Run: python3 karabiner/apply_excluded_apps.py")
                        sys.exit(0)
                    break
            break
        break

print(f"PASS|karabiner.json matches excluded_apps.json ({len(expected)} patterns)")
PYEOF
    )
    level=$(echo "$sync_result" | head -1 | cut -d'|' -f1)
    msg=$(echo "$sync_result" | head -1 | cut -d'|' -f2-)
    result "$level" "$msg"
else
    result WARN "excluded_apps.json not found at $EXCLUDED_APPS (skipping sync check)"
fi
echo ""

# =============================================================================
# Check 4: 同一キー+同一条件のルール重複検出
# =============================================================================
echo -e "${BOLD}--- Rule Duplicate Detection ---${NC}"

dup_result=$(KARABINER_JSON="$KARABINER_JSON" python3 << 'PYEOF'
import json, os

path = os.environ["KARABINER_JSON"]
with open(path) as f:
    data = json.load(f)

profiles = data.get("profiles", [])
if not profiles:
    print("PASS|No profiles to check")
    import sys; sys.exit(0)

rules = profiles[0].get("complex_modifications", {}).get("rules", [])

# manipulator の from キー+修飾キーの組み合わせで重複検出
seen = {}  # "key_code+modifiers+conditions_hash" -> rule description
duplicates = []

for rule in rules:
    desc = rule.get("description", "unnamed")
    for manip in rule.get("manipulators", []):
        from_key = manip.get("from", {})
        key_code = from_key.get("key_code", "")
        modifiers = from_key.get("modifiers", {})
        mandatory = tuple(sorted(modifiers.get("mandatory", [])))
        optional = tuple(sorted(modifiers.get("optional", [])))

        # 条件（変数の値）もキーに含める（variable_if / variable_unless 両方）
        cond_parts = []
        for cond in manip.get("conditions", []):
            ctype = cond.get("type", "")
            if ctype in ("variable_if", "variable_unless"):
                cond_parts.append(f"{ctype}:{cond.get('name')}={cond.get('value')}")
        cond_key = tuple(sorted(cond_parts))

        lookup = f"{key_code}|{mandatory}|{optional}|{cond_key}"

        if lookup in seen:
            duplicates.append(f"{desc} vs {seen[lookup]} (key={key_code}, mods={mandatory})")
        else:
            seen[lookup] = desc

total = len(seen)
if not duplicates:
    print(f"PASS|No duplicates detected ({total} manipulators checked)")
else:
    # Karabiner はルール順序で先にマッチしたものが有効になるため、
    # 同一条件の重複は意図的な場合がある（後続ルールは無効）→ WARN
    for dup in duplicates:
        print(f"WARN|Duplicate (first-match wins): {dup}")
PYEOF
)

while IFS= read -r line; do
    level=$(echo "$line" | cut -d'|' -f1)
    msg=$(echo "$line" | cut -d'|' -f2-)
    result "$level" "$msg"
done <<< "$dup_result"
echo ""

# --- Summary ---
echo -e "${BOLD}=== Summary ===${NC}"
echo -e "  ${GREEN}PASS${NC}: $pass"
echo -e "  ${YELLOW}WARN${NC}: $warn"
echo -e "  ${RED}FAIL${NC}: $fail"

[[ $fail -gt 0 ]] && exit 1 || exit 0
