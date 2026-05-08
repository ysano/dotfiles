#!/usr/bin/env bash
# asdf-doctor: Cross-reference project .tool-versions ⇄ installed asdf versions.
#
# Usage:   scripts/asdf-doctor.sh [--help] [--json] [--clean-suggestions]
# Env:     SEARCH_DIRS (default ~/ghq:~/work), ASDF_DATA_DIR (default ~/.asdf)
# Exit:    0 on success or graceful skip, 1 on runtime error, 2 on bad args.

set -euo pipefail

# ---------------------------------------------------------------------------
# Bash version check (script uses associative arrays = bash 4+)
# ---------------------------------------------------------------------------
if [[ -z "${BASH_VERSION:-}" ]] || (( ${BASH_VERSINFO[0]:-0} < 4 )); then
    echo "Error: asdf-doctor requires bash 4+. Detected: ${BASH_VERSION:-(non-bash)}." >&2
    echo "       On macOS: 'brew install bash' and ensure /opt/homebrew/bin in PATH." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Environment & defaults
# ---------------------------------------------------------------------------
ASDF_DATA_DIR="${ASDF_DATA_DIR:-$HOME/.asdf}"
SEARCH_DIRS_RAW="${SEARCH_DIRS:-$HOME/ghq:$HOME/work}"

MODE="markdown"  # markdown | json | clean

# ---------------------------------------------------------------------------
# Usage / args
# ---------------------------------------------------------------------------
usage() {
    cat <<EOF
asdf-doctor: Cross-reference project .tool-versions ⇄ installed asdf versions.

Usage: ${0##*/} [OPTIONS]

Options:
  --help, -h             Show this help and exit.
  --json                 Output machine-readable JSON.
  --clean-suggestions    Output 'asdf uninstall' / 'asdf plugin remove' commands
                         for unused entries (review before executing).

Environment:
  SEARCH_DIRS    Colon-separated project root directories to scan for
                 .tool-versions and go.mod toolchain directives.
                 Default: \$HOME/ghq:\$HOME/work
  ASDF_DATA_DIR  asdf data directory.
                 Default: \$HOME/.asdf

Behavior:
  - When asdf is not installed (\`command -v asdf\` fails) or ASDF_DATA_DIR
    is missing, exits 0 with "asdf not found, skip" on stderr (CI-safe).
  - Empty SEARCH_DIRS or no .tool-versions found yields a report with empty
    sections — the script does not crash.
  - Sections in default Markdown output:
      1. Unused Versions       (installed but unreferenced)
      2. Missing Versions      (referenced but not installed)
      3. Healthy Versions      (installed and referenced)
      4. Unused Plugins        (no versions installed and no references)
      5. Index freshness       (asdf plugin-index last update)

Exit codes:
  0  Success or graceful skip
  1  Runtime error (bash version, etc.)
  2  Invalid arguments
EOF
}

while (( $# > 0 )); do
    case "$1" in
        --help|-h) usage; exit 0 ;;
        --json) MODE="json"; shift ;;
        --clean-suggestions) MODE="clean"; shift ;;
        --) shift; break ;;
        *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
done

# ---------------------------------------------------------------------------
# Graceful skip
# ---------------------------------------------------------------------------
if ! command -v asdf >/dev/null 2>&1; then
    echo "asdf not found, skip" >&2
    exit 0
fi
if [[ ! -d "$ASDF_DATA_DIR" ]]; then
    echo "asdf data dir not found at $ASDF_DATA_DIR, skip" >&2
    exit 0
fi

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Portable mtime in epoch seconds. BSD (-f %m) → GNU (-c %Y) fallback.
file_mtime() {
    stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null || echo 0
}

# Portable atime in epoch seconds. BSD (-f %a) → GNU (-c %X) fallback.
file_atime() {
    stat -f %a "$1" 2>/dev/null || stat -c %X "$1" 2>/dev/null || echo 0
}

# Human-readable directory size.
dir_disk_usage() {
    /usr/bin/du -sh "$1" 2>/dev/null | awk '{print $1}'
}

# epoch seconds → YYYY-MM-DD. BSD (date -r) → GNU (date -d @) fallback.
epoch_to_date() {
    local e="$1"
    [[ "$e" =~ ^[0-9]+$ ]] || { echo "?"; return; }
    date -r "$e" +%Y-%m-%d 2>/dev/null || date -d "@$e" +%Y-%m-%d 2>/dev/null || echo "?"
}

# Days between epoch seconds and now.
days_ago() {
    local e="$1"
    [[ "$e" =~ ^[0-9]+$ ]] || { echo "?"; return; }
    local now
    now=$(date +%s)
    echo $(( (now - e) / 86400 ))
}

# RFC 8259 JSON string escaping. Handles all control characters U+0000-U+001F
# in addition to the standard backslash-quote-newline-tab cases.
json_escape() {
    local s="$1"
    # Standard short escapes
    s="${s//\\/\\\\}"
    s="${s//\"/\\\"}"
    s="${s//$'\b'/\\b}"
    s="${s//$'\f'/\\f}"
    s="${s//$'\n'/\\n}"
    s="${s//$'\r'/\\r}"
    s="${s//$'\t'/\\t}"
    # Remaining control chars U+0001-U+001F (excluding the seven above) → \u00XX
    s="${s//$'\x01'/\\u0001}"; s="${s//$'\x02'/\\u0002}"; s="${s//$'\x03'/\\u0003}"
    s="${s//$'\x04'/\\u0004}"; s="${s//$'\x05'/\\u0005}"; s="${s//$'\x06'/\\u0006}"
    s="${s//$'\x07'/\\u0007}"; s="${s//$'\x0b'/\\u000b}"; s="${s//$'\x0e'/\\u000e}"
    s="${s//$'\x0f'/\\u000f}"; s="${s//$'\x10'/\\u0010}"; s="${s//$'\x11'/\\u0011}"
    s="${s//$'\x12'/\\u0012}"; s="${s//$'\x13'/\\u0013}"; s="${s//$'\x14'/\\u0014}"
    s="${s//$'\x15'/\\u0015}"; s="${s//$'\x16'/\\u0016}"; s="${s//$'\x17'/\\u0017}"
    s="${s//$'\x18'/\\u0018}"; s="${s//$'\x19'/\\u0019}"; s="${s//$'\x1a'/\\u001a}"
    s="${s//$'\x1b'/\\u001b}"; s="${s//$'\x1c'/\\u001c}"; s="${s//$'\x1d'/\\u001d}"
    s="${s//$'\x1e'/\\u001e}"; s="${s//$'\x1f'/\\u001f}"
    printf '%s' "$s"
}

# ---------------------------------------------------------------------------
# Globals (populated by collection functions, read by formatters)
# ---------------------------------------------------------------------------
declare -A plugin_present              # plugin -> 1
declare -A plugin_installed_versions   # plugin -> "v1\nv2\n..."
declare -A plugin_disk                 # "plugin\tversion" -> disk
declare -A plugin_atime                # "plugin\tversion" -> atime YYYY-MM-DD
declare -A plugin_referenced_versions  # plugin -> "v1\nv2\n..."
declare -A plugin_referenced_sources   # "plugin\tversion" -> "f1;f2;..."
declare -A plugin_unused               # plugin -> "v1\nv2\n..."
declare -A plugin_missing              # plugin -> "v1\nv2\n..."
declare -A plugin_healthy              # plugin -> "v1\nv2\n..."
unused_plugins=()                      # plugins with no installs and no refs

idx_date="?"
idx_days="?"

# ---------------------------------------------------------------------------
# Data collection
# ---------------------------------------------------------------------------

collect_installed() {
    local plugins_dir="$ASDF_DATA_DIR/plugins"
    local installs_dir="$ASDF_DATA_DIR/installs"
    [[ -d "$plugins_dir" ]] || return 0

    shopt -s nullglob
    local plugin pname pinstalls_dir
    for plugin in "$plugins_dir"/*/; do
        pname=$(basename "$plugin")
        plugin_present["$pname"]=1
        pinstalls_dir="$installs_dir/$pname"

        if [[ -d "$pinstalls_dir" ]]; then
            local v_dirs=("$pinstalls_dir"/*/)
            local v vname disk atime
            for v in "${v_dirs[@]}"; do
                [[ -d "$v" ]] || continue
                vname=$(basename "$v")
                disk=$(dir_disk_usage "$v")
                atime=$(epoch_to_date "$(file_atime "$v")")
                plugin_installed_versions["$pname"]+="$vname"$'\n'
                plugin_disk["$pname"$'\t'"$vname"]="$disk"
                plugin_atime["$pname"$'\t'"$vname"]="$atime"
            done
        fi
    done
    shopt -u nullglob
}

# Parse a .tool-versions file. Accepts "plugin v1 v2 ...".
# Skips comments and blank lines.
parse_tool_versions() {
    local f="$1"
    awk -v F="$f" '
        /^[[:space:]]*#/ { next }
        /^[[:space:]]*$/ { next }
        {
            for (i = 2; i <= NF; i++) {
                printf "%s\t%s\t%s\n", $1, $i, F
            }
        }
    ' "$f" 2>/dev/null
}

# Parse go.mod toolchain directive: "toolchain go1.21.0"
parse_go_mod_toolchain() {
    local f="$1"
    awk -v F="$f" '
        /^toolchain[[:space:]]+go[0-9]/ {
            ver = $2
            sub(/^go/, "", ver)
            printf "golang\t%s\t%s\n", ver, F
        }
    ' "$f" 2>/dev/null
}

collect_referenced() {
    local out
    out=$(
        # Global ~/.tool-versions
        if [[ -f "$HOME/.tool-versions" ]]; then
            parse_tool_versions "$HOME/.tool-versions"
        fi

        local IFS=':'
        local dir
        for dir in $SEARCH_DIRS_RAW; do
            [[ -n "$dir" && -d "$dir" ]] || continue
            # Single find call covers both .tool-versions and go.mod to avoid
            # walking the same large directory tree twice.
            while IFS= read -r f; do
                case "${f##*/}" in
                    .tool-versions) parse_tool_versions "$f" ;;
                    go.mod) parse_go_mod_toolchain "$f" ;;
                esac
            done < <(find "$dir" -type f \
                \( -name ".tool-versions" -o -name "go.mod" \) \
                -not -path "*/node_modules/*" \
                -not -path "*/.git/*" \
                -not -path "*/Library/*" \
                2>/dev/null)
        done
    )

    [[ -z "$out" ]] && return 0
    local p v src k
    while IFS=$'\t' read -r p v src; do
        [[ -z "$p" || -z "$v" ]] && continue
        plugin_present["$p"]=1
        k="$p"$'\t'"$v"
        # Append the version to plugin_referenced_versions only on first sighting
        # of (plugin, version); accumulate sources for every sighting.
        if [[ -n "${plugin_referenced_sources[$k]:-}" ]]; then
            plugin_referenced_sources["$k"]="${plugin_referenced_sources[$k]};$src"
        else
            plugin_referenced_sources["$k"]="$src"
            plugin_referenced_versions["$p"]+="$v"$'\n'
        fi
    done <<< "$out"
}

# Cross-reference installed and referenced data.
compute_diffs() {
    local p inst refs v
    for p in "${!plugin_present[@]}"; do
        inst="${plugin_installed_versions[$p]:-}"
        refs="${plugin_referenced_versions[$p]:-}"

        # Plugin completely unused: no installs, no references.
        if [[ -z "$inst" && -z "$refs" ]]; then
            unused_plugins+=("$p")
            continue
        fi

        # Unused versions: installed but not referenced.
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            if [[ -z "$refs" ]] || ! grep -qxF "$v" <<< "$refs"; then
                plugin_unused["$p"]+="$v"$'\n'
            fi
        done <<< "$inst"

        # Missing versions: referenced but not installed.
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            if [[ -z "$inst" ]] || ! grep -qxF "$v" <<< "$inst"; then
                plugin_missing["$p"]+="$v"$'\n'
            fi
        done <<< "$refs"

        # Healthy versions: in both.
        if [[ -n "$inst" && -n "$refs" ]]; then
            while IFS= read -r v; do
                [[ -z "$v" ]] && continue
                if grep -qxF "$v" <<< "$refs"; then
                    plugin_healthy["$p"]+="$v"$'\n'
                fi
            done <<< "$inst"
        fi
    done
}

collect_index_freshness() {
    local plugin_index_dir="$ASDF_DATA_DIR/plugin-index"
    [[ -d "$plugin_index_dir" ]] || return 0
    local m
    m=$(file_mtime "$plugin_index_dir")
    if [[ "$m" =~ ^[0-9]+$ ]] && (( m > 0 )); then
        idx_date=$(epoch_to_date "$m")
        idx_days=$(days_ago "$m")
    fi
}

# ---------------------------------------------------------------------------
# Output: Markdown (default)
# ---------------------------------------------------------------------------
format_markdown() {
    local today
    today=$(date +%Y-%m-%d)
    echo "# asdf doctor report ($today)"
    echo
    echo "asdf data dir: \`$ASDF_DATA_DIR\`"
    echo "search dirs: \`$SEARCH_DIRS_RAW\`"
    echo

    # Section 1: Unused Versions
    echo "## Unused Versions (uninstall candidates)"
    local has=0 p v k
    for p in "${!plugin_unused[@]}"; do
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            if (( has == 0 )); then
                echo
                echo "| Plugin | Version | Disk | Last access |"
                echo "|--------|---------|------|-------------|"
                has=1
            fi
            k="$p"$'\t'"$v"
            echo "| $p | $v | ${plugin_disk[$k]:-?} | ${plugin_atime[$k]:-?} |"
        done <<< "${plugin_unused[$p]}"
    done
    if (( has == 0 )); then
        echo
        echo "_None — all installed versions are referenced._"
    fi
    echo

    # Section 2: Missing Versions
    echo "## Missing Versions (referenced but not installed)"
    has=0
    local srcs first_src n_more src_disp
    for p in "${!plugin_missing[@]}"; do
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            if (( has == 0 )); then
                echo
                echo "| Plugin | Version | Referenced in |"
                echo "|--------|---------|---------------|"
                has=1
            fi
            k="$p"$'\t'"$v"
            srcs="${plugin_referenced_sources[$k]:-}"
            first_src="${srcs%%;*}"
            n_more=0
            if [[ "$srcs" != "$first_src" ]]; then
                n_more=$(awk -F';' '{print NF-1}' <<< "$srcs")
            fi
            if (( n_more > 0 )); then
                src_disp="$first_src (+$n_more more)"
            else
                src_disp="$first_src"
            fi
            echo "| $p | $v | $src_disp |"
        done <<< "${plugin_missing[$p]}"
    done
    if (( has == 0 )); then
        echo
        echo "_None — all referenced versions are installed._"
    fi
    echo

    # Section 3: Healthy Versions
    echo "## Healthy Versions"
    local healthy_count=0 plugins_with_healthy=0
    for p in "${!plugin_healthy[@]}"; do
        plugins_with_healthy=$((plugins_with_healthy + 1))
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            healthy_count=$((healthy_count + 1))
        done <<< "${plugin_healthy[$p]}"
    done
    echo
    echo "$healthy_count versions across $plugins_with_healthy plugins, all referenced."
    echo

    # Section 4: Unused Plugins
    echo "## Unused Plugins (no versions, no references)"
    if (( ${#unused_plugins[@]} > 0 )); then
        echo
        echo "| Plugin | Action |"
        echo "|--------|--------|"
        local up
        for up in "${unused_plugins[@]}"; do
            echo "| $up | \`asdf plugin remove $up\` |"
        done
    else
        echo
        echo "_None._"
    fi
    echo

    # Section 5: Index freshness
    echo "## Index freshness"
    echo
    echo "Last \`asdf plugin update --all\`: $idx_date ($idx_days days ago)"
}

# ---------------------------------------------------------------------------
# Output: JSON
# ---------------------------------------------------------------------------
versions_to_json_array() {
    local vlist="${1:-}"
    if [[ -z "$vlist" ]]; then
        echo -n "[]"
        return
    fi
    local first=1 v out="["
    while IFS= read -r v; do
        [[ -z "$v" ]] && continue
        if (( first )); then first=0; else out="$out,"; fi
        out="$out\"$(json_escape "$v")\""
    done <<< "$vlist"
    out="$out]"
    echo -n "$out"
}

format_json() {
    local today
    today=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    echo "{"
    echo "  \"generated_at\": \"$today\","
    echo "  \"asdf_data_dir\": \"$(json_escape "$ASDF_DATA_DIR")\","
    echo -n "  \"search_dirs\": ["
    local first=1
    local IFS=':'
    local d
    for d in $SEARCH_DIRS_RAW; do
        [[ -z "$d" ]] && continue
        if (( first )); then first=0; else echo -n ","; fi
        echo -n "\"$(json_escape "$d")\""
    done
    echo "],"
    echo "  \"plugins\": {"
    local pfirst=1 p
    for p in "${!plugin_present[@]}"; do
        if (( pfirst )); then pfirst=0; else echo ","; fi
        echo -n "    \"$(json_escape "$p")\": {"
        echo -n "\"installed\": $(versions_to_json_array "${plugin_installed_versions[$p]:-}"),"
        echo -n "\"referenced\": $(versions_to_json_array "${plugin_referenced_versions[$p]:-}"),"
        echo -n "\"unused\": $(versions_to_json_array "${plugin_unused[$p]:-}"),"
        echo -n "\"missing\": $(versions_to_json_array "${plugin_missing[$p]:-}"),"
        echo -n "\"healthy\": $(versions_to_json_array "${plugin_healthy[$p]:-}")"
        echo -n "}"
    done
    echo
    echo "  },"
    echo -n "  \"unused_plugins\": ["
    first=1
    local up
    # Use ${arr[@]+"${arr[@]}"} guard for portability with bash 4.0-4.3 + nounset
    for up in ${unused_plugins[@]+"${unused_plugins[@]}"}; do
        if (( first )); then first=0; else echo -n ","; fi
        echo -n "\"$(json_escape "$up")\""
    done
    echo "],"
    echo "  \"index_freshness\": {"
    echo "    \"last_updated\": \"$idx_date\","
    if [[ "$idx_days" == "?" ]]; then
        echo "    \"days_ago\": null"
    else
        echo "    \"days_ago\": $idx_days"
    fi
    echo "  }"
    echo "}"
}

# ---------------------------------------------------------------------------
# Output: clean suggestions
# ---------------------------------------------------------------------------
format_clean() {
    echo "# asdf-doctor: clean suggestions"
    echo "# Review carefully before executing!"
    echo
    local has_unused=0 p v
    for p in "${!plugin_unused[@]}"; do
        while IFS= read -r v; do
            [[ -z "$v" ]] && continue
            if (( has_unused == 0 )); then
                echo "# Unused versions"
                has_unused=1
            fi
            echo "asdf uninstall $p $v"
        done <<< "${plugin_unused[$p]}"
    done
    (( has_unused == 1 )) && echo
    if (( ${#unused_plugins[@]} > 0 )); then
        echo "# Unused plugins"
        local up
        for up in "${unused_plugins[@]}"; do
            echo "asdf plugin remove $up"
        done
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main() {
    collect_installed
    collect_referenced
    compute_diffs
    collect_index_freshness

    case "$MODE" in
        markdown) format_markdown ;;
        json) format_json ;;
        clean) format_clean ;;
    esac
}

main
