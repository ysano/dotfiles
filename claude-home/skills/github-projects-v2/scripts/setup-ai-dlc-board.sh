#!/usr/bin/env bash
# setup-ai-dlc-board.sh - Create and configure a GitHub Projects V2 board for AI-DLC workflows.
# Supports 4 team scales: solo, pod, squad, enterprise.
#
# Usage:
#   setup-ai-dlc-board.sh --owner <OWNER> --title <TITLE> --scale <solo|pod|squad|enterprise> [--repo <REPO>]
#
# Requirements:
#   - gh CLI with `project` scope: gh auth refresh -s project
#   - jq
#
# Output:
#   - Creates project with scale-appropriate fields
#   - Prints CLAUDE.md XML tags to stdout
#   - Displays manual setup instructions for Built-in Workflows and Iteration field

set -euo pipefail

# --- Defaults ---
OWNER=""
TITLE=""
SCALE=""
REPO=""
RATE_LIMIT_DELAY=1  # seconds between mutations

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- Helpers ---
info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
ok()    { echo -e "${GREEN}[OK]${NC} $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }
die()   { error "$@"; exit 1; }

usage() {
  cat <<'EOF'
Usage: setup-ai-dlc-board.sh --owner <OWNER> --title <TITLE> --scale <solo|pod|squad|enterprise> [--repo <REPO>]

Options:
  --owner    GitHub user or organization login
  --title    Project title
  --scale    Team scale: solo | pod | squad | enterprise
  --repo     Repository to link (optional, format: owner/repo or repo)

Examples:
  setup-ai-dlc-board.sh --owner myuser --title "Sprint 3" --scale pod --repo calculator-experiment
  setup-ai-dlc-board.sh --owner my-org --title "Q1 Board" --scale squad
EOF
  exit 1
}

# --- Parse Arguments ---
while [[ $# -gt 0 ]]; do
  case "$1" in
    --owner) OWNER="$2"; shift 2 ;;
    --title) TITLE="$2"; shift 2 ;;
    --scale) SCALE="$2"; shift 2 ;;
    --repo)  REPO="$2";  shift 2 ;;
    -h|--help) usage ;;
    *) die "Unknown option: $1" ;;
  esac
done

# --- Validate ---
[[ -z "$OWNER" ]] && die "Missing --owner"
[[ -z "$TITLE" ]] && die "Missing --title"
[[ -z "$SCALE" ]] && die "Missing --scale"
[[ "$SCALE" =~ ^(solo|pod|squad|enterprise)$ ]] || die "Invalid --scale: $SCALE (must be solo|pod|squad|enterprise)"
command -v gh  &>/dev/null || die "gh CLI not found. Install: https://cli.github.com/"
command -v jq  &>/dev/null || die "jq not found. Install: https://jqlang.github.io/jq/"

# Check auth scope
if ! gh auth status 2>&1 | grep -q "project"; then
  warn "Token may lack 'project' scope. Run: gh auth refresh -s project"
fi

# --- Status Options by Scale ---
get_status_options() {
  case "$1" in
    solo|pod)    echo "Todo,In Progress,Review,Done" ;;
    squad)       echo "Triage,Backlog,Ready,In Progress,Review,Done" ;;
    enterprise)  echo "Triage,Backlog,Ready,In Progress,In CI,Review,Done" ;;
  esac
}

# --- Step 1: Create Project ---
info "Creating project '$TITLE' for $OWNER..."

PROJECT_NUMBER=$(gh project create --owner "$OWNER" --title "$TITLE" --format json 2>/dev/null | jq -r '.number')
if [[ -z "$PROJECT_NUMBER" || "$PROJECT_NUMBER" == "null" ]]; then
  # Fallback: parse from non-JSON output
  PROJECT_NUMBER=$(gh project create --owner "$OWNER" --title "$TITLE" 2>&1 | grep -oP 'number \K\d+' || true)
  if [[ -z "$PROJECT_NUMBER" ]]; then
    die "Failed to create project"
  fi
fi
ok "Project created: number=$PROJECT_NUMBER"
sleep "$RATE_LIMIT_DELAY"

# Get project Node ID and URL
PROJECT_JSON=$(gh project view "$PROJECT_NUMBER" --owner "$OWNER" --format json)
PROJECT_ID=$(echo "$PROJECT_JSON" | jq -r '.id')
PROJECT_URL=$(echo "$PROJECT_JSON" | jq -r '.url')
ok "Project ID: $PROJECT_ID"

# --- Step 2: Configure Status Field Options via GraphQL ---
info "Configuring Status field options for scale=$SCALE..."

STATUS_OPTIONS=$(get_status_options "$SCALE")

# Get the Status field ID
FIELDS_JSON=$(gh project field-list "$PROJECT_NUMBER" --owner "$OWNER" --format json)
STATUS_FIELD_ID=$(echo "$FIELDS_JSON" | jq -r '.fields[] | select(.name == "Status") | .id')

if [[ -z "$STATUS_FIELD_ID" || "$STATUS_FIELD_ID" == "null" ]]; then
  die "Status field not found"
fi

# Build the options array for GraphQL mutation
IFS=',' read -ra OPTS <<< "$STATUS_OPTIONS"
OPTIONS_GQL=""
for opt in "${OPTS[@]}"; do
  if [[ -n "$OPTIONS_GQL" ]]; then
    OPTIONS_GQL="$OPTIONS_GQL, "
  fi
  OPTIONS_GQL="${OPTIONS_GQL}{name: \"$opt\", color: GRAY, description: \"\"}"
done

# Update Status field options via GraphQL
# Note: correct input uses fieldId (not projectId) and singleSelectOptions (not singleSelectField)
gh api graphql -f query="
mutation {
  updateProjectV2Field(input: {
    fieldId: \"$STATUS_FIELD_ID\"
    singleSelectOptions: [$OPTIONS_GQL]
  }) {
    projectV2Field {
      ... on ProjectV2SingleSelectField {
        id
        options { id name }
      }
    }
  }
}" > /dev/null 2>&1 || warn "Status field update failed (options may need manual adjustment)"
sleep "$RATE_LIMIT_DELAY"
ok "Status options configured: $STATUS_OPTIONS"

# --- Step 3: Create Custom Fields by Scale ---
info "Creating custom fields for scale=$SCALE..."

create_field() {
  local name="$1"
  local type="$2"
  local options="${3:-}"

  # Check if field already exists (idempotent)
  local existing
  existing=$(gh project field-list "$PROJECT_NUMBER" --owner "$OWNER" --format json 2>/dev/null \
    | jq -r ".fields[] | select(.name == \"$name\") | .id")
  if [[ -n "$existing" && "$existing" != "null" ]]; then
    warn "Field '$name' already exists, skipping"
    return 0
  fi

  local cmd=(gh project field-create "$PROJECT_NUMBER" --owner "$OWNER" --name "$name" --data-type "$type")
  if [[ -n "$options" && "$type" == "SINGLE_SELECT" ]]; then
    cmd+=(--single-select-options "$options")
  fi

  if "${cmd[@]}" 2>/dev/null; then
    ok "Created field: $name ($type)"
  else
    warn "Failed to create field: $name ($type)"
  fi
  sleep "$RATE_LIMIT_DELAY"
}

# Solo fields (always created): Priority, Size
create_field "Priority" "SINGLE_SELECT" "P0,P1,P2"
create_field "Size" "SINGLE_SELECT" "S,M,L"

# Pod fields (solo + 4)
if [[ "$SCALE" =~ ^(pod|squad|enterprise)$ ]]; then
  create_field "AI-Confidence" "NUMBER"
  create_field "Turns-Used" "NUMBER"
  create_field "Spec-Link" "TEXT"
  create_field "Review-Priority" "SINGLE_SELECT" "High,Medium,Low"
fi

# Squad fields (pod + 6)
if [[ "$SCALE" =~ ^(squad|enterprise)$ ]]; then
  create_field "Component" "SINGLE_SELECT" "Pod-A,Pod-B,Pod-C"
  create_field "Agent-Assigned" "SINGLE_SELECT" "AI,Human,Pair"
  create_field "MTTV-Hours" "NUMBER"
  create_field "Rework-Count" "NUMBER"
  create_field "Sprint-Goal" "TEXT"
  create_field "Blocked-By" "TEXT"
fi

# Enterprise fields (squad + 6)
if [[ "$SCALE" == "enterprise" ]]; then
  create_field "Security-Flag" "SINGLE_SELECT" "None,Review-Required,Approved"
  create_field "Domain-Cluster" "SINGLE_SELECT" "Cluster-A,Cluster-B,Cluster-C"
  create_field "Compliance-Tag" "SINGLE_SELECT" "None,SOC2,HIPAA,PCI"
  create_field "Cost-USD" "NUMBER"
  create_field "Approval-Status" "SINGLE_SELECT" "Pending,Approved,Rejected"
  # Note: In CI status already configured in Status field options
fi

# --- Step 4: Link Repository ---
if [[ -n "$REPO" ]]; then
  info "Linking repository: $REPO..."
  if gh project link "$PROJECT_NUMBER" --owner "$OWNER" --repo "$REPO" 2>/dev/null; then
    ok "Repository linked: $REPO"
  else
    warn "Repository link failed (may already be linked or repo not found)"
  fi
fi

# --- Step 5: Generate CLAUDE.md XML Tags ---
info "Generating CLAUDE.md XML tags..."

# Re-fetch fields to get all IDs including newly created ones
FIELDS_JSON=$(gh project field-list "$PROJECT_NUMBER" --owner "$OWNER" --format json)

echo ""
echo "=========================================="
echo " CLAUDE.md XML Tags"
echo "=========================================="
echo ""
echo "Add the following to your CLAUDE.md under '## Task Management':"
echo ""

# Start XML
echo "<github-project id=\"$PROJECT_ID\" url=\"$PROJECT_URL\" scale=\"$SCALE\">"

# Emit fields (filter out built-in metadata fields)
BUILTIN_FIELDS="Title|Assignees|Labels|Milestone|Repository|Linked pull requests|Reviewers|Tracks|Tracked by|Parent issue|Sub-issues progress"
echo "$FIELDS_JSON" | jq -r --arg skip "$BUILTIN_FIELDS" '
  .fields[] |
  select(.name as $n | ($skip | split("|") | map(. == $n) | any | not)) |
  if .type == "ProjectV2SingleSelectField" then
    "  <field name=\"\(.name)\" id=\"\(.id)\">\n" +
    (.options // [] | map("    <option name=\"\(.name)\" id=\"\(.id)\"/>") | join("\n")) +
    "\n  </field>"
  elif .type == "ProjectV2IterationField" then
    "  <field name=\"\(.name)\" id=\"\(.id)\"/>"
  elif .type == "ProjectV2Field" then
    "  <field name=\"\(.name)\" id=\"\(.id)\"/>"
  else
    empty
  end
' 2>/dev/null || {
  # Fallback: simpler parsing for non-standard jq output
  echo "$FIELDS_JSON" | jq -r '
    .fields[] | select(.name != "Title" and .name != "Assignees" and .name != "Labels" and .name != "Milestone" and .name != "Repository" and .name != "Linked pull requests" and .name != "Reviewers" and .name != "Tracks" and .name != "Tracked by" and .name != "Parent issue" and .name != "Sub-issues progress") |
    if .options then
      "  <field name=\"\(.name)\" id=\"\(.id)\">\n" +
      (.options | map("    <option name=\"\(.name)\" id=\"\(.id)\"/>") | join("\n")) +
      "\n  </field>"
    else
      "  <field name=\"\(.name)\" id=\"\(.id)\"/>"
    end
  '
}

echo "</github-project>"

# --- Step 6: Manual Setup Instructions ---
echo ""
echo "=========================================="
echo " Manual Setup Required"
echo "=========================================="
echo ""
echo "1. Built-in Workflows (Settings > Workflows):"
echo "   - Item added to project → Status: $(echo "$STATUS_OPTIONS" | cut -d',' -f1)"
echo "   - Item closed → Status: Done"
echo "   - Pull request merged → Status: Done"
echo "   - Auto-archive: items in Done for 14 days"
echo ""
echo "2. Views (create manually):"
echo "   - Board View: group by Status, sort by Priority"
echo "   - Table View: show all custom fields"
if [[ "$SCALE" =~ ^(squad|enterprise)$ ]]; then
  echo "   - Roadmap View: date field = Iteration"
fi
echo ""

# --- Step 7: Iteration Field GraphQL Template ---
echo "3. Iteration field (requires GraphQL or UI):"
echo ""
cat <<'GRAPHQL'
   gh api graphql -f query='
   mutation($projectId: ID!) {
     createProjectV2Field(input: {
       projectId: $projectId
       dataType: ITERATION
       name: "Iteration"
     }) {
       projectV2Field { ... on ProjectV2IterationField { id name } }
     }
   }' -f projectId="<PROJECT_ID>"
GRAPHQL
echo ""
echo "   Replace <PROJECT_ID> with: $PROJECT_ID"
echo ""

ok "Setup complete! Scale=$SCALE, Fields=$(echo "$FIELDS_JSON" | jq '.fields | length')"
