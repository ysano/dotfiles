#!/bin/bash
# mock-gh.sh - Mock gh CLI for integration tests.
# Inject via PATH="$(dirname "$0"):$PATH" before running tests.
# Records all calls to $MOCK_GH_LOG for assertion.

MOCK_GH_LOG="${MOCK_GH_LOG:-/tmp/mock-gh.log}"

log_call() {
  echo "$*" >> "$MOCK_GH_LOG"
}

# --- Dispatch by subcommand chain ---
case "$1" in
  auth)
    case "$2" in
      status)
        log_call "gh auth status"
        echo "github.com"
        echo "  Token scopes: project, repo, read:org"
        ;;
    esac
    ;;

  project)
    case "$2" in
      item-list)
        log_call "gh project item-list $*"
        # Return mock items JSON matching the test CLAUDE.md
        cat <<'JSON'
{
  "items": [
    {
      "id": "PVTI_item001",
      "content": {
        "url": "https://github.com/testuser/testrepo/issues/42",
        "type": "Issue",
        "title": "Test Issue 42"
      },
      "status": "Todo"
    },
    {
      "id": "PVTI_item002",
      "content": {
        "url": "https://github.com/testuser/testrepo/issues/99",
        "type": "Issue",
        "title": "Test Issue 99 - already Review"
      },
      "status": "Review"
    }
  ]
}
JSON
        ;;

      item-edit)
        log_call "gh project item-edit $*"
        # Success - output the edited item
        echo '{"id":"PVTI_item001"}'
        ;;

      field-list)
        log_call "gh project field-list $*"
        # Return only built-in fields (Status + Title).
        # Custom fields are NOT included so create_field idempotency checks pass.
        cat <<'JSON'
{
  "fields": [
    {"name": "Title", "id": "PVTF_title", "type": "ProjectV2Field"},
    {"name": "Status", "id": "PVTSSF_status001", "type": "ProjectV2SingleSelectField", "options": [
      {"name": "Todo", "id": "98456001"},
      {"name": "In Progress", "id": "98456002"},
      {"name": "Review", "id": "98456003"},
      {"name": "Done", "id": "98456004"}
    ]}
  ]
}
JSON
        ;;

      field-create)
        log_call "gh project field-create $*"
        echo '{"id":"PVTF_new001"}'
        ;;

      create)
        log_call "gh project create $*"
        echo '{"number": 99}'
        ;;

      view)
        log_call "gh project view $*"
        echo '{"id": "PVT_new123", "url": "https://github.com/users/testuser/projects/99"}'
        ;;

      link)
        log_call "gh project link $*"
        echo "Linked."
        ;;

      *)
        log_call "gh project $*"
        ;;
    esac
    ;;

  api)
    log_call "gh api $*"
    echo '{"data":{}}'
    ;;

  *)
    log_call "gh $*"
    ;;
esac

exit 0
