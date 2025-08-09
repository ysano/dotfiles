#!/bin/bash
# Migration script to switch to refactored implementation

echo "=== Claude Status Detection Migration Script ==="
echo ""

# Backup original scripts
echo "Creating backups..."
if [[ -f "$HOME/.tmux/scripts/claude-status-enhanced.sh" ]]; then
    cp "$HOME/.tmux/scripts/claude-status-enhanced.sh" "$HOME/.tmux/scripts/claude-status-enhanced.sh.bak"
    echo "  ✅ Backed up claude-status-enhanced.sh"
fi

if [[ -f "$HOME/.tmux/scripts/claude-notify.sh" ]]; then
    cp "$HOME/.tmux/scripts/claude-notify.sh" "$HOME/.tmux/scripts/claude-notify.sh.bak"
    echo "  ✅ Backed up claude-notify.sh"
fi

# Create symbolic links to refactored versions
echo ""
echo "Switching to refactored implementation..."

# Option 1: Replace original with refactored (safer approach)
ln -sf "$HOME/.tmux/scripts/claude-status-refactored.sh" "$HOME/.tmux/scripts/claude-status-enhanced.sh"
echo "  ✅ Linked claude-status-enhanced.sh → claude-status-refactored.sh"

ln -sf "$HOME/.tmux/scripts/claude-notify-refactored.sh" "$HOME/.tmux/scripts/claude-notify.sh"
echo "  ✅ Linked claude-notify.sh → claude-notify-refactored.sh"

# Update tmux configuration if needed
echo ""
echo "Checking tmux configuration..."

# Reload tmux configuration
if tmux info &>/dev/null; then
    tmux source-file "$HOME/.tmux.conf" 2>/dev/null && echo "  ✅ Reloaded tmux configuration"
fi

echo ""
echo "=== Migration Complete ==="
echo ""
echo "To rollback to original implementation, run:"
echo "  mv ~/.tmux/scripts/claude-status-enhanced.sh.bak ~/.tmux/scripts/claude-status-enhanced.sh"
echo "  mv ~/.tmux/scripts/claude-notify.sh.bak ~/.tmux/scripts/claude-notify.sh"
echo ""