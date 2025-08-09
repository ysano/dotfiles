#!/bin/bash
# Cleanup script for old/unused Claude status detection files

echo "=== Claude Status Detection Cleanup ==="
echo ""
echo "This script will remove old/unused status detection files."
echo "Make sure the refactored version is working correctly before proceeding."
echo ""
read -p "Do you want to continue? (y/N): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Cleanup cancelled."
    exit 0
fi

# Files to remove
OLD_FILES=(
    "$HOME/.tmux/scripts/claude-status-v2.sh"
    "$HOME/.tmux/scripts/claude-status-v3.sh"
    "$HOME/.tmux/scripts/claude-status-process.sh"
)

# Remove old version files
echo ""
echo "Removing old version files..."
for file in "${OLD_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        rm -f "$file"
        echo "  ✅ Removed: $(basename "$file")"
    else
        echo "  ⏭️  Not found: $(basename "$file")"
    fi
done

# Check if migration is complete
echo ""
echo "Checking migration status..."

if [[ -L "$HOME/.tmux/scripts/claude-status-enhanced.sh" ]]; then
    echo "  ✅ claude-status-enhanced.sh is already linked to refactored version"
    
    # Remove backup if exists
    if [[ -f "$HOME/.tmux/scripts/claude-status-enhanced.sh.bak" ]]; then
        read -p "Remove backup file claude-status-enhanced.sh.bak? (y/N): " -n 1 -r
        echo ""
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            rm -f "$HOME/.tmux/scripts/claude-status-enhanced.sh.bak"
            echo "  ✅ Removed backup"
        fi
    fi
else
    echo "  ⚠️  Migration not complete. Run migrate-to-refactored.sh first."
fi

if [[ -L "$HOME/.tmux/scripts/claude-notify.sh" ]]; then
    echo "  ✅ claude-notify.sh is already linked to refactored version"
    
    # Remove backup if exists
    if [[ -f "$HOME/.tmux/scripts/claude-notify.sh.bak" ]]; then
        read -p "Remove backup file claude-notify.sh.bak? (y/N): " -n 1 -r
        echo ""
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            rm -f "$HOME/.tmux/scripts/claude-notify.sh.bak"
            echo "  ✅ Removed backup"
        fi
    fi
else
    echo "  ⚠️  Migration not complete. Run migrate-to-refactored.sh first."
fi

# Optional: Remove migration and benchmark scripts
echo ""
read -p "Remove migration and benchmark scripts? (y/N): " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -f "$HOME/.tmux/scripts/migrate-to-refactored.sh"
    rm -f "$HOME/.tmux/scripts/benchmark.sh"
    echo "  ✅ Removed migration and benchmark scripts"
fi

echo ""
echo "=== Cleanup Complete ==="
echo ""
echo "Remaining Claude scripts:"
ls -la "$HOME/.tmux/scripts/claude-"*.sh 2>/dev/null | grep -v ".bak$" | awk '{print "  - "$NF}'