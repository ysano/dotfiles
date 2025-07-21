#!/bin/bash
# Run Zsh tests without shell snapshots

echo "=== Zsh Configuration Test Results ==="
echo

echo "1. Testing test_zsh_config.zsh script:"
/bin/zsh ./test_zsh_config.zsh 2>&1

echo
echo "2. Testing Zsh configuration loading:"
/bin/zsh -c 'source ~/.zshrc && echo "Zsh設定読み込み成功"' 2>&1

echo
echo "3. Testing find command (should be builtin):"
/bin/zsh -c 'type find' 2>&1

echo
echo "4. Testing grep command (should be builtin):"
/bin/zsh -c 'type grep' 2>&1

echo
echo "5. Testing cat command (should show alias if bat is available):"
/bin/zsh -c 'type cat' 2>&1

echo
echo "6. Testing top command (should show alias if btop/htop is available):"
/bin/zsh -c 'type top' 2>&1

echo
echo "7. Testing Zsh startup time:"
# Use non-interactive mode to avoid gitstatus and zle errors
time /bin/zsh -c 'source ~/.zprofile; exit' 2>&1

echo
echo "=== Test Complete ==="