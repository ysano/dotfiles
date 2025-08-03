#!/bin/bash
# WSL音声機能の簡単なテスト

echo "=== WSL Speech Test ==="

# WSL環境確認
if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
    echo "✅ WSL Environment: Detected"
else
    echo "❌ WSL Environment: Not detected"
fi

# PowerShell確認
echo ""
echo "PowerShell Tests:"

# 方法1: powershell.exe
if command -v powershell.exe >/dev/null 2>&1; then
    echo "✅ powershell.exe: Available"
    version=$(powershell.exe -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null | tr -d '\r\n')
    echo "   Version: $version"
else
    echo "❌ powershell.exe: Not available"
fi

# 方法2: /mnt/c経由
if [[ -f /mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe ]]; then
    echo "✅ Windows PowerShell: Found at /mnt/c/Windows/System32/WindowsPowerShell/v1.0/"
    POWERSHELL_PATH="/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
else
    echo "❌ Windows PowerShell: Not found"
fi

# 方法3: PowerShell Core
if [[ -f /mnt/c/Program\ Files/PowerShell/7/pwsh.exe ]]; then
    echo "✅ PowerShell Core: Found"
    PWSH_PATH="/mnt/c/Program Files/PowerShell/7/pwsh.exe"
else
    echo "❌ PowerShell Core: Not found"
fi

# Speech System Test
echo ""
echo "Speech System Test:"

# PowerShellが見つかった場合のテスト
if [[ -n "$POWERSHELL_PATH" ]]; then
    echo "Testing with Windows PowerShell..."
    result=$("$POWERSHELL_PATH" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            Write-Output 'Speech system available';
        } catch {
            Write-Output 'Speech system unavailable';
        }
    " 2>/dev/null | tr -d '\r\n')
    echo "Result: $result"
    
    if [[ "$result" == "Speech system available" ]]; then
        echo ""
        echo "Testing Japanese speech..."
        "$POWERSHELL_PATH" -Command "
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            \$synth.Speak('WSL音声テストです');
        " 2>/dev/null
        echo "Japanese speech test completed"
    fi
    
elif [[ -n "$PWSH_PATH" ]]; then
    echo "Testing with PowerShell Core..."
    result=$("$PWSH_PATH" -Command "
        try {
            Add-Type -AssemblyName System.Speech;
            \$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;
            Write-Output 'Speech system available';
        } catch {
            Write-Output 'Speech system unavailable';
        }
    " 2>/dev/null | tr -d '\r\n')
    echo "Result: $result"
    
else
    echo "❌ No PowerShell found for testing"
fi

echo ""
echo "=== Test Complete ==="