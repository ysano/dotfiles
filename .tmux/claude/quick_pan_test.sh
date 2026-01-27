#!/bin/bash
# 統一インターフェース パンニング基本テスト

echo "🎯 統一パンニングインターフェース 基本テスト"
echo "   通知音 + 読み上げ音声の左右パンニング"
echo ""

export TMUX_CLAUDE_VOICE_DEBUG=1

echo "📢 1. 通知音パンニングテスト"
echo "   Glass音を使用して左右パンニング"

# 通知音: 左側
echo -n "🔊 通知音 左端(-0.8): "
./panning_engine.sh pan "/System/Library/Sounds/Glass.aiff" -0.8
sleep 2

# 通知音: 中央 
echo -n "🔊 通知音 中央(0.0): "
./panning_engine.sh pan "/System/Library/Sounds/Glass.aiff" 0.0
sleep 2

# 通知音: 右側
echo -n "🔊 通知音 右端(+0.8): "  
./panning_engine.sh pan "/System/Library/Sounds/Glass.aiff" 0.8
sleep 2

echo ""
echo "🗣️ 2. 読み上げ音声パンニングテスト"
echo "   音声合成の左右パンニング"

# 読み上げ: 左側
echo -n "🎤 読み上げ 左端(-0.8): "
./panning_engine.sh speak "左端からです" -0.8
sleep 3

# 読み上げ: 中央
echo -n "🎤 読み上げ 中央(0.0): "
./panning_engine.sh speak "中央からです" 0.0
sleep 3

# 読み上げ: 右側
echo -n "🎤 読み上げ 右端(+0.8): "
./panning_engine.sh speak "右端からです" 0.8
sleep 3

echo ""
echo "✅ 統一インターフェース基本テスト完了"
echo "   通知音と読み上げ音声の両方でパンニング確認"
