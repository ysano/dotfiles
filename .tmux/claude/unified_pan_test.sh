#!/bin/bash
# 統一パンニングインターフェーステスト

echo "=== 🎯 統一パンニングインターフェース テスト ==="
echo ""

export TMUX_CLAUDE_VOICE_DEBUG=1

echo "📍 新しいコマンド形式："
echo "  ./panning_engine.sh speak \"テキスト\" -0.8  (左端)"
echo "  ./panning_engine.sh speak \"テキスト\" 0.0   (中央)"
echo "  ./panning_engine.sh speak \"テキスト\" 0.8   (右端)"
echo ""

echo "🔊 左端からテスト (-0.8):"
./panning_engine.sh speak "左端からの音声です" -0.8
sleep 4

echo ""
echo "🔊 中央からテスト (0.0):"
./panning_engine.sh speak "中央からの音声です" 0.0
sleep 4

echo ""
echo "🔊 右端からテスト (0.8):"
./panning_engine.sh speak "右端からの音声です" 0.8
sleep 4

echo ""
echo "🎵 連続パンニング移動テスト:"
positions=(-0.8 -0.4 0.0 0.4 0.8)
for pos in "${positions[@]}"; do
    echo -n "位置 $pos: "
    ./panning_engine.sh speak "位置 $pos です" "$pos"
    sleep 2
done

echo ""
echo "✅ 統一インターフェーステスト完了！"
echo "音声が左端→中央→右端で正しくパンニングされましたか？"
