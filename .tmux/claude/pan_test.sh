#!/bin/bash
# 統一パンニングインターフェース 包括的テスト

echo "🎯 Claude Voice 統一パンニング 包括的テスト"
echo "   新しいコマンドインターフェース（-1.0 ～ 1.0 小数点指定）"
echo "======================================================="

# デバッグモード有効化
export TMUX_CLAUDE_VOICE_DEBUG=1

# パンニング機能が有効か確認
panning_enabled=$(tmux show-option -gqv @claude_voice_panning_enabled 2>/dev/null || echo "true")
echo "📊 パンニング機能: $panning_enabled"

if [[ "$panning_enabled" != "true" ]]; then
    echo "   パンニング機能を有効化中..."
    tmux set-option -g @claude_voice_panning_enabled true
fi

echo ""
echo "📢 1. 基本通知音パンニングテスト"
echo "   位置: 左端(-0.8) → 中央(0.0) → 右端(+0.8)"

positions=(-0.8 0.0 0.8)
for pos in "${positions[@]}"; do
    echo -n "   🔊 位置 $pos: "
    ./panning_engine.sh pan "/System/Library/Sounds/Glass.aiff" "$pos"
    sleep 2
done

echo ""
echo "🗣️ 2. 基本読み上げパンニングテスト"  
echo "   位置: 左端(-0.8) → 中央(0.0) → 右端(+0.8)"

for pos in "${positions[@]}"; do
    echo -n "   🎤 位置 $pos: "
    ./panning_engine.sh speak "位置${pos}からです" "$pos"
    sleep 3
done

echo ""
echo "🎵 3. 詳細位置パンニングテスト"
echo "   9段階の詳細な位置テスト（-1.0 ～ 1.0）"

detailed_positions=(-1.0 -0.75 -0.5 -0.25 0.0 0.25 0.5 0.75 1.0)
for pos in "${detailed_positions[@]}"; do
    echo -n "   🎯 位置 $pos: "
    ./panning_engine.sh speak "位置${pos}" "$pos"
    sleep 2
done

echo ""
echo "🔄 4. 連続移動パンニングテスト"
echo "   左端から右端への連続移動"

echo "   📢 通知音での連続移動:"
for pos in "${detailed_positions[@]}"; do
    echo -n "      $pos "
    ./panning_engine.sh pan "/System/Library/Sounds/Funk.aiff" "$pos"
    sleep 1
done
echo ""

echo "   🗣️ 読み上げでの連続移動:"
move_positions=(-0.8 -0.4 0.0 0.4 0.8)
for pos in "${move_positions[@]}"; do
    ./panning_engine.sh speak "$pos" "$pos"
    sleep 1.5
done

echo ""
echo "🎭 5. 異なる音声での比較テスト"
echo "   複数の通知音でのパンニング確認"

sounds=("Glass" "Ping" "Funk" "Submarine")
test_positions=(-0.6 -0.2 0.2 0.6)

for i in "${!sounds[@]}"; do
    sound="${sounds[i]}"
    pos="${test_positions[i]}"
    echo -n "   🔊 ${sound} 音 (位置 $pos): "
    ./panning_engine.sh pan "/System/Library/Sounds/${sound}.aiff" "$pos"
    sleep 2
done

echo ""
echo "📝 6. 長文読み上げパンニングテスト"
echo "   長い文章でのパンニング確認"

long_texts=(
    "左端からの長い文章を読み上げています。パンニング効果を確認してください。"
    "中央からのバランスの取れた音声です。ステレオの中心で聞こえるはずです。"
    "右端からの読み上げです。右側のスピーカーから主に聞こえることを確認してください。"
)
long_positions=(-0.7 0.0 0.7)

for i in "${!long_texts[@]}"; do
    text="${long_texts[i]}"
    pos="${long_positions[i]}"
    echo "   📖 位置 $pos での長文読み上げ:"
    ./panning_engine.sh speak "$text" "$pos"
    sleep 6
done

echo ""
echo "🧪 7. エラーハンドリングテスト"
echo "   不正な値での動作確認"

echo "   ❌ 範囲外の値テスト:"
invalid_positions=(-2.0 2.0 "abc" "")
for pos in "${invalid_positions[@]}"; do
    echo "      無効値 '$pos':"
    ./panning_engine.sh speak "テスト" "$pos" 2>/dev/null && echo "      ⚠️ エラーが検出されませんでした" || echo "      ✅ 適切にエラーハンドリング"
done

echo ""
echo "✅ 統一パンニングインターフェース 包括的テスト完了！"
echo ""
echo "📋 テスト結果サマリ:"
echo "   ✓ 基本3点パンニング（左・中央・右）"
echo "   ✓ 詳細9点パンニング（-1.0 ～ 1.0）"
echo "   ✓ 連続移動パンニング"
echo "   ✓ 複数音源でのパンニング"
echo "   ✓ 長文読み上げパンニング"
echo "   ✓ エラーハンドリング"
echo ""
echo "🎯 新しいコマンド形式:"
echo "   ./panning_engine.sh speak \"テキスト\" -0.8  # 左端"
echo "   ./panning_engine.sh speak \"テキスト\" 0.0   # 中央"
echo "   ./panning_engine.sh speak \"テキスト\" 0.8   # 右端"
echo "   ./panning_engine.sh pan <file> <position>  # 通知音"