# Claude Voice Core Modules

## アーキテクチャ概要

```
┌─────────────────────────────┐
│   Application Layer          │
│   (claude-voice)            │
└──────────────┬──────────────┘
               ↓
┌─────────────────────────────┐
│   Business Logic Layer       │
│   (summary_engine.sh)        │
│   (summary_interface.sh)     │
└──────────┬──────────────────┘
           ↓
┌─────────────────────────────┐
│   Data Access Layer          │
│   (screen_capture.sh)        │
│   (llm_manager.sh)          │
└─────────────────────────────┘
```

## モジュール責務

### summary_interface.sh
- **責務**: 統一された要約インターフェースの提供
- **主要関数**:
  - `create_summary_options()`: オプション構造体の初期化
  - `generate_unified_summary()`: 統一要約生成
  - `detect_context_from_content()`: コンテキスト自動判定
  - `status_to_context()`: ステータス→コンテキスト変換

### summary_engine.sh
- **責務**: 要約生成のビジネスロジック
- **主要関数**:
  - `generate_summary()`: 汎用要約生成
  - `generate_brief_summary()`: 短文要約生成
  - `generate_status_change_summary()`: ステータス変更時要約

### llm_manager.sh
- **責務**: LLM通信とOllama統合
- **主要関数**:
  - `query_llm()`: LLMへのクエリ実行
  - `summarize_screen_content()`: スクリーン内容の要約
  - `get_best_available_model()`: 最適モデル選択

### screen_capture.sh
- **責務**: tmuxペインからのテキスト取得
- **主要関数**:
  - `capture_screen_text()`: スクリーンテキスト取得
  - `capture_with_smart_expansion()`: 智的範囲拡大
  - `detect_character_frames()`: 枠文字検出

## 使用例

### 基本的な要約生成
```bash
# オプション構造体を使用
declare -A opts
create_summary_options opts
opts[content]="要約したいテキスト"
opts[max_length]=100
opts[context]="complete"

summary=$(generate_unified_summary opts)
echo "$summary"
```

### レガシーAPI互換
```bash
# 従来の方法も動作
summary=$(summarize_screen_content "テキスト" 50 "general")
```

## モデル優先順位

Ollamaモデルは以下の優先順位で自動選択されます：
1. gemma3:1b (最軽量・高速)
2. gemma2:2b (バランス型)
3. phi4-mini:latest (技術文書向け)
4. orca-mini:latest (汎用)

## エラーハンドリング

すべての関数は適切なフォールバック処理を実装：
- LLM不在時: 簡易要約へフォールバック
- モジュール未ロード時: ローカル処理へフォールバック
- tmuxペイン不在時: エラーログ出力

## パフォーマンス最適化

- モデル選択結果のキャッシュ
- 短文(100文字以下)の処理スキップ
- 智的範囲拡大による効率的キャプチャ