# Claude Voice Platform Separation Architecture

## 概要

OS固有コードをcoreディレクトリから分離し、プラットフォーム別のモジュール化を実現しました。

## 実装した変更

### 1. プラットフォーム別ディレクトリ構造

```
~/.tmux/claude/
├── core/                           # クロスプラットフォーム共通機能
│   ├── platform_detector.sh        # プラットフォーム検出エンジン
│   ├── cross_platform_voice.sh     # 統合音声API
│   └── [共通モジュール群]
└── platforms/                      # プラットフォーム固有機能
    ├── windows/                     # Windows固有
    │   ├── windows_audio_system.sh
    │   ├── windows_notification_system.sh
    │   ├── windows_tts_engine.sh
    │   └── powershell_engine.sh
    ├── wsl/                         # WSL固有
    │   ├── wsl_voice_engine_v2.sh
    │   ├── wsl_voice_engine.sh
    │   ├── wsl_host_resolver.sh
    │   ├── wsl_integration.sh
    │   ├── base.sh
    │   └── modules/
    │       ├── wsl_environment.sh
    │       ├── powershell_interface.sh
    │       ├── status_sound_engine.sh
    │       └── voice_detection.sh
    ├── macos/                       # macOS固有
    ├── linux/                       # Linux固有
    └── freebsd/                     # FreeBSD固有
```

### 2. クロスプラットフォーム抽象化層

#### platform_detector.sh
- 自動プラットフォーム検出（WSL、Linux、macOS、Windows、FreeBSD）
- 60秒キャッシュによる高速化
- プラットフォーム固有機能の可用性チェック
- 統一コマンド実行インターフェース

#### cross_platform_voice.sh
- 統一音声API（speak、play_sound、notify）
- プラットフォーム固有実装への自動デリゲート
- Claude Codeステータス別効果音統合
- フォールバック機能

### 3. 改善されたモジュール統合

#### WSL Voice Engine v2.0の更新
- プラットフォーム分離パス対応
- 統合platform_detector使用
- クリーンな依存関係管理

#### Smart Status Script v2.0
- プラットフォーム統合対応
- 複数エンジンフォールバック
- 2秒キャッシュによるパフォーマンス最適化

## 利点

### 1. 保守性の向上
- OS固有コードの明確な分離
- 責任範囲の明確化
- 新プラットフォーム対応の簡素化

### 2. パフォーマンス改善
- プラットフォーム検出キャッシュ（60秒）
- ステータス検出キャッシュ（2秒）
- 不要なOS固有モジュール読み込み回避

### 3. 拡張性
- 新プラットフォーム追加の簡素化
- 機能別モジュールの独立性
- 統一APIによる互換性保証

## 使用方法

### プラットフォーム検出
```bash
# 現在のプラットフォーム取得
~/.tmux/claude/core/platform_detector.sh detect

# プラットフォーム固有パス取得
~/.tmux/claude/core/platform_detector.sh path

# 音声機能可用性チェック
~/.tmux/claude/core/platform_detector.sh check audio
```

### 統合音声API
```bash
# 音声合成
~/.tmux/claude/core/cross_platform_voice.sh speak "テスト音声"

# ステータス音再生
~/.tmux/claude/core/cross_platform_voice.sh status "✅"

# 複合通知
~/.tmux/claude/core/cross_platform_voice.sh notify "処理完了" "✅" 1 "both"
```

### ステータス検出
```bash
# スマートステータス検出v2.0
~/.tmux/scripts/claude-status-smart-v2.sh 1 0 test
```

## 互換性

- 既存のClaude Voice設定との後方互換性を維持
- tmux設定への影響なし
- 段階的移行可能

## テスト状況

- ✅ プラットフォーム検出（WSL）
- ✅ パス解決
- ✅ ステータス検出
- ✅ WSL Voice Engine v2.0統合
- ⚠️ 音声出力（要調整）

## 今後の課題

1. 他プラットフォーム（macOS、Linux）の実装完了
2. 音声出力の詳細調整
3. パフォーマンステストの実施
4. 包括的統合テストの実行