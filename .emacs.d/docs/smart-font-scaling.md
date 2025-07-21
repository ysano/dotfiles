# Smart Font Scaling for High-DPI Displays

高解像度ディスプレイ対応の自動フォントスケーリングシステム

## 概要

このシステムは、ディスプレイの解像度とDPIに基づいて、Emacsのフォントサイズを自動的に調整します。特に高解像度ディスプレイ（UWQHD、4K、Retina等）で、テキストの視認性を大幅に向上させます。

## 主な機能

### ✨ 自動DPI検出とスケーリング
- ディスプレイの物理サイズとピクセル解像度から正確なDPIを計算
- プラットフォーム固有の補正（macOS Retina、Windows High DPI等）
- 外部ディスプレイ接続時の自動調整

### 🎯 包括的UI要素対応
- **ベースフォント**: エディタ本体のテキスト
- **ミニバッファ**: コマンド入力エリア
- **モードライン**: ステータス表示バー
- **ヘッダーライン**: ファイル情報表示
- **ツールチップ**: ヘルプ表示

### 🔧 プラットフォーム最適化
- **macOS**: Retina対応、SF Mono等ネイティブフォント優先
- **Windows**: ClearType対応、Cascadia Code等推奨フォント
- **Linux**: アンチエイリアス対応、Fira Code等オープンソースフォント

### ⚙️ 柔軟なカスタマイゼーション
- 手動フォントサイズオーバーライド機能
- DPIスケーリング閾値のカスタマイズ
- 優先フォントファミリーの設定

## 現在の環境での効果

### 検出される解像度
```
メインディスプレイ: 3440×1440 UWQHD
→ 推定DPI: 140
→ スケーリング倍率: 1.6倍
→ フォントサイズ: 13pt → 21pt

セカンダリディスプレイ: 2560×1440 QHD  
→ 推定DPI: 130
→ スケーリング倍率: 1.3倍
→ フォントサイズ: 13pt → 17pt
```

## 使用方法

### 基本操作

システムは自動的に有効化されます。特別な設定は不要です。

### キーバインド

| キー | 機能 |
|------|------|
| `C-c f s` | 最適スケーリングを手動適用 |
| `C-c f r` | ディスプレイ情報を再取得して更新 |
| `C-c f i` | 現在の設定情報を表示 |
| `C-c f m` | 手動フォントサイズ設定 |
| `C-c f c` | 手動設定をクリア（自動に戻す） |

### 対話的コマンド

```elisp
;; 現在の設定を確認
M-x sfs-show-current-info

;; 手動でフォントサイズを設定
M-x sfs-set-manual-override

;; 自動スケーリングに戻す
M-x sfs-clear-manual-override

;; ディスプレイ情報を更新
M-x sfs-refresh-display-info
```

## 設定のカスタマイズ

### 基本設定

```elisp
;; 基準フォントサイズ（96 DPI時）
(setq sfs-base-font-size 13)

;; デバッグモード（詳細ログ出力）
(setq sfs-debug-mode t)

;; 手動フォントサイズ設定（自動を無効化）
(setq sfs-manual-override 16)
```

### DPIスケーリング閾値のカスタマイズ

```elisp
(setq sfs-dpi-thresholds 
      '((96 . 1.0)    ; 標準DPI
        (120 . 1.3)   ; 高DPI
        (140 . 1.6)   ; UWQHD
        (180 . 2.0)   ; Retina
        (220 . 2.5))) ; 高DPI Retina
```

### 優先フォントファミリーの設定

```elisp
(setq sfs-font-families 
      '(("Your Preferred Font" . "Your Preferred Font")
        ("Cica" . "Cica")
        ("Sarasa Term J" . "Sarasa Term J")))
```

### 適用するUI要素の設定

```elisp
(setq sfs-ui-elements 
      '(default minibuffer-prompt mode-line mode-line-inactive 
        header-line tooltip fringe))
```

## トラブルシューティング

### フォントが正しく適用されない

1. **デバッグモードを有効化**:
   ```elisp
   (setq sfs-debug-mode t)
   (sfs-refresh-display-info)
   ```

2. **利用可能フォントを確認**:
   ```elisp
   M-x sfs-show-current-info
   ```

3. **手動でフォントサイズを設定**:
   ```elisp
   M-x sfs-set-manual-override
   ```

### 外部ディスプレイ接続時に更新されない

```elisp
;; 手動で更新
M-x sfs-refresh-display-info

;; または
C-c f r
```

### モードラインの高さが調整されない

doom-modelineが読み込まれた後に、以下を実行:

```elisp
(sfs-apply-optimal-scaling)
```

### フォント変更後にレイアウトが崩れる

```elisp
;; フレームを再描画
(redraw-display)

;; または
(force-mode-line-update t)
```

## 技術的詳細

### DPI計算方法

1. **物理計算**: `DPI = (ピクセル数 × 25.4) / 物理サイズ(mm)`
2. **プラットフォーム補正**: システム固有の特性を考慮
3. **フォールバック**: 物理サイズ不明時の解像度ベース推定

### スケーリング決定ロジック

```
DPI値 → スケーリング閾値マッチ → 倍率決定 → フォントサイズ計算
```

### 動的更新機能

- **モニター変更検知**: `window-configuration-change-hook`
- **自動再調整**: ディスプレイ構成変更時の自動スケーリング更新
- **キャッシュ管理**: パフォーマンス最適化

## 互換性

### Emacs バージョン
- **推奨**: Emacs 28+ (ネイティブコンパイル対応)
- **最小**: Emacs 25+ (use-package対応版)

### プラットフォーム
- ✅ **macOS**: 完全対応（Retina最適化）
- ✅ **Windows**: High DPI対応
- ✅ **Linux**: X11/Wayland両対応
- ✅ **その他Unix**: 基本的対応

### 依存関係
- **必須**: なし（Emacs標準機能のみ）
- **推奨**: doom-modeline（モードライン自動調整用）

## よくある質問

### Q: 起動時にフォントが一瞬小さく表示される
A: early-init.elでの早期フォント設定を検討してください:

```elisp
;; early-init.el
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 160)) ; 一時的な大きめ設定
```

### Q: 複数モニターで異なるスケーリングが必要
A: 現在の実装では主モニターのDPIを使用します。将来のバージョンでフレーム単位の調整を予定しています。

### Q: 特定のフォントサイズに固定したい
A: 手動オーバーライドを使用:

```elisp
(setq sfs-manual-override 18)
```

### Q: パフォーマンスへの影響は？
A: 軽微です。キャッシュ機構により、計算は最初の一回のみ実行されます。

## ライセンス

このコードは.emacs.d設定の一部として、同一ライセンス下で提供されます。

## 更新履歴

### v1.0.0 (2024-01-21)
- 初回リリース
- 基本的なDPI検出とスケーリング機能
- プラットフォーム固有最適化
- doom-modeline統合
- 包括的なUI要素対応