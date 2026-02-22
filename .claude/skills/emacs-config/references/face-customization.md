# Face カスタマイズ

## フェイス優先順位（高→低）
1. `face-spec-set FACE SPEC 'face-override-spec` — 最優先、テーマも上書き
2. `custom-set-faces` — user テーマ（テーマより優先）
3. `custom-theme-set-faces` — テーマ定義
4. `set-face-attribute` — 直接設定（テーマより低い）
5. `defface` — デフォルト定義

## 遅延ロードパッケージのフェイス設定

フェイスは `defface` 前には存在しない。`set-face-attribute` や `face-spec-set` は存在しないフェイスにはエラーになる。

```elisp
;; NG: magit ロード前は magit-diff-added が存在しない
(set-face-attribute 'magit-diff-added nil :background "#1a3a1a")

;; OK: パッケージロード後に適用
(with-eval-after-load 'magit
  (face-spec-set 'magit-diff-added
    '((t :background "#1a3a1a" :foreground "#a3be8c" :extend t))
    'face-override-spec))
```

**配置場所**: テーマ設定（`init-ui-simple.el`）の `doom-themes` `:config` ブロック内。テーマ `load-theme` の後に記述する。

## ターミナル固有のフェイス設定

- ターミナルでは `COLORTERM=truecolor` が必要（hex color 使用時）
- `diff-hl-margin-mode` では foreground/background 同色にしない（記号が見えなくなる）
- `:extend t` で行末まで背景色を伸ばす

```elisp
(unless (display-graphic-p)
  (diff-hl-margin-mode 1)
  ;; margin 記号の foreground は見える色に設定
  (set-face-foreground 'diff-hl-insert "#a3be8c")
  (set-face-foreground 'diff-hl-delete "#e06c75")
  (set-face-foreground 'diff-hl-change "#ecbe7b"))
```

## パッケージバージョン不整合

magit と magit-section 等、分離されたパッケージ間のバージョン不整合はフェイスペイント処理の不具合を引き起こす。更新時は関連パッケージを全て揃えて更新すること。

```
M-x straight-pull-all  ;; 全パッケージ一括更新
```
