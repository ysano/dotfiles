# Debugging

## 基本コマンド

```bash
# 特定モジュールの構文チェック
emacs --batch -l ~/.emacs.d/inits/init-XXX.el 2>&1 | cat

# 全体の起動テスト
emacs --batch -l ~/.emacs.d/init.el 2>&1 | cat

# デバッグモードで起動
emacs --debug-init

# 起動時間測定
# Emacs内: M-x emacs-init-time
```

## フェイスのデバッグ

### batch モードでの検証

```bash
# フェイス属性を確認（遅延ロードパッケージは require が必要）
emacs --batch -l ~/.emacs.d/init.el \
  --eval '(progn (require (quote magit))
    (princ (format "bg: %S\n" (face-attribute (quote magit-diff-added) :background))))'

# override spec が設定されているか確認
emacs --batch -l ~/.emacs.d/init.el \
  --eval '(progn (require (quote magit))
    (princ (format "%S\n" (get (quote magit-diff-added) (quote face-override-spec)))))'
```

### Emacs 内での対話的デバッグ

| コマンド | 用途 |
|---------|------|
| `C-u C-x =` | カーソル位置のフェイス・オーバーレイ・テキストプロパティを表示 |
| `M-x describe-face` | フェイスの定義と現在の属性を確認 |
| `M-: (text-properties-at (point))` | カーソル位置のテキストプロパティを確認 |
| `M-x list-faces-display` | 全フェイスの一覧と色サンプルを表示 |

### デバッグフロー

1. **色が出ない** → `C-u C-x =` でどのフェイスが使われているか確認
2. **フェイスが適用されていない** → `text-properties-at` で `font-lock-face` があるか確認
3. **フェイスはあるが色が違う** → `describe-face` で属性確認、テーマとの競合を疑う
4. **テーマが上書きしている** → `face-spec-set` + `'face-override-spec` で最優先適用
5. **遅延ロードパッケージ** → `with-eval-after-load` でフェイス定義後に設定
