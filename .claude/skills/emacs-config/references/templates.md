# Templates & Examples

## パッケージ追加テンプレート

```elisp
;; init-<category>.el に追加
(use-package PACKAGE-NAME
  :ensure t
  :defer t                           ; 必須: 遅延読み込み
  :hook (TARGET-MODE . PACKAGE-mode) ; または :commands
  :bind (("C-c <prefix> <key>" . COMMAND))
  :config
  (setq PACKAGE-option value))
```

## 実例: LLM バックエンドパッケージの追加

**やりたいこと**: Ollama 連携のための gptel パッケージを追加したい

**配置先**: `init-ai.el`（AI統合カテゴリ）

```elisp
(use-package gptel
  :ensure t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(qwen2.5-coder:latest)))
```

**ポイント**: `:defer t` + `:commands` で遅延化。外部サービス (Ollama) に依存するため、接続エラーでも起動をブロックしない。

## 実例: OS固有パッケージの条件付き追加

**やりたいこと**: macOS のみ pbcopy 連携を追加したい

```elisp
;; init-platform.el に追加
(when (eq system-type 'darwin)
  (use-package osx-clipboard
    :ensure t
    :config
    (osx-clipboard-mode +1)))
```

**ポイント**: `when` ガードで macOS 以外では読み込まない。
