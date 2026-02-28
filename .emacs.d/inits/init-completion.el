;;; init-completion.el --- Completion frameworks
;;; Commentary:
;; Code completion using corfu, copilot, etc.
;;; Code:

;; --------------------------------
;; Corfu - 軽量補完UI (company代替)
;; --------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                         ;; 自動補完有効
  (corfu-auto-delay 0.2)                 ;; 遅延時間
  (corfu-auto-prefix 2)                  ;; 2文字から補完開始
  (corfu-cycle t)                        ;; 循環移動
  (corfu-preselect 'prompt)              ;; 最初の候補を選択しない
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<tab>" . corfu-complete)
              ("RET" . corfu-insert))
  :init
  (global-corfu-mode))

;; Cape - 追加の補完ソース
(use-package cape
  :ensure t
  :init
  ;; 補完関数を追加
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("C-c c p" . completion-at-point)  ;; capf (C-c p は projectile)
         ("C-c c d" . cape-dabbrev)         ;; dabbrev
         ("C-c c f" . cape-file)            ;; ファイルパス
         ("C-c c h" . cape-history)))

;; --------------------------------
;; GitHub Copilot
;; --------------------------------
;; Helper function to check if copilot server is installed
(defun my/copilot-server-installed-p ()
  "Check if copilot language server is installed."
  (let ((install-dir (expand-file-name "copilot" user-emacs-directory)))
    (or (executable-find "copilot-language-server" t)
        (file-exists-p (expand-file-name "bin/copilot-language-server" install-dir))
        (file-exists-p (expand-file-name "lib/node_modules/@github/copilot-language-server/bin/copilot-language-server" install-dir)))))

(use-package copilot :disabled
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :defer t
  :commands (copilot-mode copilot-install-server copilot-diagnose copilot-login)
  :if (and (executable-find "node") (executable-find "npm"))
  :custom
  (copilot-idle-delay 0.1)                              ;; Delay before showing completions
  (copilot-max-char -1)                                 ;; No character limit
  :bind ("<f2>" . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("M-<tab>" . copilot-accept-completion-by-line)
              ("M-TAB" . copilot-accept-completion-by-line)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)
              ("C-g" . copilot-clear-overlay))
  :config
  ;; Disable in sensitive buffers
  (setq copilot-disable-predicates
        '((lambda () (derived-mode-p 'org-mode))
          (lambda () (string-match-p "secret\\|password\\|token\\|\\.env" (buffer-name)))))
  ;; Only add hook if server is installed
  (when (my/copilot-server-installed-p)
    (add-hook 'prog-mode-hook 'copilot-mode)))

;; --------------------------------
;; Eglot (組み込みLSPクライアント, Emacs 26.3+対応)
;; --------------------------------
(use-package eglot
  :ensure t  ;; Emacs 28以前では自動インストール、29+では組み込み
  :defer t
  :commands (eglot eglot-ensure)
  :hook
  ((go-mode rust-mode python-mode typescript-mode js-mode
    css-mode json-mode html-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . xref-find-definitions)
              ("C-c l D" . xref-find-references)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l h" . eldoc)
              ("C-c l ?" . eglot-install-help))
  :custom
  (eglot-autoshutdown t)              ;; 未使用サーバーを自動シャットダウン
  (eglot-events-buffer-size 0)        ;; イベントログ無効化（パフォーマンス向上）
  (eglot-sync-connect nil)            ;; 非同期接続
  :config
  ;; corfu との統合（補完候補にドキュメント表示）
  (setq eglot-stay-out-of '(company))
  ;; flymake との統合（eglot は flymake を使用）
  (add-to-list 'eglot-stay-out-of 'flymake)

  ;; 言語サーバーインストールヘルプ
  (defun eglot-install-help ()
    "Display language server installation instructions."
    (interactive)
    (with-help-window "*Eglot Install Help*"
      (princ "=== Language Server Installation Guide ===\n\n")
      (princ "【Go】 gopls\n")
      (princ "  go install golang.org/x/tools/gopls@latest\n\n")
      (princ "【Rust】 rust-analyzer\n")
      (princ "  rustup component add rust-analyzer\n\n")
      (princ "【Python】 pyright (推奨)\n")
      (princ "  pip install pyright\n")
      (princ "  # or: npm install -g pyright\n\n")
      (princ "【Python】 pylsp (代替)\n")
      (princ "  pip install python-lsp-server\n\n")
      (princ "【TypeScript/JavaScript】 typescript-language-server\n")
      (princ "  npm install -g typescript typescript-language-server\n\n")
      (princ "【HTML/CSS/JSON】 vscode-langservers\n")
      (princ "  npm install -g vscode-langservers-extracted\n\n")
      (princ "【Bash】 bash-language-server\n")
      (princ "  npm install -g bash-language-server\n\n")
      (princ "【YAML】 yaml-language-server\n")
      (princ "  npm install -g yaml-language-server\n\n")
      (princ "【Dockerfile】 dockerfile-language-server\n")
      (princ "  npm install -g dockerfile-language-server-nodejs\n\n")
      (princ "【PHP】 intelephense\n")
      (princ "  npm install -g intelephense\n\n")
      (princ "【Lua】 lua-language-server\n")
      (princ "  brew install lua-language-server  # macOS\n")
      (princ "  # or download from GitHub releases\n\n")
      (princ "【Terraform】 terraform-ls\n")
      (princ "  brew install hashicorp/tap/terraform-ls  # macOS\n")
      (princ "  # or download from releases.hashicorp.com\n\n")
      (princ "---\n")
      (princ "キーバインド: C-c l ? でこのヘルプを表示\n")
      (princ "サーバー状態: M-x eglot-show-workspace-configuration\n"))))


;; --------------------------------
;; Spell checking
;; --------------------------------
(use-package ispell
  :ensure t
  :defer t
  :custom
  (ispell-program-name "aspell")
  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package flyspell
  :ensure t
  :defer t
  :delight
  :bind (:map flyspell-mode-map
              ("C-c ;" . flyspell-auto-correct-previous-word)
              ("C-c ," . flyspell-goto-next-error)
              ("C-c ." . flyspell-auto-correct-word)
              ("C-c $" . nil)
              ("C-c #" . flyspell-correct-word-before-point))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))


(provide 'init-completion)
;;; init-completion.el ends here
