;;; init-dev-core.el --- Core development tools
;;; Commentary:
;; Common development tools and utilities shared across all programming languages
;;; Code:

;; --------------------------------
;; Syntax Checking (flymake - 組み込み)
;; --------------------------------
(use-package flymake
  :bind ([f7] . flymake-mode)
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)          ;; Check after 0.5s idle
  (flymake-start-on-save-buffer t)          ;; Check on save
  :config
  (setq eldoc-idle-delay 1.5)
  ;; エラー表示のカスタマイズ
  (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; --------------------------------
;; Version Control - Git
;; --------------------------------
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-blame magit-dispatch)
  :bind ("C-x g" . magit-status)
  :custom
  (magit-need-cygwin-noglob (eq system-type 'windows-nt))
  (magit-diff-refine-hunk t)                 ;; Show word-level diffs
  (magit-save-repository-buffers 'dontask))  ;; Auto-save buffers

;; GitHub/GitLab integration - Forge
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit '(60 . 0))  ;; 60 open, 0 closed
  :config
  (setq auth-sources '("~/.netrc" "~/.authinfo.gpg" "~/.authinfo")))

;; diff-hl - Git差分をフリンジに表示 (git-gutter代替、より軽量)
(use-package diff-hl
  :ensure t
  :defer t
  :delight
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-margin-symbols-alist
   '((insert . "+") (delete . "-") (change . "~")
     (unknown . "?") (ignored . "i")))
  :bind (("C-x v =" . diff-hl-show-hunk)
         ("C-x p" . diff-hl-previous-hunk)
         ("C-x n" . diff-hl-next-hunk)
         ("C-x v r" . diff-hl-revert-hunk))
  :chords (("sg" . transient-diff-hl))
  :config
  ;; ターミナルではマージンモードを使用
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)
    (set-face-foreground 'diff-hl-insert "#a3be8c")
    (set-face-foreground 'diff-hl-delete "#e06c75")
    (set-face-foreground 'diff-hl-change "#ecbe7b"))
  ;; diff-hl transient menu for quick access (transient は magit 経由で遅延ロード)
  (with-eval-after-load 'transient
    (transient-define-prefix transient-diff-hl ()
    "diff-hl and magit commands."
    ["Magit"
     ("m" "status" magit-status)
     ("b" "blame" magit-blame)
     ("d" "dispatch" magit-dispatch)
     ("f" "forge" forge-dispatch)]
    ["Hunk Navigation"
     ("p" "previous" diff-hl-previous-hunk :transient t)
     ("n" "next" diff-hl-next-hunk :transient t)
     ("h" "first" (lambda () (interactive) (goto-char (point-min)) (diff-hl-next-hunk)) :transient t)
     ("l" "last" (lambda () (interactive) (goto-char (point-max)) (diff-hl-previous-hunk)) :transient t)]
    ["Hunk Actions"
     ("r" "revert" diff-hl-revert-hunk :transient t)
     ("u" "show" diff-hl-show-hunk :transient t)])))  ;; end transient-define-prefix, with-eval-after-load, use-package

;; --------------------------------
;; Password / Secret Management
;; --------------------------------
(use-package id-manager
  :ensure t
  :defer t
  :commands id-manager
  :bind ("M-7" . id-manager)
  :custom
  (idm-database-file "~/secret/idm-db.gpg")
  :config
  ;; Enhanced GPG security settings
  (setq epa-file-cache-passphrase-for-symmetric-encryption nil)  ;; Don't cache for security
  (setq epa-pinentry-mode 'loopback)                             ;; Use Emacs for pinentry
  (setq epa-armor t)                                             ;; Use ASCII armor
  (setq epg-gpg-program "gpg2")                                  ;; Use gpg2 if available
  (setq epg-pinentry-mode 'loopback)
  ;; Security warning for GPG
  (when (not (executable-find "gpg2"))
    (message "Warning: gpg2 not found, falling back to gpg")))

;; --------------------------------
;; Code Formatting and EditorConfig
;; --------------------------------
(use-package editorconfig
  :ensure t
  :defer t
  :commands editorconfig-mode
  :delight "EC"
  :hook ((prog-mode text-mode) . editorconfig-mode))

;; --------------------------------
;; Documentation and Comments
;; --------------------------------
;; comment-dwim-2 は MELPA から削除済み、組み込み comment-dwim を使用

;; --------------------------------
;; Compilation and Build Tools
;; --------------------------------
(use-package compile
  :defer t
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-window-height 15)
  :config
  ;; ANSI color support in compilation buffer
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; --------------------------------
;; Debugging Support
;; --------------------------------
(use-package gdb-mi
  :defer t
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

;; --------------------------------
;; Performance Monitoring
;; --------------------------------
;; Show current function name in modeline
(use-package which-func
  :defer t
  :hook (prog-mode . which-function-mode)
  :custom
  (which-func-unknown "n/a"))

;; --------------------------------
;; File Templates (tempel - yasnippet代替)
;; --------------------------------
;; tempel: Lisp構文テンプレート、capf統合、軽量
(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)    ;; テンプレート補完
         ("M-*" . tempel-insert))     ;; テンプレート挿入
  :init
  ;; completion-at-point-functions に追加
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :custom
  (tempel-path (expand-file-name "templates" user-emacs-directory)))

;; tempel-collection: 多言語テンプレート集 (yasnippet-snippets代替)
(use-package tempel-collection
  :ensure t
  :after tempel)

;; --------------------------------
;; Directory and File Management
;; --------------------------------
(use-package dired
  :defer t
  :custom
  (dired-dwim-target t)                    ;; Guess target directory
  (dired-recursive-copies 'always)        ;; Always copy recursively
  (dired-recursive-deletes 'top)          ;; Ask once for recursive deletes
  :config
  ;; Use ls-lisp for consistent behavior across platforms
  (when (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil)))

;; --------------------------------
;; Shell Integration
;; --------------------------------
(use-package sh-script
  :defer t
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.fish\\'" . sh-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(provide 'init-dev-core)
;;; init-dev-core.el ends here
