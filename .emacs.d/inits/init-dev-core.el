;;; init-dev-core.el --- Core development tools
;;; Commentary:
;; Common development tools and utilities shared across all programming languages
;;; Code:

;; --------------------------------
;; Syntax Checking
;; --------------------------------
(use-package flycheck
  :ensure t
  :defer t
  :commands flycheck-mode
  :bind ([f7] . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.1)
  (flycheck-idle-delay 0.5)                 ;; Check after 0.5s idle
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :config
  (setq eldoc-idle-delay 1.5))

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

;; Git time machine for viewing file history
(use-package git-timemachine
  :ensure t
  :defer t
  :commands git-timemachine)

;; Git gutter for showing changes in the buffer
(use-package git-gutter
  :ensure t
  :defer t
  :commands (global-git-gutter-mode git-gutter:toggle)
  :diminish (git-gutter-mode . "gg")
  :custom
  (git-gutter:modified-sign "  ")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  :custom-face
  (git-gutter:modified ((t (:background "purple"))))
  (git-gutter:added ((t (:foreground "green"))))
  (git-gutter:deleted ((t (:foreground "red"))))
  :bind (("C-x v =" . git-gutter:popup-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v SPC" . git-gutter:mark-hunk))
  :chords (("sg" . hydra-git-gutter/body))
  :config
  (global-git-gutter-mode)
  
  ;; Git gutter hydra for quick access
  (defhydra hydra-git-gutter (:color red :hint nil)
    "
_m_agit  _b_lame  _d_ispatch  _t_imemachine  |  hunk: _p_revious  _n_ext  _s_tage  _r_evert  pop_u_p  _SPC_:toggle
"
    ("m" magit-status :exit t)
    ("b" magit-blame :exit t)
    ("t" git-timemachine :exit t)
    ("d" magit-dispatch :exit t)
    ("p" git-gutter:previous-hunk)
    ("n" git-gutter:next-hunk)
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("u" git-gutter:popup-hunk)
    ("SPC" git-gutter:toggle-popup-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)))

;; --------------------------------
;; Project Management
;; --------------------------------
(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-mode projectile-find-file projectile-switch-project)
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-cache-file (concat user-emacs-directory "projectile.cache"))
  (projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))
  :config
  (projectile-mode 1))

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
  :diminish "EC"
  :hook ((prog-mode text-mode) . editorconfig-mode))

;; --------------------------------
;; Documentation and Comments
;; --------------------------------
;; Better comment handling
(use-package comment-dwim-2
  :ensure t
  :defer t
  :bind ("M-;" . comment-dwim-2))

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
;; File Templates
;; --------------------------------
(use-package yasnippet
  :ensure t
  :defer t
  :commands (yas-minor-mode yas-expand)
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

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