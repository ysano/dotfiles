;;; init-navigation.el --- Navigation and search
;;; Commentary:
;; Functionality for navigating code and files
;;; Code:

;; --------------------------------
;; Ivy, Counsel & Swiper
;; --------------------------------
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 20)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)
         ([f6] . ivy-resume)))

(use-package swiper
  :ensure t
  :after ivy
  :custom
  (search-default-mode #'char-fold-to-regexp)
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :config
  (counsel-mode 1)
  :custom
  (counsel-find-file-ignore-regexp "\\.\\(~undo-tree~\\|#\\)\\'")
  :bind (("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)))

;; --------------------------------
;; Jump Navigation
;; --------------------------------
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync))

;; Window navigation
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  (aw-background nil)
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))

;; --------------------------------
;; Project Management
;; --------------------------------
(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;; --------------------------------
;; Tags and Definitions
;; --------------------------------
(use-package counsel-gtags
  :ensure t
  :diminish (counsel-gtags-mode . "Gtags")
  :after counsel
  :hook ((prog-mode php-mode) . counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ("M-," . counsel-gtags-go-backward)))

;; --------------------------------
;; File browsing
;; --------------------------------
(use-package imenu-list
  :ensure t
  :custom
  (imenu-list-focus-after-activation t)
  :bind ("C-'" . imenu-list-smart-toggle))

;; --------------------------------
;; Recent Files
;; --------------------------------
(use-package recentf-ext
  :ensure t
  :custom
  (recentf-max-saved-items 50)
  :config
  (setq recentf-auto-save-timer (run-with-idle-timer 180 t 'recentf-save-list))
  (recentf-mode 1))

;; --------------------------------
;; Ripgrep for fast search
;; --------------------------------
(use-package rg
  :ensure t
  :ensure-system-package
  (rg . ripgrep))

;; --------------------------------
;; Interactive mode for grep results
;; --------------------------------
(use-package wgrep
  :ensure t
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; --------------------------------
;; Guide key
;; --------------------------------
(use-package guide-key
  :ensure t
  :defer t
  :custom
  (guide-key/idle-delay 1.5)
  (guide-key/guide-key-sequence
   '("C-x r" "C-x 4" "C-c"
     (org-mode "C-c C-x")
     (outline-minor-mode "C-c @")))
  (guide-key/highlight-command-regexp
   '("rectangle"
     ("register" . font-lock-type-face)
     ("bookmark" . "hot pink")))
  :config
  (guide-key-mode 1))

;; --------------------------------
;; Hydra for key sequences
;; --------------------------------
(use-package hydra
  :ensure t
  :bind ("C-c C-v" . hydra-toggle/body)
  :chords (("jk" . hydra-general/body))
  :config
  (defhydra hydra-general ()
    "move"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("b" backward-char)
    ("a" beginning-of-line)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("l" recenter-top-bottom)
    ;; clipboard
    ("w" clipboard-kill-ring-save)
    ("SPC" set-mark-command)
    ;; window
    ("S" window-swap-states)
    ("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)
    ("0" delete-window)
    ("x" delete-window)
    ;; buffer
    ("q" kill-buffer)
    (";" counsel-switch-buffer)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("M-n" next-buffer)
    ("M-p" previous-buffer))
  
  (defhydra hydra-toggle (:color blue)
    "toggle"
    ("a" abbrev-mode "abbrev")
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("q" nil "cancel")))

;; --------------------------------
;; TRAMP remote editing
;; --------------------------------
(use-package counsel-tramp
  :ensure t
  :defer t
  :after counsel
  :bind ("C-c s" . counsel-tramp)
  :config
  (if (eq window-system 'w32)
      (setq tramp-default-method "plinkx")
    (setq tramp-default-method "sshx"))
  (setq make-backup-files nil)
  (setq create-lockfiles nil))

(provide 'init-navigation)
;;; init-navigation.el ends here