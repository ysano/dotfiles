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
  ;; Better ivy behavior
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)
         ([f6] . ivy-resume)
         ;; Better completion navigation
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)))

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
  ;; Enhanced counsel-find-file directory navigation
  (with-eval-after-load 'counsel
    (define-key counsel-find-file-map (kbd "C-<backspace>") 'counsel-up-directory)
    (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory)
    (define-key counsel-find-file-map (kbd "C-M-y") 'ivy-insert-current-full))
  :custom
  (counsel-find-file-ignore-regexp "\\.\\(~undo-tree~\\|#\\)\\'")
  ;; Better counsel-find-file behavior
  (counsel-find-file-at-point t)
  (counsel-preselect-current-file t)
  :bind (("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-x C-r" . counsel-buffer-or-recentf)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))


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
  :if (executable-find "rg"))

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
;; Which-key (modern replacement for guide-key)
;; --------------------------------
(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 1.0)
  (which-key-max-description-length 32)
  :config
  (which-key-mode 1))

;; --------------------------------
;; Hydra for key sequences
;; --------------------------------
(use-package hydra
  :ensure t
  :bind ("C-c C-v" . hydra-toggle/body)
  :config
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