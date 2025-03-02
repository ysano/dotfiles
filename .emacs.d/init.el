;;; init.el --- Emacs initialization file
;;; Commentary:
;; Main configuration file for Emacs
;;; Code:

;; --------------------------------
;; Core Setup
;; --------------------------------

;; Suppress byte-compile warnings
(setq byte-compile-warnings '(not free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;; Add load paths
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Custom file
(setq custom-file (expand-file-name "custom-settings.el" user-emacs-directory))
(load custom-file t)

;; --------------------------------
;; Package System Setup
;; --------------------------------
(require 'package)

;; Add package repositories
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

;; TLS settings
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-compute-statistics t)
(setq use-package-verbose t)
(setq load-prefer-newer t)

;; Package manager helpers
(use-package quelpa-use-package :ensure t)
(use-package use-package-ensure-system-package :ensure t)
(use-package use-package-chords :ensure t
  :config (key-chord-mode 1))

;; Auto-update packages
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))

;; For diminishing minor modes in modeline
(use-package diminish :ensure t
  :config
  (defmacro safe-diminish (file mode &optional new-name)
    `(with-eval-after-load ,file
       (diminish ,mode ,new-name)))
  (safe-diminish 'autorevert 'auto-revert-mode))
(use-package delight :ensure t)

;; --------------------------------
;; Personal Settings
;; --------------------------------
;; Load secrets file if exists
(load "~/.emacs.secrets" t)

;; User info
(setq user-full-name "Yoshiaki Sano"
      user-mail-address "ysano@ysnet.org")

;; Calendar location
(setq calendar-latitude 35.7
      calendar-longitude 139.6)

;; Browser settings
(setq browse-url-browser-function 'browse-url-default-browser)

;; --------------------------------
;; Startup Optimizations
;; --------------------------------
;; Server mode - start server if in GUI mode
(when window-system
  (use-package server
    :config
    (unless (server-running-p)
      (server-start))))

;; Memory management
(use-package gcmh
  :ensure t
  :diminish
  :custom
  (gcmh-verbose t)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16MB
  (gcmh-low-cons-threshold (* 2 1024 1024))   ;; 2MB
  :config
  (gcmh-mode 1))

;; PATH setup for GUI Emacs
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables 
   '("PATH" "GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  :config
  (exec-path-from-shell-initialize))

;; --------------------------------
;; Core Editor Settings
;; --------------------------------
;; Buffer defaults
(setq-default 
 tab-width 4                         ;; Tab width
 indent-tabs-mode nil                ;; Use spaces instead of tabs
 fill-column 80                      ;; Line width for auto-fill
 truncate-lines t                    ;; Don't wrap lines
 truncate-partial-width-windows t    ;; Don't wrap in split windows
 cursor-type t                       ;; Cursor type
 line-spacing 0.0)                   ;; No extra line spacing

;; Cursor enhancements
(setq x-stretch-cursor t)            ;; Stretch cursor over tabs

;; Highlight current line more efficiently
(global-hl-line-mode 0)
(defun global-hl-line-timer-function ()
  "Update the line highlighting."
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))

;; Ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Encoding settings
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

;; History and session management
(use-package savehist
  :custom
  (history-length t)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

;; Simple UI improvements
(fset 'yes-or-no-p 'y-or-n-p)             ;; y/n instead of yes/no

;; --------------------------------
;; Key Bindings
;; --------------------------------
;; C-h for backspace (help moved to M-1)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-1") 'help)

;; Font scaling
(when (display-graphic-p)
  (global-set-key (kbd "C-<wheel-up>") (lambda () (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "S-=") (lambda () (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-<wheel-down>") (lambda () (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "S--") (lambda () (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "S-0") (lambda () (interactive) (text-scale-set 0))))

;; Navigation
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; --------------------------------
;; Load Component Modules
;; --------------------------------

;; UI and Theming
(load "init-ui")

;; Encoding
(load "init-encode")

;; Environment
(load "init-env")

;; Mozc
;(load "init-mozc")

;; Editor Enhancements
(load "init-editor")

;; Navigation & Search
(load "init-navigation")

;; Completion and autocompletion
(load "init-completion")

;; Development Tools
(load "init-dev")

;; Text modes
(load "init-text-modes")

;; Org mode
;; (load "init-org-mode")

;; Org-roam
;; (load "init-org-roam")

;; Org-integrated
(load "init-org-integrated")

;; AI Assistance
(load "init-ai")

;; OS and Platform specific
(load "init-platform")

;; --------------------------------
;; Libraries
;; --------------------------------
(use-package dash 
  :ensure t
  :config
  (dash-enable-font-lock))

(provide 'init)
;;; init.el ends here
