;;; init.el --- Emacs initialization file
;;; Commentary:
;; Main configuration file for Emacs
;;; Code:

;; --------------------------------
;; Core Setup
;; --------------------------------

;; Suppress byte-compile warnings
(setq byte-compile-warnings '(not free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))

;; --------------------------------
;; Error Handling and Debugging
;; --------------------------------
;; Better error handling during initialization
(setq debug-on-error nil)                              ;; Don't debug on error by default
(setq debug-on-quit nil)                               ;; Don't debug on quit

;; Function to safely load configuration files
(defun safe-load (file &optional noerror)
  "Safely load a configuration FILE with error handling."
  (condition-case err
      (load file noerror)
    (error
     (message "Error loading %s: %s" file (error-message-string err))
     nil)))

;; Function to check if a file is readable
(defun check-config-file (file)
  "Check if configuration FILE exists and is readable."
  (let ((full-path (expand-file-name file user-emacs-directory)))
    (unless (file-readable-p full-path)
      (message "Warning: Configuration file %s is not readable" full-path)
      nil)))

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
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

;; TLS and Security settings
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq gnutls-verify-error t)                           ;; Verify TLS certificates
(setq gnutls-min-prime-bits 2048)                      ;; Minimum prime bits for security
(setq package-check-signature 'allow-unsigned)         ;; Allow unsigned but warn
(setq package-unsigned-archives nil)                   ;; Don't silently allow unsigned

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
(use-package quelpa-use-package 
  :ensure t
  :custom
  (quelpa-update-melpa-p nil)                    ;; Don't auto-update MELPA
  (quelpa-checkout-melpa-p nil)                  ;; Don't checkout MELPA
  (quelpa-upgrade-interval 7))                   ;; Check for upgrades weekly

(use-package use-package-ensure-system-package :ensure t)

(use-package use-package-chords 
  :ensure t
  :config (key-chord-mode 1))

;; Pin critical packages to specific versions for stability
(setq package-pinned-packages
      '((org . "gnu")                           ;; Use GNU version of org
        (magit . "melpa-stable")                ;; Use stable magit
        (company . "melpa-stable")              ;; Use stable company
        (ivy . "melpa-stable")                  ;; Use stable ivy
        (projectile . "melpa-stable")))         ;; Use stable projectile

;; Auto-update packages (defer to prevent startup delay)
(use-package auto-package-update
  :ensure t
  :defer t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-prompt-before-update t)
  :config
  ;; Remove auto-update-maybe to prevent automatic updates on startup
  ;; Call manually with M-x auto-package-update-now when needed
  )

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
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

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
;; Load configuration modules with error handling
(let ((config-modules '("init-ui"
                        "init-encode"
                        "init-env"
                        "init-editor"
                        "init-navigation"
                        "init-completion"
                        "init-dev-core"
                        "init-dev-languages"
                        "init-dev-web"
                        "init-text-modes"
                        "init-org-complete"
                        "init-ai"
                        "init-claude-code-workflows"
                        "init-platform")))
  (dolist (module config-modules)
    (message "Loading configuration module: %s" module)
    (condition-case err
        (load module)
      (error
       (message "Error loading %s: %s" module (error-message-string err))
       (sit-for 1)))))  ;; Brief pause to see error messages

;; Optional modules (load if present)
(dolist (optional-module '("init-mozc" "init-local"))
  (when (locate-library optional-module)
    (message "Loading optional module: %s" optional-module)
    (condition-case err
        (load optional-module)
      (error
       (message "Error loading optional module %s: %s" 
                optional-module (error-message-string err))))))

;; --------------------------------
;; Libraries
;; --------------------------------
(use-package dash 
  :ensure t
  :defer t
  :config
  (dash-enable-font-lock))

;; --------------------------------
;; Startup Performance Monitoring
;; --------------------------------
(defun my-startup-hook ()
  "Display startup time and configuration statistics."
  (message "Emacs started in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done)
  
  ;; Show package count
  (when (fboundp 'package-installed-p)
    (message "Loaded %d packages" (length package-activated-list)))
  
  ;; Show use-package statistics if available
  (when (and (fboundp 'use-package-statistics)
             use-package-compute-statistics)
    (message "Use-package statistics available. Run M-x use-package-report for details.")))

(add-hook 'emacs-startup-hook #'my-startup-hook)

;; --------------------------------
;; Final Error Handling
;; --------------------------------
;; Catch any remaining initialization errors
(setq after-init-time (current-time))

(provide 'init)
;;; init.el ends here
