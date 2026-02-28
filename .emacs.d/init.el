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

;; Fix GPG path for MINGW64/MSYS2/Cygwin environment (must be before package-initialize)
;; MSYS2's gpg cannot handle Windows-style paths (d:/...) in --homedir,
;; treating them as relative paths. Convert to MSYS2-style (/d/...).
(when (eq system-type 'windows-nt)
  (defun my/windows-to-msys2-path (path)
    "Convert Windows-style PATH (d:/...) to MSYS2-style (/d/...)."
    (when (and path (string-match "\\`\\([a-zA-Z]\\):/" path))
      (concat "/" (downcase (match-string 1 path))
              (substring path 2))))

  ;; Fix package-gnupghome-dir for MSYS2 gpg (must run before package-initialize)
  (with-eval-after-load 'package
    (when-let ((converted (my/windows-to-msys2-path package-gnupghome-dir)))
      (setq package-gnupghome-dir converted)))

  ;; Fix EPG homedir for general GPG operations
  (defun my/epg-convert-homedir-for-msys2 (context &rest _)
    "Convert Windows-style EPG homedir to MSYS2-style."
    (when-let ((converted (my/windows-to-msys2-path
                           (epg-context-home-directory context))))
      (epg-context-set-home-directory context converted)))
  (with-eval-after-load 'epg
    (advice-add 'epg-verify-string :before #'my/epg-convert-homedir-for-msys2)
    (advice-add 'epg-verify-file :before #'my/epg-convert-homedir-for-msys2)))

;; Add package repositories (early-init.elで設定済み、追加設定のみ)
;; early-init.elで gnu, nongnu, melpa が設定されている
;; ここでは必要に応じて追加のリポジトリのみ設定
;; melpa-stableは使用しない（MELPAのみで統一）

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
;; デバッグ時のみ有効化: M-x use-package-report で統計確認可能
(setq use-package-compute-statistics nil)
(setq use-package-verbose nil)
(setq load-prefer-newer t)

;; Auto-refresh package archives on install failure (stale MELPA cache)
(defvar my/package-refreshed-p nil
  "Non-nil if package archives have been refreshed in this session.")

(defun my/package-install-refresh-on-error (orig-fn &rest args)
  "Refresh package archives and retry if install fails."
  (condition-case err
      (apply orig-fn args)
    (error
     (if my/package-refreshed-p
         (signal (car err) (cdr err))
       (setq my/package-refreshed-p t)
       (message "Package install failed, refreshing archives and retrying...")
       (package-refresh-contents)
       (apply orig-fn args)))))

(advice-add 'package-install :around #'my/package-install-refresh-on-error)

;; Package manager helpers
(use-package quelpa-use-package
  :ensure t
  :custom
  (quelpa-update-melpa-p nil)                    ;; Don't auto-update MELPA
  (quelpa-checkout-melpa-p nil)                  ;; Don't checkout MELPA
  (quelpa-upgrade-interval 7))                   ;; Check for upgrades weekly


(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; Pin critical packages to specific versions for stability
(setq package-pinned-packages
      '((org . "gnu")))                         ;; Use GNU version of org
        ;; melpa-stableを削除したため、magitとprojectileのピン留めも削除

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

;; For diminishing minor modes in modeline (using delight only)
(use-package delight :ensure t)

;; --------------------------------
;; Personal Settings
;; --------------------------------
(use-package emacs
  :custom
  (user-full-name "Yoshiaki Sano")
  (user-mail-address "ysano@ysnet.org")
  (calendar-latitude 35.7)
  (calendar-longitude 139.6)
  (browse-url-browser-function 'browse-url-default-browser)
  :config
  (load "~/.emacs.secrets" t))

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
  :defer 0.5
  :delight
  :custom
  (gcmh-verbose t)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16MB
  (gcmh-low-cons-threshold (* 2 1024 1024))   ;; 2MB
  :config
  (gcmh-mode 1))

;; PATH setup for GUI Emacs (macOS only, WSLでは不要)
(use-package exec-path-from-shell
  :ensure t
  :if (and (memq window-system '(mac ns))  ;; macOSのみ有効（WSL/Xは除外）
           (not (string-match-p "WSL\\|microsoft" (or (getenv "WSL_DISTRO_NAME") (shell-command-to-string "uname -r")))))
  :custom
  (exec-path-from-shell-variables
   '("PATH" "GOPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  :config
  (exec-path-from-shell-initialize))

;; --------------------------------
;; Core Editor Settings
;; --------------------------------
(use-package emacs
  :custom
  ;; Buffer defaults
  (tab-width 4)
  (indent-tabs-mode nil)
  (fill-column 80)
  (truncate-lines t)
  (truncate-partial-width-windows t)
  (cursor-type t)
  (line-spacing 0.0)
  (x-stretch-cursor t)
  ;; Ediff settings
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; History settings
  (history-length t)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  ;; Encoding settings
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (set-buffer-file-coding-system 'utf-8-unix)
  ;; History mode
  (savehist-mode 1)
  ;; Simple UI improvements
  (fset 'yes-or-no-p 'y-or-n-p))

;; Highlight current line
(use-package hl-line
  :custom
  (global-hl-line-sticky-flag t)
  :config
  (global-hl-line-mode 1))

;; --------------------------------
;; Key Bindings
;; --------------------------------
;; C-h for backspace (help moved to M-1)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-1") 'help)

;; Font scaling
(use-package face-remap
  :if (display-graphic-p)
  :bind (("C-<wheel-up>" . text-scale-increase)
         ("S-=" . text-scale-increase)
         ("C-<wheel-down>" . text-scale-decrease)
         ("S--" . text-scale-decrease)
         ("S-0" . (lambda () (interactive) (text-scale-set 0)))))

;; Navigation
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; --------------------------------
;; Load Component Modules
;; --------------------------------
;; --------------------------------
;; Load Configuration Modules
;; --------------------------------
;; Core modules (required)
(require 'init-ui-simple)
(require 'init-editor)
(require 'init-navigation)
(require 'init-completion)
(require 'init-dev-core)
(require 'init-dev-languages)
(require 'init-dev-web)
(require 'init-text-modes)
(require 'init-org-simple)
(require 'init-ai)
(require 'init-japanese)
(require 'init-platform)

;; Optional modules (load if present)
(when (locate-library "init-local")
  (require 'init-local))

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
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (when (fboundp 'package-installed-p)
              (message "Loaded %d packages" (length package-activated-list)))))

;; --------------------------------
;; Final Error Handling
;; --------------------------------
;; Catch any remaining initialization errors
(setq after-init-time (current-time))

(provide 'init)
;;; init.el ends here
