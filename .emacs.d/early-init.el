;;; early-init.el --- Early initialization file
;;; Commentary:
;; Early initialization for performance optimization
;; This file is processed before package.el and init.el
;;; Code:

;; --------------------------------
;; Performance Optimizations
;; --------------------------------
;; Defer garbage collection during startup for better performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset garbage collection settings after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216  ;; 16mb
                  gc-cons-percentage 0.1)))

;; --------------------------------
;; Disable unnecessary UI early
;; --------------------------------
;; Disable UI elements as early as possible to prevent flash
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable menu-bar, tool-bar, and scroll-bar immediately
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; --------------------------------
;; Package system early setup
;; --------------------------------
;; Prevent package.el from loading packages prior to init-file loading
(setq package-enable-at-startup nil)

;; Set package archives early
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; --------------------------------
;; Modern Emacs Features and Native Compilation
;; --------------------------------
;; Native compilation optimizations (Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil     ;; Silence warnings
        native-comp-deferred-compilation t               ;; Compile packages when idle
        native-comp-speed 2                              ;; Optimize for speed
        native-comp-debug 0                              ;; No debug info
        native-comp-verbose 0                            ;; Quiet compilation
        native-comp-jit-compilation t                    ;; Enable JIT compilation
        native-comp-driver-options '("-O2"))             ;; Compiler optimization
  
  ;; Set native compilation cache directory
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory))))

;; Modern UI improvements for Emacs 29+
(when (>= emacs-major-version 29)
  ;; Enable pixel-precise scrolling
  (setq pixel-scroll-precision-mode t)
  (setq pixel-scroll-precision-large-scroll-height 40.0)
  
  ;; Better long line handling
  (setq bidi-inhibit-bpa t)                             ;; Improve performance with long lines
  (global-so-long-mode 1)                              ;; Handle very long lines better
  
  ;; Modern scrolling
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 101)
  (setq auto-window-vscroll nil))

;; Enhanced minibuffer (Emacs 28+)
(when (>= emacs-major-version 28)
  (setq enable-recursive-minibuffers t)                 ;; Allow recursive minibuffers
  (setq resize-mini-windows t)                          ;; Resize minibuffer to fit content
  (setq max-mini-window-height 0.3))                    ;; Max 30% of frame height

;; Prefer newer files
(setq load-prefer-newer t)

;; --------------------------------
;; File handling optimizations
;; --------------------------------
;; Disable file name handlers during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file name handlers after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

;; --------------------------------
;; Frame optimizations
;; --------------------------------
;; Prevent unwanted runtime compilation and frame resizing
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; --------------------------------
;; Site-run-file
;; --------------------------------
;; Disable site-run-file to prevent loading site-specific configurations
(setq site-run-file nil)

;; --------------------------------
;; Security Settings
;; --------------------------------
;; Disable risky functions
(put 'eval-expression 'disabled nil)                    ;; Keep eval enabled but be careful
(put 'downcase-region 'disabled nil)                   ;; Enable downcase-region
(put 'upcase-region 'disabled nil)                     ;; Enable upcase-region

;; Secure defaults
(setq auth-source-save-behavior nil)                   ;; Don't save auth info automatically
(setq create-lockfiles t)                              ;; Create lockfiles to prevent conflicts
(setq delete-by-moving-to-trash t)                     ;; Move deleted files to trash

;; File safety
(setq require-final-newline t)                         ;; Always end files with newline
(setq backup-by-copying t)                             ;; Don't clobber symlinks

;; --------------------------------
;; Initial settings
;; --------------------------------
;; Set initial scratch message
(setq initial-scratch-message
      ";; Welcome to Emacs!\n;; This configuration is optimized for performance and productivity.\n;; Remember: With great power comes great responsibility.\n\n")

;; Set initial frame settings
(when (display-graphic-p)
  (push '(width . 120) default-frame-alist)
  (push '(height . 40) default-frame-alist)
  (push '(alpha . (95 . 90)) default-frame-alist))

(provide 'early-init)
;;; early-init.el ends here