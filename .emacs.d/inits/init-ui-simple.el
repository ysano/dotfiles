;;; init-ui-simple.el --- Simple UI configuration
;;; Commentary:
;; Simple UI related settings using use-package patterns
;;; Code:

;; --------------------------------
;; Basic UI settings
;; --------------------------------
(use-package emacs
  :custom
  ;; Disable unnecessary UI elements
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  ;; Basic UI settings
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  (initial-buffer-choice (lambda () (dashboard-refresh-buffer) (get-buffer "*dashboard*")))
  ;; Font settings
  (use-default-font-for-symbols nil)
  ;; Line spacing for better readability (in pixels)
  (line-spacing 4)
  :config
  ;; Apply settings
  (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

;; --------------------------------
;; Unified Font Management
;; --------------------------------
(defvar my-ui-scale-factor 1.0
  "Global UI scale factor for all font sizes and UI elements.")

(defvar my-base-font-size 16
  "Base font size in points (before scaling).")

(defun my-scaled-font-size ()
  "Calculate scaled font size in Emacs height units (1/10 point)."
  (round (* my-base-font-size my-ui-scale-factor 10)))

(defun my-scaled-modeline-height ()
  "Calculate scaled modeline height."
  (round (* 30 my-ui-scale-factor)))

(defun my-scaled-modeline-bar-width ()
  "Calculate scaled modeline bar width."
  (max 3 (round (* 4 my-ui-scale-factor))))

(defun my-apply-unified-font-scaling ()
  "Apply unified font scaling to all UI elements."
  (interactive)
  (when (display-graphic-p)
    (let ((font-height (my-scaled-font-size)))
      ;; Apply to all text faces
      (set-face-attribute 'default nil :height font-height)
      (set-face-attribute 'minibuffer-prompt nil :height font-height)
      (set-face-attribute 'mode-line nil :height font-height)
      (set-face-attribute 'mode-line-inactive nil :height font-height)
      (set-face-attribute 'header-line nil :height font-height)
      (set-face-attribute 'fringe nil :height font-height)
      (message "Unified font scaling applied: scale=%.1f, size=%dpt"
               my-ui-scale-factor (/ font-height 10)))))

;; Apply scaling immediately and after initialization
(when (display-graphic-p)
  (my-apply-unified-font-scaling)
  (run-with-timer 1.0 nil #'my-apply-unified-font-scaling))

;; --------------------------------
;; Icons
;; --------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; --------------------------------
;; Modern theme
;; --------------------------------
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; --------------------------------
;; Modern modeline
;; --------------------------------
(use-package doom-modeline
  :ensure t
  :custom
  ;; 統一スケーリング対応
  (doom-modeline-height (my-scaled-modeline-height))
  (doom-modeline-bar-width (my-scaled-modeline-bar-width))
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-from-project)
  :config
  (doom-modeline-mode 1))

;; --------------------------------
;; Line numbers
;; --------------------------------
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

;; --------------------------------
;; Rainbow delimiters
;; --------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; --------------------------------
;; Dashboard
;; --------------------------------
(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui-simple)
;;; init-ui-simple.el ends here
