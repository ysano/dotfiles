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
  :config
  ;; Apply settings
  (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

;; --------------------------------
;; Font Configuration
;; --------------------------------
(use-package faces
  :if (display-graphic-p)
  :config
  ;; Try to set better fonts if available
  (when (find-font (font-spec :name "Cica"))
    (set-face-font 'default "Cica-13"))
  (when (find-font (font-spec :name "Sarasa Term J"))
    (set-face-font 'default "Sarasa Term J-12"))
  (when (find-font (font-spec :name "Migu 1M"))
    (set-face-font 'default "Migu 1M-13")))

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
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
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
;; Whitespace visualization
;; --------------------------------
(use-package whitespace
  :custom
  (whitespace-style '(face tabs spaces trailing space-before-tab
                      indentation empty space-after-tab))
  (whitespace-space-regexp "\\( +\\)")
  :hook (prog-mode . whitespace-mode))

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