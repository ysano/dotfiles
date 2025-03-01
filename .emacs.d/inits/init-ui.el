;;; init-ui.el --- UI configuration
;;; Commentary:
;; UI related settings including themes, fonts, modeline etc.
;;; Code:

;; --------------------------------
;; Basic UI settings
;; --------------------------------
;; Disable UI elements
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; --------------------------------
;; Font Configuration
;; --------------------------------
(when (display-graphic-p)
  (defun my-set-face-font (face fontstring-alist)
    "Evaluate FACE with first working font from FONTSTRING-ALIST."
    (while (and fontstring-alist
               (eq nil (ignore-errors (set-face-font face (car fontstring-alist)) t))
               (setq fontstring-alist (cdr fontstring-alist)))))
  
  ;; Default font (monospace)
  (my-set-face-font 'default
                    '("Cica-13:antialias=standard"
                      "Sarasa Term J-12:antialias=standard"
                      "Migu 1M-13:antialias=standard"
                      "MS Gothic-13:antialias=standard"))
  
  ;; Variable-pitch font (proportional)
  (my-set-face-font 'variable-pitch
                    '("IPAexMincho-13:antialias=standard"
                      "IPAexGothic-13:antialias=standard"
                      "Migu 1C-13:antialias=standard"
                      "MS PGothic-13:antialias=standard"))
  
  ;; Fixed-pitch font (monospace in mixed contexts)
  (my-set-face-font 'fixed-pitch
                    '("Cica-13:antialias=standard"
                      "Sarasa Term J-12:antialias=standard"
                      "Migu 1M-13:antialias=standard"
                      "MS Gothic-13:antialias=standard"))
  
  ;; Fixed-pitch-serif font (monospace serif)
  (my-set-face-font 'fixed-pitch-serif
                    '("IPAMincho-13:antialias=standard"
                      "MS Mincho-13:antialias=standard"))
  
  ;; Tooltip font
  (my-set-face-font 'tooltip
                    '("Cica-11:antialias=standard"
                      "Migu 1M-11:antialias=standard"
                      "MS Gothic-11:antialias=standard")))

;; --------------------------------
;; Icons
;; --------------------------------
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; --------------------------------
;; Themes
;; --------------------------------
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; --------------------------------
;; Mode Line
;; --------------------------------
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-height 25)
  (doom-modeline-bar-width 6)
  (doom-modeline-minor-modes t)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq inhibit-compacting-font-caches t))

;; Hide mode line in certain modes
(use-package hide-mode-line
  :ensure t
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

;; Dimmer - highlight active buffer
(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.15)
  :config
  (dimmer-mode))

;; --------------------------------
;; Window Management
;; --------------------------------
;; XTerm mouse mode for terminal
(use-package xt-mouse
  :if (not (display-graphic-p))
  :config
  (xterm-mouse-mode t))

;; --------------------------------
;; Whitespace visualization
;; --------------------------------
;; Show tabs and trailing whitespace
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)

(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

;; Use advice to add keywords to all major modes
(defun my-add-watchwords ()
  "Highlight tabs and trailing whitespace."
  (font-lock-add-keywords
   nil
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append))))

(add-hook 'prog-mode-hook 'my-add-watchwords)
(add-hook 'text-mode-hook 'my-add-watchwords)

;; --------------------------------
;; Dashboard
;; --------------------------------
(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 10)
                     (bookmarks . 5)
                     (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui)
;;; init-ui.el ends here