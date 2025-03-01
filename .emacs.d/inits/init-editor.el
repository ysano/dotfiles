;;; init-editor.el --- Editor enhancements
;;; Commentary:
;; Editor functionality enhancements
;;; Code:

;; --------------------------------
;; Editor Enhancements
;; --------------------------------

;; Highlight matching parentheses
(use-package paren
  :custom
  (show-paren-style 'expression)
  :config
  (show-paren-mode 1))

;; Auto-pair parentheses
(use-package electric
  :config
  (electric-pair-mode 1))

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight color strings with their color
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode text-mode))

;; Expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Undo system
(use-package undo-tree
  :ensure t
  :diminish
  :custom
  (undo-tree-limit 160000)
  (undo-tree-strong-limit 240000)
  (undo-tree-outer-limit 24000000)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode)
  :bind
  ("C-c u" . (lambda () (interactive) (setq buffer-undo-tree nil))))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Highlight indentation
(use-package highlight-indent-guides
  :ensure t
  :diminish "hig"
  :hook (prog-mode yaml-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'fill))

;; Beacon - flash cursor when switching windows
(use-package beacon
  :ensure t
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode t))

;; camelCase aware editing
(use-package subword
  :ensure t
  :diminish subword-mode
  :config
  (global-subword-mode))

;; EditorConfig support
(use-package editorconfig
  :ensure t
  :diminish "EC"
  :config
  (editorconfig-mode 1))

;; Sudo edit files
(use-package sudo-edit
  :ensure t
  :bind ("C-c C-r" . sudo-edit))

;; Auto-revert mode for all files
(setq auto-revert-interval 10)
(setq auto-revert-check-vc-info t)
(add-hook 'text-mode-hook 'auto-revert-mode)
(add-hook 'prog-mode-hook 'auto-revert-mode)

;; Time display (disabled by default)
(setq display-time-24hr-format 1)
(setq display-time-string-forms 
      '(month "/" day " " dayname " " 24-hours ":" minutes))
(display-time-mode 0)

;; Modeline settings
(line-number-mode 1)
(column-number-mode 0)

;; Line truncation
(setq truncate-lines nil)
(setq truncate-partial-width-windows t)

(provide 'init-editor)
;;; init-editor.el ends here