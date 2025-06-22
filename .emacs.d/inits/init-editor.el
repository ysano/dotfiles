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
(use-package autorevert
  :diminish auto-revert-mode
  :custom
  (auto-revert-verbose nil)                     ;; Silence revert messages
  (auto-revert-check-vc-info t)                 ;; Auto-revert VC info
  (global-auto-revert-non-file-buffers t)      ;; Revert dired and other buffers
  :config
  (global-auto-revert-mode 1))

;; --------------------------------
;; Modern Emacs Features
;; --------------------------------
;; Electric pair mode improvements (Emacs 28+)
(use-package elec-pair
  :config
  (electric-pair-mode 1)
  ;; Don't pair quotes in text modes
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs 
                         '((?\" . ?\")
                           (?\{ . ?\})
                           (?\[ . ?\])
                           (?\( . ?\)))))))

;; Better whitespace handling (modern alternative to whitespace-mode)
(use-package whitespace
  :diminish whitespace-mode
  :custom
  (whitespace-style '(face tabs empty trailing))
  (whitespace-action '(auto-cleanup warn-if-read-only))
  :hook ((prog-mode text-mode) . whitespace-mode))

;; Enhanced abbrev mode
(use-package abbrev
  :diminish abbrev-mode
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (save-abbrevs 'silently)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; Modern repeat mode (Emacs 28+)
(when (>= emacs-major-version 28)
  (use-package repeat
    :custom
    (repeat-exit-timeout 3)                     ;; Exit repeat mode after 3 seconds
    (repeat-exit-key "RET")                     ;; Exit with Return key
    :config
    (repeat-mode 1)))

;; Better tab handling
(use-package tab-bar
  :when (>= emacs-major-version 27)
  :custom
  (tab-bar-show 1)                              ;; Show tab bar when more than 1 tab
  (tab-bar-close-button-show nil)              ;; Hide close button
  (tab-bar-new-button-show nil)                ;; Hide new button
  (tab-bar-tab-hints t)                        ;; Show tab numbers
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;; Improved help system
(use-package help
  :custom
  (help-window-select t)                        ;; Select help window automatically
  (help-enable-symbol-autoload t))              ;; Auto-load symbols in help

;; Enhanced search and replace
(use-package replace
  :custom
  (replace-char-fold t)                         ;; Case-fold search
  (query-replace-highlight t)                   ;; Highlight during query-replace
  (query-replace-show-replacement t))           ;; Show replacement text

;; Modern file handling
(use-package files
  :custom
  (auto-save-visited-mode t)                    ;; Auto-save visited files
  (auto-save-visited-interval 30)               ;; Auto-save every 30 seconds
  (backup-by-copying t)                         ;; Don't clobber symlinks
  (delete-old-versions t)                       ;; Delete old backup versions
  (kept-new-versions 6)                         ;; Keep 6 new versions
  (kept-old-versions 2)                         ;; Keep 2 old versions
  (version-control t)                           ;; Use version control for backups
  (vc-make-backup-files t))                     ;; Backup files under version control

;; Enhanced clipboard and kill ring
(use-package simple
  :custom
  (kill-ring-max 200)                           ;; Larger kill ring
  (kill-do-not-save-duplicates t)              ;; Don't save duplicate kills
  (save-interprogram-paste-before-kill t))     ;; Save clipboard before killing

;; Better buffer switching
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)        ;; Use forward slashes
  (uniquify-separator "/")                      ;; Separator character
  (uniquify-after-kill-buffer-p t)              ;; Rename after killing
  (uniquify-ignore-buffers-re "^\\*"))         ;; Ignore special buffers

;; --------------------------------
;; Modern Display Features
;; --------------------------------
;; Enhanced line numbers (Emacs 26+)
(when (>= emacs-major-version 26)
  (use-package display-line-numbers
    :custom
    (display-line-numbers-type 'relative)      ;; Relative line numbers
    (display-line-numbers-width-start t)       ;; Auto-adjust width
    :hook (prog-mode . display-line-numbers-mode)))

;; Modern window and frame handling
(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)    ;; Modern buffer switching
  (split-width-threshold 80)                   ;; Split windows threshold
  (split-height-threshold 40))

;; Enhanced time display
(use-package time
  :custom
  (display-time-24hr-format t)                 ;; 24-hour format
  (display-time-load-average nil)              ;; Don't show load average
  (display-time-default-load-average nil))

;; Better modeline
(use-package simple
  :config
  (line-number-mode 1)                         ;; Show line numbers in modeline
  (column-number-mode 1)                       ;; Show column numbers in modeline
  (size-indication-mode 1))                    ;; Show buffer size

(provide 'init-editor)
;;; init-editor.el ends here