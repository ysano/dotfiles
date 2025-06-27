;;; init-completion.el --- Completion frameworks
;;; Commentary:
;; Code completion using company, copilot, etc.
;;; Code:

;; --------------------------------
;; Company Mode
;; --------------------------------
(use-package company
  :ensure t
  :diminish "cm"
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)                 ;; No delay
  (company-minimum-prefix-length 2)      ;; Start completing after 2 chars
  (company-selection-wrap-around t)      ;; Wrap around completion list
  (company-show-numbers t)               ;; Show numbers for quick selection
  (company-transformers '(company-sort-by-backend-importance))
  :config
  (global-company-mode)
  ;; Disable inline preview
  (delq 'company-preview-if-just-one-frontend company-frontends)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-selection))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; --------------------------------
;; GitHub Copilot
;; --------------------------------
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :commit "c83ff157eaa8b9ef3c5c86e2c7b1c55f0b48e3d6"  ;; Pin to specific commit
                   :files ("dist" "*.el"))
  :defer t
  :commands copilot-mode
  :if (and (executable-find "node") (executable-find "npm"))
  :custom
  (copilot-idle-delay 0.1)                              ;; Delay before showing completions
  (copilot-max-char -1)                                 ;; No character limit
  :bind ("<f2>" . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("M-<tab>" . copilot-accept-completion-by-line)
              ("M-TAB" . copilot-accept-completion-by-line)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)
              ("C-g" . copilot-clear-overlay))
  :hook (prog-mode . copilot-mode)
  :config
  ;; Only enable in specific modes for security
  (setq copilot-enable-predicates
        '(evil-insert-state-p
          (lambda () (not (or (org-mode-p) (buffer-file-name))))
          (lambda () (not (string-match-p "secret\\|password\\|token" (buffer-name)))))))

;; --------------------------------
;; LSP Mode
;; --------------------------------
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet nil)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-signature-auto-activate nil)  ;; Improve performance
  (lsp-signature-render-documentation nil)
  (lsp-completion-provider :capf)    ;; Use capf for better performance
  :hook
  ((js-mode typescript-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil))

;; LSP with Ivy integration
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; LSP with Treemacs integration
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; --------------------------------
;; Which Key
;; --------------------------------
(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

;; --------------------------------
;; Spell checking
;; --------------------------------
(use-package ispell
  :ensure t
  :defer t
  :custom
  (ispell-program-name "aspell")
  :config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package flyspell
  :ensure t
  :defer t
  :diminish
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-auto-correct-previous-word)
              ("C-," . flyspell-goto-next-error)
              ("C-." . flyspell-auto-correct-word)
              ("C-c $" . nil)  ;; Unbind default key
              ("C-c #" . flyspell-correct-word-before-point))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

;; --------------------------------
;; Translation
;; --------------------------------
(use-package google-translate
  :ensure t
  :defer t
  :commands (google-translate-at-point google-translate-query-translate)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind
  (("C-c t" . google-translate-at-point)
   ("C-c T" . google-translate-query-translate)))

(provide 'init-completion)
;;; init-completion.el ends here
