;;; init-dev.el --- Development tools
;;; Commentary:
;; Settings for programming languages and development tools
;;; Code:

;; --------------------------------
;; Common Development Settings
;; --------------------------------

;; Flycheck syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :bind ([f7] . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.1)
  (eldoc-idle-delay 1.5))

;; --------------------------------
;; Version Control
;; --------------------------------

;; Git support
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-need-cygwin-noglob (eq system-type 'windows-nt)))

;; Git time machine for viewing file history
(use-package git-timemachine
  :ensure t)

;; Git gutter
(use-package git-gutter
  :ensure t
  :diminish (git-gutter-mode . "gg")
  :custom
  (git-gutter-modified-sign "  ")
  (git-gutter-added-sign "++")
  (git-gutter-deleted-sign "--")
  :custom-face
  (git-gutter-modified ((t (:background "purple"))))
  (git-gutter-added ((t (:foreground "green"))))
  (git-gutter-deleted ((t (:foreground "red"))))
  :bind (("C-x v =" . git-gutter:popup-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v SPC" . git-gutter:mark-hunk))
  :chords (("sg" . hydra-git-gutter/body))
  :config
  (global-git-gutter-mode)
  
  ;; Git gutter hydra
  (defhydra hydra-git-gutter (:color red :hint nil)
    "
_m_agit  _b_lame  _d_ispatch  _t_imemachine  |  hunk: _p_revious  _n_ext  _s_tage  _r_evert  pop_u_p  _SPC_:toggle
"
    ("m" magit-status :exit t)
    ("b" magit-blame :exit t)
    ("t" git-timemachine :exit t)
    ("d" magit-dispatch :exit t)
    ("p" git-gutter:previous-hunk)
    ("n" git-gutter:next-hunk)
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("u" git-gutter:popup-hunk)
    ("SPC" git-gutter:toggle-popup-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)))

;; --------------------------------
;; Password / ID Management
;; --------------------------------
(use-package id-manager
  :ensure t
  :bind ("M-7" . id-manager)
  :custom
  (idm-database-file "~/secret/idm-db.gpg")
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setenv "GPG_AGENT_INFO" nil)
  (setq epa-pinentry-mode 'loopback))

;; --------------------------------
;; Languages
;; --------------------------------

;; Ruby
(use-package ruby-mode
  :ensure t
  :ensure-system-package
  ((rubocop     . "gem install rubocop")
   (ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry         . "gem install pry"))
  :mode ("\\.rb\\'" "Rakefile")
  :interpreter "ruby")

(use-package ruby-compilation
  :ensure t
  :after ruby-mode
  :bind (:map ruby-mode-map
              ("C-x t" . ruby-compilation-this-buffer)
              ("C-x T" . ruby-compilation-this-test)))

;; Python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package py-yapf
  :ensure t
  :hook (python-mode . py-yapf-enable-on-save))

;; Golang
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :config
  (set (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet"))

(use-package go-eldoc
  :ensure t
  :hook (go-mode . go-eldoc-setup))

;; PHP
(use-package php-mode
  :ensure t
  :custom
  (php-manual-url 'ja)
  (php-mode-coding-style 'psr2)
  (php-mode-template-compatibility nil)
  :bind (:map php-mode-map
              ([f5] . phpunit-current-test)
              ("S-<f5>" . phpunit-current-project)
              ("C-c -" . php-current-class)
              ("C-c =" . php-current-namespace))
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t
                    c-tab-always-indent t
                    c-auto-newline nil
                    c-hungry-delete-key t
                    c-basic-offset 4
                    tab-width 4
                    indent-tabs-mode nil)
              (subword-mode 1))))

(use-package phpunit
  :ensure t
  :after php-mode)

(use-package php-eldoc
  :ensure t
  :hook (php-mode . php-eldoc-enable))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; --------------------------------
;; Web development
;; --------------------------------
(use-package web-mode
  :ensure t
  :mode ("\\.p?html\\'"
         "\\.tpl\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.blade\\.php\\'")
  :custom
  (web-mode-engines-alist
   '(("php"    . "\\.phtml\\'")
     ("blade"  . "\\.blade\\.php\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode html-mode css-mode web-mode))

;; Vue.js
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :custom
  (mmm-submode-decoration-level 2)
  :config
  (add-hook 'vue-mode-hook #'add-node-modules-path)
  (add-hook 'vue-mode-hook 'flycheck-mode)
  (add-hook 'vue-mode-hook (lambda () 
                             (local-set-key (kbd "TAB") 'indent-relative-first-indent-point)))
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode))

(use-package add-node-modules-path
  :ensure t
  :hook (js-mode json-mode web-mode))

;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :custom
  (json-mode-indent-level 4)
  (json-encoding-default-indentation "    "))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2)
  :hook (terraform-mode . my-terraform-mode-init)
  :config
  (defun my-terraform-mode-init ()
    ;; Add terraform-specific setup here
    ))

;; PowerShell (on Windows)
(use-package powershell
  :if (memq system-type '(cygwin windows-nt))
  :ensure t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :interpreter "powershell")

;; Nginx config
(use-package nginx-mode
  :ensure t
  :mode "nginx.*\\.conf\\'")

;; Apache config
(use-package apache-mode
  :ensure t
  :mode ("\\.htaccess\\'"
         "/etc/httpd.+\\.conf\\'"
         "/etc/apache.+\\.conf\\'"
         "sites-\\(available\\|enabled\\)/"))

;; PlantUML
(use-package plantuml-mode
  :ensure t
  :mode "\\.puml\\'"
  :custom
  (plantuml-defalt-exec-mode 'jar)
  (org-plantuml-jar-path "~/.emacs.d/lib/plantuml.jar"))

(provide 'init-dev)
;;; init-dev.el ends here