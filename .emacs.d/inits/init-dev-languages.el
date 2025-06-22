;;; init-dev-languages.el --- Language-specific development configurations
;;; Commentary:
;; Programming language support and configurations
;;; Code:

;; --------------------------------
;; Ruby
;; --------------------------------
(use-package ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb\\'" "Rakefile" "Gemfile" "\\.rake\\'" "\\.gemspec\\'")
  :interpreter "ruby"
  :ensure-system-package
  ((rubocop     . "gem install rubocop")
   (ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry         . "gem install pry"))
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

(use-package ruby-compilation
  :ensure t
  :after ruby-mode
  :defer t
  :bind (:map ruby-mode-map
              ("C-x t" . ruby-compilation-this-buffer)
              ("C-x T" . ruby-compilation-this-test)))

;; --------------------------------
;; Python
;; --------------------------------
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" "python3" . python-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

(use-package py-yapf
  :ensure t
  :after python
  :defer t
  :hook (python-mode . py-yapf-enable-on-save))

;; Virtual environment support
(use-package pyvenv
  :ensure t
  :after python
  :defer t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon))

;; --------------------------------
;; Go
;; --------------------------------
(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports")              ;; Use goimports instead of gofmt
  :config
  (setq-local compile-command "go build -v && go test -v && go vet"))

(use-package go-eldoc
  :ensure t
  :after go-mode
  :defer t
  :hook (go-mode . go-eldoc-setup))

;; Go playground
(use-package go-playground
  :ensure t
  :after go-mode
  :defer t
  :commands (go-playground go-playground-download))

;; --------------------------------
;; PHP
;; --------------------------------
(use-package php-mode
  :ensure t
  :defer t
  :mode ("\\.php\\'" "\\.phtml\\'" "\\.inc\\'")
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
  :after php-mode
  :defer t
  :commands (phpunit-current-test phpunit-current-project))

(use-package php-eldoc
  :ensure t
  :after php-mode
  :defer t
  :hook (php-mode . php-eldoc-enable))

;; --------------------------------
;; JavaScript / TypeScript
;; --------------------------------
(use-package js
  :defer t
  :mode ("\\.js\\'" "\\.mjs\\'")
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :ensure t
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :custom
  (typescript-indent-level 2))

;; Node.js path support
(use-package add-node-modules-path
  :ensure t
  :defer t
  :hook ((js-mode json-mode typescript-mode) . add-node-modules-path))

;; --------------------------------
;; Rust
;; --------------------------------
(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :config
  (setq-local compile-command "cargo check"))

(use-package cargo
  :ensure t
  :after rust-mode
  :defer t
  :hook (rust-mode . cargo-minor-mode))

;; --------------------------------
;; C/C++
;; --------------------------------
(use-package cc-mode
  :defer t
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.h\\'" . c-mode)
         ("\\.hpp\\'" . c++-mode))
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  :config
  (c-set-offset 'substatement-open 0))

;; --------------------------------
;; Java
;; --------------------------------
(use-package java-mode
  :defer t
  :mode "\\.java\\'"
  :custom
  (c-basic-offset 4)
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (setq c-basic-offset 4
                    tab-width 4
                    indent-tabs-mode nil))))

;; --------------------------------
;; Lua
;; --------------------------------
(use-package lua-mode
  :ensure t
  :defer t
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level 2))

;; --------------------------------
;; Lisp Family
;; --------------------------------
(use-package lisp-mode
  :defer t
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.cl\\'" . lisp-mode))
  :hook ((emacs-lisp-mode lisp-mode) . eldoc-mode))

;; Common Lisp support
(use-package slime
  :ensure t
  :defer t
  :commands slime
  :custom
  (inferior-lisp-program "sbcl"))

;; --------------------------------
;; Clojure
;; --------------------------------
(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :after clojure-mode
  :defer t
  :commands (cider-jack-in cider-connect))

;; --------------------------------
;; Scheme
;; --------------------------------
(use-package scheme
  :defer t
  :mode "\\.scm\\'"
  :interpreter "scheme")

;; --------------------------------
;; Haskell
;; --------------------------------
(use-package haskell-mode
  :ensure t
  :defer t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))
  :hook (haskell-mode . haskell-indentation-mode))

;; --------------------------------
;; Erlang/Elixir
;; --------------------------------
(use-package erlang
  :ensure t
  :defer t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)))

(use-package elixir-mode
  :ensure t
  :defer t
  :mode "\\.exs?\\'"
  :hook (elixir-mode . subword-mode))

;; --------------------------------
;; Scala
;; --------------------------------
(use-package scala-mode
  :ensure t
  :defer t
  :mode "\\.scala\\'"
  :interpreter "scala")

;; --------------------------------
;; Kotlin
;; --------------------------------
(use-package kotlin-mode
  :ensure t
  :defer t
  :mode "\\.kt\\'")

;; --------------------------------
;; Swift
;; --------------------------------
(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'")

;; --------------------------------
;; PowerShell (Windows)
;; --------------------------------
(use-package powershell
  :if (memq system-type '(cygwin windows-nt))
  :ensure t
  :defer t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :interpreter "powershell")

;; --------------------------------
;; Terraform
;; --------------------------------
(use-package terraform-mode
  :ensure t
  :defer t
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2)
  :hook (terraform-mode . my-terraform-mode-init)
  :config
  (defun my-terraform-mode-init ()
    "Initialize terraform mode with specific settings."
    (setq-local tab-width 2)))

;; --------------------------------
;; Docker
;; --------------------------------
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "Dockerfile\\'")

;; --------------------------------
;; Markdown
;; --------------------------------
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t))

;; --------------------------------
;; Language Server Protocol (LSP) Integration
;; --------------------------------
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :hook ((go-mode rust-mode python-mode typescript-mode js-mode) . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.5)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(provide 'init-dev-languages)
;;; init-dev-languages.el ends here