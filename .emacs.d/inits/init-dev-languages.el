;;; init-dev-languages.el --- Language-specific development configurations
;;; Commentary:
;; Programming language support and configurations
;; LSP (eglot) と組み合わせて使用
;;; Code:

;; --------------------------------
;; Ruby (組み込みモード)
;; --------------------------------
(use-package ruby-mode
  :defer t
  :mode ("\\.rb\\'" "Rakefile" "Gemfile" "\\.rake\\'" "\\.gemspec\\'")
  :interpreter "ruby"
  :custom
  (ruby-indent-level 2)
  (ruby-indent-tabs-mode nil))

;; --------------------------------
;; Python (組み込みモード)
;; --------------------------------
(use-package python
  :defer t
  :mode "\\.py\\'"
  :interpreter ("python" "python3")
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

;; --------------------------------
;; Go
;; --------------------------------
(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports")
  :config
  (setq-local compile-command "go build -v && go test -v && go vet"))

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
              ("C-c -" . php-current-class)
              ("C-c =" . php-current-namespace))
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t
                    c-basic-offset 4
                    tab-width 4
                    indent-tabs-mode nil)
              (subword-mode 1))))

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
  :mode ("\\.ts\\'" "\\.tsx\\'")
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
  (rust-format-on-save t))

(use-package cargo
  :ensure t
  :after rust-mode
  :defer t
  :hook (rust-mode . cargo-minor-mode))

;; --------------------------------
;; C/C++ (組み込みモード)
;; --------------------------------
(use-package cc-mode
  :defer t
  :custom
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "linux")))
  (c-basic-offset 4))

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
;; Lisp (組み込みモード)
;; --------------------------------
(use-package emacs-lisp-mode
  :defer t
  :hook (emacs-lisp-mode . eldoc-mode))

;; --------------------------------
;; Terraform
;; --------------------------------
(use-package terraform-mode
  :ensure t
  :defer t
  :mode "\\.tf\\'"
  :custom
  (terraform-indent-level 2))

;; --------------------------------
;; Docker
;; --------------------------------
(use-package dockerfile-mode
  :ensure t
  :defer t
  :mode "Dockerfile\\'")

(provide 'init-dev-languages)
;;; init-dev-languages.el ends here