;;; init-dev-web.el --- Web development tools and configurations
;;; Commentary:
;; Frontend and web development specific tools and frameworks
;;; Code:

;; --------------------------------
;; Web Mode (Multi-language web files)
;; --------------------------------
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.p?html\\'"
         "\\.tpl\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.blade\\.php\\'"
         "\\.handlebars\\'"
         "\\.hbs\\'"
         "\\.ejs\\'"
         "\\.jsx\\'"
         "\\.tsx\\'")
  :custom
  (web-mode-engines-alist
   '(("php"    . "\\.phtml\\'")
     ("blade"  . "\\.blade\\.php\\'")
     ("django" . "\\.djhtml\\'")
     ("jsx"    . "\\.jsx\\'")
     ("tsx"    . "\\.tsx\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-block-padding 0)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (lsp)))))

;; --------------------------------
;; HTML/CSS/SCSS
;; --------------------------------
(use-package sgml-mode
  :defer t
  :mode ("\\.html\\'" "\\.htm\\'")
  :custom
  (sgml-basic-offset 2))

(use-package css-mode
  :defer t
  :mode ("\\.css\\'" "\\.scss\\'" "\\.sass\\'")
  :custom
  (css-indent-offset 2))


;; --------------------------------
;; Emmet for HTML expansion
;; --------------------------------
(use-package emmet-mode
  :ensure t
  :defer t
  :hook ((sgml-mode html-mode css-mode scss-mode web-mode) . emmet-mode)
  :custom
  (emmet-indentation 2)
  :bind (:map emmet-mode-keymap
              ("C-j" . emmet-expand-line)))


;; --------------------------------
;; JSON
;; --------------------------------
(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" "\\.jsonc\\'")
  :custom
  (json-mode-indent-level 2)
  (json-encoding-default-indentation "  "))

;; --------------------------------
;; YAML
;; --------------------------------
(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.ya?ml\\'" "\\.yml\\'")
  :custom
  (yaml-indent-offset 2))

;; --------------------------------
;; TOML
;; --------------------------------
(use-package toml-mode
  :ensure t
  :defer t
  :mode "\\.toml\\'")


;; --------------------------------
;; REST Client
;; --------------------------------
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.http\\'" "\\.rest\\'")
  :commands restclient-mode)


;; --------------------------------
;; Build Tools and Package Managers
;; --------------------------------
;; Webpack config (manual mode for webpack.config.js)
(add-to-list 'auto-mode-alist '("webpack\\.config\\.js\\'" . js-mode))


;; --------------------------------
;; Server Configuration Files
;; --------------------------------
;; Nginx configuration
(use-package nginx-mode
  :ensure t
  :defer t
  :mode ("nginx.*\\.conf\\'" "/etc/nginx/.*"))

;; Apache configuration
(use-package apache-mode
  :ensure t
  :defer t
  :mode ("\\.htaccess\\'"
         "/etc/httpd.+\\.conf\\'"
         "/etc/apache.+\\.conf\\'"
         "sites-\\(available\\|enabled\\)/"))

;; --------------------------------
;; API Documentation
;; --------------------------------
;; OpenAPI/Swagger - flycheck hook added to yaml-mode above
(add-hook 'yaml-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "swagger\\|openapi" buffer-file-name))
              (flycheck-mode 1))))

;; --------------------------------
;; Version Control for Web Assets
;; --------------------------------
;; .gitignore mode (use conf-mode as fallback)
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . conf-mode))

;; --------------------------------
;; Database Integration
;; --------------------------------
;; SQL mode improvements
(use-package sql
  :defer t
  :mode ("\\.sql\\'" . sql-mode)
  :custom
  (sql-mysql-login-params '(user password server database port))
  (sql-postgres-login-params '(user password server database port)))

;; --------------------------------
;; Performance Monitoring
;; --------------------------------
;; Bundle analyzer support
(defun my-web-bundle-analyzer ()
  "Run webpack bundle analyzer for the current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "package.json")
        (async-shell-command "npm run analyze")
      (message "No package.json found in project root"))))

;; --------------------------------
;; Development Server Support
;; --------------------------------
(defun my-web-dev-server ()
  "Start development server for current web project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (cond
     ((file-exists-p "package.json")
      (async-shell-command "npm run dev"))
     ((file-exists-p "yarn.lock")
      (async-shell-command "yarn dev"))
     ((file-exists-p "Gemfile")
      (async-shell-command "bundle exec rails server"))
     (t (message "No recognized web project configuration found")))))

;; --------------------------------
;; Code Formatting
;; --------------------------------
;; Prettier for JavaScript/CSS/HTML
(use-package prettier
  :ensure t
  :defer t
  :commands (prettier-prettify prettier-prettify-region)
  :hook ((js-mode typescript-mode web-mode css-mode scss-mode json-mode) . prettier-mode))

;; --------------------------------
;; Testing Support
;; --------------------------------
;; Jest test runner
(defun my-run-jest-tests ()
  "Run Jest tests for current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "package.json")
        (compile "npm test")
      (message "No package.json found"))))

;; Cypress support
(defun my-run-cypress-tests ()
  "Run Cypress tests for current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "cypress.json")
        (async-shell-command "npx cypress run")
      (message "No cypress.json found"))))

;; --------------------------------
;; Keybindings
;; --------------------------------
(global-set-key (kbd "C-c w d") 'my-web-dev-server)
(global-set-key (kbd "C-c w a") 'my-web-bundle-analyzer)
(global-set-key (kbd "C-c w t") 'my-run-jest-tests)
(global-set-key (kbd "C-c w c") 'my-run-cypress-tests)

(provide 'init-dev-web)
;;; init-dev-web.el ends here