;;-----------------------------------------------------------------
;; elpa packages
;;-----------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq my-packages
      '(
        auto-install

        auto-complete
        ace-jump-mode
        color-theme
        color-theme-solarized
        expand-region
        fill-column-indicator
        flymake-cursor
        rainbow-delimiters
        rainbow-mode
        undo-tree

        magit

        cl-lib
        eldoc-extension

        apache-mode

        php-mode
        php-extras
        flymake-php

        ;; js2-mode
        js3-mode

        ruby-mode
        ruby-block
        ruby-end
        flymake-ruby

        python-mode
        jade-mode
        yaml-mode
        zencoding-mode

        ;; -- init-*.el
        org-plus-contrib
        w3m
        yasnippet
        ))
(package-initialize)

(require 'cl)                           ;built-in

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p my-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

;;-----------------------------------------------------------------
;; auto-install
;;-----------------------------------------------------------------

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name nil)
(add-to-load-path auto-install-directory)
(setq auto-install-use-wget nil)
(setq my-batches '("anything"))
(defun my-auto-installed-file-exists-p (name)
  "check auto-install directory"
  (file-exists-p (concat auto-install-directory name ".el")))
(let ((not-installed (remove-if 'my-auto-installed-file-exists-p my-batches)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (dolist (package not-installed)
                   (auto-install-batch package))))))

;;-----------------------------------------------------------------
;; .emacs.d/auto-install/*
;;-----------------------------------------------------------------

;; anything
(require 'anything-startup)
;; anything-config rewrite
(eval-after-load 'anything-config
  '(progn
     (setq w3m-command "w3m")
     (setq anything-c-home-url "http://www.google.co.jp")
     ))

;;-----------------------------------------------------------------
;; .emacs.d/epla/*
;;-----------------------------------------------------------------

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.3)
(setq ac-menu-height 20)
(setq ac-ignore-case 'smart)

;; ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; color-theme-solarized
(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme-solarized"
  (color-theme-solarized-dark))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(setq alphabet-start "abc def")

;; fill-column-indicator
(require 'fill-column-indicator)
; M-x fci-mode

;; enhancements for displaying flymake errors
(require 'flymake-cursor)

;; rainbow-mode
(require 'rainbow-mode)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(global-rainbow-delimiters-mode)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
;C-x u

;; magit
(require 'magit)

;; eldoc-extension
(require 'eldoc-extension)

;; apache-mode
(autoload 'apache-mode "apache-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; php-mode and php-extras
(require 'php-mode)
(require 'php-extras)
(setq php-executable
      (executable-find "php"))
(add-hook 'php-mode-hook
          '(lambda ()
             (abbrev-mode 0)
             (require 'flymake-php)
             (flymake-php-load)
             (setq php-manual-url "http://jp.php.net/manual/ja/")
             (setq php-search-url "http://jp.php.net/")
             (setq php-manual-path "share/php_manual_ja.tar.gz")
             (setq indent-tabs-mode nil)
             ;; (c-set-offset 'basic-offset 2)
             (c-set-offset 'case-label '+)
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'block-close 0)
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-cont-nonempty '+)
             (c-set-offset 'arglist-close '+)))
(add-to-list 'auto-mode-alist '("\\.php[s345t]$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$" . php-mode))

;; js3-mode
(autoload 'js3-mode "js3" nil t nil)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

;; jade-mode
(autoload 'jade-mode "jade-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(autoload 'sws-mode "sws-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

;; python-mode
(require 'python-mode)
; fix ruby-calculate-indent error
(setq ruby-indent-level 2)
(setq nxml-child-indent 2)

;; ruby-mode
(autoload 'ruby-mode "ruby-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(eval-after-load 'ruby-mode
  '(progn
     (add-hook 'ruby-mode-hook
               '(lambda ()
                  ;; (abbrev-mode 1)
                  (electric-pair-mode t)
                  (electric-indent-mode t)
                  (electric-layout-mode t)))
     ))
(require 'ruby-end)
(require 'ruby-block)
(ruby-block-mode t)
;; do overlay
(setq ruby-block-highlight-toggle 'overlay)
;; display to minibuffer
(setq ruby-block-highlight-toggle 'minibuffer)
;; display to minibuffer and do overlay
(setq ruby-block-highlight-toggle t)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; zencoding-mode
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

