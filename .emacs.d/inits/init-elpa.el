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
        helm

        auto-complete
        ac-math

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

        js3-mode

        ruby-mode
        ruby-block
        ruby-end
        flymake-ruby
        inf-ruby
        ruby-compilation

        python-mode
        jade-mode
        yaml-mode
        emmet-mode
        web-mode
        vue-mode
        scss-mode
        flymake-sass

        ess
        ess-R-object-popup

        e2wm
;        e2wm-R
        e2wm-bookmark

        auctex
        cdlatex

        ;; -- init-*.el
        org-plus-contrib
        w3m
        yasnippet
        dropdown-list                   ;work with yasnippet
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
;; .emacs.d/epla/*
;;-----------------------------------------------------------------

;; helm
(require 'helm-config)
(helm-mode 1)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'js3-mode-hook 'helm-gtags-mode)
;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.3)
(setq ac-menu-height 20)
(setq ac-ignore-case 'smart)
(define-key global-map [f2] 'auto-complete-mode)

;; ac-math
(require 'ac-math)
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources)))

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

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php$" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml$")
        ("blade"  . "\\.blade\\.php$"))
)

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
             (setq php-mode-force-pear t)
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
(add-to-list 'auto-mode-alist '("\\.json$" . js3-mode))
(setq js3-auto-indent-p t         ; it's nice for commas to right themselves.
      js3-enter-indents-newline t ; don't need to push tab before typing
      js3-indent-on-enter-key t   ; fix indenting before moving on
      js3-indent-level 2)

;; jade-mode
(autoload 'jade-mode "jade-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(autoload 'sws-mode "sws-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

;; python-mode
(require 'python-mode)
(eval-after-load 'python-mode
  '(progn
     (add-hook 'python-mode-hook
               '(lambda()
                  (setq indent-tabs-mode nil)
                  (setq truncate-lines t)
                  (setq tab-width 4)))
     ))
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
                  (make-local-variable 'ac-ignores)
                  (add-to-list 'ac-ignores "end")
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

;; inf-ruby
(defconst inf-ruby-implementations
  '(("ruby"     . "bash -c irb --prompt default -r irb/completion"))
  "An alist of ruby implementations to irb executable names.")
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
(inf-ruby-switch-setup)

;; ruby-complition
(eval-after-load 'ruby-mode
  '(progn (define-key ruby-mode-map (kbd "C-x t") 'ruby-compilation-this-buffer)
          (define-key ruby-mode-map (kbd "C-x T") 'ruby-compilation-this-test)))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

;; scss
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook
          '(lambda ()
             (require 'flymake-sass)
             (flymake-sass-load)
             ))

;; ess
(if run-w32 (progn
              (setq inferior-R-program-name "C:\\Program Files\\R\\R-3.0.1\\bin\\R.exe")
              ))
(require 'ess-site)
(require 'ess-R-object-popup)
(setq ess-ask-for-ess-directory nil)

;; e2wm
(require 'e2wm)
(require 'e2wm-bookmark)
;(require 'e2wm-R)
(global-set-key (kbd "M-+") 'e2wm:start-management)
;(global-set-key (kbd "C-c R") 'e2wm:start-R-code)

(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(("C-,"      . e2wm:pst-history-forward-command) ; 履歴を進む
   ("C-."      . e2wm:pst-history-back-command) ; 履歴をもどる
   ("M-m"      . e2wm:pst-window-select-main-command) ; メイン選択
   ("prefix q" . e2wm:stop-management)
   ("prefix l" . e2wm:pst-update-windows-command)
   ("prefix 1" . e2wm:dp-code)
   ("prefix 2" . e2wm:dp-two)
   ("prefix 3" . e2wm:dp-htwo)
   ("prefix 4" . e2wm:dp-doc)
   ("prefix 5" . e2wm:dp-array)
;   ("prefix 6" . e2wm:dp-R-code)
;   ("prefix 7" . e2wm:dp-R-view)
   ("prefix v" . e2wm:dp-vcs)
   ("C-M-s"    . e2wm:my-toggle-sub) ; subの表示をトグルする
   ) e2wm:prefix-key)

(e2wm:add-keymap
 e2wm:dp-code-minor-mode-map
 '(("prefix I" . e2wm:dp-code-imenu-toggle-command)
   ("prefix S" . e2wm:dp-code-sub-toggle-command)
   ("prefix C" . e2wm:dp-code-toggle-clock-command)
   ("prefix c" . e2wm:dp-code-toggle-svg-clock-command)
   ("prefix M" . e2wm:dp-code-main-maximize-toggle-command)
   ("prefix h" . e2wm:dp-code-navi-history-command)
   ("prefix f" . e2wm:dp-code-navi-files-command)
   ("prefix i" . e2wm:dp-code-navi-imenu-command)
   ("prefix s" . e2wm:dp-code-navi-sub-command)
   ("C-c m"    . e2wm:dp-code-popup-messages)
   ("prefix b" . e2wm:dp-code-navi-bookmarks-command)
   ) e2wm:prefix-key)

(e2wm:add-keymap
 e2wm:dp-two-minor-mode-map
 '(("C-,"       . e2wm:dp-two-right-history-down-command)
   ("C-."       . e2wm:dp-two-right-history-up-command)
   ("prefix h"  . e2wm:dp-two-navi-history-command)
   ("prefix l"  . e2wm:pst-update-windows-command)
   ("prefix j"  . e2wm:dp-two-navi-left-command)
   ("prefix k"  . e2wm:dp-two-navi-right-command)
   ("prefix d"  . e2wm:dp-two-double-column-command)
   ("prefix S"  . e2wm:dp-two-sub-toggle-command)
   ("prefix -"  . e2wm:dp-two-swap-buffers-command)
   ("prefix H"  . e2wm:dp-two-history-toggle-command)
   ("prefix M"  . e2wm:dp-two-main-maximize-toggle-command)
   ) e2wm:prefix-key)

(defun e2wm:my-toggle-sub () ; Subをトグルする関数
  (interactive)
  (e2wm:pst-window-toggle 'sub t 'main))

