;;-----------------------------------------------------------------
;; elpa packages
;;-----------------------------------------------------------------
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq my-packages
      '(
        ;; helm
        ;; helm-descbinds
        ;; helm-gtags

        counsel
        counsel-gtags

        wgrep

        auto-complete
        ac-math

        ace-jump-mode

        color-theme
        solarized-theme

        expand-region
        fill-column-indicator
        flymake-cursor
        rainbow-delimiters
        rainbow-mode
        undo-tree

        ;; magit

        cl-lib
        ;; eldoc-extension

        apache-mode

        php-mode
        flymake-php

        js2-mode
        vue-mode

        ruby-mode
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
        edts

        scss-mode
        sass-mode
        flymake-sass

        auctex
        cdlatex

        ;; -- init-*.el
        org-plus-contrib
        ox-reveal
        w3m
        yasnippet
        ;; dropdown-list                   ;work with yasnippet
        ))

(require 'cl)                           ;built-in
;(require 'cl-lib)

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
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (helm-mode 1)

;; counsel
(ivy-mode 1)
(counsel-mode 1)
;;; 下記は任意で有効化
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)
(add-hook 'php-mode 'counsel-gtags-mode)

(eval-after-load 'counsel-gtags
  '(progn
     (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
     (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
     (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
     (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))

;; wgrep
;;; eでwgrepモードにする
(setf wgrep-enable-key "e")
;;; wgrep終了時にバッファを保存
(setq wgrep-auto-save-buffer t)
;;; read-only bufferにも変更を適用する
(setq wgrep-change-readonly-file t)

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
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 0.3)
(setq ac-menu-height 20)
(setq ac-ignore-case 'smart)
(define-key global-map [f2] 'auto-complete-mode)

;; ac-math
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
(load-theme 'solarized-light t)
;(load-theme 'solarized-dark t)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(setq alphabet-start "abc def")

;; fill-column-indicator
; M-x fci-mode

;; enhancements for displaying flymake errors

;; rainbow-mode

;; rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(global-rainbow-delimiters-mode)

;; undo-tree
(global-undo-tree-mode)
;C-x u

;; magit
;(setq magit-last-seen-setup-instructions "1.4.0")

;; eldoc-extension

;; apache-mode
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; web-mode
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
;; (setq php-executable
;;       (executable-find "php"))
(add-hook 'php-mode-hook
          '(lambda ()
             (abbrev-mode 0)
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

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; jade-mode
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))

;; python-mode
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

(eval-after-load 'ruby-block
  '(progn
     (ruby-block-mode t)
     ))

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

(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
(inf-ruby-switch-setup)

;; ruby-complition
(eval-after-load 'ruby-mode
  '(progn (define-key ruby-mode-map (kbd "C-x t") 'ruby-compilation-this-buffer)
          (define-key ruby-mode-map (kbd "C-x T") 'ruby-compilation-this-test)))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; emmet-mode
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
