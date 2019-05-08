;;; init.el --- init init
;;; Commentary:
;;
;;; Code:

;; --------------------------------
;; Starting up
;; --------------------------------

;; enable debug
;; (setq debug-on-error  t
;;       init-file-debug t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/inits")
;; custom-file
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; --------------------------------
;; Personal information
;; --------------------------------
;; Load secrets
(load "~/.emacs.secrets" t)
(setq user-full-name "Yoshiaki Sano"
      user-mail-address "ysano@ysnet.org")

;; --------------------------------
;; Emacs core
;; --------------------------------

;; Memory allocation
(setq gc-cons-threshold (* 2 gc-cons-threshold)
      garbage-collection-messages t)

;; Buffer
(setq-default tab-width        4        ; tab width
              indent-tabs-mode nil      ; dont use tab code
              fill-column      80       ; 80col
              truncate-lines   t        ; dont wrap lines
              truncate-partial-width-windows t ; dont wrap lines in partial
              cursor-type      t        ; per frame
              line-spacing     0.0)

;; Cursor
(setq x-stretch-cursor t)
(global-hl-line-mode t)

;; Shell
(setq shell-file-name "/bin/bash")

;; Coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(defvar default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

;; Cygwin
(when (memq system-type '(cygwin))
  (progn
    ;; Cygwin coding tweak
    (set-keyboard-coding-system 'cp932)
    (set-file-name-coding-system 'cp932)
    (setq default-process-coding-system '(undecided-dos . utf-8-unix))
    (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                          'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
    (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)
    ;; Cygwin shell tweak
    (defvar explicit-shell-file-name "bash.exe")
    (setq shell-command-switch "-c")
    (setq shell-file-name "bash.exe")
    ;; (M-! and M-| and compile.el)
    (setq shell-file-name "bash.exe")
    (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)))

;; Disp notification
(setq visible-bell nil)

;; Font
(when (memq window-system '(x w32 ns))
  (progn
    (defun my-set-face-font (face fontstring-alist)
      "Evaluate font in order from top while noerror."
      (while (and fontstring-alist
                 (eq nil (ignore-errors (set-face-font face (car fontstring-alist)) t))
                 (setq fontstring-alist (cdr fontstring-alist)))))
    (my-set-face-font 'default
                      '(
                        "Cica-13:antialias=standard"
                        "Sarasa Term J Emoji-12:antialias=standard" ; original
                        "Migu 1M Symbola-13:antialias=standard"     ; original
                        "Sarasa Term J-12:antialias=standard"
                        "Inziu Iosevka J-12:antialias=standard"
                        "Migu 1M-13:antialias=standard"
                        "MS Gothic-13:antialias=standard"
                        "MS Mincho-13:antialias=standard"
                        ))
    (my-set-face-font 'variable-pitch
                      '(
                        "Migu 1C-13:antialias=standard"
                        "MigMix 1P-13:antialias=standard"
                        "MS PGothic-13:antialias=standard"
                        "MS PMincho-13:antialias=standard"
                        ))
    (my-set-face-font 'fixed-pitch
                      '(
                        "Cica-13:antialias=standard"
                        "Sarasa Term J Emoji-12:antialias=standard" ; original
                        "Migu 1M Symbola-13:antialias=standard"     ; original
                        "Sarasa Term J-12:antialias=standard"
                        "Inziu Iosevka J-12:antialias=standard"
                        "Migu 1M-13:antialias=standard"
                        "MigMix 1M-13:antialias=standard"
                        "MS Gothic-13:antialias=standard"
                        "MS Mincho-13:antialias=standard"
                        ))
    (my-set-face-font 'tooltip
                      '(
                        "Cica-11:antialias=standard"
                        "Migu 1M-11:antialias=standard"
                        "MigMix 1M-11:antialias=standard"
                        "MS Gothic-11:antialias=standard"
                        "MS PMincho-11:antialias=standard"
                        )))
  ;; 012345,6789.
  ;; abcdef,ghijKL;
  ;; ABCDEF,GHIJKL:
  ;; !@#$%^&*()_+\<>?"'|liL
  ;; 漢字日本語あいうえお
  ;; 😁💢🀄🃏🐰🐵🐔😁💢
  ;; ↑↑↓↓←→←→BA

  ;; フォントサイズ調整
  (global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

  ;; フォントサイズ リセット
  (global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0))))

;; History
;; From http://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html:
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(defvar savehist-save-minibuffer-history 1)
(defvar savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; C-h key
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-1") 'help)

;; タブ, 全角スペースを表示する
(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;; ("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; --------------------------------
;; Package initialization
;; --------------------------------

;; Add package sources
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

;; Add my elisp directory and other files
(add-to-list 'load-path "~/.emacs.d/elisp")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(defvar use-package-verbose t)

(require 'use-package)
;; (use-package auto-compile
;;   :ensure t
;;   :config (auto-compile-on-load-mode))
;; (setq load-prefer-newer t)

;; --------------------------------
;; Emacs standard lisp
;; --------------------------------

;; Libraries
(use-package dash :ensure t
  :config
  (dash-enable-font-lock))

;; Auto Revert like tail
(global-auto-revert-mode 1)
(defvar auto-revert-interval 10)
(defvar auto-revert-check-vc-info t)

;; Color theme
(use-package solarized-theme :ensure t
  :config
  (load-theme 'solarized-dark))

;; Dimmer
;; Visually highlight the selected buffer.
(use-package dimmer :ensure t
  :config
  (dimmer-mode))

;; Dired

;; Menu bar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; XTerm mouse mode
(use-package xt-mouse
  :if (eq window-system nil) :ensure t
  :config
  (xterm-mouse-mode t))

;; Save cursor place
(save-place-mode 1)

;; Input Method
(when (featurep 'w32-ime)
  (progn
    ;; モードラインの表示文字列
    (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
    (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

    ;; IME初期化
    (w32-ime-initialize)

    ;; デフォルトIME
    (setq default-input-method "W32-IME")

    ;; IME変更
    ;; (global-set-key (kbd "C-\\") 'toggle-input-method)

    ;; 漢字/変換キー入力時のエラーメッセージ抑止
    (global-set-key (kbd "<M-kanji>") 'ignore)
    (global-set-key (kbd "<kanji>") 'ignore)
    ))

;; Flymake
(use-package flycheck :ensure t
  :config
  (setq flycheck-display-errors-delay 0.1)
  (setq eldoc-idle-delay 1.5)
  (global-flycheck-mode))

;; Paren
(show-paren-mode 1)
(defvar show-paren-style 'expression)

;; Mode line format
(line-number-mode t)
(column-number-mode t)

;; Time in the mode line
(defvar display-time-24hr-format 1)
(defvar display-time-string-forms '(month "/" day " " dayname " " 24-hours ":" minutes))
(display-time-mode 1)

;; P is cp932 in mode line
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; region infomation in mode line
(defun count-lines-and-chars ()
  "Count lines and chars in current region."
  (if mark-active
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))

;; --------------------------------
;; Editor configuration
;; --------------------------------

;; background color to strings that match color
(use-package rainbow-mode :ensure t
  :hook (sh-mode c-mode c++mode
		 html-mode css-mode php-mode nxml-mode xml-mode
		 latex-mode ess-mode
		 emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode))

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package yasnippet :ensure t
  :disabled
  :custom
  (yas-indent-line 'fixed)
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y l" . yas-describe-tables)
              ("C-c y g" . yas-reload-all))
  :config
  (use-package yasnippet-snippets :ensure t
    :disabled
    )
  (use-package yatemplate :ensure t
    :disabled
    )
  (yas-global-mode 1))

;; TODO LSP

;;--------------------------------
;; Navigation
;;--------------------------------

;; Pop to mark
;; Handy way of getting back to previous places.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; ace-jump
(use-package ace-jump-mode :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-x SPC" . ace-jump-mode-pop-mark))
  :config
  (ace-jump-mode-enable-mark-sync)
  )

;; Winner mode - undo and redo window configuration
(use-package winner :ensure t
  :bind (("M-p" . 'previous-buffer)
	 ("M-n" . 'next-buffer)))

;;--------------------------------
;; Eazy interface
;;--------------------------------

;; ivy, swiper, counsel - interactive completion
(use-package ivy :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . 'ivy-resume)
	 ([f6] . 'ivy-resume)))
(use-package swiper :ensure t
  :after ivy
  :init
  (setq search-default-mode #'char-fold-to-regexp)
  :bind ("C-s" . 'swiper))
(use-package counsel :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  :bind (("<f2> u" . 'counsel-unicode-char)
	 ("C-c g" . 'counsel-git)
	 ("C-c j" . 'counsel-git-grep)
	 ("C-c k" . 'counsel-ag)
	 ("C-x l" . 'counsel-locate)
	 ("C-S-o" . 'counsel-rhythmbox)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package counsel-gtags :ensure t
  :after counsel
  :bind (
	 :map counsel-gtags-mode-map
	 ("M-t" . counsel-gtags-find-definition)
	 ("M-r" . counsel-gtags-find-reference)
	 ("M-s" . counsel-gtags-find-symbol)
	 ("M-," . counsel-gtags-go-backward))
)

;; Help - guide-key
(use-package guide-key :ensure t
  :init
  (setq guide-key/idle-delay 1.5)
  (setq guide-key/guide-key-sequence
	'("C-x r" "C-x 4" "C-c"
	  (org-mode "C-c C-x")
	  (outline-minor-mode "C-c @")))
  (setq guide-key/highlight-command-regexp
	'("rectangle"
          ("register" . font-lock-type-face)
          ("bookmark" . "hot pink")))
  :config
  (guide-key-mode 1))

;; --------------------------------
;; Utility
;; --------------------------------

;; TODO Magit

;; Grep
(use-package wgrep :ensure t
  :init
  (setf wgrep-enable-key "e")           ; eでwgrepモードにする
  (setq wgrep-auto-save-buffer t)       ; wgrep終了時にバッファを保存
  (setq wgrep-change-readonly-file t))  ; read-only bufferにも変更を適用する

;; TODO Migemo

;; TODO Google Translate

;;--------------------------------
;; auto-complete
;;--------------------------------

(use-package auto-complete :ensure t
  :config
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-quick-help-delay 0.3)
  (setq ac-menu-height 20)
  (setq ac-ignore-case 'smart)
  )
(use-package ac-math :ensure t
  :after auto-complete
  :hook (TeX-mode org-mode)
  :config
  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-sources
	(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
		ac-sources)) )

;;--------------------------------
;; org-mode
;;--------------------------------

(use-package org
  :pin "org"
  :ensure org-plus-contrib
  :ensure (ox-reveal :pin "melpa")
  :config
  (load "init-org"))

;;--------------------------------
;; prog-mode children
;;--------------------------------
(use-package ruby-mode :ensure nil
  :mode ("\\.rb\\'" "Rakefile")
  :interpreter "ruby"
)
  
(use-package ruby-compilation :ensure nil
  :bind (
	 :map ruby-mode-map
	 ("C-x t" . ruby-compilation-this-buffer)
	 ("C-x T" . ruby-compilation-this-test)))

(use-package python :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;;--------------------------------
;; text-mode children
;;--------------------------------
(use-package emmet-mode :ensure nil
  :hook (sgml-mode html-mode css-mode web-mode))

(use-package web-mode :ensure nil
  :after emmet-mode
  :mode
  "\\.p?html\\'"
  "\\.tpl\\.php\\'"
  "\\.jsp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.blade\\.php\\'"
  :init
  (setq web-mode-engines-alist
	'(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.php\\'")))
  (setq web-mode-ac-sources-alist
	'(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
	  ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
	  ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
		 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
		 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))))))

(use-package yaml-mode :ensure nil)
