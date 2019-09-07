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

;; Add my elisp directory and other files
(add-to-list 'load-path "~/.emacs.d/inits")
(add-to-list 'load-path "~/.emacs.d/elisp")

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
(setq gc-cons-threshold (expt 10 8)
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

;; Coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(defvar default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

;; Cygwin shell fix when Windows-nt
(if (eq system-type 'windows-nt)
  (progn
    (setq cygwin-root-directory (getenv "CYGWIN_DIR"))
    (setq cygwin-mount-cygwin-bin-directory (expand-file-name "bin" cygwin-root-directory))
    (require 'setup-cygwin)
    ;; zsh if exists
    (if (file-exists-p (expand-file-name "bin/zsh.exe" cygwin-root-directory))
        (progn
          (add-to-list 'exec-path (expand-file-name "bin" cygwin-root-directory))
          (setq shell-file-name  (expand-file-name "bin/zsh.exe" cygwin-root-directory)) ; Subprocesses invoked by shell.
          (setenv "SHELL" shell-file-name)
          (setenv "PATH" (concat (expand-file-name "bin" cygwin-root-directory) ";" (getenv "PATH")))
          (setq explicit-shell-file-name  shell-file-name) ; Interactive shell
          (setq ediff-shell               shell-file-name)    ; Ediff shell
          (setq explicit-shell-args       '("--login" "-i"))
          ))
    ;; Cygwin shell tweak
    (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8))
  ;; like gnu/linux etc
  (progn
    (setq shell-file-name "zsh") ; Subprocesses invoked by shell.
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name  shell-file-name) ; Interactive shell
    (setq ediff-shell               shell-file-name)    ; Ediff shell
    (setq explicit-shell-args       '("--login" "-i"))
    ))

;; Windows
(when (memq system-type '(cygwin windows-nt))
  (progn
    ;; Cygwin coding tweak
    (set-keyboard-coding-system 'cp932)
    (set-file-name-coding-system 'cp932)
    (set-terminal-coding-system 'cp932)
    (setq default-process-coding-system '(undecided-dos . utf-8-unix))
    (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                          'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
    (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)))

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
                        ))
    (my-set-face-font 'variable-pitch
                      '(
                        "IPAexMincho-13:antialias=standard" ; jp-fixed-pich
                        "IPAexGothic-13:antialias=standard" ; jp-fixed-pich
                        "IPAPMincho-13:antialias=standard"
                        "IPAPGothic-13:antialias=standard"
                        "Migu 1C-13:antialias=standard"
                        "MigMix 1P-13:antialias=standard"
                        "MS PGothic-13:antialias=standard"
                        "MS PMincho-13:antialias=standard"
                        ))
    (my-set-face-font 'fixed-pitch      ;sans only
                      '(
                        "Cica-13:antialias=standard"
                        "Sarasa Term J Emoji-12:antialias=standard" ; original
                        "Migu 1M Symbola-13:antialias=standard"     ; original
                        "Sarasa Term J-12:antialias=standard"
                        "Inziu Iosevka J-12:antialias=standard"
                        "Migu 1M-13:antialias=standard"
                        "MigMix 1M-13:antialias=standard"
                        "IPAGothic-13:antialias=standard"
                        "MS Gothic-13:antialias=standard"
                        ))
    (my-set-face-font 'fixed-pitch-serif
                      '(
                        "IPAMincho-13:antialias=standard"
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
(package-initialize)

;; Add package sources
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

;; use-package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(defvar use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(use-package auto-compile :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package diminish :ensure t
  :config
  (defmacro safe-diminish (file mode &optional new-name)
    `(with-eval-after-load ,file
       (diminish ,mode ,new-name)))
  (safe-diminish 'autorevert 'auto-revert-mode))
(use-package delight :ensure t)


;; --------------------------------
;; Emacs standard lisp
;; --------------------------------

;; Libraries
(use-package dash :ensure t
  :config
  (dash-enable-font-lock))

;; Auto Revert
(defvar auto-revert-interval 10)
(defvar auto-revert-check-vc-info t)
(add-hook 'text-mode-hook 'auto-revert-mode)
(add-hook 'prog-mode-hook 'auto-revert-mode)

;; Color theme
(use-package solarized-theme :ensure t :disabled
  :config
  (load-theme 'solarized-dark))

(use-package doom-themes :ensure t
  :config
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package hide-mode-line :ensure t
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package doom-modeline :ensure t :disabled
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  :init
  (message "Download latest icons: M-x all-the-icons-install-fonts")
  :hook
  (after-init . doom-modeline-mode)
  :config
  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)
  ;; How wide the mode-line bar should be (only respected in GUI Emacs).
  (setq doom-modeline-bar-width 6)
  ;; Whether display minor modes in mode-line or not.
  (setq doom-modeline-minor-modes t)
  )

;; Dimmer
;; Visually highlight the selected buffer.
(use-package dimmer :ensure t :disabled
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
(if (>= (string-to-number emacs-version) 25.1)
    (save-place-mode 1)
  (progn
    (require 'saveplace)
    (setq-default save-place t)))

;; Input Method
(when (featurep 'w32-ime)
  (progn
    ;; モードラインの表示文字列
    (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
    (defvar w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

    ;; IME初期化
    ;(w32-ime-initialize)

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
  :defer t
  :bind ("S-<f2>" . flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.1)
  (setq eldoc-idle-delay 1.5))

;; Paren
(show-paren-mode 1)
(defvar show-paren-style 'expression)

;; Time in the mode line
(defvar display-time-24hr-format 1)
(defvar display-time-string-forms '(month "/" day " " dayname " " 24-hours ":" minutes))
(display-time-mode 0)

;; P is cp932 in mode line
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; modeline
(line-number-mode 1)
(column-number-mode 0)

;; 同一バッファ名にディレクトリ付与
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)
;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; --------------------------------
;; Editor configuration
;; --------------------------------

;; background color to strings that match color
(use-package rainbow-mode :ensure t
  :diminish
  :hook (sh-mode c-mode c++mode
                 html-mode css-mode php-mode nxml-mode xml-mode
                 latex-mode ess-mode
                 emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode))

(use-package rainbow-delimiters :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree :ensure t
  :diminish
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package yasnippet :ensure nil
  :custom
  (yas-indent-line 'fixed)
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y l" . yas-describe-tables)
              ("C-c y g" . yas-reload-all))
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets :ensure nil
  :after yasnippet)
(use-package yatemplate :ensure nil
  :after yasnippet)

;; TODO LSP

;; imenu-list
(use-package imenu-list :ensure t
  :custom
  (imenu-list-focus-after-activation t)
  :bind ("C-'" . imenu-list-smart-toggle))

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
(use-package winner :ensure t :disabled
  :bind (("M-p" . 'previous-buffer)
         ("M-n" . 'next-buffer)))

(use-package beacon :ensure t
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode t))

;; subword (camelCase Navigation)
(use-package subword :ensure t
  :diminish subword-mode
  :init
  (global-subword-mode))

;;--------------------------------
;; Eazy interface
;;--------------------------------

;; ivy, swiper, counsel - interactive completion
(use-package ivy :ensure t
  :diminish
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20)
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . 'ivy-resume)
         ([f6] . 'ivy-resume)))
(use-package ivy-rich :ensure nil
  :after ivy
  :defer t
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file
  (ivy-rich-mode 1))
(use-package swiper :ensure t
  :after ivy
  :init
  (setq search-default-mode #'char-fold-to-regexp)
  :bind ("C-s" . 'swiper))
(use-package counsel :ensure t
  :diminish
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
         ("C-r" . 'counsel-minibuffer-history)
         :map counsel-find-file-map
         ("C-l" . 'counsel-up-directory)
         ))

(use-package counsel-gtags :ensure t
  :after counsel
  :bind (
         :map counsel-gtags-mode-map
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ("M-," . counsel-gtags-go-backward))
  )

(use-package counsel-tramp :ensure t
  :after counsel
  :bind ("C-c s" . 'counsel-tramp))

;; Help - guide-key
(use-package guide-key :ensure t
  :defer t
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

;; Dashboard
(use-package dashboard :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Recentf
(use-package recentf-ext :ensure t
  :init
  (setq recentf-max-saved-items 50)
  (setq recentf-auto-save-timer (run-with-idle-timer 180 t 'recentf-save-list))
  :config
  (recentf-mode 1)
  )

;; Magit
(use-package magit :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (if (eq system-type 'windows-nt)
      (setq magit-need-cygwin-noglob t))) ; noglob on Cygwin and MSYS2

;; Git gutter
(use-package git-gutter :ensure t
  :diminish
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:modified-sign "  ")
  (git-gutter:added-sign "++")
  (git-gutter:deleted-sign "--")
  :custom-face
  (git-gutter:modified ((t (:background "purple"))))
  (git-gutter:added ((t (:foreground "green"))))
  (git-gutter:deleted ((t (:foreground "red"))))
  :bind (("C-x v =" . 'git-gutter:popup-hunk)
         ("C-x p" . 'git-gutter:previous-hunk)
         ("C-x n" . 'git-gutter:next-hunk)
         ("C-x v s" . 'git-gutter:stage-hunk)
         ("C-x v r" . 'git-gutter:revert-hunk)
         ("C-x v SPC" . #'git-gutter:mark-hunk))
  :config
  (global-git-gutter-mode t))

;; Grep
(use-package wgrep :ensure t
  :defer t
  :init
  (setf wgrep-enable-key "e")           ; eでwgrepモードにする
  (setq wgrep-auto-save-buffer t)       ; wgrep終了時にバッファを保存
  (setq wgrep-change-readonly-file t))  ; read-only bufferにも変更を適用する

;; TODO Migemo

;; Google Translate
(use-package google-translate :ensure t
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  :bind (("C-c t" . 'google-translate-at-point)
         ("C-c T" . 'google-translate-query-translate)))

;; id-manager
(use-package id-manager :ensure t
  :bind ("M-7" . id-manager)
  :init
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)  ; saving password
  (setenv "GPG_AGENT_INFO" nil)                                ; non-GUI password dialog.
  (setq idm-database-file "~/secret/idm-db.gpg")
  )

;; eww
(use-package eww :ensure t
  :custom
  (eww-search-prefix "http://www.google.com/?k1=-1&q=")
  (url-user-agent "Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Mobile Safari/537.36")
  )

;; text-adjust
(when (require 'text-adjust nil t)
  (defun text-adjust-space-before-save-if-needed ()
    (when (memq major-mode '(org-mode text-mode))
      (progn
        ;; 括弧は対象外,org tables
        (setq text-adjust-rule-space
              '((("\\cj" "" "[[0-9a-zA-Z]")   " ")
                (("[]/!?0-9a-zA-Z]" "" "\\cj") " ")))
        (text-adjust-space-buffer))))
  (defalias 'spacer 'text-adjust-space-buffer)
  (add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed))

;; aspell
(use-package ispell :ensure t
  :defer t
  :init
  (setq ispell-program-name "aspell")
  :config
  ;; (setq ispell-alternate-dictionary (expand-file-name "~/.emacs.d/etc/dict/words"))
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; flyspell
(use-package flyspell :ensure t
  :defer t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))

;; sudo-edit
(use-package sudo-edit :ensure t
  :bind ("C-c C-r" . sudo-edit))
  

;;--------------------------------
;; auto-complete
;;--------------------------------

(use-package auto-complete :ensure t
  :defer nil
  :bind (("<f2>" . auto-complete-mode)
         :map ac-menu-map
         ("M-n" . ac-next)
         ("M-p" . ac-previous)
         )
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-user-dictionary user-full-name)
  (add-to-list 'ac-user-dictionary user-mail-address)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case 'smart)
  (global-auto-complete-mode t)
  )
(use-package ac-math :ensure t :disabled
  :after auto-complete
  :hook (TeX-mode LaTeX-mode laTeX-mode org-mode)
  :config
  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources)) )

;;--------------------------------
;; org-mode
;;--------------------------------

(use-package org
  :defer t
  :ensure (org-plus-contrib :pin "org")
  :after ox-reveal
  :custom
  (org-babel-load-languages '((emacs-lisp . t)
                              (perl . t)
                              (python . t)
                              (R . t)
                              (js . t)
                              (shell . t)))
  :config
  (if (or (file-directory-p "~/org")
          (file-symlink-p "~/org"))
      (load "init-org")))

(use-package ox-reveal
  :ensure (ox-reveal :pin "melpa"))

(use-package auctex :ensure nil :disabled)
(use-package cdlatex :ensure nil :disabled
  :after auctex
  :hook ((LaTeX-mode . turn-on-cdlatex)   ; with AUCTeX LaTeX mode
         (laTeX-mode . turn-on-cdlatex))) ; with Emacs latex mode

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

(use-package powershell
  :if (memq system-type '(cygwin windows-nt)) :ensure t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :interpreter "powershell")

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

(use-package yaml-mode :ensure nil
  :mode "\\.ya?ml\\'")

(use-package json-mode :ensure t
  :custom
  (json-mode-indent-level 4)
  (json-encoding-default-indentation "    ") ;json.el
  :mode "\\.json\\'")

(use-package nginx-mode :ensure t
  :mode "nginx.*\\.conf\\'")

(use-package apache-mode :ensure t
  :mode 
  "\\.htaccess\\'"
  "/etc/httpd.+\\.conf\\'"
  "/etc/apache.+\\.conf\\'"
  "sites-\\(available\\|enabled\\)/")

;; Local Variables:
;; coding: utf-8-unix
;; End:
