;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; for GNUPACK

;;-----------------------------------------------------------------
;; lang
;;-----------------------------------------------------------------
(set-keyboard-coding-system 'cp932)
(prefer-coding-system 'utf-8-dos)
(set-file-name-coding-system 'cp932)
(setq default-process-coding-system '(cp932 . cp932))

;; ------------------------------------------------------------------------
;; @ font

;; 標準フォントの設定
;; (set-default-font "M+2VM+IPAG circle-12")
(set-default-font "Migu 1M")

;; IME変換時フォントの設定（テストバージョンのみ）
;; (setq w32-ime-font-face "MigMix 1M")
;; (setq w32-ime-font-height 22)

;; 固定等幅フォントの設定
;; (set-face-attribute 'fixed-pitch    nil :family "M+2VM+IPAG circle")

;; 可変幅フォントの設定
;; (set-face-attribute 'variable-pitch nil :family "M+2VM+IPAG circle")

;; ------------------------------------------------------------------------
;; @ image-library
(setq image-library-alist
      '((xpm "libxpm.dll")
        (png "libpng14.dll")
        (jpeg "libjpeg.dll")
        (tiff "libtiff3.dll")
        (gif "libungif4.dll")
        (svg "librsvg-2-2.dll")
        (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
        (glib "libglib-2.0-0.dll")
        (gobject "libgobject-2.0-0.dll"))
      )

;; ------------------------------------------------------------------------
;; @ print

(setq ps-print-color-p t
      ps-lpr-command "gswin32c.exe"
      ps-multibyte-buffer 'non-latin-printer
      ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
      printer-name nil
      ps-printer-name nil
      ps-printer-name-option nil
      ps-print-header nil          ; ヘッダの非表示
      )

;; ------------------------------------------------------------------------
;; @ setup-cygwin
(setq cygwin-mount-cygwin-bin-directory
      (concat (getenv "CYGWIN_DIR") "\\bin"))
(require 'setup-cygwin)
(file-name-shadow-mode -1)

;; ------------------------------------------------------------------------
;; @ shell
(require 'shell)
(setq explicit-shell-file-name "bash.exe")
(setq shell-command-switch "-c")
(setq shell-file-name "bash.exe")

;; (M-! and M-| and compile.el)
(setq shell-file-name "bash.exe")
(modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

;; shellモードの時の^M抑制
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; shell-modeでの補完 (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

;; エスケープシーケンス処理の設定
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(setq shell-mode-hook
      (function
       (lambda ()

         ;; シェルモードの入出力文字コード
         (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
         (set-buffer-file-coding-system    'sjis-unix)
         )))

;; ------------------------------------------------------------------------
;; @ menu-tree
(setq menu-tree-coding-system 'utf-8)
(require 'menu-tree)

;; ------------------------------------------------------------------------
;; @ migemo/cmigemo
(setq migemo-command (concat (getenv "INST_DIR")
                             "\\app\\cmigemo\\cmigemo"))
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary (concat (getenv "INST_DIR")
                                "\\app\\cmigemo\\dict\\utf-8\\migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; ------------------------------------------------------------------------
;; @ w32-symlinks

(custom-set-variables '(w32-symlinks-handle-shortcuts t))
(require 'w32-symlinks)

(defadvice insert-file-contents-literally
  (before insert-file-contents-literally-before activate)
  (set-buffer-multibyte nil))

(defadvice minibuffer-complete (before expand-symlinks activate)
  (let ((file (expand-file-name
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))))
    (when (file-symlink-p file)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (w32-symlinks-parse-symlink file)))))

