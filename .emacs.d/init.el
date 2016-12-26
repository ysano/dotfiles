;;-----------------------------------------------------------------
;; utils
;;-----------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))
;; inits elisp path
(add-to-load-path "~/.emacs.d/inits")
;; .emacs.d/lisp/*
(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
(load "init-env")

;; suppress byte compile warning
(setq byte-compile-warnings
      '(not
        free-vars unresolved callargs redefine obsolete
        noruntime cl-function interactive-only make-local
        ))
(fset 'yes-or-no-p 'y-or-n-p)

;(setq debug-on-error t)
(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 6000)

;; Symbol's value as Variable is void: last-command-char in 24.3.1
(define-obsolete-variable-alias 'last-input-char
  'last-input-event "at least 19.34")
(define-obsolete-variable-alias 'last-command-char
  'last-command-event "at least 19.34")

;;-----------------------------------------------------------------
;; lang
;;-----------------------------------------------------------------
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)

;;-----------------------------------------------------------------
;; keybind
;;-----------------------------------------------------------------
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-1") 'help)

;;-----------------------------------------------------------------
;; visible
;;-----------------------------------------------------------------
(load "init-visible")
(load "init-encode")

;;-----------------------------------------------------------------
;; elpa package
;;-----------------------------------------------------------------
(load "init-elpa")
(load "init-yasnippet")
(load "init-w3m")
(load "init-org")

;;-----------------------------------------------------------------
;; built-in
;;-----------------------------------------------------------------

;; auto insert
(add-hook 'find-file-hook 'auto-insert)
(eval-after-load "yasnippet"
  '(progn
     (custom-set-variables '(auto-insert-alist '(())))
     (dolist (mode '(html-mode cperl-mode))
       (define-auto-insert mode (lambda () (insert "template") (yas/expand))))))

;; scheme-mode
(setq process-coding-system-alist
        (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq gosh-program-name "gosh -i")
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme gosh-program-name))
(define-key global-map
  "\C-cS" 'scheme-other-window)

;;-----------------------------------------------------------------
;; .emacs.d/elisp/*
;;-----------------------------------------------------------------

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-auto-update t)
(setq gtags-ignore-case t)
(setq gtags-suggested-key-mapping t)
(add-hook 'c-mode-hook
          '(lambda ()
             (gtags-mode 1)
             ))
;; [Setting to make 'Gtags select mode' easy to see]
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))

;; grep-edit
(require 'grep-edit)

;; text-adjust
(require 'text-adjust)
(defun text-adjust-space-before-save-if-needed ()
  (when (memq major-mode '(org-mode text-mode))
    (progn
      ;; 括弧は対象外
      (setq text-adjust-rule-space
            '((("\\cj" "" "[[0-9a-zA-Z+]")   " ")
              (("[]/!?0-9a-zA-Z+]" "" "\\cj") " ")))
      (text-adjust-space-buffer))))
(defalias 'spacer 'text-adjust-space-buffer)
(add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed)

;; id-manager
(autoload 'id-manager "id-manager" nil t)
(global-set-key (kbd "M-7") 'id-manager)                     ; anything UI
(setq epa-file-cache-passphrase-for-symmetric-encryption t)  ; saving password
(setenv "GPG_AGENT_INFO" nil)                                ; non-GUI password dialog.
(setq idm-database-file "~/secret/idm-db.gpg")

;; environmental diag
(if run-w32
    (load "init-w32"))
(if run-cygwin
    (load "init-cygwin"))
(setq temporary-file-directory "~/tmp/")
