;;; init-platform.el --- Platform-specific settings
;;; Commentary:
;; OS and hardware specific settings
;;; Code:

;; --------------------------------
;; Windows & Cygwin
;; --------------------------------
(when (eq system-type 'windows-nt)
  ;; GPG path fix for MSYS2 is in init.el (before package-initialize)

  ;; Windows-specific settings
  (when (getenv "CYGWIN_DIR")
    ;; Cygwin integration
    (setq cygwin-root-directory (getenv "CYGWIN_DIR"))
    (setq cygwin-mount-cygwin-bin-directory 
          (expand-file-name "bin" cygwin-root-directory))
    
    ;; Load cygwin setup
    (require 'setup-cygwin)
    
    ;; Use zsh if available
    (when (file-exists-p (expand-file-name "bin/zsh.exe" cygwin-root-directory))
      (add-to-list 'exec-path (expand-file-name "bin" cygwin-root-directory))
      (setq shell-file-name (expand-file-name "bin/zsh.exe" cygwin-root-directory))
      (setenv "SHELL" shell-file-name)
      (setenv "PATH" (concat (expand-file-name "bin" cygwin-root-directory) 
                             ";" (getenv "PATH")))
      (setq explicit-shell-file-name shell-file-name)
      (setq ediff-shell shell-file-name)
      (setq explicit-shell-args '("--login" "-i")))
    
    ;; Cygwin shell encoding tweak
    (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8))

  ;; Windows encoding settings
  (set-keyboard-coding-system 'cp932)
  (set-file-name-coding-system 'cp932)
  (set-terminal-coding-system 'cp932)
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932))

;; --------------------------------
;; macOS
;; --------------------------------
(when (eq system-type 'darwin)
  ;; macOS Option key as Meta
  (when window-system
    (setq mac-option-modifier 'meta))
  
  ;; Terminal mode Meta/Alt key support
  (unless window-system
    ;; Enable proper Meta key handling in terminal
    (setq meta-prefix-char nil)
    ;; Ensure Meta sequences are properly recognized
    (define-key input-decode-map "\e[1;9A" [M-up])
    (define-key input-decode-map "\e[1;9B" [M-down])
    (define-key input-decode-map "\e[1;9C" [M-right])
    (define-key input-decode-map "\e[1;9D" [M-left])
    ;; Add more Meta key combinations as needed
    (define-key input-decode-map "\ef" [M-f])
    (define-key input-decode-map "\eb" [M-b])
    (define-key input-decode-map "\ed" [M-d])
    (define-key input-decode-map "\e<" [M-<])
    (define-key input-decode-map "\e>" [M->])
    ;; Ensure tmux doesn't interfere with Meta keys
    (when (getenv "TMUX")
      (message "Running in tmux - Meta keys configured"))))

;; --------------------------------
;; Linux / Unix
;; --------------------------------
(when (not (memq system-type '(windows-nt darwin)))
  ;; Linux/Unix shell settings
  (setq shell-file-name "zsh")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)
  (setq ediff-shell shell-file-name)
  (setq explicit-shell-args '("--login" "-i")))

;; --------------------------------
;; WSL Mozc input method
;; --------------------------------
(when (file-exists-p "/mnt/c/opt/mozc/mozc_emacs_helper.sh")
  (use-package mozc-im
    :ensure t)
  (use-package mozc-popup
    :ensure t)
  (load "init-mozc"))

;; --------------------------------
;; Windows Japanese IME
;; --------------------------------
(when (featurep 'w32-ime)
  ;; Mode line indicator
  (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  
  ;; Default input method
  (setq default-input-method "W32-IME")
  
  ;; Suppress some input errors
  (global-set-key (kbd "<M-kanji>") 'ignore)
  (global-set-key (kbd "<kanji>") 'ignore))

;; --------------------------------
;; TRAMP remote editing settings
;; --------------------------------
;; Speed up by disabling version control on remote files
(setq vc-handled-backends '(Git))
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; TRAMP auto-save directory
(setq tramp-auto-save-directory 
      (expand-file-name "tramp-autosave" user-emacs-directory))

(provide 'init-platform)
;;; init-platform.el ends here