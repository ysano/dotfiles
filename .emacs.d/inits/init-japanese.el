;;; init-japanese.el --- Japanese input and encoding
;;; Commentary:
;; Simple Japanese language support using use-package patterns
;;; Code:

;; --------------------------------
;; Basic encoding settings
;; --------------------------------
(use-package emacs
  :custom
  ;; Language environment
  (language-environment "Japanese")
  (default-input-method "japanese-skk")
  ;; Encoding settings
  (locale-coding-system 'utf-8-unix)
  (file-name-coding-system 'utf-8-unix)
  (default-buffer-file-coding-system 'utf-8-unix)
  :config
  ;; Set coding systems
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8-unix))

;; --------------------------------
;; Mozc (Japanese Input Method)
;; --------------------------------
(use-package mozc
  :if (or (executable-find "mozc_emacs_helper")
          (file-exists-p "/usr/bin/mozc_emacs_helper")
          (file-exists-p "/mnt/c/opt/mozc/mozc_emacs_helper.exe"))
  :custom
  (default-input-method "japanese-mozc")
  (mozc-candidate-style 'overlay)
  :bind (("C-o" . toggle-input-method))
  :config
  ;; Set mozc helper path if WSL version exists
  (when (file-exists-p "/mnt/c/opt/mozc/mozc_emacs_helper.exe")
    (setq mozc-helper-program-name "/mnt/c/opt/mozc/mozc_emacs_helper.exe")))

;; --------------------------------
;; Japanese text handling
;; --------------------------------
(use-package text-mode
  :hook (text-mode . (lambda ()
                       ;; Enable word wrap for Japanese text
                       (setq truncate-lines nil)
                       (setq word-wrap t))))

(provide 'init-japanese)
;;; init-japanese.el ends here