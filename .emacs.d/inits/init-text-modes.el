;;; init-text-modes.el --- Text editing modes
;;; Commentary:
;; Settings for text-focused major modes
;;; Code:

;; --------------------------------
;; Markdown
;; --------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t))

;; --------------------------------
;; EWW web browser
;; --------------------------------
(use-package eww
  :ensure t
  :defer t
  :custom
  (eww-search-prefix "http://www.google.com/?k1=-1&q=")
  :config
  ;; Color handling
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    "Disable colorization of region by advising ORIG function with START, END, FG, BG and _ args."
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  
  (defun eww-disable-color ()
    "Turn off color in eww."
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  
  (defun eww-enable-color ()
    "Turn on color in eww."
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))
  
  ;; Load eww-hatebu if available
  (when (require 'eww-hatebu nil t)
    (with-eval-after-load 'eww
      (eww-hatebu-setup))))

;; --------------------------------
;; Octave/Matlab mode
;; --------------------------------
(use-package octave-mode
  :mode ("\\.m\\'" . octave-mode)
  :interpreter "octave")

(provide 'init-text-modes)
;;; init-text-modes.el ends here