;;; init-claude-code.el --- Claude Code integration
;;; Commentary:
;; Simple Claude Code integration using use-package patterns
;;; Code:

;; --------------------------------
;; Claude Code CLI Integration
;; --------------------------------
(use-package claude-code
  :if (executable-find "claude")
  :commands (claude-code-query
             claude-code-send-buffer
             claude-code-review-region
             claude-code-explain-function)
  :bind (("C-c C-q" . claude-code-query)
         ("C-c C-b" . claude-code-send-buffer)
         ("C-c C-r" . claude-code-review-region)
         ("C-c C-f" . claude-code-explain-function)
         ("C-c C-d" . claude-code-generate-docs))
  :custom
  (claude-code-auto-save t)
  (claude-code-show-diff t)
  :config
  ;; Simple wrapper functions
  (defun claude-code-query (prompt)
    "Send PROMPT to Claude Code."
    (interactive "sPrompt: ")
    (when (executable-find "claude")
      (let ((response (shell-command-to-string 
                       (format "claude -p %s" (shell-quote-argument prompt)))))
        (with-current-buffer (get-buffer-create "*Claude Code*")
          (erase-buffer)
          (insert response)
          (pop-to-buffer (current-buffer))))))
  
  (defun claude-code-send-buffer ()
    "Send current buffer to Claude Code for analysis."
    (interactive)
    (claude-code-query (format "Analyze this code:\n\n%s" (buffer-string))))
  
  (defun claude-code-review-region (start end)
    "Review selected region with Claude Code."
    (interactive "r")
    (claude-code-query (format "Review this code:\n\n%s" 
                               (buffer-substring start end))))
  
  (defun claude-code-explain-function ()
    "Explain function at point."
    (interactive)
    (let ((func-name (thing-at-point 'symbol t)))
      (when func-name
        (claude-code-query (format "Explain the function: %s" func-name)))))
  
  (defun claude-code-generate-docs ()
    "Generate documentation for current buffer."
    (interactive)
    (claude-code-query (format "Generate documentation for:\n\n%s" (buffer-string)))))

;; --------------------------------
;; AI Assistant Integration
;; --------------------------------
(use-package ai-assistant
  :bind (("C-c a" . ai-assistant-hydra/body))
  :config
  (defhydra ai-assistant-hydra (:color blue)
    "AI Assistant"
    ("c" claude-code-query "Claude query")
    ("b" claude-code-send-buffer "Analyze buffer")
    ("r" claude-code-review-region "Review region")
    ("f" claude-code-explain-function "Explain function")
    ("d" claude-code-generate-docs "Generate docs")
    ("q" nil "quit")))

(provide 'init-claude-code)
;;; init-claude-code.el ends here