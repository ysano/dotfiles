;;; init-claude-code-simple.el --- Simple Claude Code integration
;;; Commentary:
;; Basic Claude Code integration without advanced use-package features
;;; Code:

;; --------------------------------
;; Manual Claude Code Integration (for older Emacs versions)
;; --------------------------------

;; Check if claude binary is available
(defvar claude-code-available-p (executable-find "claude")
  "Whether Claude Code CLI is available.")

;; --------------------------------
;; Claude Code Diff and History Management
;; --------------------------------

;; Variables for change tracking
(defvar claude-code-change-history '()
  "History of Claude Code changes.")

(defvar claude-code-before-change-content nil
  "Content before Claude Code change.")

(defvar claude-code-last-change-info nil
  "Information about the last Claude Code change.")

(when claude-code-available-p
  ;; Define basic Claude Code functions manually
  
  (defun claude-code-query (prompt)
    "Send PROMPT to Claude Code and return response."
    (when claude-code-available-p
      (shell-command-to-string 
       (format "claude -p \"%s\"" (shell-quote-argument prompt)))))
  
  (defun claude-code-save-buffer-state ()
    "Save current buffer state before Claude Code changes."
    (setq claude-code-before-change-content (buffer-string))
    (setq claude-code-last-change-info
          (list :buffer (current-buffer)
                :file (buffer-file-name)
                :timestamp (current-time-string)
                :content claude-code-before-change-content)))
  
  (defun claude-code-show-diff (old-content new-content)
    "Show diff between OLD-CONTENT and NEW-CONTENT."
    (let ((diff-buffer (get-buffer-create "*Claude Code Diff*"))
          (temp-old (make-temp-file "claude-old"))
          (temp-new (make-temp-file "claude-new")))
      ;; Write contents to temp files
      (with-temp-file temp-old (insert old-content))
      (with-temp-file temp-new (insert new-content))
      ;; Generate diff
      (with-current-buffer diff-buffer
        (erase-buffer)
        (call-process "diff" nil t nil "-u" temp-old temp-new)
        (goto-char (point-min))
        (diff-mode)
        (display-buffer diff-buffer))
      ;; Clean up temp files
      (delete-file temp-old)
      (delete-file temp-new)))
  
  (defun claude-code-apply-changes-with-diff (original-content new-content)
    "Apply changes with diff display and confirmation."
    (claude-code-show-diff original-content new-content)
    (when (y-or-n-p "Apply these changes? ")
      (erase-buffer)
      (insert new-content)
      ;; Add to history
      (push (list :timestamp (current-time-string)
                  :buffer (current-buffer)
                  :old-content original-content
                  :new-content new-content)
            claude-code-change-history)
      (message "Changes applied successfully")
      t))
  
  (defun claude-code-query-json (prompt)
    "Send PROMPT to Claude Code and return JSON response."
    (when claude-code-available-p
      (let ((result (shell-command-to-string 
                     (format "claude -p \"%s\" --output-format json" 
                             (shell-quote-argument prompt)))))
        (condition-case nil
            (json-parse-string result)
          (error result)))))
  
  (defun claude-code-send-buffer-simple ()
    "Send current buffer to Claude Code for analysis."
    (interactive)
    (when claude-code-available-p
      (let* ((file-ext (or (file-name-extension (buffer-file-name)) "text"))
             (content (buffer-string))
             (prompt (format "Please analyze this %s file and provide suggestions:\n\n```%s\n%s\n```"
                           file-ext file-ext content)))
        (message "Sending buffer to Claude Code...")
        (let ((response (claude-code-query prompt)))
          (with-current-buffer (get-buffer-create "*Claude Code Response*")
            (erase-buffer)
            (insert response)
            (goto-char (point-min))
            (display-buffer (current-buffer)))))))
  
  (defun claude-code-refactor-with-diff ()
    "Refactor code with diff preview."
    (interactive)
    (when claude-code-available-p
      (claude-code-save-buffer-state)
      (let* ((file-ext (or (file-name-extension (buffer-file-name)) "text"))
             (content (buffer-string))
             (prompt (format "Please refactor and improve this %s code. Return ONLY the improved code, no explanations:\n\n```%s\n%s\n```"
                           file-ext file-ext content)))
        (message "Refactoring code with Claude Code...")
        (let ((response (claude-code-query prompt)))
          ;; Extract code from response (remove potential markdown)
          (let ((clean-code (if (string-match "```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)```" response)
                               (match-string 1 response)
                             response)))
            (claude-code-apply-changes-with-diff content clean-code))))))
  
  (defun claude-code-send-region-simple (start end)
    "Send selected region to Claude Code for review."
    (interactive "r")
    (when claude-code-available-p
      (let* ((file-ext (or (file-name-extension (buffer-file-name)) "text"))
             (content (buffer-substring-no-properties start end))
             (prompt (format "Please review this %s code and suggest improvements:\n\n```%s\n%s\n```"
                           file-ext file-ext content)))
        (message "Sending region to Claude Code...")
        (let ((response (claude-code-query prompt)))
          (with-current-buffer (get-buffer-create "*Claude Code Response*")
            (erase-buffer)
            (insert response)
            (goto-char (point-min))
            (display-buffer (current-buffer)))))))
  
  (defun claude-code-explain-function-simple ()
    "Ask Claude Code to explain the function at point."
    (interactive)
    (when claude-code-available-p
      (let* ((func-bounds (bounds-of-thing-at-point 'defun))
             (func-text (when func-bounds
                          (buffer-substring-no-properties 
                           (car func-bounds) (cdr func-bounds))))
             (file-ext (or (file-name-extension (buffer-file-name)) "text")))
        (when func-text
          (let ((prompt (format "Please explain what this %s function does:\n\n```%s\n%s\n```"
                              file-ext file-ext func-text)))
            (message "Asking Claude Code to explain function...")
            (let ((response (claude-code-query prompt)))
              (with-current-buffer (get-buffer-create "*Claude Code Response*")
                (erase-buffer)
                (insert response)
                (goto-char (point-min))
                (display-buffer (current-buffer)))))))))
  
  (defun claude-code-debug-error-simple ()
    "Send compilation errors to Claude Code for debugging help."
    (interactive)
    (when claude-code-available-p
      (let ((error-buffer (get-buffer "*compilation*")))
        (if error-buffer
            (with-current-buffer error-buffer
              (let ((errors (buffer-string)))
                (message "Sending errors to Claude Code...")
                (let ((response (claude-code-query 
                               (format "Please help debug these errors:\n\n```\n%s\n```" errors))))
                  (with-current-buffer (get-buffer-create "*Claude Code Response*")
                    (erase-buffer)
                    (insert response)
                    (goto-char (point-min))
                    (display-buffer (current-buffer))))))
          (message "No compilation buffer found")))))
  
  ;; Diff and history management functions
  (defun claude-code-show-last-diff ()
    "Show diff of the last Claude Code change."
    (interactive)
    (if claude-code-last-change-info
        (let ((old-content (plist-get claude-code-last-change-info :content))
              (new-content (with-current-buffer (plist-get claude-code-last-change-info :buffer)
                            (buffer-string))))
          (claude-code-show-diff old-content new-content))
      (message "No recent Claude Code changes to show")))
  
  (defun claude-code-show-change-history ()
    "Show Claude Code change history."
    (interactive)
    (let ((history-buffer (get-buffer-create "*Claude Code History*")))
      (with-current-buffer history-buffer
        (erase-buffer)
        (insert "Claude Code Change History\n")
        (insert "===========================\n\n")
        (if claude-code-change-history
            (dolist (change claude-code-change-history)
              (insert (format "Timestamp: %s\n" (plist-get change :timestamp)))
              (insert (format "Buffer: %s\n" (buffer-name (plist-get change :buffer))))
              (insert "---\n"))
          (insert "No changes recorded yet.\n"))
        (goto-char (point-min))
        (display-buffer (current-buffer)))))
  
  (defun claude-code-undo-last-change ()
    "Undo the last Claude Code change."
    (interactive)
    (if claude-code-change-history
        (let ((last-change (car claude-code-change-history)))
          (when (buffer-live-p (plist-get last-change :buffer))
            (with-current-buffer (plist-get last-change :buffer)
              (erase-buffer)
              (insert (plist-get last-change :old-content))
              (setq claude-code-change-history (cdr claude-code-change-history))
              (message "Last Claude Code change undone"))))
      (message "No Claude Code changes to undo")))
  
  ;; Bridge functions for workflow compatibility
  (defun claude-code-send-command (command)
    "Send COMMAND to Claude Code and display response."
    (when claude-code-available-p
      (message "Sending command to Claude Code...")
      (let ((response (claude-code-query command)))
        (with-current-buffer (get-buffer-create "*Claude Code Response*")
          (erase-buffer)
          (insert response)
          (goto-char (point-min))
          (display-buffer (current-buffer))))))
  
  ;; Aliases for workflow compatibility
  (defalias 'claude-code-send-buffer-with-context 'claude-code-send-buffer-simple)
  (defalias 'claude-code-send-region-for-review 'claude-code-send-region-simple)
  (defalias 'claude-code-explain-function 'claude-code-explain-function-simple)
  (defalias 'claude-code-debug-error 'claude-code-debug-error-simple)
  (defalias 'claude-code-generate-docstring 'claude-code-explain-function-simple)
  (defalias 'claude-code-toggle 'claude-code-send-buffer-simple)
  
  ;; Simple keybindings for Claude Code (avoid conflicts)
  (global-set-key (kbd "C-c C-c b") 'claude-code-send-buffer-simple)
  (global-set-key (kbd "C-c C-c r") 'claude-code-send-region-simple)
  (global-set-key (kbd "C-c C-c f") 'claude-code-explain-function-simple)
  (global-set-key (kbd "C-c C-c e") 'claude-code-debug-error-simple)
  
  ;; Diff and refactoring keybindings
  (global-set-key (kbd "C-c C-c R") 'claude-code-refactor-with-diff)
  (global-set-key (kbd "C-c C-c d") 'claude-code-show-last-diff)
  (global-set-key (kbd "C-c C-c h") 'claude-code-show-change-history)
  (global-set-key (kbd "C-c C-c u") 'claude-code-undo-last-change)
  
  (message "Claude Code simple integration loaded"))

(provide 'init-claude-code-simple)
;;; init-claude-code-simple.el ends here