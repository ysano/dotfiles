;;; init-claude-code-workflows.el --- Advanced Claude Code workflows
;;; Commentary:
;; Advanced workflows and integrations for Claude Code in Emacs
;;; Code:

;; --------------------------------
;; Advanced Claude Code Workflows
;; --------------------------------

(defgroup claude-code-workflows nil
  "Advanced workflows for Claude Code integration."
  :group 'ai-tools
  :prefix "claude-code-")

(defcustom claude-code-auto-context-files
  '("README.md" "package.json" "Cargo.toml" "pyproject.toml" "go.mod")
  "Files to automatically include as context when starting Claude Code."
  :type '(repeat string)
  :group 'claude-code-workflows)

(defcustom claude-code-excluded-directories
  '(".git" "node_modules" "__pycache__" "target" "dist" "build")
  "Directories to exclude from project context."
  :type '(repeat string)
  :group 'claude-code-workflows)

;; --------------------------------
;; Project Context Management
;; --------------------------------

(defun claude-code-get-project-files (directory &optional max-files)
  "Get relevant project files from DIRECTORY, limited to MAX-FILES."
  (let ((max-files (or max-files 20))
        (files '()))
    (dolist (file (directory-files-recursively directory "\\.\\(py\\|js\\|ts\\|el\\|go\\|rs\\|java\\|cpp\\|h\\)$"))
      (unless (or (>= (length files) max-files)
                  (cl-some (lambda (dir) (string-match-p dir file))
                           claude-code-excluded-directories))
        (push file files)))
    (nreverse files)))

(defun claude-code-send-project-overview ()
  "Send project overview to Claude Code."
  (interactive)
  (when-let ((root (projectile-project-root)))
    (let* ((project-name (projectile-project-name))
           (context-files (cl-remove-if-not 
                          (lambda (f) (file-exists-p (expand-file-name f root)))
                          claude-code-auto-context-files))
           (source-files (claude-code-get-project-files root 10))
           (overview (format "Project: %s\nRoot: %s\n\nContext files:\n%s\n\nMain source files:\n%s"
                            project-name
                            root
                            (mapconcat (lambda (f) (format "- %s" f)) context-files "\n")
                            (mapconcat (lambda (f) (format "- %s" (file-relative-name f root))) source-files "\n"))))
      (claude-code-send-command
       (format "Here's an overview of my current project:\n\n%s\n\nPlease familiarize yourself with this project structure." overview)))))

;; --------------------------------
;; Git Integration
;; --------------------------------

(defun claude-code-review-git-diff ()
  "Send current git diff to Claude Code for review."
  (interactive)
  (let ((diff (shell-command-to-string "git diff")))
    (if (string-empty-p diff)
        (message "No git diff found")
      (claude-code-send-command
       (format "Please review these git changes and provide feedback:\n\n```diff\n%s\n```" diff)))))

(defun claude-code-review-git-staged ()
  "Send staged git changes to Claude Code for review."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --staged")))
    (if (string-empty-p diff)
        (message "No staged changes found")
      (claude-code-send-command
       (format "Please review these staged changes before commit:\n\n```diff\n%s\n```" diff)))))

(defun claude-code-suggest-commit-message ()
  "Ask Claude Code to suggest a commit message for staged changes."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --staged")))
    (if (string-empty-p diff)
        (message "No staged changes found")
      (claude-code-send-command
       (format "Please suggest a concise commit message for these changes:\n\n```diff\n%s\n```" diff)))))

;; --------------------------------
;; Testing Integration
;; --------------------------------

(defun claude-code-generate-tests ()
  "Generate tests for the current function or class."
  (interactive)
  (let* ((func-bounds (bounds-of-thing-at-point 'defun))
         (func-text (when func-bounds
                      (buffer-substring-no-properties 
                       (car func-bounds) (cdr func-bounds))))
         (file-ext (file-name-extension (buffer-file-name))))
    (when func-text
      (claude-code-send-command
       (format "Please generate comprehensive unit tests for this %s code:\n\n```%s\n%s\n```\n\nInclude edge cases and error conditions."
               (or file-ext "")
               (or file-ext "")
               func-text)))))

(defun claude-code-explain-test-failure ()
  "Explain test failures from compilation buffer."
  (interactive)
  (let ((test-output (with-current-buffer "*compilation*"
                       (buffer-string))))
    (claude-code-send-command
     (format "Please explain these test failures and suggest fixes:\n\n```\n%s\n```" test-output))))

;; --------------------------------
;; Code Quality and Security
;; --------------------------------

(defun claude-code-security-review ()
  "Ask Claude Code to review current buffer for security issues."
  (interactive)
  (claude-code-send-command
   (format "Please review this %s code for security vulnerabilities and best practices:\n\n```%s\n%s\n```"
           (or (file-name-extension (buffer-file-name)) "")
           (or (file-name-extension (buffer-file-name)) "")
           (buffer-string))))

(defun claude-code-performance-review ()
  "Ask Claude Code to review code for performance issues."
  (interactive)
  (claude-code-send-command
   (format "Please review this %s code for performance optimizations:\n\n```%s\n%s\n```"
           (or (file-name-extension (buffer-file-name)) "")
           (or (file-name-extension (buffer-file-name)) "")
           (buffer-string))))

(defun claude-code-accessibility-review ()
  "Review frontend code for accessibility issues."
  (interactive)
  (when (member (file-name-extension (buffer-file-name)) '("html" "jsx" "tsx" "vue"))
    (claude-code-send-command
     (format "Please review this frontend code for accessibility (a11y) issues:\n\n```%s\n%s\n```"
             (file-name-extension (buffer-file-name))
             (buffer-string)))))

;; --------------------------------
;; Architecture and Design
;; --------------------------------

(defun claude-code-architecture-analysis ()
  "Analyze project architecture and suggest improvements."
  (interactive)
  (when-let ((root (projectile-project-root)))
    (let ((file-structure (shell-command-to-string 
                          (format "find %s -name '*.py' -o -name '*.js' -o -name '*.ts' -o -name '*.go' -o -name '*.rs' | head -20" root))))
      (claude-code-send-command
       (format "Please analyze this project's architecture and suggest improvements:\n\nProject structure:\n%s" file-structure)))))

(defun claude-code-design-patterns ()
  "Suggest design patterns for current code."
  (interactive)
  (claude-code-send-command
   (format "Please analyze this code and suggest appropriate design patterns:\n\n```%s\n%s\n```"
           (or (file-name-extension (buffer-file-name)) "")
           (buffer-string))))

;; --------------------------------
;; Documentation Generation
;; --------------------------------

(defun claude-code-generate-readme ()
  "Generate README.md for current project."
  (interactive)
  (when-let ((root (projectile-project-root)))
    (let* ((project-name (projectile-project-name))
           (package-json (expand-file-name "package.json" root))
           (cargo-toml (expand-file-name "Cargo.toml" root))
           (pyproject-toml (expand-file-name "pyproject.toml" root))
           (context ""))
      (cond
       ((file-exists-p package-json)
        (setq context (with-temp-buffer
                        (insert-file-contents package-json)
                        (buffer-string))))
       ((file-exists-p cargo-toml)
        (setq context (with-temp-buffer
                        (insert-file-contents cargo-toml)
                        (buffer-string))))
       ((file-exists-p pyproject-toml)
        (setq context (with-temp-buffer
                        (insert-file-contents pyproject-toml)
                        (buffer-string)))))
      (claude-code-send-command
       (format "Please generate a comprehensive README.md for this project:\n\nProject: %s\nConfiguration:\n```\n%s\n```"
               project-name context)))))

(defun claude-code-generate-api-docs ()
  "Generate API documentation for current file."
  (interactive)
  (claude-code-send-command
   (format "Please generate API documentation for this %s file:\n\n```%s\n%s\n```"
           (or (file-name-extension (buffer-file-name)) "")
           (or (file-name-extension (buffer-file-name)) "")
           (buffer-string))))

;; --------------------------------
;; Workflow Automation
;; --------------------------------

(defun claude-code-auto-workflow ()
  "Automatically run appropriate Claude Code workflow based on context."
  (interactive)
  (cond
   ;; If in a git repository with unstaged changes
   ((and (vc-git-root default-directory)
         (not (string-empty-p (shell-command-to-string "git diff"))))
    (claude-code-review-git-diff))
   
   ;; If compilation errors exist
   ((get-buffer "*compilation*")
    (claude-code-debug-error))
   
   ;; If in a test file
   ((string-match-p "test\\|spec" (buffer-file-name))
    (claude-code-explain-function))
   
   ;; Default: analyze current buffer
   (t
    (claude-code-send-buffer-with-context))))

;; --------------------------------
;; Keybindings for Advanced Workflows
;; --------------------------------

(defvar claude-code-workflows-map (make-sparse-keymap)
  "Keymap for Claude Code advanced workflows.")

;; Project and git workflows
(define-key claude-code-workflows-map (kbd "p") 'claude-code-send-project-overview)
(define-key claude-code-workflows-map (kbd "g d") 'claude-code-review-git-diff)
(define-key claude-code-workflows-map (kbd "g s") 'claude-code-review-git-staged)
(define-key claude-code-workflows-map (kbd "g c") 'claude-code-suggest-commit-message)

;; Testing workflows
(define-key claude-code-workflows-map (kbd "t g") 'claude-code-generate-tests)
(define-key claude-code-workflows-map (kbd "t f") 'claude-code-explain-test-failure)

;; Code quality workflows
(define-key claude-code-workflows-map (kbd "q s") 'claude-code-security-review)
(define-key claude-code-workflows-map (kbd "q p") 'claude-code-performance-review)
(define-key claude-code-workflows-map (kbd "q a") 'claude-code-accessibility-review)

;; Architecture workflows
(define-key claude-code-workflows-map (kbd "a a") 'claude-code-architecture-analysis)
(define-key claude-code-workflows-map (kbd "a d") 'claude-code-design-patterns)

;; Documentation workflows
(define-key claude-code-workflows-map (kbd "D r") 'claude-code-generate-readme)
(define-key claude-code-workflows-map (kbd "D a") 'claude-code-generate-api-docs)

;; Auto workflow
(define-key claude-code-workflows-map (kbd "RET") 'claude-code-auto-workflow)

;; Only bind if ai-tools-map exists
(when (boundp 'ai-tools-map)
  (define-key ai-tools-map (kbd "w") claude-code-workflows-map))

;; Create a simple prefix map for workflows if the complex one doesn't work
(defvar claude-code-simple-workflows-map (make-sparse-keymap)
  "Simple keymap for Claude Code workflows.")

(define-key claude-code-simple-workflows-map (kbd "p") 'claude-code-send-project-overview)
(define-key claude-code-simple-workflows-map (kbd "a") 'claude-code-auto-workflow)

;; Bind to a different prefix to avoid conflicts
(global-set-key (kbd "C-c w") claude-code-simple-workflows-map)

(provide 'init-claude-code-workflows)
;;; init-claude-code-workflows.el ends here