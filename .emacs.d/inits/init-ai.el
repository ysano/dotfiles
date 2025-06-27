;;; init-ai.el --- AI assistance
;;; Commentary:
;; AI related integrations like Copilot, Ellama, Claude Code, etc.
;;; Code:

;; Check if Emacs supports :vc keyword (Emacs 29+)
(defvar use-package-supports-vc (>= emacs-major-version 29)
  "Whether this Emacs version supports use-package :vc keyword.")

;; --------------------------------
;; Ellama - LLM interface
;; --------------------------------
;; Ellama設定
(use-package ellama
  :ensure t
  :defer t  ;; 遅延読み込みを有効にする
  :init
  ;; 基本設定のみinitブロックで行う
  (setq ellama-keymap-prefix "C-c e")
  (setq ellama-language "Japanese"))

;; 実際の設定はEmacsの起動後に行う
(with-eval-after-load 'ellama
  (require 'llm)
  (require 'llm-ollama)
  
  ;; Default model
  (setq ellama-provider
        (make-llm-ollama
         :host "172.29.80.1"
         :chat-model "llama3.1:8b-instruct-q4_K_S"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  
  ;; Predefined providers
  (setq ellama-providers
        '(("deepseek-coder-v2" . (make-llm-ollama
                                  :chat-model "deepseek-coder-v2:16b-lite-instruct-q2_K"
                                  :embedding-model "deepseek-coder-v2:16b-lite-instruct-q2_K"))
          ("zephyr" . (make-llm-ollama
                       :chat-model "zephyr:7b-beta-q6_K"
                       :embedding-model "zephyr:7b-beta-q6_K"))
          ("mistral" . (make-llm-ollama
                        :chat-model "mistral:7b-instruct-v0.2-q6_K"
                        :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
          ("mixtral" . (make-llm-ollama
                        :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
                        :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  
  ;; Naming sessions with LLM
  (setq ellama-naming-provider
        (make-llm-ollama
         :chat-model "llama3.1:8b-instruct-q4_K_S"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm)
  
  ;; Translation provider
  (setq ellama-translation-provider 
        (make-llm-ollama
         :chat-model "aya:8b-23-q4_K_S"
         :embedding-model "aya:8b-23-q4_K_S")))

;; --------------------------------
;; Claude Code Integration Note
;; --------------------------------
;; Claude Code integration is now handled in init-claude-code.el

;; --------------------------------
;; AI Tools Integration and Keybindings
;; --------------------------------
;; Unified AI tool access under C-c a prefix
(global-set-key (kbd "C-c a") nil)  ;; Clear any existing binding

;; Create a keymap for AI tools
(defvar ai-tools-map (make-sparse-keymap)
  "Keymap for AI tools.")

;; Ellama bindings (existing LLM interface)
(define-key ai-tools-map (kbd "e") ellama-keymap-prefix)
(define-key ai-tools-map (kbd "l") 'ellama-chat)                   ;; Chat with LLM
(define-key ai-tools-map (kbd "t") 'ellama-translate)              ;; Translate text
(define-key ai-tools-map (kbd "s") 'ellama-summarize)              ;; Summarize text
(define-key ai-tools-map (kbd "w") 'ellama-write)                  ;; Write with AI

;; Claude Code bindings (new integration)
(define-key ai-tools-map (kbd "c") 'claude-code-toggle)            ;; Claude Code
(define-key ai-tools-map (kbd "r") 'claude-code-send-region-for-review) ;; Review code
(define-key ai-tools-map (kbd "f") 'claude-code-explain-function)  ;; Explain function
(define-key ai-tools-map (kbd "d") 'claude-code-generate-docstring) ;; Generate docs
(define-key ai-tools-map (kbd "R") 'claude-code-refactor-with-diff) ;; Refactor with diff
(define-key ai-tools-map (kbd "D") 'claude-code-show-last-diff)     ;; Show last diff
(define-key ai-tools-map (kbd "h") 'claude-code-show-change-history) ;; Change history
(define-key ai-tools-map (kbd "u") 'claude-code-undo-last-change)   ;; Undo last change

;; Copilot bindings (existing code completion)
(define-key ai-tools-map (kbd "C") 'copilot-mode)                  ;; Toggle Copilot
(define-key ai-tools-map (kbd "a") 'copilot-accept-completion)     ;; Accept completion

;; Bind the AI tools map to C-c a
(global-set-key (kbd "C-c a") ai-tools-map)

;; --------------------------------
;; AI Tools Mode Line Indicator
;; --------------------------------
(defun ai-tools-mode-line-indicator ()
  "Show active AI tools in mode line."
  (let ((indicators '()))
    (when (and (boundp 'copilot-mode) copilot-mode)
      (push "Co" indicators))
    (when (and (boundp 'claude-code-active) claude-code-active)
      (push "CC" indicators))
    (when (and (boundp 'ellama-mode) ellama-mode)
      (push "El" indicators))
    (when indicators
      (format " [AI:%s]" (string-join indicators ",")))))

;; Add to mode line (optional)
;; (add-to-list 'mode-line-misc-info '(:eval (ai-tools-mode-line-indicator)))

(provide 'init-ai)
;;; init-ai.el ends here
