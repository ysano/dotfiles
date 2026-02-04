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

  ;; Ollama host configuration (環境変数OLLAMA_HOSTを優先、未設定時はWSL用デフォルト)
  (defvar my/ollama-host (or (getenv "OLLAMA_HOST") "172.29.80.1")
    "Ollama server host. Set OLLAMA_HOST environment variable to override.")

  ;; Default model
  (setq ellama-provider
        (make-llm-ollama
         :host my/ollama-host
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
;; gptel - 軽量LLMチャットクライアント
;; --------------------------------
;; ellamaとの使い分け:
;; - gptel: バッファ内チャット、Org分岐会話、対話重視
;; - ellama: タスク別コマンド（翻訳、要約、コードレビュー等）
(use-package gptel
  :ensure t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :custom
  ;; Ollama backend (環境変数OLLAMA_HOSTを使用)
  (gptel-default-mode 'org-mode)  ;; デフォルトでOrg-modeバッファ
  :config
  ;; Ollamaバックエンド設定
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host (or (getenv "OLLAMA_HOST") "172.29.80.1:11434")
          :stream t
          :models '("llama3.1:8b-instruct-q4_K_S"
                    "deepseek-coder-v2:16b-lite-instruct-q2_K"
                    "mistral:7b-instruct-v0.2-q6_K")))
  (setq gptel-model "llama3.1:8b-instruct-q4_K_S"))

;; --------------------------------
;; AI Tools Integration and Keybindings
;; --------------------------------
;; Unified AI tool access under C-c a prefix
(global-set-key (kbd "C-c a") nil)  ;; Clear any existing binding

;; Create a keymap for AI tools
(defvar ai-tools-map (make-sparse-keymap)
  "Keymap for AI tools.")

;; gptel bindings (chat-focused interface)
(define-key ai-tools-map (kbd "g") 'gptel)                         ;; Open gptel chat
(define-key ai-tools-map (kbd "RET") 'gptel-send)                  ;; Send message
(define-key ai-tools-map (kbd "m") 'gptel-menu)                    ;; gptel menu

;; Ellama bindings (task-focused interface)
(define-key ai-tools-map (kbd "e") ellama-keymap-prefix)
(define-key ai-tools-map (kbd "l") 'ellama-chat)                   ;; Chat with LLM
(define-key ai-tools-map (kbd "t") 'ellama-translate)              ;; Translate text
(define-key ai-tools-map (kbd "s") 'ellama-summarize)              ;; Summarize text
(define-key ai-tools-map (kbd "w") 'ellama-write)                  ;; Write with AI

;; Copilot bindings (code completion)
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
    (when (and (boundp 'ellama-mode) ellama-mode)
      (push "El" indicators))
    (when indicators
      (format " [AI:%s]" (string-join indicators ",")))))

;; Add to mode line (optional)
;; (add-to-list 'mode-line-misc-info '(:eval (ai-tools-mode-line-indicator)))

(provide 'init-ai)
;;; init-ai.el ends here
