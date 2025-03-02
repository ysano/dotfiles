;;; init-ai.el --- AI assistance
;;; Commentary:
;; AI related integrations like Copilot, Ellama, etc.
;;; Code:

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

(provide 'init-ai)
;;; init-ai.el ends here