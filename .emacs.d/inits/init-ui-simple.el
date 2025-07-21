;;; init-ui-simple.el --- Simple UI configuration
;;; Commentary:
;; Simple UI related settings using use-package patterns
;;; Code:

;; --------------------------------
;; Basic UI settings
;; --------------------------------
(use-package emacs
  :custom
  ;; Disable unnecessary UI elements
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  ;; Basic UI settings
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  (initial-buffer-choice (lambda () (dashboard-refresh-buffer) (get-buffer "*dashboard*")))
  ;; Font settings
  (use-default-font-for-symbols nil)
  :config
  ;; Apply settings
  (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

;; --------------------------------
;; Smart Font Scaling Configuration
;; --------------------------------
(use-package smart-font-scaling
  :ensure nil
  :load-path "elisp/"
  :if (display-graphic-p)
  :custom
  ;; 基本フォントサイズ（96 DPI基準）
  (sfs-base-font-size 13)
  ;; 優先フォントファミリー（既存設定を継承）
  (sfs-font-families '(("Cica" . "Cica")
                       ("Sarasa Term J" . "Sarasa Term J") 
                       ("Migu 1M" . "Migu 1M")
                       ("Monaco" . "Monaco")
                       ("Menlo" . "Menlo")))
  ;; DPIスケーリング閾値の調整（高解像度環境向け）
  (sfs-dpi-thresholds '((96 . 1.0)    ; 標準DPI
                        (120 . 1.3)   ; 高DPI（少し大きめ）
                        (140 . 1.6)   ; UWQHD相当
                        (180 . 2.0)   ; Retina相当
                        (220 . 2.5))) ; 高DPI Retina
  ;; 適用するUI要素を拡張
  (sfs-ui-elements '(default minibuffer-prompt mode-line mode-line-inactive 
                     header-line tooltip fringe))
  ;; デバッグモードを一時的に有効化
  (sfs-debug-mode t)
  :config
  ;; スマートフォントスケーリングを有効化
  (smart-font-scaling-mode 1)
  
  ;; 初期化後に確実に適用するための遅延実行
  (run-with-timer 1.0 nil 
                  (lambda ()
                    (when (display-graphic-p)
                      (sfs-apply-optimal-scaling)
                      (message "Smart font scaling re-applied after initialization"))))
  
  ;; 強制的な大フォント適用（高解像度ディスプレイ用）
  (when (display-graphic-p)
    (message "Applying large font for high-resolution display...")
    ;; より大きなサイズで強制設定
    (set-face-attribute 'default nil :height 200)  ; 20pt (より大きく)
    (set-face-attribute 'minibuffer-prompt nil :height 200)
    (set-face-attribute 'mode-line nil :height 180)
    (set-face-attribute 'mode-line-inactive nil :height 180)
    (set-face-attribute 'header-line nil :height 200)
    
    ;; フォント設定を他のパッケージから保護
    (run-with-timer 3.0 nil 
                    (lambda ()
                      ;; 最終的に確実に大きなフォントを設定
                      (set-face-attribute 'default nil :height 200)
                      (set-face-attribute 'minibuffer-prompt nil :height 200)
                      (message "Final font protection applied: 20pt")))))

;; --------------------------------
;; Icons
;; --------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; --------------------------------
;; Modern theme
;; --------------------------------
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; --------------------------------
;; Modern modeline
;; --------------------------------
(use-package doom-modeline
  :ensure t
  :custom
  ;; 高解像度対応: フォントサイズに基づいて動的調整
  (doom-modeline-height (if (and (boundp 'sfs--current-font-size) 
                                 sfs--current-font-size
                                 (> sfs--current-font-size 15))
                           35  ; 大きなフォント時は高いモードライン
                         25)) ; 標準時
  (doom-modeline-bar-width (if (and (boundp 'sfs--current-font-size)
                                   sfs--current-font-size  
                                   (> sfs--current-font-size 15))
                              4   ; 大きなフォント時は太いバー
                            3))   ; 標準時
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-from-project)
  :config
  (doom-modeline-mode 1))

;; --------------------------------
;; Line numbers
;; --------------------------------
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

;; --------------------------------
;; Rainbow delimiters
;; --------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; --------------------------------
;; Whitespace visualization
;; --------------------------------
(use-package whitespace
  :custom
  (whitespace-style '(face tabs spaces trailing space-before-tab
                      indentation empty space-after-tab))
  (whitespace-space-regexp "\\( +\\)")
  :hook (prog-mode . whitespace-mode))

;; --------------------------------
;; Dashboard
;; --------------------------------
(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(provide 'init-ui-simple)
;;; init-ui-simple.el ends here