;;; init-navigation.el --- Navigation and search
;;; Commentary:
;; Functionality for navigating code and files
;;; Code:

;; --------------------------------
;; Vertico - 縦型補完UI (ivy代替)
;; --------------------------------
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)                  ;; 表示候補数
  (vertico-cycle t)                   ;; 循環移動
  (vertico-resize nil)                ;; ウィンドウサイズ固定
  :config
  (vertico-mode 1))

;; Vertico ディレクトリ操作拡張
(use-package vertico-directory
  :after vertico
  :ensure nil                         ;; vertico同梱、別途インストール不要
  :bind (:map vertico-map
         ("C-l" . vertico-directory-up)
         ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; 補完スタイル - スペース区切りで柔軟にマッチ
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 補完候補に注釈を追加
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; --------------------------------
;; Consult - 検索・ナビゲーション (counsel/swiper代替)
;; --------------------------------
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)              ;; swiper代替
         ("C-c C-r" . consult-history)        ;; ivy-resume代替
         ([f6] . consult-history)
         ("C-c g" . consult-git-grep)         ;; counsel-git代替
         ("C-c j" . consult-git-grep)         ;; counsel-git-grep代替
         ("C-c k" . consult-ripgrep)          ;; counsel-ag代替
         ("C-x l" . consult-locate)           ;; counsel-locate代替
         ("C-x C-r" . consult-recent-file)    ;; counsel-buffer-or-recentf代替
         ("C-x b" . consult-buffer)           ;; switch-to-buffer強化
         ("M-g g" . consult-goto-line)        ;; goto-line強化
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)            ;; imenu強化
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  :config
  ;; プレビュー設定
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.4 any)))

;; Embark - ミニバッファアクション
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

;; Consult と Embark の統合
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; --------------------------------
;; Jump Navigation (avy - ace-jump-modeの後継)
;; --------------------------------
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char-timer)  ;; 2文字入力で絞り込み
         ("C-:" . avy-goto-char)
         ("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :custom
  (avy-timeout-seconds 0.3)  ;; char-timerのタイムアウト
  (avy-background t)         ;; 背景をグレーアウト
  (avy-style 'at-full))

;; Window navigation (winum - ace-window代替)
;; モードラインにウィンドウ番号を表示、M-0〜M-9でジャンプ
(use-package winum
  :ensure t
  :custom
  (winum-auto-setup-mode-line nil)  ;; doom-modelineが表示するため
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-o" . winum-select-window-by-number)  ;; ace-window互換
         ("C-x w 1" . winum-select-window-1)
         ("C-x w 2" . winum-select-window-2)
         ("C-x w 3" . winum-select-window-3)
         ("C-x w 4" . winum-select-window-4))
  :config
  (winum-mode 1))

;; --------------------------------
;; Project Management (project.el - 組み込み)
;; --------------------------------
(use-package project
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-regexp "Find regexp" ?g)
     (project-find-dir "Find dir" ?d)
     (project-dired "Dired" ?D)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)))
  :config
  ;; projectile互換キーバインド (C-c p)
  (define-key global-map (kbd "C-c p f") #'project-find-file)
  (define-key global-map (kbd "C-c p g") #'project-find-regexp)
  (define-key global-map (kbd "C-c p d") #'project-find-dir)
  (define-key global-map (kbd "C-c p b") #'project-switch-to-buffer)
  (define-key global-map (kbd "C-c p k") #'project-kill-buffers)
  (define-key global-map (kbd "C-c p p") #'project-switch-project)
  (define-key global-map (kbd "C-c p c") #'project-compile)
  (define-key global-map (kbd "C-c p !") #'project-shell-command)
  (define-key global-map (kbd "C-c p &") #'project-async-shell-command)
  (define-key global-map (kbd "C-c p e") #'project-eshell)
  (define-key global-map (kbd "C-c p s") #'project-shell)

  ;; projectile互換: プロジェクト名をモードラインに表示
  (defun my-project-name ()
    "Get current project name."
    (when-let ((proj (project-current)))
      (file-name-nondirectory (directory-file-name (project-root proj)))))

  ;; projectile-project-root の互換関数
  (defun my-project-root ()
    "Get project root directory (projectile-project-root compatible)."
    (when-let ((proj (project-current)))
      (project-root proj))))


;; --------------------------------
;; File browsing
;; --------------------------------
(use-package imenu-list
  :ensure t
  :custom
  (imenu-list-focus-after-activation t)
  :bind ("C-'" . imenu-list-smart-toggle))

;; --------------------------------
;; Recent Files
;; --------------------------------
(use-package recentf
  :custom
  (recentf-max-saved-items 50)
  :config
  (setq recentf-auto-save-timer (run-with-idle-timer 180 t 'recentf-save-list))
  (recentf-mode 1))

;; --------------------------------
;; Ripgrep for fast search
;; --------------------------------
(use-package rg
  :ensure t
  :if (executable-find "rg"))

;; --------------------------------
;; Interactive mode for grep results
;; --------------------------------
(use-package wgrep
  :ensure t
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; --------------------------------
;; Which-key (modern replacement for guide-key)
;; --------------------------------
(use-package which-key
  :ensure t
  :defer 2  ;; 2秒後に遅延読み込み
  :delight
  :custom
  (which-key-idle-delay 1.0)
  (which-key-max-description-length 32)
  :config
  (which-key-mode 1))

;; --------------------------------
;; Transient menus (hydra代替、magit依存)
;; --------------------------------
(use-package transient
  :after magit  ;; magit経由で既にインストール済み
  :config
  ;; Toggle menu for various modes
  (transient-define-prefix transient-toggle ()
    "Toggle various modes."
    ["Toggle Modes"
     ("a" "abbrev-mode" abbrev-mode)
     ("d" "debug-on-error" toggle-debug-on-error)
     ("f" "auto-fill-mode" auto-fill-mode)
     ("t" "truncate-lines" toggle-truncate-lines)
     ("w" "whitespace-mode" whitespace-mode)])
  :bind ("C-c C-v" . transient-toggle))

;; --------------------------------
;; TRAMP remote editing
;; --------------------------------
(use-package tramp
  :defer t
  :config
  (if (eq window-system 'w32)
      (setq tramp-default-method "plinkx")
    (setq tramp-default-method "sshx")))

(provide 'init-navigation)
;;; init-navigation.el ends here