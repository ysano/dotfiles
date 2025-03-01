;;; init-org-mode.el --- Org-mode configuration
;;; Commentary:
;; Org-mode settings and extensions
;;; Code:

;; --------------------------------
;; Basic Org-mode
;; --------------------------------
(use-package org
  :ensure t
  :pin gnu
  :custom
  ;; Make sure we use the version from package, not built-in
  (org-latex-to-mathml-convert-command "latexmlmath \"%i\" --presentationmathml=%o")
  (org-babel-load-languages '((emacs-lisp . t)
                              (gnuplot . t)
                              (perl . t)
                              (python . t)
                              (R . t)
                              (js . t)
                              (plantuml . t)
                              (shell . t)))
  :config
  ;; Remove org from builtin packages
  (assq-delete-all 'org package--builtins)
  (assq-delete-all 'org package--builtin-versions)
  
  ;; Load existing org config if org directory exists
  (if (or (file-directory-p "~/org")
          (file-symlink-p "~/org"))
      (load "init-org")))

;; --------------------------------
;; Org Extensions
;; --------------------------------
(use-package org-contrib
  :ensure t
  :pin nongnu
  :after org)

;; GitHub-flavored Markdown export
(use-package ox-gfm
  :ensure t
  :after org)

;; Reveal.js export for presentations
(use-package org-re-reveal
  :ensure t
  :after org
  :custom
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-re-reveal-revealjs-version "4")
  (org-re-reveal-theme "solarized"))

;; Prettier bullets in headings
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("❀" "☯" "♥" "★" "●" "◇" "◆" "►" "•" "▸")))

;; Super Agenda for better agenda views
(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode))

;; --------------------------------
;; Org-roam - Zettelkasten実装
;; --------------------------------
(use-package org-roam
  :ensure t
  :after org
  :custom
  ;; Zettelkastenのメインディレクトリ
  (org-roam-directory (file-truename "~/zettelkasten"))
  ;; どこでも補完を有効に
  (org-roam-completion-everywhere t)
  
  ;; ノートテンプレート - Zettelkastenの原則に基づく
  (org-roam-capture-templates
   '(("z" "ゼッテル（永続的なノート）" plain
      "* アイデア\n\n%?\n\n* 参考文献\n\n* 関連ゼッテル\n\n* タグ\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: :zettel:")
      :immediate-finish t
      :unnarrowed t)
     
     ("r" "文献ノート" plain
      "* 出典\n\n著者: %^{著者}\nタイトル: ${title}\n発行年: %^{発行年}\n\n* メモ\n\n%?\n\n* 抽出したアイデア\n\n* 参考文献\n\n* 関連ゼッテル\n\n* タグ\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" 
                         "#+title: ${title}\n#+date: %U\n#+filetags: :reference:")
      :immediate-finish t
      :unnarrowed t)
     
     ("f" "一時的なノート" plain
      "* クイックキャプチャ\n\n%?\n\n* 永続的なノートへの変換タスク\n- [ ] このノートを見直して整理する\n- [ ] 重要なアイデアを抽出する\n- [ ] 永続的なノートを作成する\n- [ ] 関連するノートにリンクする\n\n* 出典\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :fleeting:")
      :immediate-finish t
      :unnarrowed t)))
  
  ;; ID形式 - タイムスタンプをIDとして使用（一意性と順序を確保）
  (org-id-method 'ts)
  (org-id-link-to-org-use-id t)
  
  ;; バックリンクと未リンク参照をバッファに表示
  (org-roam-buffer-display-dedicated t)
  
  ;; クイックアクセスのためのキーバインディング
  :bind (("C-c n l" . org-roam-buffer-toggle)     ; リンクバッファの表示/非表示
         ("C-c n f" . org-roam-node-find)         ; ノードを検索
         ("C-c n i" . org-roam-node-insert)       ; ノードへのリンクを挿入
         ("C-c n c" . org-roam-capture)           ; 新規ノートの作成
         ("C-c n g" . org-roam-graph)             ; グラフ表示
         ("C-c n t" . org-roam-tag-add)           ; タグの追加
         ("C-c n a" . org-roam-alias-add)         ; エイリアスの追加
         ("C-c n j" . org-roam-dailies-capture-today) ; 今日の日誌を作成
         ("C-c n s" . org-roam-db-sync)           ; DBを手動で同期
         :map org-mode-map
         ("C-M-i" . completion-at-point)          ; 補完
         ("C-c n I" . org-roam-node-insert))      ; org-modeでのノード挿入
  :config
  ;; ディレクトリが存在しない場合は作成
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; org-roamの初期化
  (org-roam-db-autosync-mode)
  
  ;; 補完UIでのノード表示をカスタマイズ（タグを表示）
  (setq org-roam-node-display-template 
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  
  ;; org-roamバッファの表示設定
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)                   ; 右側に表示
                 (slot . 0)
                 (window-width . 0.33)            ; 幅は画面の1/3
                 (window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t)))))
  
  ;; 日誌の設定
  (setq org-roam-dailies-directory "journals/")  ; 日誌用のサブディレクトリ
  (setq org-roam-dailies-capture-templates
        '(("j" "日誌" entry
           "* %<%H:%M> 日次記録\n\n%?\n\n* アイデア\n\n* タスク\n\n* 解決した問題\n\n* 疑問点\n\n"
           :if-new (file+head "%<%Y-%m-%d>.org" 
                             "#+title: 日誌 %<%Y-%m-%d>\n#+filetags: :journal:"))))
  
  ;; タグでノートを検索するカスタム関数
  (defun my/org-roam-find-by-tag ()
    "タグでorg-roamノードを検索します。"
    (interactive)
    (org-roam-node-find nil nil 
                       (lambda (node) 
                         (member (completing-read "タグ: " 
                                                (org-roam-tag-completions))
                                (org-roam-node-tags node)))))
  
  ;; カスタム関数のバインド
  (global-set-key (kbd "C-c n T") #'my/org-roam-find-by-tag))

;; ネットワーク可視化のためのorg-roam-ui追加
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)        ; Emacsのテーマと同期
  (org-roam-ui-follow t)            ; カーソルに追従
  (org-roam-ui-update-on-save t)    ; 保存時に更新
  (org-roam-ui-open-on-start t)     ; 起動時にブラウザを開く
  :config
  (define-key global-map (kbd "C-c n u") #'org-roam-ui-mode))

(provide 'init-org-mode)
;;; init-org-mode.el ends here