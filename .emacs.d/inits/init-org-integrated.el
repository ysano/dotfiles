;;; init-org-integrated.el --- Integrated Org-mode and Org-roam configuration
;;; Commentary:
;; Combined GTD workflow and Zettelkasten knowledge management
;;; Code:

;; --------------------------------
;; Basic Org-mode Setup
;; --------------------------------
(use-package org
  :ensure t
  :pin gnu
  :custom
  ;; ディレクトリ設定
  (org-directory "~/org")
  (org-mobile-directory "~/MobileOrg")
  (org-mobile-inbox-for-pull "~/org/flagged.org")
  
  ;; LaTeX関連設定
  (org-latex-pdf-process '("latexmk %f"))
  (org-latex-to-mathml-convert-command "latexmlmath \"%i\" --presentationmathml=%o")
  
  ;; Babel言語設定
  (org-babel-load-languages '((emacs-lisp . t)
                              (gnuplot . t)
                              (perl . t)
                              (python . t)
                              (R . t)
                              (js . t)
                              (plantuml . t)
                              (shell . t)))
  
  ;; 基本表示設定
  (org-startup-truncated nil)  ;; 折り返しを有効に
  
  ;; ロケール設定 - 英語の日付表示
  (system-time-locale "C")
  
  ;; TODOキーワード設定
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "APPT(a)" "|" "DONE(d)" "CANCELED(c)")))
  
  ;; TODOキーワードの色設定
  (org-todo-keyword-faces
   '(("TODO" . org-todo)
     ("NEXT" . "yellow")
     ("WAIT" . "brown")
     ("APPT" . "red")
     ("CANCELED" . (:foreground "blue" :weight bold))))
  
  ;; 時間表示フォーマット
  (org-duration-format '(h:mm))
  
  ;; Effort見積もり
  (org-global-properties '(("Effort_ALL" . "0:30 1:00 2:00 3:00 4:00 5:00 6:00")))
  
  ;; グローバルタグ
  (org-tag-alist '((:startgroup . nil)
                   ("EnergyAndTime" . ?+)
                   (:grouptags . nil)
                   ("@FullFocus" . ?f)    ;; 創造的タスク: 執筆、設計、コーディング
                   ("@ShortDashes" . ?s)  ;; 短時間集中タスク: 10-15分
                   ("@HangingAround" . ?h) ;; 読書、調査、視聴、計画
                   ("@BrainDead" . ?b)    ;; 単調なタスク: 掃除、更新、バックアップ
                   ("@Thinking" . ?t)     ;; インスピレーション: 場所変更、マインドマッピング
                   ("@Call" . ?c)         ;; 短時間タスク
                   (:endgroup . nil)
                   ))
  
  ;; ハイパーリンク設定
  (org-link-abbrev-alist
   '(("google" . "http://www.google.com/search?q=%s")
     ("gmap" . "http://maps.google.com/maps?q=%s")))
  
  :config
  ;; 組み込みorgを削除
  (assq-delete-all 'org package--builtins)
  (assq-delete-all 'org package--builtin-versions)
  
  ;; org-modeでの強調表示を有効に
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  
  ;; org-indent-mode
  (add-hook 'org-mode-hook 'org-indent-mode)
  
  ;; ロケール修正 - 特定のモードで英語の日付表示を強制
  (mapcar (lambda (hook) 
            (add-hook hook
                      (lambda ()
                        (set (make-local-variable 'system-time-locale) "C"))))
          '(org-mode-hook
            org-agenda-mode-hook
            org-capture-mode-hook
            org-remember-mode-hook
            org-src-mode-hook))
  
  ;; inline latex format
  (add-hook 'org-mode-hook
            (lambda ()
              (make-local-variable 'ac-ignores)
              (add-to-list 'ac-ignores "|-") ; ignore for table
              (setq org-format-latex-options
                    (plist-put org-format-latex-options :scale 1.4))))
  
  ;; キーバインディング
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (define-key global-map [f9] 'org-capture))

;; --------------------------------
;; GTD Workflow Setup
;; --------------------------------
(use-package org
  :after org
  :custom
  ;; アジェンダファイル設定
  (org-agenda-files (cons (concat org-directory "/gtd/gtd.org")
                          (cadr (mapcar (lambda (w)
                                    (file-expand-wildcards
                                     (concat org-directory w)))
                                  '("/sch/*.org" "/project/*.org")))))
  
  ;; アジェンダ表示設定
  (org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
  (org-agenda-overriding-columns-format
   "%10CATEGORY %35ITEM %TODO %3PRIORITY %Time_Estimate(Time){:} %CLOCKSUM(Total) %CLOCKSUM_T(Today) %DEADLINE")
  (org-agenda-sorting-strategy
   '((agenda time-up priority-down tag-up category-keep effort-up)
     (todo todo-state-up priority-down effort-up)
     (tags user-defined-up)
     (search category-keep)))
  (org-agenda-start-day nil)
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  
  ;; 依存関係の設定
  (org-agenda-dim-blocked-tasks t)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  
  ;; 行き詰まりプロジェクト定義
  (org-stuck-projects
   '("+LEVEL=1+PROJECT/-DONE-CANCEL-SOMEDAY-DEFERRED" ("TODO" "NEXT" "STARTED" "WAIT") () "\\<IGNORE\\>"))
  
  ;; リファイル設定
  (org-refile-targets '((nil :maxlevel . 2)
                         ("gtd.org" :maxlevel . 1)
                         ("someday.org" :maxlevel . 1)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  
  ;; iCalエクスポート設定
  (org-icalendar-combined-description "orgmode schedule")
  (org-icalendar-timezone nil)
  (org-icalendar-include-todo t)
  (org-icalendar-use-scheduled '(event-if-todo))
  (org-icalendar-use-deadline '(event-if-todo))
  
  :config
  ;; アジェンダ表示で下線を用いる
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  (setq hl-line-face 'underline)
  
  ;; iCalエクスポート関数
  (defun my-org-export-icalendar ()
    (interactive)
    (require 'org-agenda)
    (require 'ox-icalendar)
    (org-icalendar-export-current-agenda "~/public_ical/org-ical.ics"))
  
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "C-c 1") 'my-org-export-icalendar))))

;; --------------------------------
;; Capture Templates
;; --------------------------------
(use-package org
  :after org
  :custom
  (org-capture-templates
   '(
     ;; GTD関連テンプレート
     ("t" "Tasks (Inbox)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %? %^g\n %x\n %a")
     
     ("q" "Quick task (Inbox scheduled today)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %^{Task} %^g\nSCHEDULED: %t\n %i\n  %a"
      :immediate-finish t)
     
     ("i" "Interrupting task (Inbox clock-in)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %^{Task} :@ShortDashes:"
      :clock-in :clock-resume)
     
     ;; ジャーナル関連テンプレート
     ("j" "Journal (Resume memory)" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?\n     %i\n     %a")
     )))

;; --------------------------------
;; Agenda Custom Commands
;; --------------------------------
(use-package org
  :after org
  :custom
  (org-agenda-custom-commands
   '(
     ;; GTD高レベルビュー
     ("V" "3-5 year Vision"
      ((tags "+LEVEL=2+VISION")))
     ("G" "1-2 year Goals and objective"
      ((tags "+LEVEL=2+GOAL")))
     ("A" "Area of Focus and accountability"
      ((tags "+LEVEL=2+AOF")))
     ("P" "Projects"
      ((tags "+LEVEL=2+PROJECT")))
     
     ;; 日次アクションリスト
     ("d" "Daily Action List (Next Action)"
      ((agenda "" ((org-agenda-ndays 1)
                   (org-agenda-sorting-strategy
                    (quote ((agenda time-up priority-down tag-up))))
                   (org-deadline-warning-days 0)))))
     
     ;; 週次レビュー
     ("R" "Weekly Review"
      ((agenda "" ((org-agenda-ndays 7)))
       (stuck "")
       (tags-todo "+LEVEL=2+PROJECT" ((org-agenda-sorting-strategy '(priority-up effort-down))))
       (todo "NEXT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "WAIT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "APPT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "TODO" ((org-agenda-sorting-strategy '(category-up priority-up))))))
     
     ;; ワークハブ
     ("h" "WorkHub"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-start-with-log-mode t)
                   (org-agenda-clockreport-mode t)
                   (org-agenda-start-with-follow-mode t)))
       (tags "+LEVEL=2+PROJECT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "WAIT" ((org-agenda-sorting-strategy '(ts-up category-up priority-up))))
       (todo "NEXT" ((org-agenda-sorting-strategy '(ts-up category-up priority-up))))
       (todo "TODO" ((org-agenda-sorting-strategy '(ts-up category-up priority-up))))
       (todo "APPT" ((org-agenda-sorting-strategy '(ts-up category-up priority-up))))))
     
     ;; 未スケジュールTODO
     ("U" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"" nil)
     
     ;; GTDコンテキスト
     ("g" . "GTD Energy and Time contexts")
     ("gf" "@FullFocus" tags-todo "@FullFocus")
     ("gs" "@ShortDashes" tags-todo "@ShortDashes")
     ("gh" "@HangingAround" tags-todo "@HangingAround")
     ("gb" "@BrainDead" tags-todo "@BrainDead")
     ("gt" "@Thinking" tags-todo "@Thinking")
     ("gc" "@Call" tags-todo "@Call")
     
     ;; 優先度
     ("p" . "Priorities")
     ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
     ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
     ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
     
     ;; カレンダービュー
     ("w" "Weekly schedule" agenda ""
      ((org-agenda-ndays 7)
       (org-agenda-repeating-timestamp-show-all t)
       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
     
     ;; 締め切りビュー
     ("D" "Upcoming deadlines" agenda ""
      ((org-agenda-entry-types '(:deadline))
       (org-agenda-ndays 1)
       (org-deadline-warning-days 60)
       (org-agenda-time-grid nil)))
     )))

;; --------------------------------
;; Org Extensions
;; --------------------------------
(use-package org-contrib
  :ensure t
  :pin nongnu
  :after org)

(use-package ox-gfm
  :ensure t
  :after org)

(use-package org-re-reveal
  :ensure t
  :after org
  :custom
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-re-reveal-revealjs-version "4")
  (org-re-reveal-theme "solarized"))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("❀" "☯" "♥" "★" "●" "◇" "◆" "►" "•" "▸")))

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
      :unnarrowed t)
     
     ("t" "技術メモ" plain
      "* 概要\n\n%?\n\n* 詳細\n\n* コード例\n\n```\n\n```\n\n* 参考リンク\n\n* 関連ゼッテル\n\n* タグ\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: :tech:")
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

;; --------------------------------
;; ファイルアクセスショートカット
;; --------------------------------
(defun gtd ()
  "Open GTD file."
  (interactive)
  (find-file (concat org-directory "/gtd/gtd.org")))

(defun someday ()
  "Open Someday/Maybe file."
  (interactive)
  (find-file (concat org-directory "/gtd/someday.org")))

(defun journal ()
  "Open Journal file."
  (interactive)
  (find-file (concat org-directory "/journal.org")))

(defun upper ()
  "Open Upper level goals file."
  (interactive)
  (find-file (concat org-directory "/gtd/upper.org")))

;; 知識ベース関連のショートカットをorg-roamに置き換え
(defun note ()
  "Find notes in org-roam."
  (interactive)
  (org-roam-node-find nil nil (lambda (node) 
                               (member "zettel" (org-roam-node-tags node)))))

(defun tech ()
  "Find technical notes in org-roam."
  (interactive)
  (org-roam-node-find nil nil (lambda (node) 
                               (member "tech" (org-roam-node-tags node)))))

;; ファンクションキーバインディング
(global-set-key (kbd "C-c g") 'gtd)
(define-key global-map [f5] 'note)
(define-key global-map [S-f5] 'tech)
(define-key global-map [f6] 'journal)
(define-key global-map [f7] 'someday)
(define-key global-map [f8] 'gtd)
(define-key global-map [S-f8] 'upper)

(provide 'init-org-integrated)
;;; init-org-integrated.el ends here 