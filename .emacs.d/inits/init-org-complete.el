;;; init-org-complete.el --- Complete Org-mode configuration
;;; Commentary:
;; Comprehensive Org-mode setup combining GTD workflow and Zettelkasten
;; Consolidated from multiple org configuration files for better organization
;;; Code:

;; --------------------------------
;; Core Org-mode Package Setup
;; --------------------------------
(use-package org
  :ensure t
  :pin gnu
  :defer t
  :commands (org-mode org-capture org-agenda org-store-link org-iswitchb)
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; ディレクトリ設定
  (org-directory "~/org")
  (org-mobile-directory "~/MobileOrg")
  (org-mobile-inbox-for-pull "~/org/flagged.org")
  
  ;; 基本表示設定
  (org-startup-truncated nil)              ;; 折り返しを有効に
  (org-startup-indented t)                 ;; インデント表示
  (org-startup-folded 'content)            ;; 起動時の折りたたみ
  (org-hide-leading-stars t)               ;; 余分な*を隠す
  (org-adapt-indentation t)                ;; 自動インデント
  
  ;; ロケール設定 - 英語の日付表示
  (system-time-locale "C")
  
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
  (org-confirm-babel-evaluate nil)         ;; 実行確認を無効化
  
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
  
  ;; グローバルタグ - GTDエネルギー・時間コンテキスト
  (org-tag-alist '((:startgroup . nil)
                   ("EnergyAndTime" . ?+)
                   (:grouptags . nil)
                   ("@FullFocus" . ?f)      ;; 創造的タスク: 執筆、設計、コーディング
                   ("@ShortDashes" . ?s)    ;; 短時間集中タスク: 10-15分
                   ("@HangingAround" . ?h)  ;; 読書、調査、視聴、計画
                   ("@BrainDead" . ?b)      ;; 単調なタスク: 掃除、更新、バックアップ
                   ("@Thinking" . ?t)       ;; インスピレーション: 場所変更、マインドマッピング
                   ("@Call" . ?c)           ;; 短時間タスク
                   (:endgroup . nil)))
  
  ;; ハイパーリンク設定
  (org-link-abbrev-alist
   '(("google" . "http://www.google.com/search?q=%s")
     ("gmap" . "http://maps.google.com/maps?q=%s")))
  
  :config
  ;; 組み込みorgを削除（新しいバージョンを使用）
  (assq-delete-all 'org package--builtins)
  (assq-delete-all 'org package--builtin-versions)
  
  ;; 基本フック設定
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode)
  
  ;; ロケール修正 - 特定のモードで英語の日付表示を強制
  (dolist (hook '(org-mode-hook org-agenda-mode-hook org-capture-mode-hook
                 org-remember-mode-hook org-src-mode-hook))
    (add-hook hook (lambda ()
                     (set (make-local-variable 'system-time-locale) "C"))))
  
  ;; LaTeX数式のインライン表示設定
  (add-hook 'org-mode-hook
            (lambda ()
              (make-local-variable 'ac-ignores)
              (add-to-list 'ac-ignores "|-")   ; テーブル用の無視設定
              (setq org-format-latex-options
                    (plist-put org-format-latex-options :scale 1.4))))
  
  ;; 基本キーバインディング
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("<f9>" . org-capture)))

;; --------------------------------
;; GTD Workflow Configuration
;; --------------------------------
(use-package org
  :after org
  :custom
  ;; アジェンダファイル設定
  (org-agenda-files 
   (append (list (concat org-directory "/gtd/gtd.org"))
           (when (file-directory-p (concat org-directory "/sch"))
             (file-expand-wildcards (concat org-directory "/sch/*.org")))
           (when (file-directory-p (concat org-directory "/project"))
             (file-expand-wildcards (concat org-directory "/project/*.org")))))
  
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
   '("+LEVEL=1+PROJECT/-DONE-CANCEL-SOMEDAY-DEFERRED" 
     ("TODO" "NEXT" "STARTED" "WAIT") () "\\<IGNORE\\>"))
  
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
  (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))
  (setq hl-line-face 'underline)
  
  ;; iCalエクスポート関数
  (defun my-org-export-icalendar ()
    "Export current agenda to iCal format."
    (interactive)
    (require 'org-agenda)
    (require 'ox-icalendar)
    (org-icalendar-export-current-agenda "~/public_ical/org-ical.ics"))
  
  ;; ショートカット関数群
  (defun gtd () "Open GTD file." (interactive) 
         (find-file (concat org-directory "/gtd/gtd.org")))
  (defun someday () "Open someday file." (interactive) 
         (find-file (concat org-directory "/gtd/someday.org")))
  (defun journal () "Open journal file." (interactive) 
         (find-file (concat org-directory "/journal.org")))
  (defun note () "Open note file." (interactive) 
         (find-file (concat org-directory "/note.org")))
  
  :bind (("C-c g" . gtd)
         ("<f5>" . note)
         ("<f6>" . journal)
         ("<f7>" . someday)
         ("<f8>" . gtd)
         :map org-mode-map
         ("C-c 1" . my-org-export-icalendar)))

;; --------------------------------
;; Capture Templates
;; --------------------------------
(use-package org
  :after org
  :custom
  (org-capture-templates
   '(;; GTD関連テンプレート
     ("t" "Tasks (Inbox)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %? %^g\n %x\n %a")
     
     ("q" "Quick task (scheduled today)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %^{Task} %^g\nSCHEDULED: %t\n %i\n  %a"
      :immediate-finish t)
     
     ("i" "Interrupting task (clock-in)" entry
      (file+headline "~/org/gtd/gtd.org" "Inbox")
      "* TODO %^{Task} :@ShortDashes:"
      :clock-in :clock-resume)
     
     ("s" "Sub-task (current clock)" entry
      (clock)
      "* %? %^g\n %x\n %a"
      :clock-in :clock-resume)
     
     ;; ジャーナル関連
     ("j" "Journal entry" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?\n     %i\n     %a")
     
     ("M" "Journal with clipboard" entry
      (file+olp+datetree "~/org/journal.org")
      "* %? %^g\n %x\n"))))

;; --------------------------------
;; Agenda Custom Commands
;; --------------------------------
(use-package org
  :after org
  :custom
  (org-agenda-custom-commands
   '(;; GTD High Focus
     ("V" "3-5 year Vision" ((tags "+LEVEL=2+VISION")))
     ("G" "1-2 year Goals" ((tags "+LEVEL=2+GOAL")))
     ("A" "Areas of Focus" ((tags "+LEVEL=2+AOF")))
     ("P" "Projects" ((tags "+LEVEL=2+PROJECT")))
     
     ;; Daily and Weekly Views
     ("d" "Daily Action List"
      ((agenda "" ((org-agenda-ndays 1)
                   (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)))
                   (org-deadline-warning-days 0)))))
     
     ("R" "Weekly Review"
      ((agenda "" ((org-agenda-ndays 7)))
       (stuck "")  ; 行き詰まりプロジェクト
       (tags-todo "+LEVEL=2+PROJECT" ((org-agenda-sorting-strategy '(priority-up effort-down))))
       (todo "NEXT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "WAIT" ((org-agenda-sorting-strategy '(category-up priority-up))))
       (todo "TODO" ((org-agenda-sorting-strategy '(category-up priority-up))))))
     
     ;; Context-based views
     ("g" . "GTD Contexts")
     ("gf" "@FullFocus" tags-todo "@FullFocus")
     ("gs" "@ShortDashes" tags-todo "@ShortDashes")
     ("gh" "@HangingAround" tags-todo "@HangingAround")
     ("gb" "@BrainDead" tags-todo "@BrainDead")
     ("gt" "@Thinking" tags-todo "@Thinking")
     ("gc" "@Call" tags-todo "@Call")
     
     ;; Priority views
     ("p" . "Priorities")
     ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
     ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
     ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
     
     ;; Utility views
     ("U" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"")
     ("w" "Weekly schedule" agenda ""
      ((org-agenda-ndays 7)
       (org-agenda-repeating-timestamp-show-all t)
       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))

;; --------------------------------
;; Org Extensions
;; --------------------------------
(use-package org-contrib
  :ensure t
  :pin nongnu
  :after org
  :defer t)

;; GitHub-flavored Markdown export
(use-package ox-gfm
  :ensure t
  :after org
  :defer t)

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
  :defer t
  :config
  (org-super-agenda-mode))

;; --------------------------------
;; Org-roam Zettelkasten Configuration
;; --------------------------------
(use-package org-roam
  :ensure t
  :after org
  :defer t
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert 
             org-roam-capture org-roam-graph org-roam-tag-add org-roam-alias-add
             org-roam-dailies-capture-today org-roam-db-sync)
  :custom
  ;; Zettelkastenのメインディレクトリ
  (org-roam-directory (file-truename "~/zettelkasten"))
  (org-roam-completion-everywhere t)
  
  ;; ノードテンプレート - Zettelkastenの原則に基づく
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
  
  ;; ID形式設定
  (org-id-method 'ts)
  (org-id-link-to-org-use-id t)
  
  ;; バッファ表示設定
  (org-roam-buffer-display-dedicated t)
  
  ;; 日誌設定
  (org-roam-dailies-directory "journals/")
  (org-roam-dailies-capture-templates
   '(("j" "日誌" entry
      "* %<%H:%M> 日次記録\n\n%?\n\n* アイデア\n\n* タスク\n\n* 解決した問題\n\n* 疑問点\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" 
                        "#+title: 日誌 %<%Y-%m-%d>\n#+filetags: :journal:"))))
  
  :config
  ;; ディレクトリが存在しない場合は作成
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; org-roamの初期化
  (org-roam-db-autosync-mode)
  
  ;; ノード表示のカスタマイズ
  (setq org-roam-node-display-template 
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  
  ;; バッファ表示設定
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t)))))
  
  ;; タグ検索のカスタム関数
  (defun my/org-roam-find-by-tag ()
    "タグでorg-roamノードを検索します。"
    (interactive)
    (org-roam-node-find nil nil 
                       (lambda (node) 
                         (member (completing-read "タグ: " 
                                                (org-roam-tag-completions))
                                (org-roam-node-tags node)))))
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n s" . org-roam-db-sync)
         ("C-c n T" . my/org-roam-find-by-tag)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-c n I" . org-roam-node-insert)))

;; ネットワーク可視化のためのorg-roam-ui
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :defer t
  :commands org-roam-ui-mode
  :custom
  (org-roam-ui-sync-theme t)        ; Emacsのテーマと同期
  (org-roam-ui-follow t)            ; カーソルに追従
  (org-roam-ui-update-on-save t)    ; 保存時に更新
  (org-roam-ui-open-on-start nil)   ; 起動時にブラウザを開かない
  :bind ("C-c n u" . org-roam-ui-mode))

;; --------------------------------
;; Performance and Cleanup
;; --------------------------------
;; Auto-save org-roam database
(add-hook 'kill-emacs-hook #'org-roam-db-sync)

;; Clean up old backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(provide 'init-org-complete)
;;; init-org-complete.el ends here