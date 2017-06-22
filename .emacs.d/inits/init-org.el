;;-----------------------------------------------------------------
;; org
;;-----------------------------------------------------------------
(require 'org-loaddefs)
(setq org-directory "~/org")                    ; org directory
(setq org-mobile-directory "~/MobileOrg")       ; MobileOrg directory
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

;; (setq org-default-note-files (concat org-directory "/note.org"))
;; (setq org-agenda-files (concat org-directory "/main.org"))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; 折り返しを有効に
(setq org-startup-truncated nil)

;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)

;; latexmk
(setq org-latex-pdf-process '("latexmk %f"))

;; inline latex format
(add-hook 'org-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (add-to-list 'ac-ignores "|-") ; ignore for table
            (setq org-format-latex-options
                  (plist-put org-format-latex-options :scale 1.4))))

;; cdlatex
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; ac-math
(add-to-list 'ac-modes 'org-mode)
(add-hook 'org-mode-hook 'ac-latex-mode-setup)

;; ロケール修正
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))
(add-hook 'org-capture-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))
(add-hook 'org-remember-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))
(add-hook 'org-src-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))

;;------------------------------------------------------------
;; Hyperlinks Setup
;;------------------------------------------------------------
(setq org-link-abbrev-alist
      '(
        ("google"    . "http://www.google.com/search?q=%s")
        ("gmap"      . "http://maps.google.com/maps?q=%s")
        ))

;;------------------------------------------------------------
;; Capture Setup
;;------------------------------------------------------------
(setq org-capture-templates
      '(
        ("i" "Inbox (GTD)" entry
          (file+headline (concat org-directory "/gtd.org") "Inbox")
           "* TODO %?\n  %i\n  %a")
        ("j" "Journal (Resume memory)" entry
          (file+datetree (concat org-directory "/journal.org"))
           "* %?\n     %i\n     %a")
        )
        )

;;------------------------------------------------------------
;; Agenda Setup
;;------------------------------------------------------------

(setq org-agenda-files (cons (concat org-directory "/gtd.org")
                             (car (mapcar (lambda (w)
                                       (file-expand-wildcards
                                        (concat org-directory w)))
                                     '("/sch/*.org" "/project/sch/*.org")))))
;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)
;; アジェンダカラム
;; (setq org-agenda-overriding-columns-format
;;   "%25ITEM %TODO %3PRIORITY %TAGS %DEADLINE")
;; 標準の祝日を利用しない
;(setq calendar-holidays nil)

;; 積み残しプロジェクトタスクの定義
(setq org-stuck-projects
        '("+LEVEL=2+project/-DONE-CANCEL-SOMEDAY-DEFERRED" ("TODO" "WAIT") () "\\<IGNORE\\>"))
;; (setq org-stuck-projects "+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
;(setq org-stuck-projects '("+LEVEL=2/-DONE-CANCEL" ("TODO" "WAITING") ("goal") ""))
;; (setq org-stuck-projects
;;   ;;Select project cond of TAGS/TODO/PROPERTY matches
;;   '("+LEVEL=2+project/-DONE-CANCEL/-SCHEDULED"
;; ;; If the subtree contains any TODO keywords, non stuck.
;; ("")
;; ;; If the subtree contains any TAGS keywords, non stuck.
;; nil
;; ;; If the subtree matches regexp, non stuck.
;; ""))
;; (setq org-stuck-projects
;;   '("+project/-DONE-CANCEL" ("-SCHEDULED") ("project" "goal")
;; "\\<IGNORE\\>"))

;;------------------------------------------------------------
;; Agenda Grobal Setting
;;------------------------------------------------------------
;; Keep tasks with dates off the global todo lists
;(setq org-agenda-todo-ignore-with-date t)
;; Remove completed deadline tasks from the agenda view
;(setq org-agenda-skip-deadline-if-done t)
;; Remove completed scheduled tasks from the agenda view
;(setq org-agenda-skip-scheduled-if-done t)
;; Remove completed items from search results
;(setq org-agenda-skip-timestamp-if-done t)

;;------------------------------------------------------------
;; Agenda Custom Commands
;;------------------------------------------------------------
(setq org-agenda-custom-commands
'(
  ;; ("p" "Projects"
  ;;  ((tags "PROJECT")))
  ;; ("g" "Goals"
  ;;  ((tags "GOAL")))
  ;; ("h" "Office and Home Lists"
  ;;  ((agenda)
  ;;   (tags-todo "@OFFICE")
  ;;   (tags-todo "@HOME")
  ;;   (tags-todo "@WEB")
  ;;   (tags-todo "@DOWNTOWN")
  ;;   (tags-todo "@CALL")
  ;;   ))
  ("d" "Daily Action List (Next Action)"
   (
    (agenda "" ((org-agenda-ndays 1)
                (org-agenda-sorting-strategy
                  (quote ((agenda time-up priority-down tag-up) )))
                (org-deadline-warning-days 0)
                ))
    ))

  ;;------------------------------------------------------------
  ;; GTD weekly review
  ;;------------------------------------------------------------
  ("R" "Weekly Review"
   ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
    ;; type "l" in the agenda to review logged items 
    ;; review stuck projects as designated by org-stuck-projects
    (stuck ""); 行き詰まり＝TODOが無いProject
    ;; review all projects (assuming you use todo keywords to designate projects)
    (tags "+LEVEL=2+project")
    ;; review undone items
    (todo "TODO")
    ;; review waiting items
    (todo "WAIT")
    ;; review someday/maybe items
    (tags-todo "reading" ((org-agenda-files '("~/org/someday.org"))
                            (org-agenda-sorting-strategy '(priority-up effort-down))
                              ))
    ;; review someday/maybe items
    (tags-todo "-reading" ((org-agenda-files '("~/org/someday.org"))
                             (org-agenda-sorting-strategy '(priority-up effort-down))
                               ))
    ))

  ;;------------------------------------------------------------
  ;; 浮いてるTODO
  ;;------------------------------------------------------------
  ("U" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"" nil)
  
  ;;------------------------------------------------------------
  ;; Queries for local files
  ;;------------------------------------------------------------
  ;; ("Q" . "Custom queries") ;; gives label to "Q"
  ;; ("Qa" "Archive search" search ""
  ;;  ((org-agenda-files (file-expand-wildcards "~/org/*.org_archive"))))
  ;; ("Qb" "Projects and Archive" search ""
  ;;  ((org-agenda-text-search-extra-files (file-expand-wildcards "~/archive/*.org"))))
  ;; ;; searches both projects and archive directories
  ;; ("QA" "Archive tags search" org-tags-view "" 
  ;;  ((org-agenda-files (file-expand-wildcards "~/org/*.org_archive"))))
  ;; ("x" "With deadline columns" alltodo ""
  ;;  ((org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
  ;; (org-agenda-view-columns-initially t)))

  ;;------------------------------------------------------------
  ;; Speeding up custom agenda commands
  ;;------------------------------------------------------------
  ;; ("c" "Weekly Calendar" agenda ""
  ;;  ((org-agenda-ndays 7)                          ;; [1] a weekly calendar
  ;; (org-agenda-start-on-weekday 0)               ;; [2] starts on Sunday
  ;; (org-agenda-time-grid nil)                    
  ;; (org-agenda-repeating-timestamp-show-all t)   ;; [3] includes all instances of repeating
  ;; (org-agenda-entry-types '(:timestamp :sexp))))  ;; [4] only searchs for timestamp and diary sexps
  ;; ("D" "Upcoming deadlines" agenda "" 
  ;;  ((org-agenda-time-grid nil)
  ;; (org-deadline-warning-days 365)        ;; [1] fall due within the upcoming year
  ;; (org-agenda-entry-types '(:deadline))  ;; [2] looking for deadlines and nothing else
  ;; ))

  ;;------------------------------------------------------------
  ;; GTD contexts
  ;;------------------------------------------------------------
  ("g" . "GTD contexts")
  ("go" "@office" tags-todo "@office")
  ("gh" "@home" tags-todo "@home")
  ("ge" "@errands" tags-todo "@errands")
  ("gc" "@call" tags-todo "@call")
  ;; ("G" "GTD Agenda of Contexts"
  ;;  ((tags-todo "@office")
  ;; (tags-todo "@home")
  ;; (tags-todo "@errands")
  ;; (tags-todo "@call")
  ;; )
  ;;  nil                      ;; i.e., no local settings
  ;;  ("~/next-actions.html")) ;; exports block to this file with C-c a e

  ;;------------------------------------------------------------
  ;; Priorities
  ;;------------------------------------------------------------
  ("p" . "Priorities")
  ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
  ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
  ("pc" "C items" tags-todo "+PRIORITY=\"C\"")

  ;;------------------------------------------------------------
  ;; Calendar style view
  ;;------------------------------------------------------------
  ("w" "Weekly schedule" agenda ""
   ((org-agenda-ndays 7)          ;agenda will start in week view
    (org-agenda-repeating-timestamp-show-all t)   ;ensures that repeating events appear on all relevant dates
    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
  ;; limits agenda view to timestamped items

  ;;------------------------------------------------------------
  ;; Calendar style view (deadlines)
  ;;------------------------------------------------------------
  ("D" "Upcoming deadlines" agenda ""
   ((org-agenda-entry-types '(:deadline))
    ;; a slower way to do the same thing
    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
    (org-agenda-ndays 1)
    (org-deadline-warning-days 60)
    (org-agenda-time-grid nil)))

  ))

;;------------------------------------------------------------
;; Refile Setup
;;------------------------------------------------------------
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote (
                                 ;; (nil :maxlevel . 1)
                                 ("gtd.org" :maxlevel . 1)
                                 ("someday.org" :maxlevel . 1)
                                 )))
;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)


;;------------------------------------------------------------
;; Export Setup
;;------------------------------------------------------------
;; iCal の説明文
(setq org-icalendar-combined-description "orgmode schedule")
;; カレンダーに適切なタイムゾーンを設定する（google 用には nil が必要）
(setq org-icalendar-timezone nil)
;; DONE になった TODO は出力対象から除外する
(setq org-icalendar-include-todo t)
;; （通常は，<>--<> で区間付き予定をつくる．非改行入力で日付がNoteに入らない）
(setq org-icalendar-use-scheduled '(event-if-todo))
;; DL 付きで終日予定にする：締め切り日（スタンプで時間を指定しないこと）
(setq org-icalendar-use-deadline '(event-if-todo))

(defun my-org-export-icalendar ()
  (interactive)
            (require 'org-agenda)
            (require 'ox-icalendar)
  (org-icalendar-export-current-agenda "~/public_ical/org-ical.ics"))
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c 1") 'my-org-export-icalendar)))

;;------------------------------------------------------------
;; Shortcut
;;------------------------------------------------------------
(defun note ()
    (interactive)
    (find-file (concat org-directory "/note.org")))
(defun journal ()
    (interactive)
    (find-file (concat org-directory "/journal.org")))
(defun someday ()
    (interactive)
    (find-file (concat org-directory "/someday.org")))
(defun gtd ()
    (interactive)
    (find-file (concat org-directory "/gtd.org")))
(global-set-key (kbd "C-c g") 'gtd)
(define-key global-map [f5] 'note)
(define-key global-map [f6] 'journal)
(define-key global-map [f7] 'someday)
(define-key global-map [f8] 'gtd)
(define-key global-map [f9] 'org-capture)
;; (define-key global-map [f9] 'remember-region)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
