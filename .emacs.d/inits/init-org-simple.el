;;; init-org-simple.el --- Simplified Org-mode configuration
;;; Commentary:
;; Simple Org-mode setup using use-package patterns
;;; Code:

;; --------------------------------
;; Core Org-mode Package Setup
;; --------------------------------
(use-package org
  :ensure t
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; Basic settings
  (org-directory "~/org")
  (org-startup-truncated nil)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-leading-stars t)
  (org-adapt-indentation t)
  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))
  ;; Basic agenda
  (org-agenda-files '("~/org/inbox.org" "~/org/projects.org" "~/org/someday.org"))
  ;; Time format
  (org-duration-format '(h:mm))
  ;; Tags
  (org-tag-alist '(("@home" . ?h)
                   ("@work" . ?w)
                   ("@call" . ?c)
                   ("@errand" . ?e)))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; Basic hooks
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook 'org-indent-mode))

;; --------------------------------
;; Org Capture Templates
;; --------------------------------
(use-package org-capture
  :after org
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file "~/org/inbox.org")
      "* TODO %?\n  %i\n  %a")
     ("n" "Note" entry (file "~/org/notes.org")
      "* %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %?\nEntered on %U\n  %i\n  %a"))))

;; --------------------------------
;; Org Agenda
;; --------------------------------
(use-package org-agenda
  :after org
  :custom
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  :bind (("C-c a" . org-agenda)))

;; --------------------------------
;; Org Babel
;; --------------------------------
(use-package ob-core
  :after org
  :custom
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (org-confirm-babel-evaluate nil))

;; --------------------------------
;; Org Export
;; --------------------------------
(use-package ox
  :after org
  :custom
  (org-export-with-sub-superscripts nil)
  (org-export-with-toc t))

;; --------------------------------
;; Optional: Org-roam for note-taking
;; --------------------------------
(use-package org-roam
  :ensure t
  :if (and (>= emacs-major-version 26) (executable-find "sqlite3"))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-setup))

(provide 'init-org-simple)
;;; init-org-simple.el ends here