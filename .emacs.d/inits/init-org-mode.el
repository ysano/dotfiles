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
;; Org-roam - knowledge base
;; --------------------------------
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "literature" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :literature:") 
      :unnarrowed t)
     ("p" "project" plain
      "\n* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  ;; Create directory if it doesn't exist
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Initialize org-roam
  (org-roam-db-autosync-mode)
  
  ;; Setup dailies
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(provide 'init-org-mode)
;;; init-org-mode.el ends here