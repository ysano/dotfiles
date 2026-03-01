;;; init-text-modes.el --- Text editing modes
;;; Commentary:
;; Settings for text-focused major modes
;;; Code:

;; --------------------------------
;; Markdown
;; --------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command (cond
                     ((executable-find "cmark-gfm") "cmark-gfm -e table -e autolink -e strikethrough")
                     ((executable-find "pandoc") "pandoc -f gfm -t html")
                     (t #'my-markdown-to-html)))
  (markdown-fontify-code-blocks-natively t)
  :bind (:map markdown-mode-map
         ("C-c m p" . my-markdown-preview-start)
         ("C-c m s" . my-markdown-preview-stop)))

(defun my-markdown-to-html (begin end output-buf)
  "Pure Elisp Markdown to HTML converter for `markdown-command'.
Converts region between BEGIN and END, writing HTML to OUTPUT-BUF."
  (let ((text (buffer-substring-no-properties begin end)))
    (with-current-buffer (get-buffer output-buf)
      (erase-buffer)
      (let ((input-lines (split-string text "\n"))
            (in-code-block nil)
            (code-lang nil)
            (code-lines nil)
            (in-list nil)
            (in-ol nil)
            (in-table nil)
            (table-header-done nil))
        (dolist (line input-lines)
          (cond
           ;; Fenced code block open/close
           ((string-match "^```\\(.*\\)$" line)
            (if in-code-block
                (progn
                  (insert "<code"
                          (if code-lang (format " class=\"%s\"" code-lang) "")
                          ">")
                  (dolist (cl (nreverse code-lines))
                    (insert (my-markdown--escape-html cl) "\n"))
                  (insert "</code></pre>\n")
                  (setq in-code-block nil code-lines nil code-lang nil))
              (setq in-code-block t
                    code-lang (let ((lang (match-string 1 line)))
                                (if (string-empty-p lang) nil lang))
                    code-lines nil)
              (insert "<pre>")))
           ;; Inside code block
           (in-code-block
            (push line code-lines))
           ;; Blank line
           ((string-match "^[ \t]*$" line)
            (when in-list
              (insert "</ul>\n")
              (setq in-list nil))
            (when in-ol
              (insert "</ol>\n")
              (setq in-ol nil))
            (insert "\n"))
           ;; Headings
           ((string-match "^\\(#\\{1,6\\}\\) \\(.*\\)$" line)
            (let ((level (length (match-string 1 line)))
                  (title (match-string 2 line)))
              (insert (format "<h%d>%s</h%d>\n" level (my-markdown--inline title) level))))
           ;; Table separator (skip)
           ((string-match "^[ \t]*|[-:|]" line)
            (when (not in-table)
              nil)
            ;; Mark header row if we just started
            (when (and in-table (not table-header-done))
              (setq table-header-done t)))
           ;; Table row
           ((string-match "^[ \t]*|\\(.*\\)|[ \t]*$" line)
            (let* ((content (match-string 1 line))
                   (cells (mapcar #'string-trim (split-string content "|")))
                   (tag (if (and in-table table-header-done) "td" "th")))
              (unless in-table
                (insert "<table>\n")
                (setq in-table t table-header-done nil))
              (insert "<tr>")
              (dolist (cell cells)
                (insert (format "<%s>%s</%s>" tag (my-markdown--inline cell) tag)))
              (insert "</tr>\n")))
           ;; Ordered list
           ((string-match "^[ \t]*\\([0-9]+\\)[.)] \\(.*\\)$" line)
            (when in-table
              (insert "</table>\n")
              (setq in-table nil table-header-done nil))
            (unless in-ol
              (insert "<ol>\n")
              (setq in-ol t))
            (insert "<li>" (my-markdown--inline (match-string 2 line)) "</li>\n"))
           ;; Unordered list
           ((string-match "^[ \t]*[-*+] \\(.*\\)$" line)
            (when in-table
              (insert "</table>\n")
              (setq in-table nil table-header-done nil))
            (unless in-list
              (insert "<ul>\n")
              (setq in-list t))
            (insert "<li>" (my-markdown--inline (match-string 1 line)) "</li>\n"))
           ;; Blockquote
           ((string-match "^> ?\\(.*\\)$" line)
            (insert "<blockquote><p>" (my-markdown--inline (match-string 1 line)) "</p></blockquote>\n"))
           ;; Horizontal rule
           ((string-match "^\\(---\\|\\*\\*\\*\\|___\\)[ \t]*$" line)
            (insert "<hr>\n"))
           ;; Paragraph
           (t
            (when in-table
              (insert "</table>\n")
              (setq in-table nil table-header-done nil))
            (when in-list
              (insert "</ul>\n")
              (setq in-list nil))
            (when in-ol
              (insert "</ol>\n")
              (setq in-ol nil))
            (insert "<p>" (my-markdown--inline line) "</p>\n"))))
        (when in-list (insert "</ul>\n"))
        (when in-ol (insert "</ol>\n"))
        (when in-table (insert "</table>\n"))))))

(defun my-markdown--escape-html (text)
  "Escape HTML special characters in TEXT."
  (setq text (replace-regexp-in-string "&" "&amp;" text))
  (setq text (replace-regexp-in-string "<" "&lt;" text))
  (setq text (replace-regexp-in-string ">" "&gt;" text))
  text)

(defun my-markdown--inline (text)
  "Convert inline Markdown elements in TEXT to HTML."
  (setq text (my-markdown--escape-html text))
  ;; Bold
  (setq text (replace-regexp-in-string "\\*\\*\\(.+?\\)\\*\\*" "<strong>\\1</strong>" text))
  (setq text (replace-regexp-in-string "__\\(.+?\\)__" "<strong>\\1</strong>" text))
  ;; Italic
  (setq text (replace-regexp-in-string "\\*\\(.+?\\)\\*" "<em>\\1</em>" text))
  (setq text (replace-regexp-in-string "_\\(.+?\\)_" "<em>\\1</em>" text))
  ;; Inline code
  (setq text (replace-regexp-in-string "`\\(.+?\\)`" "<code>\\1</code>" text))
  ;; Links
  (setq text (replace-regexp-in-string "\\[\\(.+?\\)\\](\\(.+?\\))" "<a href=\"\\2\">\\1</a>" text))
  ;; Images
  (setq text (replace-regexp-in-string "!\\[\\(.*?\\)\\](\\(.+?\\))" "<img src=\"\\2\" alt=\"\\1\">" text))
  text)

;; --------------------------------
;; Markdown + Mermaid live preview (impatient-mode)
;; --------------------------------
(use-package simple-httpd
  :ensure t
  :defer t)

(use-package impatient-mode
  :ensure t
  :defer t
  :commands (impatient-mode))

(defun my-markdown--decode-html-entities (text)
  "Decode HTML entities in TEXT."
  (setq text (replace-regexp-in-string "&lt;" "<" text nil t))
  (setq text (replace-regexp-in-string "&gt;" ">" text nil t))
  (setq text (replace-regexp-in-string "&quot;" "\"" text nil t))
  (setq text (replace-regexp-in-string "&amp;" "&" text nil t))
  text)

(defun my-markdown--replace-mermaid-blocks (html)
  "Replace mermaid code blocks in HTML with <div class=\"mermaid\"> elements.
Handles both cmark-gfm and pandoc output formats."
  (with-temp-buffer
    (insert html)
    (dolist (pattern
             '("<pre><code class=\"language-mermaid\">\\(\\(?:.\\|\n\\)*?\\)</code></pre>"
               "<pre class=\"mermaid\"><code>\\(\\(?:.\\|\n\\)*?\\)</code></pre>"))
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((encoded (match-string 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (decoded (my-markdown--decode-html-entities encoded)))
          (delete-region beg end)
          (goto-char beg)
          (insert "<div class=\"mermaid\">\n" decoded "</div>"))))
    (buffer-string)))

(defun my-markdown-mermaid-filter (buffer)
  "Convert BUFFER content to HTML with Mermaid.js support for impatient-mode."
  (let ((html-body
         (with-current-buffer buffer
           (if (stringp markdown-command)
               (let ((out-buf (generate-new-buffer " *md-preview*")))
                 (unwind-protect
                     (progn
                       (call-process-region (point-min) (point-max)
                                            shell-file-name nil out-buf nil
                                            shell-command-switch markdown-command)
                       (with-current-buffer out-buf (buffer-string)))
                   (kill-buffer out-buf)))
             (let ((out-buf (generate-new-buffer " *md-preview*")))
               (unwind-protect
                   (progn
                     (funcall markdown-command (point-min) (point-max) out-buf)
                     (with-current-buffer out-buf (buffer-string)))
                 (kill-buffer out-buf)))))))
    ;; Mermaid blocks: decode entities in Elisp, no JS DOM manipulation needed
    (setq html-body (my-markdown--replace-mermaid-blocks html-body))
    (insert
     "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<style>\n"
     "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;\n"
     "       max-width: 800px; margin: 0 auto; padding: 2em; line-height: 1.6; color: #24292e; }\n"
     "h1, h2, h3, h4, h5, h6 { margin-top: 1.5em; margin-bottom: 0.5em; }\n"
     "code { background: #f6f8fa; padding: 0.2em 0.4em; border-radius: 3px; font-size: 85%; }\n"
     "pre { background: #f6f8fa; padding: 1em; border-radius: 6px; overflow-x: auto; }\n"
     "pre code { background: none; padding: 0; }\n"
     "table { border-collapse: collapse; width: 100%; margin: 1em 0; }\n"
     "th, td { border: 1px solid #dfe2e5; padding: 6px 13px; }\n"
     "th { background: #f6f8fa; font-weight: 600; }\n"
     "blockquote { border-left: 4px solid #dfe2e5; margin: 0; padding: 0 1em; color: #6a737d; }\n"
     "img { max-width: 100%; }\n"
     ".mermaid { text-align: center; }\n"
     "</style>\n</head>\n<body>\n"
     html-body
     "\n<script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>\n"
     "<script>mermaid.initialize({ startOnLoad: true, theme: 'default' });</script>\n"
     "</body>\n</html>")))

(defun my-markdown-preview-start ()
  "Start live Markdown+Mermaid preview in browser via impatient-mode."
  (interactive)
  (httpd-start)
  (impatient-mode 1)
  (imp-set-user-filter #'my-markdown-mermaid-filter)
  (browse-url (format "http://localhost:%d/imp/live/%s/"
                      httpd-port (url-hexify-string (buffer-name)))))

(defun my-markdown-preview-stop ()
  "Stop live Markdown preview."
  (interactive)
  (impatient-mode -1)
  (httpd-stop))


;; --------------------------------
;; EWW web browser
;; --------------------------------
(use-package eww
  :defer t
  :custom
  (eww-search-prefix "http://www.google.com/?k1=-1&q=")
  :config
  ;; Color handling
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    "Disable colorization of region by advising ORIG function with START, END, FG, BG and _ args."
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  
  (defun eww-disable-color ()
    "Turn off color in eww."
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  
  (defun eww-enable-color ()
    "Turn on color in eww."
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))
  
  ;; Load eww-hatebu if available
  (when (require 'eww-hatebu nil t)
    (with-eval-after-load 'eww
      (eww-hatebu-setup))))

;; --------------------------------
;; Octave/Matlab mode
;; --------------------------------
(use-package octave-mode
  :mode ("\\.m\\'" . octave-mode)
  :interpreter "octave")

(provide 'init-text-modes)
;;; init-text-modes.el ends here