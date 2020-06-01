;;; eww-hatebu.el --- Hatena bookmarks in mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/
;; Version: 0.01
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'eww)
(require 'url)

(defgroup eww-hatebu nil
  "Hatena bookmarks in mode-line."
  :group 'eww)

(defface eww-hatebu-bookmarks
  '((t (:foreground "blue" :weight bold)))
  "Bookmark counts in mode line."
  :group 'eww-hatebu)

(defconst eww-hatebu--endpoint
  "http://api.b.st-hatena.com/entry.count")
(defvar eww-hatebu--count nil)

(defvar eww-hatebu-mode-line
  '(:propertize
    (:eval (when eww-hatebu--count
             (concat "[HB:" eww-hatebu--count "]")))
    face eww-hatebu-bookmarks))

(defun eww-hatebu--callback (_status)
  (goto-char (point-min))
  (when (re-search-forward "\r?\n\r?\n" nil t)
    (let ((bookmarks (buffer-substring-no-properties (point) (point-max))))
      (setq eww-hatebu--count bookmarks))
    (force-mode-line-update)))

(defun eww-hatebu--get-bookmarks ()
  (let ((url-request-method "GET")
        (query-param (concat "?url=" (url-hexify-string (eww-current-url))))
        (url-show-status nil))
    (url-retrieve (concat eww-hatebu--endpoint query-param) 'eww-hatebu--callback)))

(defun eww-hatebu--setup-mode-line ()
  (setq mode-line-format (cons eww-hatebu-mode-line mode-line-format)))

;;;###autoload
(defun eww-hatebu-setup ()
  (interactive)
  (add-hook 'eww-mode-hook 'eww-hatebu--setup-mode-line)
  (add-hook 'eww-after-render-hook 'eww-hatebu--get-bookmarks))

(provide 'eww-hatebu)

;;; eww-hatebu.el ends here
