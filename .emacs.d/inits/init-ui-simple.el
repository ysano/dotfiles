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
  ;; Font settings
  (use-default-font-for-symbols nil)
  ;; Line spacing for better readability (in pixels)
  (line-spacing 3)
  :config
  ;; Apply settings
  (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

;; --------------------------------
;; Unified Font Management (fontset-based)
;; --------------------------------
(defvar my-ui-scale-factor 1.0
  "Global UI scale factor for all font sizes and UI elements.")

(defvar my-base-font-size 16
  "Base font size in points (before scaling).")

(defun my-scaled-font-size ()
  "Calculate scaled font size in Emacs height units (1/10 point)."
  (round (* my-base-font-size my-ui-scale-factor 10)))

(defun my-scaled-modeline-height ()
  "Calculate scaled modeline height."
  (round (* 30 my-ui-scale-factor)))

(defun my-scaled-modeline-bar-width ()
  "Calculate scaled modeline bar width."
  (max 3 (round (* 4 my-ui-scale-factor))))

;; ---- Data: フォント候補 ----
;; integrated = 英数+日本語一体型 (2:1 guaranteed)
;; ascii-only = 英数のみ → Layer 2 で日本語フォントを fontset 割当
(defvar my-font-candidates
  '((:name "Cica"             :type integrated)
    (:name "Sarasa Term J"    :type integrated)
    (:name "HackGen"          :type integrated)
    (:name "Fira Code"        :type ascii-only)
    (:name "Cascadia Code"    :type ascii-only)
    (:name "Consolas"         :type ascii-only)
    (:name "DejaVu Sans Mono" :type ascii-only)
    (:name "monospace"        :type ascii-only))
  "Ordered list of font candidates. First available font is used.")

(defvar my-japanese-font-candidates
  '("Noto Sans CJK JP" "Noto Sans JP" "IPAGothic"
    "VL Gothic" "VL ゴシック" "Droid Sans Fallback")
  "Japanese font candidates for set-fontset-font (ascii-only fallback).")

;; ---- Layer 1: フォント検出 (純粋関数、副作用なし) ----

(defun my--find-first-font (candidates)
  "Return the first available font name from CANDIDATES string list."
  (cl-loop for name in candidates
           when (find-font (font-spec :name name))
           return name))

(defun my--detect-fonts ()
  "Detect best monospace font and Japanese fallback font.
Return plist (:name :type :japanese)."
  (let* ((best (cl-loop for c in my-font-candidates
                        when (find-font (font-spec :name (plist-get c :name)))
                        return c
                        finally return '(:name "monospace" :type ascii-only)))
         (jp (when (eq (plist-get best :type) 'ascii-only)
               (my--find-first-font my-japanese-font-candidates))))
    (list :name (plist-get best :name)
          :type (plist-get best :type)
          :japanese jp)))

;; ---- Layer 2: Fontset 設定 (set-fontset-font のみ) ----

(defun my--setup-fontset (font-info)
  "Set up fontset for Japanese glyphs when using ascii-only font.
FONT-INFO is a plist from `my--detect-fonts'."
  (when-let ((jp (plist-get font-info :japanese)))
    (let ((jp-spec (font-spec :family jp)))
      (dolist (charset '(japanese-jisx0208 japanese-jisx0212 katakana-jisx0201))
        (set-fontset-font t charset jp-spec))
      (set-fontset-font t 'unicode jp-spec nil 'append))))

;; ---- Layer 3: フェイス適用 (set-face-attribute のみ) ----

(defun my--apply-faces (font-name font-height)
  "Apply FONT-NAME and FONT-HEIGHT to default and UI faces."
  ;; default: family + height
  (set-face-attribute 'default nil :family font-name :height font-height)
  ;; UI faces: height のみ (family は default を継承)
  (dolist (face '(minibuffer-prompt mode-line mode-line-inactive
                  header-line fringe))
    (when (facep face)
      (set-face-attribute face nil :height font-height)))
  ;; doom-modeline
  (when (and (boundp 'doom-modeline-height) (featurep 'doom-modeline))
    (setq doom-modeline-height (my-scaled-modeline-height)
          doom-modeline-bar-width (my-scaled-modeline-bar-width))
    (when (fboundp 'doom-modeline-refresh-bars)
      (doom-modeline-refresh-bars))
    (force-mode-line-update t)))

;; ---- Entry point ----

(defun my-apply-font-config ()
  "Apply fontset-based font configuration for ASCII and Japanese."
  (interactive)
  (when (display-graphic-p)
    (let* ((font-info (my--detect-fonts))
           (font-name (plist-get font-info :name))
           (font-height (my-scaled-font-size)))
      (my--apply-faces font-name font-height)        ; Layer 3
      (my--setup-fontset font-info)                   ; Layer 2
      (message "Font: %s (%s), %dpt"
               font-name (plist-get font-info :type) (/ font-height 10)))))

;; Apply on startup + 1s timer (frame may not be ready at load time)
(when (display-graphic-p)
  (my-apply-font-config)
  (run-with-timer 1.0 nil #'my-apply-font-config))

;; --------------------------------
;; Icons (doom-modeline用)
;; --------------------------------
;; doom-modeline は nerd-icons を使用
(use-package nerd-icons
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
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  ;; org-config は org-mode 使用時に遅延読み込み
  (with-eval-after-load 'org
    (doom-themes-org-config))
  ;; diff / magit-diff / smerge フェイスのカスタマイズ
  ;; 現在は doom-dracula テーマのデフォルト色を使用
  )

;; --------------------------------
;; Modern modeline
;; --------------------------------
(use-package doom-modeline
  :ensure t
  :defer 0.1  ;; わずかに遅延（テーマ読み込み後）
  :custom
  ;; 統一スケーリング対応
  (doom-modeline-height (my-scaled-modeline-height))
  (doom-modeline-bar-width (my-scaled-modeline-bar-width))
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

;; Dashboard は無効化（起動高速化のため）
;; 必要な場合は M-x dashboard-open で手動起動可能

(provide 'init-ui-simple)
;;; init-ui-simple.el ends here
