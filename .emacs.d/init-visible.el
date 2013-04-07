;;-----------------------------------------------------------------
;; タブ, 全角スペースを表示する
;;-----------------------------------------------------------------

(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ;; ("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;-----------------------------------------------------------------
;; frame
;;-----------------------------------------------------------------

;; フレームタイトルの設定
(setq frame-title-format "%b")

;;-----------------------------------------------------------------
;; buffer
;;-----------------------------------------------------------------

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;-----------------------------------------------------------------
;; fringe
;;-----------------------------------------------------------------

;; バッファ中の行番号表示
;; (global-linum-mode t)

;; 行番号のフォーマット
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
;; (set-face-attribute 'linum nil :height 0.8)
;; (setq linum-format "%4d")

;;-----------------------------------------------------------------
;; modeline
;;-----------------------------------------------------------------

(line-number-mode t)                    ;行番号の表示
(column-number-mode t)                  ;列番号の表示

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
;; (setq display-time-string-forms '(24-hours ":" minutes))
(setq display-time-string-forms '(month "/" day " " dayname " " 24-hours ":" minutes))
(display-time-mode t)

;; cp932エンコード時の表示を「P」とする
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; 選択範囲の情報表示
(defun count-lines-and-chars ()
  (if mark-active
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;;-----------------------------------------------------------------
;; cursor
;;-----------------------------------------------------------------

;; カーソル点滅表示
(blink-cursor-mode 0)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

(setq x-stretch-cursor t)
(setq visible-cursor t)
(global-hl-line-mode 0)                 ;行の強調
(show-paren-mode t)                     ;対応する括弧を強調
(setq-default tab-width 4 indent-tabs-mode nil) ;Tabは使わない
