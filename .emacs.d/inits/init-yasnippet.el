;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
;; snippet-mode
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

(add-to-list 'yas-snippet-dirs "~/yasnippet-snippets")

;; yasnippet anything interface
;; via https://github.com/sugyan/dotfiles/blob/master/.emacs.d/conf/04-yasnippet.el
;; (eval-after-load "anything-config"
;;   '(progn
;;      (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
;;      (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  choice)))
         (selected (anything-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*anything yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))

;; (require 'dropdown-list)
;; (setq yas-prompt-functions '(
;;                              my-yas/prompt
;;                              yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))

