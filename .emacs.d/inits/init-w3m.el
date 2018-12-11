;;-----------------------------------------------------------------
;; w3m
;;-----------------------------------------------------------------
;(require 'w3m)
;; (require 'mime-w3m)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
;; (global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-default-display-inline-images t)

; Fix the CYGWIN environment variable
(when (or run-cygwin run-w32)
  (cons "CYGWIN" "nodosfilewarning noenvcache strip_title title binmode"))