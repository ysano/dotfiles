;;; quick-font-fix.el --- Quick font scaling fix

;;; Commentary:
;; Temporary fix to manually apply font scaling

;;; Code:

;; Manual font scaling for immediate fix
(when (display-graphic-p)
  ;; Set larger font immediately for your high-res displays
  (set-face-attribute 'default nil :height 180)  ; 18pt equivalent
  (set-face-attribute 'minibuffer-prompt nil :height 180)
  (set-face-attribute 'mode-line nil :height 160)  ; Slightly smaller for mode line
  (set-face-attribute 'mode-line-inactive nil :height 160)
  
  ;; Also set the font family if available
  (when (find-font (font-spec :name "Cica"))
    (set-face-attribute 'default nil :family "Cica"))
  
  (message "Quick font fix applied: 18pt font size"))

;; Function to test smart font scaling
(defun test-smart-font-scaling ()
  "Test and debug smart font scaling."
  (interactive)
  (when (display-graphic-p)
    (message "Testing smart font scaling...")
    
    ;; Check if module is loaded
    (if (featurep 'smart-font-scaling)
        (progn
          (message "smart-font-scaling module is loaded")
          
          ;; Get display info
          (let ((info (sfs--get-display-info)))
            (if info
                (progn
                  (message "Display resolution: %dx%d" 
                          (plist-get info :pixel-width)
                          (plist-get info :pixel-height))
                  (message "Estimated DPI: %.1f" 
                          (or (plist-get info :dpi-avg) 96))
                  (message "Calculated font size: %d" 
                          (sfs--calculate-optimal-font-size))
                  
                  ;; Apply scaling
                  (sfs-apply-optimal-scaling))
              (message "Failed to get display info")))
          
          ;; Show current font
          (message "Current default face height: %s" 
                  (face-attribute 'default :height)))
      (message "smart-font-scaling module not loaded"))))

;; Function to apply larger font size
(defun apply-large-font ()
  "Apply large font for high-res displays."
  (interactive)
  (when (display-graphic-p)
    ;; Apply larger font based on your resolution
    (set-face-attribute 'default nil :height 200)  ; 20pt
    (set-face-attribute 'minibuffer-prompt nil :height 200)
    (set-face-attribute 'mode-line nil :height 180)
    (set-face-attribute 'mode-line-inactive nil :height 180)
    (set-face-attribute 'header-line nil :height 200)
    
    (message "Large font applied: 20pt")))

;; Auto-apply if this file is loaded directly (DISABLED to avoid conflicts)
;; (when (and (display-graphic-p) 
;;            (< (face-attribute 'default :height) 150))
;;   (message "Applying quick font fix...")
;;   ;; Apply immediately
;;   (run-with-timer 0.1 nil 
;;                   (lambda ()
;;                     (set-face-attribute 'default nil :height 180)
;;                     (set-face-attribute 'minibuffer-prompt nil :height 180)
;;                     (set-face-attribute 'mode-line nil :height 160)
;;                     (set-face-attribute 'mode-line-inactive nil :height 160)
;;                     (message "Font size increased to 18pt"))))

(provide 'quick-font-fix)
;;; quick-font-fix.el ends here