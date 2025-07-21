;;; test-font-scaling.el --- Test script for smart font scaling

;;; Commentary:
;; This script tests the smart font scaling functionality.
;; Run with: emacs -Q -l test-font-scaling.el

;;; Code:

;; Add elisp directory to load path
(add-to-list 'load-path (expand-file-name "elisp" default-directory))

;; Load the smart font scaling module
(require 'smart-font-scaling)

;; Test configuration
(setq sfs-debug-mode t)
(setq sfs-base-font-size 13)

;; Function to display test results
(defun test-sfs-display-info ()
  "Display current display information for testing."
  (interactive)
  (let ((info (sfs--get-display-info)))
    (with-current-buffer (get-buffer-create "*SFS Test Results*")
      (erase-buffer)
      (insert "Smart Font Scaling Test Results\n")
      (insert "================================\n\n")
      
      (insert "System Information:\n")
      (insert (format "Platform: %s\n" system-type))
      (insert (format "Emacs Version: %s\n" emacs-version))
      (insert (format "GUI Mode: %s\n\n" (display-graphic-p)))
      
      (when info
        (insert "Display Information:\n")
        (insert (format "Resolution: %dx%d pixels\n" 
                        (plist-get info :pixel-width)
                        (plist-get info :pixel-height)))
        (insert (format "Physical Size: %dx%d mm\n"
                        (or (plist-get info :mm-width) 0)
                        (or (plist-get info :mm-height) 0)))
        (insert (format "Calculated DPI X: %.1f\n" 
                        (or (plist-get info :dpi-x) 0)))
        (insert (format "Calculated DPI Y: %.1f\n"
                        (or (plist-get info :dpi-y) 0)))
        (insert (format "Average DPI: %.1f\n\n"
                        (or (plist-get info :dpi-avg) 0))))
      
      (insert "Font Configuration:\n")
      (let* ((font-info (sfs--find-best-font))
             (font-family (cdr font-info))
             (font-size (sfs--calculate-optimal-font-size))
             (dpi (when info (plist-get info :dpi-avg)))
             (scaling-factor (when dpi (sfs--calculate-scaling-factor dpi))))
        
        (insert (format "Best Available Font: %s\n" font-family))
        (insert (format "Base Font Size: %d\n" sfs-base-font-size))
        (insert (format "Scaling Factor: %.2f\n" (or scaling-factor 1.0)))
        (insert (format "Calculated Font Size: %d\n" font-size))
        (insert (format "Manual Override: %s\n\n" 
                        (if sfs-manual-override 
                            (format "%d" sfs-manual-override) 
                            "none"))))
      
      (insert "Available Fonts:\n")
      (dolist (font sfs-font-families)
        (let ((name (car font))
              (display-name (cdr font)))
          (insert (format "- %s: %s\n" 
                          display-name
                          (if (find-font (font-spec :name name)) 
                              "Available" 
                              "Not Available")))))
      
      (insert "\n")
      (insert "DPI Thresholds:\n")
      (dolist (threshold sfs-dpi-thresholds)
        (insert (format "- %d DPI → %.2fx scaling\n" 
                        (car threshold) (cdr threshold))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Test function for applying scaling
(defun test-sfs-apply-scaling ()
  "Test applying font scaling."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Testing smart font scaling...")
        (smart-font-scaling-mode 1)
        (sfs-apply-optimal-scaling)
        (message "Smart font scaling applied. Check *Messages* buffer for details."))
    (message "Test can only be run in GUI mode.")))

;; Test different manual overrides
(defun test-sfs-manual-sizes ()
  "Test different manual font sizes."
  (interactive)
  (when (display-graphic-p)
    (let ((test-sizes '(10 12 14 16 18 20 24)))
      (dolist (size test-sizes)
        (message "Testing font size: %d" size)
        (sfs-set-manual-override size)
        (sit-for 1))
      (sfs-clear-manual-override)
      (message "Manual size testing completed. Returned to automatic scaling."))))

;; Interactive test menu
(defun sfs-test-menu ()
  "Show interactive test menu."
  (interactive)
  (let ((choice (read-char-choice 
                 "Smart Font Scaling Test Menu:
1. Display system info
2. Apply scaling
3. Test manual sizes
4. Show current info
5. Quit
Choose: " '(?1 ?2 ?3 ?4 ?5))))
    (cond
     ((eq choice ?1) (test-sfs-display-info) (sfs-test-menu))
     ((eq choice ?2) (test-sfs-apply-scaling) (sfs-test-menu))
     ((eq choice ?3) (test-sfs-manual-sizes) (sfs-test-menu))
     ((eq choice ?4) (sfs-show-current-info) (sfs-test-menu))
     ((eq choice ?5) (message "Test completed."))
     (t (message "Invalid choice.") (sfs-test-menu)))))

;; Auto-run if loaded directly
(when (display-graphic-p)
  (message "Loading Smart Font Scaling test...")
  (test-sfs-display-info)
  (message "Run (sfs-test-menu) for interactive testing."))

(provide 'test-font-scaling)
;;; test-font-scaling.el ends here