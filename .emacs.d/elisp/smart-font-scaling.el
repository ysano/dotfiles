;;; smart-font-scaling.el --- Intelligent font scaling for high-DPI displays

;;; Commentary:
;; This package provides intelligent font scaling based on display DPI,
;; resolution, and user preferences. It supports:
;; - Automatic DPI detection and calculation
;; - Dynamic font size adjustment for all UI elements
;; - Multi-monitor support with per-display optimization
;; - User customization and manual overrides
;; - Graceful fallback for non-GUI environments

;;; Code:

(require 'cl-lib)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Custom Variables
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defgroup smart-font-scaling nil
  "Intelligent font scaling for high-DPI displays."
  :group 'faces
  :prefix "sfs-")

(defcustom sfs-base-font-size 13
  "Base font size for standard DPI (96 DPI)."
  :type 'integer
  :group 'smart-font-scaling)

(defcustom sfs-font-families 
  (cond
   ;; macOS - Retina対応フォント優先
   ((eq system-type 'darwin)
    '(("Cica" . "Cica")
      ("Sarasa Term J" . "Sarasa Term J")
      ("SF Mono" . "SF Mono")
      ("Monaco" . "Monaco")
      ("Menlo" . "Menlo")))
   ;; Windows - ClearType対応フォント優先
   ((eq system-type 'windows-nt)
    '(("Cica" . "Cica")
      ("Sarasa Term J" . "Sarasa Term J")
      ("Consolas" . "Consolas")
      ("Cascadia Code" . "Cascadia Code")
      ("Courier New" . "Courier New")))
   ;; Linux - アンチエイリアス対応フォント優先
   ((memq system-type '(gnu/linux berkeley-unix))
    '(("Cica" . "Cica")
      ("Sarasa Term J" . "Sarasa Term J")
      ("Fira Code" . "Fira Code")
      ("DejaVu Sans Mono" . "DejaVu Sans Mono")
      ("Liberation Mono" . "Liberation Mono")
      ("monospace" . "monospace")))
   ;; その他
   (t '(("Cica" . "Cica")
        ("Sarasa Term J" . "Sarasa Term J")
        ("monospace" . "monospace"))))
  "Ordered list of preferred font families with their display names.
Automatically configured based on the current platform."
  :type '(alist :key-type string :value-type string)
  :group 'smart-font-scaling)

(defcustom sfs-dpi-thresholds
  '((96 . 1.0)    ; 標準DPI
    (120 . 1.25)  ; 高DPI
    (144 . 1.5)   ; 非常に高DPI (1.5倍)
    (192 . 2.0)   ; Retina相当 (2倍)
    (288 . 3.0))  ; 超高解像度 (3倍)
  "DPI thresholds and their corresponding scaling factors."
  :type '(alist :key-type integer :value-type float)
  :group 'smart-font-scaling)

(defcustom sfs-ui-elements
  '(default minibuffer-prompt mode-line mode-line-inactive 
    header-line tooltip)
  "UI elements to apply font scaling to."
  :type '(repeat symbol)
  :group 'smart-font-scaling)

(defcustom sfs-manual-override nil
  "Manual font size override. When set, disables automatic scaling."
  :type '(choice (const :tag "Auto" nil) 
                 (integer :tag "Manual size"))
  :group 'smart-font-scaling)

(defcustom sfs-debug-mode nil
  "Enable debug output for font scaling calculations."
  :type 'boolean
  :group 'smart-font-scaling)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Internal Variables
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar sfs--current-font-size nil
  "Currently applied font size.")

(defvar sfs--current-font-family nil
  "Currently applied font family.")

(defvar sfs--display-info-cache nil
  "Cached display information to avoid repeated calculations.")

(defvar sfs--monitor-change-hook nil
  "Hook run when monitor configuration changes.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Display Information Detection
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun sfs--get-display-info ()
  "Get comprehensive display information."
  (when (display-graphic-p)
    (let* ((pixel-width (display-pixel-width))
           (pixel-height (display-pixel-height))
           (mm-width (display-mm-width))
           (mm-height (display-mm-height))
           (monitor-attrs (when (fboundp 'display-monitor-attributes-list)
                           (display-monitor-attributes-list)))
           (dpi-x (if (and mm-width (> mm-width 0))
                     (/ (* pixel-width 25.4) mm-width)
                   nil))
           (dpi-y (if (and mm-height (> mm-height 0))
                     (/ (* pixel-height 25.4) mm-height)
                   nil))
           (dpi-avg (when (and dpi-x dpi-y)
                     (/ (+ dpi-x dpi-y) 2.0))))
      
      ;; プラットフォーム固有のDPI推定（計算値がない場合）
      (unless dpi-avg
        (setq dpi-avg (sfs--estimate-platform-dpi pixel-width pixel-height)))
      
      (list :pixel-width pixel-width
            :pixel-height pixel-height
            :mm-width mm-width
            :mm-height mm-height
            :dpi-x dpi-x
            :dpi-y dpi-y
            :dpi-avg dpi-avg
            :monitor-attrs monitor-attrs))))

(defun sfs--estimate-platform-dpi (width height)
  "Estimate effective DPI based on platform and resolution."
  (cond
   ;; macOS specific estimation
   ((eq system-type 'darwin)
    (cond
     ;; 4K/5K iMac (27-inch)
     ((and (>= width 5120) (>= height 2880)) 220)
     ;; 4K (21.5-inch iMac, external 4K)
     ((and (>= width 4096) (>= height 2304)) 200)
     ;; UWQHD (Ultra-wide)
     ((and (>= width 3440) (>= height 1440)) 140)
     ;; QHD/WQHD
     ((and (>= width 2560) (>= height 1440)) 130)
     ;; MacBook Pro 16-inch Retina
     ((and (>= width 3072) (>= height 1920)) 226)
     ;; MacBook Pro 13-inch Retina
     ((and (>= width 2560) (>= height 1600)) 227)
     ;; MacBook Air Retina
     ((and (>= width 2560) (>= height 1664)) 224)
     ;; 標準解像度
     (t 96)))
   
   ;; Windows specific estimation
   ((eq system-type 'windows-nt)
    (cond
     ;; 4K displays
     ((and (>= width 3840) (>= height 2160)) 180)
     ;; UWQHD
     ((and (>= width 3440) (>= height 1440)) 130)
     ;; QHD
     ((and (>= width 2560) (>= height 1440)) 120)
     ;; Full HD high DPI
     ((and (>= width 1920) (>= height 1080) (< width 2560)) 110)
     ;; 標準解像度
     (t 96)))
   
   ;; Linux/X11 specific estimation
   ((memq system-type '(gnu/linux berkeley-unix))
    (cond
     ;; 4K displays
     ((and (>= width 3840) (>= height 2160)) 160)
     ;; UWQHD
     ((and (>= width 3440) (>= height 1440)) 125)
     ;; QHD
     ((and (>= width 2560) (>= height 1440)) 115)
     ;; Full HD
     ((and (>= width 1920) (>= height 1080) (< width 2560)) 100)
     ;; 標準解像度
     (t 96)))
   
   ;; その他のプラットフォーム
   (t (cond
      ;; 4K以上
      ((and (>= width 3840) (>= height 2160)) 150)
      ;; QHD以上
      ((and (>= width 2560) (>= height 1440)) 120)
      ;; Full HD
      ((and (>= width 1920) (>= height 1080)) 100)
      ;; 標準
      (t 96)))))

(defun sfs--calculate-scaling-factor (dpi)
  "Calculate appropriate scaling factor based on DPI."
  (if (numberp dpi)
      (cl-loop for (threshold . factor) in (reverse sfs-dpi-thresholds)
               when (<= threshold dpi)
               return factor
               finally return 1.0)
    1.0))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Font Management
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun sfs--find-best-font ()
  "Find the best available font from the preferred list."
  (cl-loop for (name . display-name) in sfs-font-families
           when (find-font (font-spec :name name))
           return (cons name display-name)
           finally return (cons "monospace" "monospace")))

(defun sfs--calculate-optimal-font-size ()
  "Calculate optimal font size based on display characteristics."
  (if sfs-manual-override
      sfs-manual-override
    (if-let* ((display-info (or sfs--display-info-cache 
                               (setq sfs--display-info-cache (sfs--get-display-info))))
              (dpi (plist-get display-info :dpi-avg))
              (scaling-factor (sfs--calculate-scaling-factor dpi)))
        (round (* sfs-base-font-size scaling-factor))
      sfs-base-font-size)))

(defun sfs--apply-font-to-face (face font-family font-size)
  "Apply font configuration to a specific face."
  (when (facep face)
    (let ((font-spec (format "%s-%d" font-family font-size)))
      (condition-case err
          (set-face-font face font-spec)
        (error 
         (when sfs-debug-mode
           (message "Failed to set font for face %s: %s" face err)))))))

(defun sfs--apply-font-configuration (font-family font-size)
  "Apply font configuration to all relevant UI elements."
  (when sfs-debug-mode
    (message "Applying font: %s-%d" font-family font-size))
  
  ;; Apply to all specified UI elements
  (dolist (face sfs-ui-elements)
    (sfs--apply-font-to-face face font-family font-size))
  
  ;; Special handling for specific elements
  (when (facep 'variable-pitch)
    (sfs--apply-font-to-face 'variable-pitch font-family font-size))
  
  ;; Update internal state
  (setq sfs--current-font-family font-family
        sfs--current-font-size font-size)
  
  ;; Update doom-modeline height dynamically if available
  (when (and (boundp 'doom-modeline-height) (featurep 'doom-modeline))
    (let ((new-height (if (> font-size 15) 35 25))
          (new-bar-width (if (> font-size 15) 4 3)))
      (setq doom-modeline-height new-height
            doom-modeline-bar-width new-bar-width)
      ;; Force modeline refresh
      (when (fboundp 'doom-modeline-refresh-bars)
        (doom-modeline-refresh-bars))
      (force-mode-line-update t)))
  
  ;; Prevent other packages from overriding our font settings
  (when sfs-debug-mode
    (message "Font configuration applied and protected: %s-%d" font-family font-size)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Public API
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun sfs-apply-optimal-scaling ()
  "Apply optimal font scaling based on current display characteristics."
  (interactive)
  (when (display-graphic-p)
    (let* ((font-info (sfs--find-best-font))
           (font-family (cdr font-info))
           (font-size (sfs--calculate-optimal-font-size)))
      
      (sfs--apply-font-configuration font-family font-size)
      
      (when sfs-debug-mode
        (let ((display-info (or sfs--display-info-cache (sfs--get-display-info))))
          (message "Smart Font Scaling Applied:")
          (message "  Resolution: %dx%d" 
                   (plist-get display-info :pixel-width)
                   (plist-get display-info :pixel-height))
          (message "  Estimated DPI: %.1f" 
                   (or (plist-get display-info :dpi-avg) 0))
          (message "  Font: %s-%d" font-family font-size)))
      
      (message "Font scaling applied: %s-%d" font-family font-size))))

(defun sfs-refresh-display-info ()
  "Refresh cached display information and reapply scaling."
  (interactive)
  (setq sfs--display-info-cache nil)
  (sfs-apply-optimal-scaling))

(defun sfs-set-manual-override (size)
  "Set manual font size override."
  (interactive "nFont size: ")
  (setq sfs-manual-override size)
  (sfs-apply-optimal-scaling))

(defun sfs-clear-manual-override ()
  "Clear manual font size override and return to automatic scaling."
  (interactive)
  (setq sfs-manual-override nil)
  (sfs-apply-optimal-scaling))

(defun sfs-show-current-info ()
  "Show current font scaling information."
  (interactive)
  (let ((display-info (or sfs--display-info-cache (sfs--get-display-info))))
    (message "Current Font Scaling Info:")
    (message "  Font: %s-%s" 
             (or sfs--current-font-family "not set")
             (or sfs--current-font-size "not set"))
    (when display-info
      (message "  Resolution: %dx%d"
               (plist-get display-info :pixel-width)
               (plist-get display-info :pixel-height))
      (message "  DPI: %.1f" (or (plist-get display-info :dpi-avg) 0)))
    (message "  Manual override: %s" 
             (if sfs-manual-override 
                 (format "%d" sfs-manual-override) 
                 "none"))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Dynamic Monitor Detection
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun sfs--monitor-change-handler ()
  "Handle monitor configuration changes."
  (when sfs-debug-mode
    (message "Monitor configuration changed, refreshing font scaling"))
  (run-with-timer 0.5 nil #'sfs-refresh-display-info))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Minor Mode
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar smart-font-scaling-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c f s") #'sfs-apply-optimal-scaling)
    (define-key map (kbd "C-c f r") #'sfs-refresh-display-info)
    (define-key map (kbd "C-c f m") #'sfs-set-manual-override)
    (define-key map (kbd "C-c f c") #'sfs-clear-manual-override)
    (define-key map (kbd "C-c f i") #'sfs-show-current-info)
    map)
  "Keymap for smart-font-scaling-mode.")

;;;###autoload
(define-minor-mode smart-font-scaling-mode
  "Enable intelligent font scaling for high-DPI displays."
  :global t
  :lighter " SFS"
  :keymap smart-font-scaling-mode-map
  (if smart-font-scaling-mode
      (progn
        ;; Enable
        (when (display-graphic-p)
          (sfs-apply-optimal-scaling))
        
        ;; Set up monitor change detection
        (when (fboundp 'display-monitor-attributes-list)
          (add-hook 'window-configuration-change-hook 
                   #'sfs--monitor-change-handler))
        
        ;; Set up font protection (reapply if changed by other packages)
        (add-hook 'after-init-hook #'sfs--setup-font-protection)
        
        (when sfs-debug-mode
          (message "Smart Font Scaling mode enabled")))
    
    ;; Disable
    (remove-hook 'window-configuration-change-hook #'sfs--monitor-change-handler)
    (remove-hook 'after-init-hook #'sfs--setup-font-protection)
    (when sfs-debug-mode
      (message "Smart Font Scaling mode disabled"))))

;; Font protection mechanism
(defun sfs--setup-font-protection ()
  "Setup protection against font changes by other packages."
  (when (display-graphic-p)
    ;; Apply our settings after everything else is loaded
    (run-with-timer 2.0 nil 
                    (lambda ()
                      (sfs-apply-optimal-scaling)
                      (when sfs-debug-mode
                        (message "Font protection applied - final font scaling"))))
    
    ;; Set up periodic check to maintain font size
    (run-with-timer 5.0 10.0  ; Check every 10 seconds after 5 seconds
                    (lambda ()
                      (when (and smart-font-scaling-mode
                                 (display-graphic-p)
                                 sfs--current-font-size)
                        (let ((current-height (face-attribute 'default :height)))
                          (when (< current-height (* sfs--current-font-size 10))
                            (when sfs-debug-mode
                              (message "Font size changed detected, reapplying..."))
                            (sfs-apply-optimal-scaling)))))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Integration with existing font configuration
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;;###autoload
(defun sfs-replace-static-font-config ()
  "Replace static font configuration with smart scaling.
This function is designed to be called from init-ui-simple.el
to replace the existing static font configuration."
  (when (display-graphic-p)
    (smart-font-scaling-mode 1)))

(provide 'smart-font-scaling)
;;; smart-font-scaling.el ends here