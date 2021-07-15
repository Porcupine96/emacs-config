(require 'dash)

(defun tabline-text (text fg)
  (propertize text 'face (list :family "Input Mono Narrow" :height 1.5 :background "#202020" :foreground fg)))

(defun format-workspaces (workspaces current)
  (string-join
   (-map (lambda (w) (if (equal w current) (tabline-text w "yellow") (tabline-text w "white")))
	 workspaces)
   (tabline-text " | " "white")))

(defun workspace-display (workspaces current)
  (let* ((margin 7)
	 (formatted-workspaces (format-workspaces workspaces current))
	 (shift (+ (length formatted-workspaces) margin)))
    (setq tab-line-format (concat
			   (propertize " "
				       'display '(space :align-to (- right-margin shift))
				       'face '(:background "#202020"))
			   ;; (propertize (string-join (-repeat (- (window-width) (length formatted-workspaces)) " "))
			   ;; 	       'face '(:height 1.0 :family "Input Mono Narrow" :background "#202020"))
			   formatted-workspaces
			   (propertize (string-join (-repeat margin "  "))
				       'face '(:height 1.5 :family "Input Mono Narrow" :background "#202020"))))
    (sit-for 5)
    (setq tab-line-format nil)))

(workspace-display (persp-all-names) (persp-name (persp-curr)))


(defun demo()
  (interactive)
  (workspace-display (persp-all-names) (persp-name (persp-curr))))


;; (global-set-key (kbd "C-c d") #'demo)

;; --> get the current perspective
;; (persp-curr)


;; --> list perspective names
;; (persp-all-names)
;; (persp-names)
























