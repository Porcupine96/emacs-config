
(defvar pjira-path "/tmp/sprint.org")

(defun pjira-refresh ()
  (interactive)
  (call-process "pjira" nil 0 nil nil))


(defun pjira-current-sprint ()
  (interactive)

  (if (file-exists-p pjira-path)
	(find-file pjira-path)
    (progn
      (pjira-refresh)
      (find-file pjira-path))))

(defvar pjira-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") #'pjira-current-sprint)
    map))

(define-minor-mode pjira-mode
  "pjira-mode"
  :init-value nil
  :lighter "pjira"
  :keymap pjira-mode-map)




(provide 'pjira)
