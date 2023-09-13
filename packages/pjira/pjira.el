(require 'org)
(require 's)

(defvar pjira-path "/tmp/sprint.org")
(defvar pjira-me "Łukasz Kaźmierczak")

(defun pjira-refresh ()
  (interactive)
  (call-process "pjira" nil 0 nil))

(defun pjira-narrow-to-me ()
  (interactive)

  (beginning-of-buffer)
  (search-forward (concat "** " pjira-me) nil t)
  (org-narrow-to-subtree))


(defun pjira--get-link ()
  (concat
   "https://chatbotize.atlassian.net/browse/"
   (s-replace "_" "-" (car (org-get-tags)))))

(defun pjira-copy-link ()
  (interactive)

  (kill-new (pjira--get-link)))

(defun pjira-open-url ()
  (interactive)

  (call-process "xdg-open" nil 0 nil (pjira--get-link)))

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
    (define-key map (kbd "C-c r") #'pjira-refresh)
    (define-key map (kbd "C-c m") #'pjira-narrow-to-me)
    map))

(define-minor-mode pjira-mode
  "pjira-mode"
  :init-value nil
  :lighter "pjira"
  :keymap pjira-mode-map)

(provide 'pjira)
