;;; org-notify.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'dash)
(require 'notifications)


(defvar org-notify-check-interval 3)
(defvar org-notify-key "notify")
(defvar org-notify--timer nil)
(defvar org-notify-log-path "~/.emacs.d/org-notify-log.el")
(defvar org-notify-before-minutes 5)

(defun org-notify--notify-headlines ()
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (org-element-map headline 'node-property
	(lambda (prop)
	  (if (and (equal (org-element-property :key prop) org-notify-key)
		  (org-element-property :scheduled headline))
	      (list
 	       (org-element-property :value prop)
 	       (org-element-property :scheduled headline)
	       (org-element-property :raw-value headline))))))))


(defun org-notify--handle-headline (id scheduled title)
  (with-current-buffer (or (get-file-buffer org-notify-log-path)
	             	   (find-file-noselect org-notify-log-path))

    (let* ((timers (eval (car (read-from-string (buffer-string)))))
	   (time (gethash id timers))
	   (scheduled (string-to-number (org-notify--timestamp-to-seconds scheduled)))
	   (now (float-time (current-time)))
           (update (org-notify--try-to-notify title scheduled time now)))

      (if update
	  (progn
	    (puthash id update timers)
	    (erase-buffer)
	    (prin1 timers (current-buffer)))))))

(defun org-notify--try-to-notify (title scheduled last-notification now)
  (if (and
       (< (- scheduled now) (* org-notify-before-minutes 60))
       (or
	(not last-notification)
        (>= (- scheduled last-notification) (* org-notify-before-minutes 60))))

      (progn
	(notifications-notify
	 :title (format "Task scheduled in %i minutes" org-notify-before-minutes)
	 :body title)
	now)))

(defun org-notify--timestamp-to-seconds (timestamp)
  (let ((org-display-custom-times t)
	(org-time-stamp-custom-formats '("%s" . "%s")))
    (org-timestamp-translate timestamp 'start)))

(defun org-notify--process-file (path)
  (with-temp-buffer
    (insert-file-contents path)

    (let ((notify-headlines (org-notify--notify-headlines)))
      (-each (-flatten-n 1 notify-headlines)
	(lambda (h)
	  (apply 'org-notify--handle-headline h))))))

(defun org-notify--check ()
  (-each org-agenda-files 'org-notify--process-file))

(defun org-notify--start ()
  (setq org-notify--timer
	(run-with-timer nil org-notify-check-interval 'org-notify--check)))

(defun org-notify--stop ()
  (cancel-timer org-notify--timer))


(define-minor-mode org-notify
  "org-notify"
  :init-value nil :lighter "org-notify" :keymap nil
  (if org-notify
      (org-notify--start)
     (org-notify--stop)))

;;; org-notify.el ends here
