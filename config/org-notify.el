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
	   (time-seconds (string-to-number (org-notify--timestamp-to-seconds scheduled)))
	   (now (float-time (current-time))))

      (print time-seconds)
      (print now)

      (if time
	(progn
	  (print "Found it."))
	(progn
	  (print "Not found.")

          (puthash id now timers)
	  (erase-buffer)
	  (prin1 timers (current-buffer)))))))

(org-notify--process-file (-first-item org-agenda-files))

  ;; (notifications-notify
  ;;  :title "Notification"
  ;;  :body title))

(defun org-notify--timestamp-to-seconds (timestamp)
  (let ((org-display-custom-times t)
	(org-time-stamp-custom-formats '("%s")))
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
