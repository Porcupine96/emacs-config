(defvar rsvp/wait-for-seconds 0.23)
(defvar rsvp/text-buffer-name "*rsvp-text*")

(defface rsvp/buffer-face 
  '((t :height 1.5))
"RSVP buffer face")

(defun rsvp/run (start end)
  (interactive "r")

  (let ((text (buffer-substring start end)))
    (with-current-buffer (get-buffer-create "*rsvp*")
      (buffer-face-set 'rsvp/buffer-face)
      (erase-buffer)

      (dolist (word (split-string text))
	(insert word)
	(sit-for rsvp/wait-for-seconds)
	(erase-buffer)))))

(defun rsvp/read-from-ocr ()
  (interactive)

  (with-current-buffer (get-buffer-create rsvp/text-buffer-name)
    (erase-buffer)
    (+ocr/screenshot)))

(defun rsvp/read-last-content ()
  (interactive)

  (with-current-buffer (get-buffer-create rsvp/text-buffer-name)
    (rsvp/run (point-min) (point-max))))
