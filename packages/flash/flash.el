;; -*- lexical-binding: t -*-

(require 's)
(require 'org)
(require 'json)
(require 'request)
(require 'org-element)

(defconst flash-directory "/home/porcupine/Dropbox/org-roam/flash")

(defcustom flash-anki-host
  "127.0.0.1"
  "AnkiConnect's host"
  :type '(string)
  :group 'flash)

(defcustom flash-anki-port
  8765
  "AnkiConnect's port"
  :type '(number)
  :group 'flash)

(defconst flash-anki-prop-note-id "ANKI_NOTE_ID")
(defconst flash-anki-prop-note-type "ANKI_TYPE")
(defconst flash-anki-prop-deck "ANKI_DECK")


(defun flash--set-note-id (id)
  (org-set-property flash-anki-prop-note-id (number-to-string id)))

(defun flash--notes-path ()
  (pcase (org-property-values "ID")
    (`(,id . nil) (expand-file-name (s-concat id ".org") flash-directory))
    (_ (error "Couldn't find org ID property."))))

(defun flash-anki--content-at-point ()
  (let* ((heading (org-element-at-point))
	 (start (org-element-property :contents-begin heading))
	 (end (org-element-property :contents-end heading)))

    (s-trim (buffer-substring-no-properties start end))))

(defun flash-anki--anki-connect-uri ()
  (concat "http://" flash-anki-host ":" (number-to-string flash-anki-port)))

(defun flash-anki--anki-connect-add-note (note on-success on-error)
  (request (flash-anki--anki-connect-uri)
    :type "POST"
    :data (json-encode `(("action" . "addNote")
			 ("version" . 6)
			 ("params" ("note" ("deckName" . ,(cdr (assoc 'deck note)))
				    ("modelName" . "Basic")
				    ("fields"
				     ("Front" .  ,(cdr (assoc 'front note)))
				     ("Back" . ,(cdr (assoc 'back note))))))))
    :parser 'json-read
    :success on-success
    :error on-error))

(defun flash-anki--headline-to-note (headline)
  (save-excursion
    (when (org-goto-first-child)
      (let ((front (flash-anki--content-at-point)))
	(org-forward-heading-same-level nil)
	(let ((back (flash-anki--content-at-point)))
	  `((note-id . ,(org-element-property :ANKI_NOTE_ID headline))
	    (deck . ,(org-element-property :ANKI_DECK headline))
	    (front . ,front)
	    (back . ,back)))))))

(defun flash-anki--on-success (data)
  (let ((result (cdr (assoc 'result data))))

    (if result
	(print result)
      (error "on-success callback with error"))))

(defun flash-anki--sync-note-at-point ()
  (let* ((note (flash-anki--headline-to-note (org-element-at-point))))
	 ;; (note-id (cdr (assq 'note-id note)))
	 ;; (headline-start (org-element-property :begin headline)))

  (flash-anki--anki-connect-add-note
     note
     (cl-function (lambda (&key data &allow-other-keys) (flash-anki--on-success data)))
     (cl-function (lambda (&key _ &key error-thrown &allow-other-keys) (print error-thrown))))))


(defun flash-traverse-top-level (fun)
  (save-excursion
    (goto-char (point-min))
    (let ((element (org-element-at-point)))
      (if (eq (org-element-type element) 'headline)
	  (flash-traverse-top-level-next (org-element-property :begin element) fun)))))

(defun flash-traverse-top-level-next (current-point fun)
  (funcall fun)
  (org-forward-heading-same-level nil)

  (let* ((element (org-element-at-point))
	 (headline-begin (org-element-property :begin element)))
    (if (not (eq headline-begin current-point))
	(flash-traverse-top-level-next headline-begin fun))))

(defun flash/test ()
  (interactive))


(defun flash-anki-sync ()
  (interactive)
  (flash-traverse-top-level 'flash-anki--sync-note-at-point))

(defun flash-open-notes ()
  (interactive)
  (find-file-other-window (flash--notes-path)))

;;;###autoload
(defvar flash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'flash-anki-sync)
    (define-key map (kbd "C-c o n") #'flash-open-notes)
    map))

(define-minor-mode flash-mode
  "flash-mode"
  :init-value nil
  :lighter "flash"
  :keymap flash-mode-map)

(provide 'flash)
