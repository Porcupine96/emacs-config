;; -*- lexical-binding: t -*-

(require 's)
(require 'org)
(require 'json)
(require 'request)
(require 'org-element)
(require 'ox-html)

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
(defconst flash-anki-prop-deck "ANKI_DECK")


(defun flash--set-note-id (id)
  (org-set-property flash-anki-prop-note-id (number-to-string id)))

(defun flash--notes-path ()
  (pcase (org-property-values "ID")
    (`(,id . nil) (expand-file-name (s-concat id ".org") flash-directory))
    (_ (error "Couldn't find org ID property."))))

(defun flash-anki--headline-content (headline)
  (let ((start (org-element-property :contents-begin headline))
	(end (org-element-property :contents-end headline)))

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

(defun flash-anki--headline-to-note ()
  (pcase (org-map-entries (lambda () (org-element-at-point)) nil 'tree)
    (`(,top ,front ,back . nil)
     `((note-id . ,(org-element-property :ANKI_NOTE_ID top))
       (deck . ,(org-element-property :ANKI_DECK top))
       (front . ,(flash-anki--headline-content front))
       (back . ,(flash-anki--headline-content back))))
    (_ (error "couldn't parse map headline to note"))))


(defun flash-anki--add-note-success (data)
  (let ((note-id (cdr (assoc 'result data))))

    (if note-id
	(org-set-property flash-anki-prop-note-id (number-to-string note-id))
      (error (concat "on-success callback with error: " (cdr (assoc 'error data)))))))

(defun flash-anki-sync-note-at-point (callback)
  (interactive "i")

  (let* ((note (flash-anki--headline-to-note))
	 (note-id (cdr (assq 'note-id note))))
    (if note-id
	(progn
	  (print "todo: update instead")
	  (if callback (funcall callback)))
	(flash-anki--anki-connect-add-note
	    note
	    (cl-function (lambda (&key data &allow-other-keys)
			   (progn (flash-anki--add-note-success data)
				  (if callback (funcall callback)))))
	    (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
			   (error (concat "error when syncing a note: " error-thrown))))))))


(defun flash-anki--sync-next (current-point step)
  (if step
      (progn
	(org-forward-heading-same-level nil)
	(let* ((element (org-element-at-point))
	       (headline-begin (org-element-property :begin element)))
	  (if (not (eq headline-begin current-point))
	      (flash-anki--sync-next headline-begin nil))))
    (flash-anki-sync-note-at-point (lambda ()
				     (flash-anki--sync-next current-point t)))))

(defun flash-anki-sync ()
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (let ((element (org-element-at-point)))
      (if (eq (org-element-type element) 'headline)
	  (flash-anki--sync-next (org-element-property :begin element) nil)))))

(defun flash-insert-skeleton ()
  (interactive)
  (goto-char (point-max))
  (org-insert-heading nil nil t)
  (save-excursion
    (insert "Flashcard")
    (org-insert-subheading nil)
    (insert "Front")
    (org-insert-heading nil)
    (insert "Back"))
  (org-set-property flash-anki-prop-deck "Default"))

(defun flash-open-notes ()
  (interactive)
  (find-file-other-window (flash--notes-path)))

;;;###autoload
(defvar flash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'flash-anki-sync)
    (define-key map (kbd "C-c a n") #'flash-insert-skeleton)
    (define-key map (kbd "C-c o n") #'flash-open-notes)
    map))

(define-minor-mode flash-mode
  "flash-mode"
  :init-value nil
  :lighter "flash"
  :keymap flash-mode-map)

(provide 'flash)


;; MAY COME USEFUL
;; ---
;; (defun flash-traverse-top-level (fun)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((element (org-element-at-point)))
;;       (if (eq (org-element-type element) 'headline)
;; 	  (flash-traverse-top-level-next (org-element-property :begin element) fun)))))

;; (defun flash-traverse-top-level-next (current-point fun)
;;   (funcall fun)
;;   (org-forward-heading-same-level nil)

;;   (let* ((element (org-element-at-point))
;; 	 (headline-begin (org-element-property :begin element)))
;;     (if (not (eq headline-begin current-point))
;; 	(flash-traverse-top-level-next headline-begin fun))))
