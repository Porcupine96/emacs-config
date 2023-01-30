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
(defconst flash-anki-prop-model-name "ANKI_MODEL_NAME")

(defun flash--set-note-id (id)
  (org-set-property flash-anki-prop-note-id (number-to-string id)))

(defun flash-anki--headline-content (headline)
  (let ((start (org-element-property :contents-begin headline))
	(end (org-element-property :contents-end headline)))

    (s-trim (buffer-substring-no-properties start end))))

(defun flash-anki--anki-connect-uri ()
  (concat "http://" flash-anki-host ":" (number-to-string flash-anki-port)))

(defun flash-anki--anki-connect-format (text)
  (with-temp-buffer
    (insert text)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-html-convert-region-to-html)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun flash-anki--anki-connect-make-request (action params on-success on-error)
  (request (flash-anki--anki-connect-uri)
	   :type "POST"
	   :data (json-encode `(("action" . ,action)
				("version" . 6)
				("params" ,params)))
	   :parser 'json-read
	   :success on-success
	   :error on-error))

(defun flash-anki--anki-connect-ensure-deck (name on-success on-error)
  (flash-anki--anki-connect-make-request
   "createDeck"
   `("deck" . ,name)
   on-success
   on-error))

(defun flash-anki--anki-connect-add-note (note on-success on-error)
  (flash-anki--anki-connect-make-request
   "addNote"
   `("note"
     ("deckName" . ,(cdr (assoc 'deck note)))
     ("modelName" . ,(cdr (assoc 'model-name note)))
     ("fields"
      ("Front" . ,(flash-anki--anki-connect-format (cdr (assoc 'front note))))
      ("Back" . ,(flash-anki--anki-connect-format (cdr (assoc 'back note))))))
   on-success
   on-error))

(defun flash-anki--anki-connect-update-note (note on-success on-error)
  (flash-anki--anki-connect-make-request
   "updateNoteFields"
   `("note"
     ("id" . ,(string-to-number (cdr (assoc 'note-id note))))
     ("fields"
      ("Front" . ,(flash-anki--anki-connect-format (cdr (assoc 'front note))))
      ("Back" . ,(flash-anki--anki-connect-format (cdr (assoc 'back note))))))
   on-success
   on-error))

(defun flash-anki--headline-to-note ()
  (pcase (org-map-entries (lambda () (org-element-at-point)) nil 'tree)
    (`(,top ,front ,back . nil)
     `((note-id . ,(org-element-property :ANKI_NOTE_ID top))
       (deck . ,(or (org-element-property :ANKI_DECK top)
	            (car (org-property-values flash-anki-prop-deck))))
       (model-name . ,(or (org-element-property :ANKI_MODEL_NAME top)
			  "Basic"))
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
	 (note-id (cdr (assq 'note-id note)))
	 (deck-name (cdr (assoc 'deck note))))
    (if note-id
	(progn
	  (flash-anki--anki-connect-update-note
	   note
	   (cl-function (lambda (&key _ &allow-other-keys)
			  (progn
			    (if callback (funcall callback)))))
	   (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
			  (error (concat "error when updating a note: " error-thrown))))))

      (flash-anki--anki-connect-ensure-deck
       deck-name
       (cl-function (lambda (&key _ &allow-other-keys)
		      (flash-anki--anki-connect-add-note
		       note
		       (cl-function (lambda (&key data &allow-other-keys)
				      (progn (flash-anki--add-note-success data)
					     (if callback (funcall callback)))))
		       (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
				      (error (concat "error when creating a note: " error-thrown)))))))
       (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
		      (error (concat "failed to ensure deck" error-thrown))))))))

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

  (unless (looking-at-p "\*.*")
    (org-next-visible-heading 1))
  
  (save-excursion
    (let ((element (org-element-at-point)))
      (if (eq (org-element-type element) 'headline)
	  (flash-anki--sync-next (org-element-property :begin element) nil))))
  ;; TODO: saving the buffer should happen in a callback
  (save-buffer))

(defun flash-insert-skeleton ()
  (interactive)
  (goto-char (point-max))

  (insert "\n")
  (evil-insert 0)
  (yas-expand-snippet (yas-lookup-snippet "flashcard"))
  
  (unless (car (org-property-values flash-anki-prop-deck))
    (org-set-property flash-anki-prop-deck "Default")))

(defun flash-select-note ()
  (interactive)

  (let ((deck-to-path nil))
    (dolist (file (directory-files flash-directory t ".*\\.org"))
      (let ((buffer (find-file-noselect file)))
	(with-current-buffer buffer
	  (let ((deck (org-property-values flash-anki-prop-deck)))
	    (if deck
		(push `(,(car deck) . ,file) deck-to-path))))))

    (let* ((sort-by-name (lambda (a b) (string< (car a) (car b))))
	   (deck (completing-read " " (sort deck-to-path sort-by-name))))
      (if deck
	  (find-file (cdr (assoc deck deck-to-path)))))))

(defun flash-create-note (name)
  (interactive "sName: ")

  (let ((path (s-concat flash-directory "/" (s-snake-case name) ".org")))
    (save-excursion
      (find-file-other-window path)
      (insert ":PROPERTIES:\n")
      (insert (s-concat ":ANKI_DECK: " name "\n"))
      (insert ":END:\n")
      (insert (s-concat "#+TITLE: " name "\n\n"))
      (flash-mode))

    (insert (s-concat "[[" path "][Flash: " name "]]"))))

;;;###autoload
(defvar flash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'flash-anki-sync)
    (define-key map (kbd "C-c f s") #'flash-anki-sync-note-at-point)
    (define-key map (kbd "C-c C-c") #'flash-insert-skeleton)
    map))

(define-minor-mode flash-mode
  "flash-mode"
  :init-value nil
  :lighter "flash"
  :keymap flash-mode-map)

(provide 'flash)
