(require 's)
(require 'org)
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
(defconst flash-anki-prop-deck "ANKI_DECK")


(defun flash--set-note-id (id)
  (org-set-property flash-anki-prop-note-id (number-to-string id)))

(defun flash--notes-path ()
  (pcase (org-property-values "ID")
    (`(,id . nil) (expand-file-name (s-concat id ".org") flash-directory))
    (_ (error "Couldn't find org ID property."))))

(defun flash-anki--headline-to-note (headline)
  (pcase (org-element-map headline '(headline) 'identity)
    (`(,top ,front ,back . nil)
     `(deck . ,(org-entry-get nil flash-anki-prop-deck)))
    (_ (error "Couldn't parse headline as a note."))))

(defun flash-anki--sync-note (headline)
  (print (flash-anki--headline-to-note headline)))

(defun flash-anki-sync ()
  (interactive)

  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (hl)
      (if (= (org-element-property :level hl) 1)
	 (flash-anki--sync-note hl)))))

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
