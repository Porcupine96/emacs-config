(require 'json)
(require 'request)


(defun memo/create-flashcard ()
  (interactive)
  (let ((word (thing-at-point 'word))
        (paragraph (thing-at-point 'paragraph)))

    (message "Creating flashcard for word: %s" word)

    (create-flashcard word paragraph)))


(defun on-success ()
  (message "Flashcard created!"))

(defun on-error ()
  (error "Failed to create the flashcard!"))

(defun create-flashcard (word excerpt)
  (request "http://127.0.0.1:8000/flashcards"
    :type "POST"
    :data (json-encode `(("word" . ,word)
			 ("excerpt" . ,excerpt)))
    :parser 'json-read
    :headers '(("Content-Type" . "application/json"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
		(on-success)))
    :error (cl-function
	    (lambda (&key _ &key error-thrown &allow-other-keys)
	      (on-error)))))


(provide 'memo)
