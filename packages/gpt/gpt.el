(require 's)
(require 'org)
(require 'json)
(require 'request)

(defun gpt-explain ()
  (interactive)
  (gpt-query "What' \"%s\"?"))

(defun gpt-define ()
  (interactive)
  (gpt-query "What does \"%s\" mean?\n\nGive a definition, some examples and a Polish translation."))

(defun gpt-query (template)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (gpt-init (format template text))))

(defun gpt-init (&optional input)
  (interactive)

  (let* ((file_id (generate-uuid))
	 (path (concat "/tmp/gpt-" file_id))
	 (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
	(insert "-*- mode: gpt-mode; -*-")
	(insert "\n---\n")
	(insert (concat "conversation_id: " "\n"))
	(insert "---\n\n")
	(insert "# Question")
	(insert "\n\n")
	(when input
	  (insert input))

	(write-file path)
	(gpt-mode))))
  

(defun gpt-on-success ()
  (message "Done!"))

(defun gpt-on-error ()
  (error "Error when processing request..."))

(defun gpt-make-request (prompt file conversation_id parent_id)
  (request "http://localhost:5000/ask"
    :type "POST"
    :data (json-encode `(("prompt" . ,prompt)
			 ("output" . ,file)
			 ("conversation_id" . ,conversation_id)
  			 ("parent_id" . ,parent_id)))
    :parser 'json-read
    :headers '(("Content-Type" . "application/json"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
		(gpt-on-success)))
    :error (cl-function
	    (lambda (&key _ &key error-thrown &allow-other-keys)
	      (gpt-on-error)))))

(defun gpt-get-conversation_id ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^conversation_id: \\(.*\\)$" nil t)
      (setq conversation_id (s-trim (substring-no-properties (match-string 1))))
      (if (string-empty-p conversation_id) nil conversation_id))))

(defun gpt-get-parent_id ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^parent_id: \\(.*\\)$" nil t)
      (setq parent_id (s-trim (substring-no-properties (match-string 1))))
      (if (string-empty-p parent_id) nil parent_id))))

(defun gpt-reset ()
  (interactive)

  (beginning-of-buffer)
  (re-search-forward "# Question" nil t)

  (let ((beg (point)))
    (markdown-next-heading)
    (previous-line 1)
    (let* ((end (point))
	   (text (s-trim (buffer-substring-no-properties beg end))))
      (gpt-init text))))

(defun gpt-format ()
  (interactive)

  (mark-whole-buffer)
  (call-interactively 'fill-paragraph)
  (write-file (buffer-file-name)))

(defun gpt-execute ()
  (interactive)

  (let* ((start (progn
		  (end-of-buffer)
		  (markdown-back-to-heading-over-code-block t)
		  (point)))
	 (end (progn
		(end-of-buffer)
		(point)))
	 (conversation_id (gpt-get-conversation_id))
	 (parent_id (gpt-get-parent_id))
         (content (buffer-substring-no-properties start end)))

    (let ((prefix "# Question"))
      (if (string-prefix-p prefix content)
	  (progn
      	    (insert "\n\n")
  	    (insert "# Answer\n\n")
	    (write-file (buffer-file-name))
	    (let ((prompt (string-trim (substring content (length prefix)))))
	       (gpt-make-request prompt (buffer-file-name) conversation_id parent_id)))
	(error "Unexpected header.")))))

(defvar gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gpt-execute)
    (define-key map (kbd "C-c r") #'gpt-reset)
    (define-key map (kbd "C-c r") #'gpt-format)
    map))

(define-derived-mode gpt-mode
  markdown-mode "gpt-mode"
  "GPT Mode"
  :lighter "gpt"
  :init-value nil
  :keymap gpt-mode-map)

(defun generate-uuid ()
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(add-hook 'gpt-mode-hook 'auto-revert-mode)
(add-hook 'gpt-mode-hook 'olivetti-mode)

(provide 'gpt)
