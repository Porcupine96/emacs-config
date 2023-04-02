(require 's)
(require 'org)
(require 'json)
(require 'request)

(defun gpt-explain ()
  (interactive)

  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (gpt-init)
    (insert
     (concat
      "What's \""
      text
      "\"" ))))

(defun gpt-init ()
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
