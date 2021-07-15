(defun +work/apply-changes ()
  (interactive)
  (+work/kubernetes-apply-changes))

(defun +work/kubernetes-apply-changes ()
  (interactive)

  (kill-region (point-min) (point-max))
  (kill-buffer)
  (let* ((raw (pop kill-ring))
         (images (s-split "\n" raw)))

    (dolist (image images)
      (let* ((image-and-tag (s-split ":" image))
             (image (car image-and-tag))
             (tag (car (cdr image-and-tag))))
        (beginning-of-buffer)
        (if (search-forward image nil t)
	  (progn 
            ;; update tag for existing image
            (next-line)
            (beginning-of-line)
            (kill-line)
            (insert (concat "    newTag: " tag)))
	  (progn
	    (beginning-of-buffer)
	    (search-forward "images:")
	    (insert (concat "\n"
			    "  - name: " image
			    "\n"
			    "    newTag: " tag)))))))
  (save-buffer))

(defun +work/swarm-apply-changes ()
  (interactive)

  (kill-region (point-min) (point-max))
  (kill-buffer)
  (let* ((raw (pop kill-ring))
         (images (s-split "\n" raw)))

    (dolist (image images)
      (let* ((image-and-tag (s-split ":" image))
             (image (car image-and-tag))
             (tag (car (cdr image-and-tag))))
        (beginning-of-buffer)
        (if (search-forward image nil t)
	  (kill-line (insert (concat ":" tag)))))))
  (save-buffer))

(defvar +work/update-versions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+work/apply-changes)
    map))

(define-minor-mode +work/update-versions-mode
  "Update versions mode."
  :init-value nil
  :global nil
  :keymap +work/update-versions-mode-map)

(defun +work/update-versions ()
  (interactive)

  (switch-to-buffer "*update-versions*")
  (+work/update-versions-mode))
