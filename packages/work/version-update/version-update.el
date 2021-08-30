(require 's)

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
        (goto-char (point-min))
        (if (search-forward image nil t)
	  (progn 
            ;; update tag for existing image
            (forward-line)
            (beginning-of-line)
            (kill-line)
            (insert (concat "    newTag: " tag)))
	  (progn
            (goto-char (point-min))
	    (if (search-forward "images:" nil t)
		(+work/kubernetes--insert-new-image image tag)
	      (progn
		(goto-char (point-max))
		(insert "images:")
		(+work/kubernetes--insert-new-image image tag))))))))
  (save-buffer))

(defun +work/kubernetes--insert-new-image (image tag)
  (insert (concat "\n"
			    "  - name: " image
			    "\n"
			    "    newTag: " tag)))

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
        (goto-char (point-min))
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
