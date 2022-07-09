(require 's)

(defun +work/apply-changes ()
  (interactive)

  (if (s-contains? "dev-cluster-conf" default-directory)
    (+work/kubernetes-apply-changes)
    (+work/swarm-apply-changes)))

(defun +work/kubernetes-apply-changes ()
  (interactive)

  (kill-region (point-min) (point-max))
  (kill-buffer)

  (let* ((images-raw (pop kill-ring)))
    (call-process "docker-bump" nil 0 nil images-raw)))

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

(defun +work/sync-env ()
  (interactive)

  (let ((env (cadr (reverse (s-split "/" (buffer-file-name)))))) 
    (vterm)
    (vterm-send-string "~/work/dev-cluster-conf")
    (vterm-send-return)
    (vterm-clear)
    (vterm-send-string (s-concat "./scripts/cli/zowiecli environment sync-diff " env))
    (vterm-send-return)))
