(require 'org-screenshot)

(defgroup pscreenshot nil
  "pscreenshot group")

(defconst pscreenshot/image-directory "./images/")


(defun pscreenshot/generate-file-name (dir)
  (concat (number-to-string (random 10000000)) ".png"))


(defun pscreenshot/org-screenshot-take (width)
  (interactive
   (list 
    (cond 
     (current-prefix-arg (read-string "Width: "))
     (t "500"))))

  (when (and (boundp 'org-screenshot-process)
             org-screenshot-process
             (member (process-status org-screenshot-process)
                     '(run stop)))
    (error "screenshot process is still running"))
  (make-directory pscreenshot/image-directory t)
  (let* ((name (pscreenshot/generate-file-name pscreenshot/image-directory))
         (file (format "%s%s" pscreenshot/image-directory name))
         (path (expand-file-name file)))
    (when (get-buffer "*screenshot*")
      (with-current-buffer (get-buffer "*screenshot*")
	(erase-buffer)))
    (setq org-screenshot-process
          (or 
           (+ocr/take-screenshot path)
           (error "Unable to start screenshot process")))
    (when org-screenshot-process 
      (message "Click on a window, or select a rectangle..."))
    (set-process-sentinel
     org-screenshot-process
     `(lambda (process event)
	(insert (format "#+ATTR_ORG: :width %s\n" ,width))
	(let ((org-inline-image-overlays t))
	  (org-screenshot-process-done
	   process event ,file ,(current-buffer) nil ',last-input-event))))))


(provide 'pscreenshot)
