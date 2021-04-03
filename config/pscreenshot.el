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
	(print event)
	(let ((org-inline-image-overlays t))
	  (if (equal event "finished\n")
	    (insert (format "#+ATTR_ORG: :width %s\n" ,width)))
	  (pscreenshot/process-done
	   process event ,file ,(current-buffer) nil ',last-input-event))))))

(defun pscreenshot/process-done (process event file orig-buffer orig-delay orig-event)
  (setq org-screenshot-process nil)
  (with-current-buffer (process-buffer process) 
    (if (not (equal event "finished\n"))
        (progn 
          (insert event) 
          (cond ((save-excursion
                   (goto-char (point-min))
                   (re-search-forward "Key was pressed" nil t))
                 (ding)
                 (message "Key was pressed, screenshot aborted"))
                (t 
                 (display-buffer (process-buffer process))
                 (message "Error running \"scrot\" program")
                 (ding))))
      (with-current-buffer orig-buffer 
        (let ((link (format "[[file:%s]]" file))) 
          (setq org-screenshot-last-file (file-name-nondirectory file))
          (let ((beg (point)))
            (insert link) 
            (when org-inline-image-overlays
              (org-display-inline-images nil t beg (point))))
          (unless (< orig-delay 3)
            (ding))
          (org-screenshot-rotate-continue t orig-event))))))


(provide 'pscreenshot)
