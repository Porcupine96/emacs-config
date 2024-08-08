;;; agenda.el --- description -*- lexical-binding: t; -*-

(require 'org-agenda)
(require 'org-super-agenda)
(require 'face-remap)
(require 'winner)


(setq
  org-agenda-window-setup 'only-window
  org-agenda-show-future-repeats nil
  org-agenda-start-on-weekday nil
  org-agenda-start-day "today"
  org-deadline-warning-days 100
  org-log-done 'time
  org-agenda-compact-blocks t
  diary-show-holidays-flag nil
  org-agenda-skip-deadline-prewarning-if-scheduled t
  org-agenda-skip-scheduled-if-deadline-is-shown t
  org-agenda-hide-tags-regexp ".*"
  org-agenda-breadcrumbs-separator " ❯ "
  org-agenda-scheduled-leaders '("" "")
  org-agenda-current-time-string "→"
  org-agenda-todo-keyword-format "%-1s"
  org-super-agenda-final-group-separator "\n"
  org-agenda-sorting-strategy '((agenda priority-down user-defined-down))
  org-agenda-prefix-format '((agenda . " %?-2i %t ")
                             (todo . " %-12:c")
                             (tags . " %-12:c")
                             (search . " %-12:c"))
  org-super-agenda-keep-order t
  org-agenda-cmp-user-defined #'org-agenda-cmp-user-defined)


(defun org-agenda-cmp-user-defined (a b)
  (let* ((a-todo (get-text-property 0 'todo-state a))
	 (b-todo (get-text-property 0 'todo-state b))
	 (todo-order '(("STRT" . 1)
		       ("TODO" . 2)
		       ("WAIT" . 3)
		       ("REVIEW" . 4)
		       ("DONE" . 5)))

	 (a-todo-prio (cdr (assoc a-todo todo-order)))
	 (b-todo-prio (cdr (assoc b-todo todo-order))))

    (cond
     ((= a-todo-prio b-todo-prio) 0)
     ((> a-todo-prio b-todo-prio) -1)
     ((< a-todo-prio b-todo-prio) 1))))

(defun pagenda-change-status ()
  (interactive)
  (org-agenda-todo))

(defun pagenda--format-days-left (days)
  (let* ((format (cond ((eql days 1) '(:foreground "#ff5555" :weight bold))
 		      ((<= days 3) '(:foreground  "yellow" :weight bold))
		      (t '(:foreground  "white"))))
	(label (s-concat (number-to-string days) " " (if (equal days 1) "day" " days") " left")))
    (propertize label 'face format)))

(defun pagenda--transform (item)
  ;; (print item)
  item)

(defun +agenda/show (span)
  (interactive)
  (-let* ((org-agenda-span span)
	  (org-super-agenda-groups
	   `((:discard (:todo ("SOMEDAY" "KILL") :scheduled nil))
	     (:name "👨‍💻 Work \n"
		    :transformer #'pagenda--transform
		    :and (:category "work"
				    :todo ("STRT" "TODO" "WAIT" "REVIEW" "DONE")))
	     (:name "🎓 Studies \n"
		    :transformer #'pagenda--transform
		    :and (:category "studies" 
				    :todo ("PROJECT" "STRT" "TODO" "REVIEW" "DONE"))
		    )
	     (:name "🦔 Private \n"
		    :transformer #'pagenda--transform
		    :and (:category "private" 
				    :todo ("PROJECT" "STRT" "TODO" "REVIEW" "DONE"))
		    ))))
    (org-agenda nil "a")
    (scroll-down)))

(defun +agenda/weekly-agenda ()
    (interactive)
    (+agenda/show 8))

(defun +agenda/daily-agenda ()
    (interactive)
    (+agenda/show 1))

(defun +agenda/filter-by-tag ()
    (interactive)
    (org-tags-view t nil))

(defun +agenda/filter-by-tag-hot ()
    (interactive)
    (org-tags-view t "@hot"))

(defun pagenda/save-at-point ()
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
	             (error "Invalid marker!")))
	 (buffer (marker-buffer marker)))

    (with-current-buffer buffer)
       (save-buffer)))

(defun pagenda--enable()
  (setq-local mode-line-format nil)
  (setq-local line-spacing 8)
  (org-super-agenda-mode))


(defun pagenda--disable()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq-local line-spacing (default-value 'line-spacing)))


(defvar pagenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'winner-undo)
    (define-key map (kbd "t") #'pagenda-change-status)
    (define-key map (kbd "RET") #'winner-undo)
    (define-key map (kbd "C-k") #'org-agenda-earlier)
    (define-key map (kbd "C-j") #'org-agenda-later)
    map))

(define-minor-mode pagenda-mode
  "pagenda-mode"
  :init-value nil
  :lighter "pagenda"
  :keymap pagenda-mode-map
  (if pagenda-mode
      (pagenda--enable)
    (pagenda--disable))
  (force-window-update (current-buffer)))

(provide 'pagenda)
;;; pagenda.el ends here
