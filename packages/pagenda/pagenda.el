;;; agenda.el --- description -*- lexical-binding: t; -*-

(require 'org-agenda)
(require 'org-super-agenda)
(require 'face-remap)
(require 'winner)

(defvar-local pagenda-transforms nil "A list of faces and their associated specs.")
(defcustom pagenda-font "Input Mono" "The font to use in an elegant agenda buffer")

(defvar pagenda-face-remappings
  (let ((face-height (face-attribute 'default :height)))
    (list
     (list 'default (list :family pagenda-font
                          :height (ceiling (* face-height 1.25)) :weight 'thin))
     (list 'header-line (list :family pagenda-font
                              :height (* face-height 1.5) :weight 'thin
                              :underline nil  :overline nil :box nil))
     (list 'org-agenda-date-today (list :weight 'bold))
     (list 'org-agenda-structure (list :weight 'regular))
     (list 'bold (list :height (ceiling (* face-height 1.1)) :weight 'thin))))
  "A list of faces and the associated specs that will be remapped
  when pagenda-mode is enabled")

(defun pagenda--file-tag (test)
  (print test)
  "tag")

(setq
  org-agenda-window-setup 'only-window
  org-agenda-show-future-repeats nil
  org-agenda-start-on-weekday nil
  org-agenda-start-day "today"
  org-deadline-warning-days 7
  org-log-done 'time
  org-agenda-block-separator 9472  ;; straight line
  org-agenda-tags-column -100
  org-agenda-compact-blocks t
  diary-show-holidays-flag nil
  org-agenda-hide-tags-regexp ".*"
  org-agenda-breadcrumbs-separator " ❯ "
  org-agenda-scheduled-leaders '("" "")
  org-agenda-current-time-string "→"
  org-agenda-todo-keyword-format "%-1s"
  org-agenda-prefix-format '((agenda . " %b %?-12t% s")
                             (timeline . "  % s")
                             (todo . "%i %-12:c%b")
                             (tags . "%i %-12:c%b")
                             (search . " %i %-12:c")))


(defun +agenda/show (span)
  (interactive)
  (-let* ((org-agenda-span span)
	  (org-super-agenda-groups
	   `((:discard (:todo ("DONE" "SOMEDAY")))
	     (:name "👔 Work"
		    :and (:category "work"
				    :todo ("STRT" "TODO" "WAIT" "REVIEW")))
	     (:name "‍📖 Studies"
		    :and (:category "studies" 
				    :todo ("PROJECT" "STRT" "TODO" "REVIEW")
				    :deadline t)
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

(defun pagenda--enable()
  (setq-local mode-line-format nil)
  (setq-local line-spacing 8)
  (org-super-agenda-mode)
  (setq pagenda-transforms
	(mapcar (lambda (face-&-spec)
		  (face-remap-add-relative (car face-&-spec) (cadr face-&-spec)))
		pagenda-face-remappings)))

(defun pagenda--disable()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq-local line-spacing (default-value 'line-spacing))
  (mapc #'face-remap-remove-relative
	pagenda-transforms))

(defvar pagenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'winner-undo)
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