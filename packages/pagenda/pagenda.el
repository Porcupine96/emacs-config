;;; agenda.el --- description -*- lexical-binding: t; -*-

(require 'org-agenda)
(require 'org-super-agenda)
(require 'face-remap)
(require 'winner)
;; (require 'svg-lib)

(defvar-local pagenda-transforms nil "A list of faces and their associated specs.")
(defcustom pagenda-font "Roboto Mono Light for Powerline" "The font to use in an elegant agenda buffer")

(defvar pagenda-face-remappings
  (let ((face-height (face-attribute 'default :height)))
    (list
     (list 'default (list :family pagenda-font
                          :height (ceiling (* face-height 1.25)) :weight 'thin))
     (list 'header-line (list :family pagenda-font
                              :height (* face-height 1.5) :weight 'thin
                              :underline nil  :overline nil :box nil))
     (list 'org-agenda-date-today (list :weight 'bold))
     (list 'org-agenda-structure (list :family pagenda-font :weight 'regular))
     (list 'bold (list :family pagenda-font :height (ceiling (* face-height 1.1)) :weight 'bold))))
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
  org-deadline-warning-days 100
  org-log-done 'time
  org-agenda-block-separator 9472  ;; straight line
  org-agenda-tags-column -100
  org-agenda-compact-blocks t
  diary-show-holidays-flag nil
  org-agenda-skip-deadline-prewarning-if-scheduled nil
  org-agenda-skip-scheduled-if-deadline-is-shown t
  org-agenda-hide-tags-regexp ".*"
  org-agenda-breadcrumbs-separator " ❯ "
  org-agenda-scheduled-leaders '("" "")
  org-agenda-current-time-string "→"
  org-agenda-todo-keyword-format "%-1s"
  org-agenda-sorting-strategy '((agenda priority-down timestamp-up category-keep))
  org-agenda-prefix-format '((agenda . " %?-20b %?-10t%s")
                             (timeline . "  % s")
                             (todo . "%i %-12:c%b")
                             (tags . "%i %-12:c%b")
                             (search . " %i %-12:c"))
  org-super-agenda-keep-order t)


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
  (let* ((days-string (replace-regexp-in-string ".*In\s+\\([0-9]+\\) d\.:.*" "\\1" item))
	 (days (string-to-number (substring-no-properties days-string)))
	 (item-stripped (replace-regexp-in-string " In\s+[0-9]+ d\.:" "" item)))

    (if (equal days 0)
	(replace-regexp-in-string "Deadline:\s+" "" item)
      (s-concat (s-pad-right 70 " " item-stripped) " " (pagenda--format-days-left days)))))

(defun +agenda/show (span)
  (interactive)
  (-let* ((org-agenda-span span)
	  (org-super-agenda-groups
	   `((:discard (:todo ("DONE" "SOMEDAY" "KILL")))
	     (:name "💻 Work"
		    :transformer #'pagenda--transform
		    :and (:category "work"
				    :todo ("STRT" "TODO" "WAIT" "REVIEW")))
	     (:name "‍🏫 Studies"
		    :transformer #'pagenda--transform
		    :and (:category "studies" 
				    :todo ("PROJECT" "STRT" "TODO" "REVIEW"))
		    )
	     (:name "🌳 Private"
		    :transformer #'pagenda--transform
		    :and (:category "private" 
				    :todo ("PROJECT" "STRT" "TODO" "REVIEW"))
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
    (define-key map (kbd "t") #'pagenda-change-status)
    (define-key map (kbd "RET") #'winner-undo)
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
