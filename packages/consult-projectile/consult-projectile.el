;;; consult-projectile.el --- Consult integration for porjectile  -*- lexical-binding: t; -*-

;;; Code:

(require 'projectile)
(require 'consult)

(defface consult-projectile-projects
  '((t :inherit font-lock-constant-face))
  "Face used to highlight projects in `consult-projectile'."
  :group 'consult-projectile)

(defvar consult-projectile--project-history nil)

(defvar consult-projectile-display-info t
  "Settings to let `consult-projectile' display project
   information in the annotation.")

(defcustom consult-projectile-sources
  '(consult-projectile--source-projectile-buffer
    consult-projectile--source-projectile-file
    consult-projectile--source-projectile-project)
  "Sources used by `consult-projectile'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'consult-projectile)

(defun consult-projectile--choose-file (root)
  "Create the list of files for the consult chooser based on
   projectile's notion of files for the project at ROOT."
  (let* ((inv-root (propertize root 'invisible t))
         (projectile-enable-caching nil) 
         (files (projectile-project-files root)))
    (mapcar (lambda (f) (concat inv-root f)) files)))

(defun consult-projectile--file (selected-project)
  "Create a view for selecting project files for the project at SELECTED-PROJECT."
  (find-file (consult--read
              (consult-projectile--choose-file selected-project)
              :prompt "Project File: "
              :sort t
              :require-match t
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))

(defvar consult-projectile--source-projectile-buffer
      `(:name      "Project Buffer"
        :narrow    (?b . "Buffer")
        :category  buffer
        :face      consult-buffer
        :history   buffer-name-history
        :state     ,#'consult--buffer-state
        :enabled   ,#'projectile-project-root
        :items
        ,(lambda ()
           (when-let (root (projectile-project-root))
             (mapcar #'buffer-name
                     (seq-filter (lambda (x)
                                   (when-let (file (buffer-file-name x))
                                     (string-prefix-p root file)))
                                 (consult--buffer-query :sort 'visibility)))))))

(defvar consult-projectile--source-projectile-file
      `(:name      "Project File"
        :narrow    (?f . "File")
        :category  file
        :face      consult-file
        :history   file-name-history
        :action    ,(lambda (f) (consult--file-action (concat (projectile-acquire-root) f)))
        :enabled   ,#'projectile-project-root
        :items
        ,(lambda ()
           (projectile-project-files (projectile-acquire-root)))))


(defvar consult-projectile--source-projectile-project
      `(:name      "Known Project"
        :narrow    (?p . "Project")
        :category  'consult-projectile-project
        :face      consult-projectile-projects
        :history   consult-projectile--project-history
        :annotate  ,(lambda (dir) (if consult-projectile-display-info (progn
                                                                        (format "Project: %s [%s]"
                                                                                (projectile-project-name dir)
                                                                                (projectile-project-vcs dir)))))
        :action    ,#'consult-projectile--file
        :items     ,#'projectile-relevant-known-projects))

;;;###autoload
(defun consult-projectile ()
  "Create a multi view with projectile integration.
   Displays known projects when there are none or the buffers/files accociated
   with the project."
  (interactive)
  (when-let (buffer (consult--multi consult-projectile-sources
                                    :prompt "Switch to: "
                                    :history 'consult-projectile--project-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))


(provide 'consult-projectile)
;;; consult-projectile.el ends here
