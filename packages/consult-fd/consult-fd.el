(require 'consult)

(defun consult--fd-builder (input)
  (pcase-let* ((cmd (consult--build-args consult-find-args))
               (type (if (eq 0 (call-process-shell-command
                          (concat (car cmd) " -regextype emacs -version")))
                   'emacs 'basic))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg type
				      (member "--ignore-case" cmd)))
	       (command (list "fd" (car re) ".")))

    (when re
      (list :command command
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Find" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
