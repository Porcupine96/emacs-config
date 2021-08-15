(require 'consult)

(defun consult--fd-builder (input)
  (pcase-let* ((cmd (split-string-and-unquote consult-find-args))
               (type (consult--find-regexp-type (car cmd)))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg type))
	       (command (list "fd" (car re) ".")))

    (when re
      (list :command command
            :highlight hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Find" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
