;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

(setenv "LSP_USE_PLISTS" "true")

(require 'seq)
(require 'cl-lib)

(defun seq-empty-p-safe (sequence)
  "Safely check if SEQUENCE is empty, handling unexpected types.
This handles edge cases where seq-empty-p receives unexpected argument types
like symbols, improper lists, or other non-sequence objects."

  (cond
   ;; nil is empty
   ((null sequence) t)
   ;; Symbols are not sequences, treat as non-empty
   ((symbolp sequence) nil)
   ;; Proper lists
   ((listp sequence) (null sequence))
   ;; Strings
   ((stringp sequence) (= (length sequence) 0))
   ;; Vectors and other sequences
   ((sequencep sequence)
    (condition-case nil
        (= (length sequence) 0)
      (error nil)))
   ;; Anything else is not a sequence, treat as non-empty
   (t nil)))

;; Override seq-empty-p completely to handle all edge cases
(defun seq-empty-p--patched (orig-fun sequence)
  "Wrapper for seq-empty-p to handle Emacs 30 edge cases.
Falls back to safe implementation when the original fails."
  (condition-case err
      (funcall orig-fun sequence)
    (cl-no-applicable-method
     (seq-empty-p-safe sequence))
    (wrong-type-argument
     (seq-empty-p-safe sequence))
    (error
     (seq-empty-p-safe sequence))))

(advice-add 'seq-empty-p :around #'seq-empty-p--patched)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
;;; early-init.el ends here
