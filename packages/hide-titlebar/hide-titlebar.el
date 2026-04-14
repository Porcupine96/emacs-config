;;; hide-titlebar.el --- Hide macOS title bar buttons -*- lexical-binding: t; -*-

(defvar hide-titlebar--dir
  (file-name-directory (or load-file-name
                           (expand-file-name "~/.emacs.d/packages/hide-titlebar/"))))

(defvar hide-titlebar--module-loaded nil)

(defun hide-titlebar--load-module ()
  "Load the native module if not already loaded."
  (unless hide-titlebar--module-loaded
    (module-load (expand-file-name "hide-titlebar.dylib" hide-titlebar--dir))
    (setq hide-titlebar--module-loaded t)))

;;;###autoload
(defun hide-titlebar ()
  "Hide title bar buttons and make titlebar transparent."
  (interactive)
  (hide-titlebar--load-module)
  (hide-titlebar--do-hide))

(provide 'hide-titlebar)
;;; hide-titlebar.el ends here
