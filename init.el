;; Configure package.el to include MELPA.

(customize-set-value 'straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(when (and (fboundp 'loaddefs-generate)
           (>= emacs-major-version 30))
  (defun loaddefs-generate--wrapper (orig-fun dir output-file &rest args)
    "Wrapper for loaddefs-generate to handle Emacs 30 compatibility."
    (condition-case err
        (apply orig-fun dir output-file args)
      (error
       (message "Warning: loaddefs-generate failed: %S, using fallback" err)
       (when (fboundp 'make-directory-autoloads)
         (make-directory-autoloads dir output-file)))))
  (advice-add 'loaddefs-generate :around #'loaddefs-generate--wrapper))

(straight-use-package 'use-package)

(use-package org
  :straight t)

(defun get-modify-time (path)
  (time-convert (file-attribute-modification-time (file-attributes path)) 'integer))

(if (let* ((org-modify-time (get-modify-time "~/.emacs.d/configuration.org"))
	   (el-modify-time (get-modify-time "~/.emacs.d/configuration.el")))
      (< el-modify-time org-modify-time))
    (progn
      (org-babel-tangle-file "~/.emacs.d/configuration.org" "~/.emacs.d/configuration.el")))


(load-file "~/.emacs.d/configuration.el")
(put 'narrow-to-region 'disabled nil)
