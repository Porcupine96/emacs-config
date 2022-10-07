;; Configure package.el to include MELPA.



(customize-set-value 'straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package org
  :straight t)

(defun get-modify-time (path)
  (time-convert (file-attribute-modification-time (file-attributes path)) 'integer))

(if (let* ((org-modify-time (get-modify-time "~/.emacs.default/configuration.org"))
	   (el-modify-time (get-modify-time "~/.emacs.default/configuration.el")))
      (< el-modify-time org-modify-time))
    (progn
      (org-babel-tangle-file "~/.emacs.default/configuration.org" "~/.emacs.default/configuration.el")))


(load-file "~/.emacs.default/configuration.el")


;; TODO: do I still need it?
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
