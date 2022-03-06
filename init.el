;; Configure package.el to include MELPA.


(require 'ob-tangle) 

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
