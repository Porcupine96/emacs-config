;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize))

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


(defun get-modify-time (path)
  (time-convert (file-attribute-modification-time (file-attributes path)) 'integer))


(if (let* ((org-modify-time (get-modify-time "~/.emacs.default/configuration.org"))
	   (el-modify-time (get-modify-time "~/.emacs.default/configuration.el")))
      (< el-modify-time org-modify-time))
    (progn
      (require 'org)
      (org-babel-tangle-file "~/.emacs.default/configuration.org" "~/.emacs.default/configuration.el")))


(load-file "~/.emacs.default/configuration.el")

;; (org-babel-load-file "~/.emacs.default/configuration.org")
