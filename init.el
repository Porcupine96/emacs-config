;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("11bb7a244fa3edf4d884aa2e1996e5646d8f0ec39811222884e00990044ef890" "436a604a458968224d520c4f09dd63d5c7a1c9e8500b206405c59fd72e199a8b" "38479f06147afd39cb47fc7b566be9489ce27c6b3da63236b37c9c49021b3c20" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(jupyter zoom-window yasnippet yaml-mode wucuo which-key vterm vertico use-package typescript-mode treemacs-projectile treemacs-icons-dired treemacs-evil synosaurus string-inflection selectrum-prescient scratch sbt-mode rust-mode request reason-mode rainbow-mode pyvenv python-mode pyenv-mode protobuf-mode prettier-js perspective origami org-superstar org-super-agenda org-roam-server org-ref org-plus-contrib org-journal org-fancy-priorities orderless olivetti ob-http ob-async ob-ammonite modus-themes marginalia lsp-ui lsp-pyright lsp-metals langtool keychain-environment hide-mode-line helpful graphql-mode git-timemachine general geiser forge flycheck-haskell fish-mode evil-surround evil-snipe evil-smartparens evil-org evil-easymotion evil-commentary evil-collection eval-expr eros emojify embark-consult elfeed doom-modeline dockerfile-mode dired-subtree delight csv-mode consult-lsp consult-flycheck company-box clojure-mode calfw-org calfw blacken auto-compile academic-phrases))
 '(safe-local-variable-values
   '((eval setq projectile-project-compilation-cmd "make" lsp-pyright-extra-paths
	   ["/home/porcupine/work/monorepo/ai/ai-supervised-clustering-backend/" "/home/porcupine/work/monorepo/ai/ai-supervised-clustering-backend/generated/"])
     (eval setq lsp-pyright-extra-paths
	   ["/home/porcupine/work/monorepo/ai/ai-supervised-clustering-backend/" "/home/porcupine/work/monorepo/ai/ai-supervised-clustering-backend/generated/"])
     (eval setq lsp-pyright-extra-paths
	   ["/home/porcupine/work/monorepo/ai/ai-supervised-clustering-backend"])
     (eval setq lsp-pyright-extra-paths
	   [default-directory])
     (eval setq lsp-pyright-extra-paths
	   [(default-directory)])
     (eval set
	   (setq lsp-pyright-extra-paths
		 [(default-directory)]))
     (eval set
	   (set lsp-pyright-extra-paths
		[(default-directory)]))))
 '(warning-suppress-log-types '((comp) (:warning)))
 '(zoom-window-mode-line-color "#4682B4"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
(put 'narrow-to-region 'disabled nil)
