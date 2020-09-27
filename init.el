;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize))

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hide-mode-line olivetti synosaurus eval-expr string-inflection elfeed csv-mode evil-smartparens smartparens prettier-js reason-mode flycheck-haskell haskell-mode yaml-mode lsp-ivy lsp-metals lsp-ui protobuf-mode sbt-mode clojure-mode lsp-python-ms blacken pyvenv pyenv-mode python-mode dockerfile-mode dired-subtree emojify org-re-reveal ox-reveal keychain-environment helpful company zoom-window doom-modeline treemacs-icons-dired treemacs-projectile treemacs-evil treemacs git-timemachine forge evil-magit magit counsel-projectile projectile yasnippet guess-language org-fancy-priorities org-roam-server org-roam jupyter which-key use-package smex scratch perspective org-super-agenda org-ref org-plus-contrib org-bullets ob-ipython ob-ammonite modus-vivendi-theme modus-operandi-theme ivy-posframe general flycheck evil-surround evil-snipe evil-org evil-easymotion evil-commentary evil-collection counsel auto-compile academic-phrases)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
