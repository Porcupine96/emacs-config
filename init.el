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

(org-babel-load-file "~/my-emacs/configuration.org")

;; ------------------------------ GENERATED ------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cc63dade39a93f82519c9eef9cbf4ba7d4378a81f46250ccfa5a29119b96e253" "7f8b4cbf5d20b71daf43378c41471130412b8fcfc3380c371d9ebb5b27416ba5" default))
 '(elfeed-search-remain-on-entry t)
 '(head-hint nil t)
 '(hydra-key-format-spec "[%s]")
 '(olivetti-body-width 90)
 '(olivetti-recall-visual-line-mode-entry-state t)
 '(org-agenda-files
   '("~/Dropbox/org/todo/private.org" "~/Dropbox/org/todo/studies.org" "~/Dropbox/org/todo/work.org"))
 '(package-selected-packages
   '(treemacs-icons-dired org-roam-server org-fancy-priorities prettier-js prettier-emacs scratch clojurescript-mode clojure-mode ob-ipython language-detection jiralib2 request ox-jira quelpa-use-package quelpa ejira hide-mode-line modus-operandi synosaurus reason-mode org-roam flycheck-haskell haskell-mode eval-expr ivy-avy auctex ob-ammonite evil-smartparens smarparens ob-async dired zoom-window yasnippet yaml-mode which-key use-package treemacs-projectile treemacs-evil string-inflection smex smartparens scala-mode sbt-mode pyvenv python-mode pyenv-mode protobuf-mode perspective ox-reveal org-super-agenda org-ref org-re-reveal org-plus-contrib org-bullets olivetti modus-operandi-theme lsp-ui lsp-python-ms lsp-metals lsp-ivy keychain-environment jupyter ivy-posframe helpful git-timemachine general forge flycheck evil-surround evil-snipe evil-org evil-magit evil-easymotion evil-commentary evil-collection emojify elfeed-goodies doom-modeline dockerfile-mode dired-subtree csv-mode counsel-projectile company-lsp blacken bind-map auto-compile academic-phrases))
 '(safe-local-variable-values
   '((projectile-project-root-files-functions projectile-root-bottom-up)
     (lsp-python-ms-extra-paths quote
				("/home/porcupine/codeheroes/chatbotize/monorepo/ai/ai-intent-backend" "/home/porcupine/codeheroes/chatbotize/monorepo/ai/ai-intent-backend/src"))
     (lsp-python-ms-extra-paths "/home/porcupine/.pyenv/versions/master/lib/python3.7/site-packages")))
 '(window-divider-mode nil)
 '(zoom-window-mode-line-color "#4682B4"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
