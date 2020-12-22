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

(setq comp-deferred-compilation-black-list
      '("jupyter-channel" "sp-show--pair-function"))

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "#ffffff"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   '("30fc0baa1abdf48212472ce64ce859596479a0db5729c4ee9ac16ab1233b94fe" "5df493eecdc91a0e88a9758daf68adb7340e7749fa4048713c30158511931184" "2e942be1f563eb7ecf867a44b35752a04cd3514acad5e25f4dce119e53c13556" "df6208e35f983c139d6f282ee69f8f8d9eadce6a46eb4acdce00bfb0001f03ae" "7786b685f50c5b5a2abb6c9f2c878026262adafd2bccaed1277811c32227f4fd" "b72ffe34e9ff6ec347cb8fc86d3f214e999363d46022e784324f2a4fe60dcff4" "d84b567e3720b90cf5f49d08f4bfd0162f8fa5fb10a1338cccac265587dfe380" "f521183a4262608a6479712d0ccf05221059d0d727e57a5bb5b4de0b0ac5d049" "6e1f2bb439c8da4dd9d85edcb79a8550e9470037fc678ad3a2a3f4e743a99f95" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "39b0c917e910f32f43f7849d07b36a2578370a2d101988ea91292f9087f28470" "cd2ba66adad28a2a4a5a51475903cbda5db89a97589b06b6da79fcdb98363638" "4949744a1febe1a2fde86941fa3418a4a6ec2ef1e7a696d83e23540240e1d40e" "6d635c969efffcd54d87700dd4439f79aea0e1ac164312d01c51417921bec297" "7f8b4cbf5d20b71daf43378c41471130412b8fcfc3380c371d9ebb5b27416ba5" "228367014b0abc4b8afe6244d3f4652aaeaac1dddbb5582c2432221e7dd05d98" "c771affc8912ccee6fc82793c31f7537063600f42b24ac8fceb55a2624035571" "4cca64aa698f103d45f4fb19bf9ac845146f08589ff29a923fd98cd3a1cce8c0" "cabf516afbc0b21ec891311714ad776a97dbae16bd0598e0a07917c6c1998163" default))
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(foreground-color "#cccccc")
 '(helm-minibuffer-history-key "M-p")
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(olivetti-body-width 90)
 '(olivetti-recall-visual-line-mode-entry-state t)
 '(package-selected-packages
   '(pdf-continuous-scroll-mode quelpa-use-package quelpa vterm typescript-mode rust-mode webkit evil-collection-webkit graphql-mode c-mode company-box emacs-lisp emacs-lisp-mode parrot org-roam-bibtex ivy-bibtex dired ob-http calfw-org calfw bufler hide-mode-line olivetti synosaurus eval-expr string-inflection elfeed csv-mode evil-smartparens smartparens prettier-js reason-mode flycheck-haskell haskell-mode yaml-mode lsp-ivy lsp-metals lsp-ui protobuf-mode sbt-mode clojure-mode lsp-python-ms blacken pyvenv pyenv-mode python-mode dockerfile-mode dired-subtree emojify org-re-reveal ox-reveal keychain-environment helpful company zoom-window doom-modeline treemacs-icons-dired treemacs-projectile treemacs-evil treemacs git-timemachine forge evil-magit magit counsel-projectile projectile yasnippet guess-language org-fancy-priorities org-roam-server org-roam jupyter which-key use-package smex scratch perspective org-super-agenda org-ref org-plus-contrib org-bullets ob-ipython ob-ammonite modus-vivendi-theme modus-operandi-theme ivy-posframe general flycheck evil-surround evil-snipe evil-org evil-easymotion evil-commentary evil-collection counsel auto-compile academic-phrases))
 '(pyenv-mode t)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
	   `((,(concat "("
		       (regexp-opt
			'("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
			t)
		       "\\_>")
	      1 'font-lock-variable-name-face)))))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#f4923b")
     (100 . "#eecc00")
     (120 . "#cfdf30")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#80d200")
     (220 . "#6ae4b9")
     (240 . "#4ae8fc")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp)))
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "#ffffff"])
 '(zoom-window-mode-line-color "#4682B4"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
