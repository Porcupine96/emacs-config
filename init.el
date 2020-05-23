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
 '(ansi-color-names-vector
   ["#292929" "#ff3333" "#aaffaa" "#aaeecc" "#aaccff" "#FF1F69" "#aadddd" "#999999"])
 '(background-color "#202020")
 '(background-mode dark)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   '("1d7b5d795dc6e49c4ee5a4a12459a8071c1510af3ed4dd37fb9d42dde128228d" "ffd928adc063eb41ef1f4a8c5776fddbcc7cddf9b67549e5275aba2c50570bda" "cfee9722f86fafb78cb8fb1b158ff55907b4fad46475013b0d674321029c7ac5" "f0b1d399130f257d454c5b9562b83f7725f19d5cc1316b93abdebefbe45de3b8" "a342ec6ecb77f12e25f6941e8328065f531a6aba8434f852f82dc5f2db229510" "f8e1dabb3a0662ed71b5a6d69ec2be8afce53e6919d54299c4d69beea47da38d" "513efa7b2d573502212db896c879112d5901cae3d4ce3cbcd75a8073166f2f21" "c63a221e6aecee6549e89d67dbec2fd269f497f6f8ef5a9bd9f74a7b093e0690" "d5d27af53d755c7aa39b856b0de53d6ac234998441248fc91ffdef80e56f578a" "a7a6fcbd34d88eb46ae4a7024535c4c080169ad1837cc891bd106640ddab66da" "3471272b1a6a8994de370f58cf14b5cdf70858f393380fc89605d8e73d231743" "e0799a8c33c3b28b02b2cbf2727a1c61d977bb7563c8960940661bf408768c0e" "2d672b5160a1da7f71b74e07bc6b4a1ff240ce892babc2b3bee4558c2ff904e7" "5e421ac2b4fb8aee1c41d6036cc10629f8ff1aa0e47fe3c89f3a710387cff468" "56f926067e36b071bdaa4314d76465001fb67299a0e627da8533d808c994a592" "0d4fe3dd22bc8b92669e1478d1d6f7887a8b5c667821e002a821a1104ab6a012" "dfe648065364b4d42be23f3c4d24d87c593216fc4b2520b6f639975b8c27294d" "0d45ddd259a6a5ad87f8a7a76e3d5382140b31d86e9dee739e7aceef8f5b18d4" "36103b50eaa957de34679ac49edbc8b38e561f87ee24e78d14b6ef235631e912" "18718c057d88474948a10394a688a7787951b1152a196cfad00978eb8730fecf" "4aac9f17d053d496cb8c2a1b0fe21d635dce99d3ec2b0627586222b7226cf120" "2a874d3ff0f0aed4e72092f6b7feef30e2df393c3ac2aabe722b716f6997e82a" "2af963c1eed87e8b859ccba491e2e01396c08789bd6c04418474830403239e3e" "6945144b0158aa2d069564f4c7c7e9ab88ea26f8052f1703a5049196aa0f0d35" "810ebaf75ba6f613d2abdd5ee7ff50c51a3cfe539481ef7df62009c9d6bf0009" "3c03b55aeb17a451e5fd23747e418f98a80db9950de203c534ac177ec32c42cf" default))
 '(doom-modeline-height 35)
 '(doom-modeline-mode t)
 '(foreground-color "#cccccc")
 '(head-hint nil t)
 '(hydra-key-format-spec "[%s]")
 '(org-fontify-done-headline t)
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "|" "DONE(d!)" "KILL(k!)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
 '(package-selected-packages
   '(elfeed-goodies emojify yaml-mode dired org-re-reveal org-reveal ox-reveal ob-jupyter jupyter elfeed bind-map org-bullets keychain-environment git-timemachine blacken dockerfile-mode pyenv-mode pyvenv pyenv lsp-ivy helpful smartparens smex request which-key synonyms synosaurus evil-collection academic-phrases org-ref evil-surround forge protobuf-mode company-mode evil-org evil-easymotion zoom-window doom-modeline evil-magit treemacs-projectile treemacs-evil evil-snipe treemacs counsel-projectile magit projectile hydra lsp-ui company-lsp lsp-mode yasnippet flycheck counsel swiper ivy perspective org-plus-contrib browse-kill-ring evil-commentary elpy python-mode evil use-package))
 '(projectile-mode t nil (projectile))
 '(pyenv-mode t)
 '(pyvenv-mode t)
 '(winner-mode t)
 '(zoom-window-mode-line-color "#4682B4"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

