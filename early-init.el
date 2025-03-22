;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

(setenv "LSP_USE_PLISTS" "true")

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
;;; early-init.el ends here
