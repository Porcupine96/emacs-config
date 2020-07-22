;;; porcupine-dark.el
;;; Commentary
;;; Code

(defgroup porcupine-theme ()
  "Porcupine's dark ttheme"
  :group 'faces
  :prefix "porcupine-theme-"
  :tag "Porcupine Dark")

(eval-and-compile
  (defconst porcupine-dark-theme-default-colors-alist
    '(
      ("bg-main" . "#202020") ("fg-main" "#ffffff"))))


;;; library provides
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'porcupine-dark)

(provide 'porcupine-dark-theme)


;;; porcupine-dark.el ends here
