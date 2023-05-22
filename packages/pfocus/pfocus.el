(defconst p/header-line-empty '(""))

(defvar p/header-line-format p/header-line-empty)

(defun p/focus-mode-enable ()
   (setq p/header-line-format header-line-format)
   (setq header-line-format p/header-line-empty)
   (hide-mode-line-mode))

(defun p/focus-mode-disable ()
  (hide-mode-line-mode -1)

  (if (and p/header-line-format (not (equal p/header-line-format p/header-line-empty)))
    (setq header-line-format p/header-line-format))

  (setq p/header-line-format p/header-line-empty))

(define-minor-mode p/focus-mode
  "Focus mode."
  :init-value nil
  :global 't
  (if p/focus-mode
      (p/focus-mode-enable)
      (p/focus-mode-disable)))
