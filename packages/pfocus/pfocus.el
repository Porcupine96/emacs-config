(defun +focus/focus-mode-function ()
    (when (derived-mode-p 'scala-mode) 
      (setq olivetti-body-width 150))
    (olivetti-mode)
    (hide-mode-line-mode))

(define-minor-mode p/focus-mode
  "Focus mode."
  :init-value nil
  :global 't
  (if p/focus-mode
      (progn
    	  (+focus/focus-mode-function)
  	  (add-hook 'prog-mode-hook '+focus/focus-mode-function))
    (remove-hook 'prog-mode-hook '+focus/focus-mode-function)
    (olivetti-mode -1)
    (hide-mode-line-mode -1)))
