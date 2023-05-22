;;; pgraphql.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'lsp) ;; TODO: here?
(require 'dash) ;; TODO: here?


(defvar p/profile-path "/home/porcupine/work/gql/first")

(defun p/graphql-init-lsp ()
  "Intialize lsp for graphql.
Requires graphql-language-service-cli (yarn global add
graphql-language-service-cli)."
  (require 'lsp)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tcp-connection (lambda (port) `("graphql-lsp" "server" "-m" "socket" "-p" ,(number-to-string port))))
		    :major-modes '(graphql-mode)
		    :initialization-options (lambda () `())
		    :server-id 'graphql))
  (add-to-list 'lsp-language-id-configuration '(graphql-mode . "graphql")))



(defun p/graphql-stubgen ()
  "Generate stub for graphql method."
  (interactive)

  (let* ((queries (json-read-file (concat p/profile-path "/.queries.json")))
         (mutations (json-read-file (concat p/profile-path "/.mutations.json")))
	 (operations (cl-concatenate 'list queries mutations))
         (names (-map (lambda (el) (alist-get 'name  el)) operations))
         (selected-name  (completing-read "Operation: " names))
         (idx  (-find-index (lambda (el) (s-equals? el selected-name)) names))
         (entry (elt operations idx)))

    (print entry)))
    ;; (find-file (j/graphql--save-entry-stub entry))))

(provide 'pgraphql)
;;; pgraphql.el ends here
