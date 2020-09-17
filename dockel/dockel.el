;;; docker.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; Usage:

;; TODO: ...

;;; Code:
(require 'dash)
(require 'transient)

(defgroup dockel nil
  "Managing Docker & Docker Swarm from within Emacs."
  :group 'dockel
  :prefix "dockel-")

(defvar dockel--service-ls-format-fields
  '(("Name" 70 t)
    ("Status" 20 nil)
    ("Mode" 30 nil)
    ("Replicas" 10 t)
    ("Image" 100 t)
    ("RunningFor" 30 nil)
    ("Ports" 30 nil))
  "Fields and column widths for docker service ls.")

(defvar dockel--ps-format-fields
  '(("ID" 30 t)
    ("Image" 70 t)
    ("Status" 20 nil)
    ("Ports" 30 nil))
  "Fields and column widths for docker ps.")


;; TODO: make it easy to configure
(defvar unix-socket "unix:///tmp/docker.sock")

(defvar dockel--socket-process "*dockel-socket*")

(defvar dockel--socket-buffer "*dockel-socket*")

(defvar dockel--socket-connected nil)

(defvar dockel-view nil)

(defun dockel--is-list-view ()
   "Return non-nil if this is the service or container list view."
   (equal dockel-view "list"))

(defun dockel--list-format (fields)
  (-map
   (-lambda ((name width _use)) (list name width))
   (-filter (-lambda ((_name _width use)) use) fields))) 


;; TODO: 
;; (defvar dockel--ssh-host "cbt-stage-jump")

(defvar dockel--ssh-host "cbt-dev-manager")
(defvar dockel--local-socket-file "/tmp/docker.sock")
(defvar dockel--ssh-socket-file "/var/run/docker.sock")


(defun dockel--forward-socket ()
  (interactive)
  (let ((forward-mapping (format "%s:%s"
				 dockel--local-socket-file
				 dockel--ssh-socket-file)))
	(start-process
	 dockel--socket-process
	 dockel--socket-buffer
	 "ssh" "-NT" "-L" forward-mapping dockel--ssh-host)))


(defun dockel--close-socket ()
  (interactive)
  (setq kill-buffer-query-functions nil)
  (kill-buffer dockel--socket-buffer)
  (delete-file dockel--local-socket-file))

;; transient commands

(define-transient-command dockel-service-popup ()
  ["Actions"
   ("p" "docker service ps")])

(define-transient-command dockel-exec-popup ()
  ["Arguments"
   ("-c" "Command" "--command=")]
  ["Actions"
   ("e" "Execute")])

(define-transient-command dockel-log-popup ()
  "Dockel Log Menu"
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-n" "Tail" "--tail=")]
  ["Actions"
   ("l" "Tail container logs" dockel-get-logs)])

;; public 

(defun dockel-get-logs (&optional args)
  (interactive
   (list (transient-args 'dockel-log-popup)))
  (let* ((container-id (tabulated-list-get-id))
	 (process-buffer (format "*dockel-logs: %s*" container-id))
	 (async nil))
    (when (member "-f" args)
      (setq async t))
    (if async
	(start-process process-buffer process-buffer "docker" "-H" "unix:///tmp/docker.sock" "service" "logs" "--follow" container-id)
      (call-process "docker" nil process-buffer nil "-H" "unix:///tmp/docker.sock" "service" "logs" container-id))
    (with-current-buffer process-buffer
      (switch-to-buffer process-buffer)
      (buffer-disable-undo))))


(defun dockel-exec ()
  (interactive)
  (message "Unimplemented"))

;; mode map
(defvar dockel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'dockel-log-popup)
    map)
  "Keymap for `dockel-mode`'")


(defun dockel--ps-cmd (host)
  (let ((docker-fmt 
	 (concat "\""
		 (string-join (-map (-lambda ((name _width)) (format "{{.%s}}" name)) (dockel--list-format dockel--ps-format-fields)) " ")
		 "\"")))
    (string-join (list "docker" "-H" host "ps" "--format" docker-fmt) " ")))

(defun dockel--service-ls-cmd (host)
  (let ((docker-fmt 
	 (concat "\""
		 (string-join (-map (-lambda ((name _width)) (format "{{.%s}}" name)) (dockel--list-format dockel--service-ls-format-fields)) " ")
		 "\"")))
    (string-join (list "docker" "-H" host "service" "ls" "--format" docker-fmt) " ")))


(defun dockel--list-view (command fields)
  (let* ((raw (shell-command-to-string command))
	 (columns (seq-into (dockel--list-format fields) 'vector))
	 (rows (seq-map 
		(lambda (row) (list (car row) (seq-into row 'vector)))
		(seq-partition (split-string raw)
			       (length (dockel--list-format fields))))))
    (buffer-disable-undo)
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)

    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))


(defun dockel-service-ls ()
  (interactive)
  (dockel--list-view (dockel--service-ls-cmd unix-socket) dockel--service-ls-format-fields))


(defun dockel-ps ()
  (interactive)
  (dockel--list-view (dockel--ps-cmd unix-socket) dockel--ps-format-fields))


;; dockel-evil ;; TODO: move to a separate file / package
(require 'evil)

(defgroup dockel-evil nil
  "Provides integration of dockel and evil."
  :group 'dockel
  :prefix "dockel-evil-")

(defvar dockel-evil-mode-map (make-sparse-keymap))

(define-minor-mode dockel-evil-mode
  "Bring evil keybindings to dockel."
  :lighter nil
  :keymap dockel-evil-mode-map
  :group 'dockel-evil)

(add-hook 'dockel-mode-hook 'dockel-evil-mode)

(evil-set-initial-state 'kubel-mode 'motion)

(evil-define-key 'motion dockel-evil-mode-map
  (kbd "l") 'dockel-log-popup)

;;;###autoload

(define-derived-mode dockel-mode tabulated-list-mode "Dockel"
  ;; TODO: it does not seem to work 
    (unless dockel--socket-connected
      (setq dockel--socket-connected t)
      (dockel--forward-socket))

    (dockel-service-ls)
    ;; (dockel-ps)

    (setq mode-name "Dockel")
    (setq major-mode 'dockel-mode)
    (use-local-map dockel-mode-map)
    (run-mode-hooks 'dockel-mode-hook))


(define-derived-mode dockel-status-mode special-mode "Dockel Status"
  (message "Hello from new mode"))


;; TODO: implement
(add-hook 'dockel-mode-hook (lambda () (message "Hook has been called!")))

(defun dockel ()
    (interactive)
    (switch-to-buffer "*dockel*")
    (dockel-mode))

;;; dockel.el ends here
