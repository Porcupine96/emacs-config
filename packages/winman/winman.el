;;; winman.el --- Window configuration management -*- lexical-binding: t -*-

;; Copyright (C) 2023 Łukasz Kaźmierczak

;; Author: Your Name <luki.pol@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://github.com/yourusername/winman

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to save, restore, and manage
;; window configurations.

;;; Code:

(defgroup winman nil
  "Window configuration management."
  :group 'convenience
  :prefix "winman-")

(defcustom winman-save-file
  (expand-file-name "winman-configs.el" user-emacs-directory)
  "File to save window configurations."
  :type 'file
  :group 'winman)

(defvar winman-configurations nil
  "Alist of saved window configurations.
Each element is a cons cell (name . config) where config is a plist with
:window-config and :buffer-list properties.")

(defun winman--get-buffer-info (buffer)
  "Get information about BUFFER for later restoration."
  (with-current-buffer buffer
    (list :name (buffer-name)
          :file (when buffer-file-name
                  (expand-file-name buffer-file-name))
          :major-mode major-mode
          :point (point)
          :mark (mark t))))

(defun winman--get-current-buffers ()
  "Get information about all buffers in the current window configuration."
  (let ((windows (window-list)) buffer-info)
    (dolist (window windows buffer-info)
      (push (cons window (winman--get-buffer-info (window-buffer window)))
            buffer-info))))

(defun winman--restore-buffer (buffer-info)
  "Restore buffer from BUFFER-INFO.
If the buffer doesn't exist anymore, create a new one."
  (let* ((name (plist-get buffer-info :name))
         (file (plist-get buffer-info :file))
         (major (plist-get buffer-info :major-mode))
         (point (plist-get buffer-info :point))
         (mark (plist-get buffer-info :mark))
         (buffer (get-buffer name)))
    
    (unless buffer
      (if file
          (if (file-exists-p file)
              (setq buffer (find-file-noselect file))
            ;; File doesn't exist anymore, create new file buffer
            (setq buffer (generate-new-buffer name))
            (with-current-buffer buffer
              (setq buffer-file-name file)
              (funcall major)))
        ;; Non-file buffer
        (setq buffer (generate-new-buffer name))
        (with-current-buffer buffer
          (funcall (or major 'fundamental-mode)))))
    
    (with-current-buffer buffer
      (when point (goto-char point)))
      ;; (when mark (set-mark mark)))
    
    buffer))

(defun winman-get-default-name ()
  "Generate a default name for the window configuration."
  (let ((buffers (mapcar (lambda (window) 
                           (buffer-name (window-buffer window)))
                         (window-list))))
    (string-join buffers " | ")))

;;;###autoload
(defun winman-save (name)
  "Save the current window configuration with NAME."
  (interactive
   (list (let ((default-name (winman-get-default-name)))
           (read-string (format "Save window configuration as (default %s): " default-name)
                        nil nil default-name))))
  (let ((config (list :window-config (current-window-configuration)
                      :buffer-list (winman--get-current-buffers))))
    (setf (alist-get name winman-configurations nil nil 'equal) config)
    (message "Window configuration '%s' saved" name)))

;;;###autoload
(defun winman-update (name)
  "Rename window configuration NAME."
  (interactive
   (list (completing-read "Update window configuration: "
			  (mapcar #'car winman-configurations) nil t)))

  (winman-save name))

;;;###autoload
(defun winman-switch (name)
  "Switch to window configuration NAME."
  (interactive
   (list (completing-read "Switch to window configuration: "
                          (mapcar #'car winman-configurations) nil t)))
  (let ((config (alist-get name winman-configurations nil nil 'equal)))
    (if config
        (progn
          ;; Restore window configuration first
          (set-window-configuration (plist-get config :window-config))
          
          ;; Then restore buffer contents in each window
          (let ((buffer-list (plist-get config :buffer-list)))
            (dolist (window-buffer buffer-list)
              (let ((window (car window-buffer))
                    (buffer-info (cdr window-buffer)))
                (when (window-live-p window)
                  (set-window-buffer window (winman--restore-buffer buffer-info))))))
          (message "Window configuration '%s' restored" name))
      (message "No window configuration named '%s'" name))))

;;;###autoload
(defun winman-delete (name)
  "Delete window configuration NAME."
  (interactive
   (list (completing-read "Delete window configuration: "
                          (mapcar #'car winman-configurations) nil t)))
  (if (alist-get name winman-configurations nil nil 'equal)
      (progn
        (setq winman-configurations
              (assoc-delete-all name winman-configurations))
        (message "Window configuration '%s' deleted" name))
    (message "No window configuration named '%s'" name)))


;;;###autoload
(defun winman-rename (name)
  "Rename window configuration NAME."
  (interactive
   (list (completing-read "Rename window configuration: "
			  (mapcar #'car winman-configurations) nil t)))
  (let ((new-name (read-string "New name: " name))
	(value (alist-get name winman-configurations nil nil 'equal)))
    (if value
	(progn
	  (setq winman-configurations
		(assoc-delete-all name winman-configurations))
	  (setf (alist-get new-name winman-configurations nil nil 'equal) value)))))

;;;###autoload
(defun winman-persist ()
  "Save all window configurations to `winman-save-file'."
  (interactive)
  (with-temp-file winman-save-file
    (prin1 `(setq winman-configurations ',winman-configurations) (current-buffer)))
  (message "Window configurations saved to %s" winman-save-file))

;;;###autoload
(defun winman-restore ()
  "Restore all window configurations from `winman-save-file'."
  (interactive)
  (if (file-exists-p winman-save-file)
      (progn
        (load-file winman-save-file)
        (message "Window configurations restored from %s" winman-save-file))
    (message "No saved window configurations found at %s" winman-save-file)))

(provide 'winman)
;;; winman.el ends here
