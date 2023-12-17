;;; -*- lexical-binding: t; -*-

(require 's)

(defun run-process-and-callback (file callback)
  (let ((process (start-process "read-me-process"    ; Process name
                                "*read-me-output*"   ; Buffer name
                                "/home/porcupine/projects/read_me/.venv/bin/python3" ; Program
                                "/home/porcupine/projects/read_me/read_me/__init__.py" ; Argument 1: script path
				file
                               )))
    (set-process-sentinel
      process
      (lambda (proc event)
        (when (string-match-p "\\(finished\\|exited\\)" event)
          (funcall callback))))))


(defun read-me-generate (&optional callback)
  (interactive)
  (let* ((fpath (buffer-file-name))
         (fname (concat (-last-item (s-split "/" fpath )) ".txt"))
	 (ftarget (concat "/home/porcupine/projects/read_me/tts_data/" fname))
	 (content (buffer-substring-no-properties (region-beginning) (region-end))))

    (with-current-buffer (find-file-noselect ftarget)
      (erase-buffer)
      (insert content)
      (save-buffer))

    (if callback
	(run-process-and-callback fname (lambda () (funcall callback fname)))
        (run-process-and-callback fname (lambda () (message "done"))))))

(defun read-me-read (fname)
  (interactive)
  (message "The callback was called!")
  (message fname)
  (message (concat "/home/porcupine/projects/read_me/tts_data/" fname ".wav"))
  (call-process "mpv" nil 0 nil "--force-window" "--speed=1.50" (concat "/home/porcupine/projects/read_me/tts_data/" fname ".wav")))


(defun read-me ()
  (interactive)

  (read-me-generate 'read-me-read))

(provide 'read-me)
