;;; koko.el --- Kokoro TTS integration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Send text to a local Kokoro TTS server and play the returned audio.
;; Supports org-roam text preprocessing, async playback, pause/resume.
;;
;;; Code:

(require 'json)
(require 'url)

(defgroup koko nil
  "Kokoro TTS integration."
  :group 'convenience
  :prefix "koko-")

(defcustom koko-server-url "http://127.0.0.1:5001"
  "URL of the Kokoro TTS server."
  :type 'string
  :group 'koko)

(defcustom koko-voice "af_heart"
  "Voice to use for TTS."
  :type 'string
  :group 'koko)

(defcustom koko-speed 1.0
  "Speech speed passed to Kokoro (1.0 = normal)."
  :type 'float
  :group 'koko)

(defvar koko--process nil
  "Current afplay process.")

(defvar koko--current-file nil
  "Path to the current WAV temp file.")

;; --- Org preprocessing ---

(defun koko--preprocess-org (text)
  "Strip org markup from TEXT for TTS."
  (let ((result text))
    ;; Links with description: [[...][desc]] -> desc
    (setq result (replace-regexp-in-string
                  "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]"
                  "\\1" result))
    ;; Bare links: [[...]] -> ""
    (setq result (replace-regexp-in-string
                  "\\[\\[[^]]*\\]\\]"
                  "" result))
    ;; :PROPERTIES: blocks
    (setq result (replace-regexp-in-string
                  ":PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?"
                  "" result))
    ;; #+KEYWORD: lines
    (setq result (replace-regexp-in-string
                  "^#\\+[^:]+:.*$"
                  "" result))
    ;; Heading stars
    (setq result (replace-regexp-in-string
                  "^\\*+ "
                  "" result))
    ;; Bold *text*
    (setq result (replace-regexp-in-string
                  "\\*\\([^*]+\\)\\*"
                  "\\1" result))
    ;; Italic /text/
    (setq result (replace-regexp-in-string
                  "/\\([^/]+\\)/"
                  "\\1" result))
    ;; Underline _text_
    (setq result (replace-regexp-in-string
                  "_\\([^_]+\\)_"
                  "\\1" result))
    ;; Code =text= and verbatim ~text~
    (setq result (replace-regexp-in-string
                  "[=~]\\([^=~]+\\)[=~]"
                  "\\1" result))
    ;; Strike-through +text+
    (setq result (replace-regexp-in-string
                  "\\+\\([^+]+\\)\\+"
                  "\\1" result))
    ;; Replace all newlines with spaces
    (setq result (replace-regexp-in-string
                  "\n+" " " result))
    (string-trim result)))

;; --- Audio playback ---

(defun koko--play (file)
  "Play WAV FILE with afplay."
  (setq koko--process
        (start-process "koko-afplay" nil "afplay" file))
  (set-process-sentinel koko--process #'koko--sentinel))

(defun koko--sentinel (_process event)
  "Clean up after playback finishes (EVENT)."
  (when (string-match-p "\\(finished\\|exited\\)" event)
    (koko--cleanup)))

(defun koko--cleanup ()
  "Delete temp file and clear state."
  (when (and koko--current-file (file-exists-p koko--current-file))
    (delete-file koko--current-file))
  (setq koko--process nil
        koko--current-file nil))

;; --- HTTP request ---

(defun koko--request (text)
  "Send TEXT to the TTS server asynchronously."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode `(("text" . ,text)
                         ("voice" . ,koko-voice)
                         ("speed" . ,koko-speed)))
          'utf-8)))
    (url-retrieve
     (concat koko-server-url "/tts")
     #'koko--handle-response
     nil t)))

(defun koko--handle-response (status)
  "Handle the TTS server response. STATUS contains error info if any."
  (if (plist-get status :error)
      (progn
        (kill-buffer)
        (message "Koko: request failed: %s" (plist-get status :error)))
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (let ((temp-file (make-temp-file "koko-" nil ".wav"))
          (coding-system-for-write 'binary))
      (write-region (point) (point-max) temp-file nil 'silent)
      (kill-buffer)
      (setq koko--current-file temp-file)
      (koko--play temp-file))))

;; --- Interactive commands ---

(defun koko-speak ()
  "Send selected region to Kokoro TTS and play the audio."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (cons (progn (backward-paragraph) (point))
                           (progn (forward-paragraph) (point))))))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (text (if (derived-mode-p 'org-mode)
                   (koko--preprocess-org text)
                 text)))
    (koko-stop)
    (message "Koko: generating speech...")
    (koko--request text)))

(defun koko-stop ()
  "Stop playback and clean up."
  (interactive)
  (when (and koko--process (process-live-p koko--process))
    (kill-process koko--process))
  (koko--cleanup))

(defun koko-pause ()
  "Pause playback."
  (interactive)
  (if (and koko--process (process-live-p koko--process))
      (progn
        (signal-process koko--process 'SIGSTOP)
        (message "Koko: paused"))
    (message "Koko: nothing playing")))

(defun koko-resume ()
  "Resume playback."
  (interactive)
  (if (and koko--process (process-live-p koko--process))
      (progn
        (signal-process koko--process 'SIGCONT)
        (message "Koko: resumed"))
    (message "Koko: nothing to resume")))

(defun koko-replay ()
  "Replay the current audio from the beginning."
  (interactive)
  (if koko--current-file
      (let ((file koko--current-file))
        (when (and koko--process (process-live-p koko--process))
          (kill-process koko--process))
        (setq koko--process nil)
        (koko--play file))
    (message "Koko: nothing to replay")))

(provide 'koko)
;;; koko.el ends here
