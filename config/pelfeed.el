;;; pgraphql.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'elfeed)
(require 'elfeed-show)

(defvar p/elfeed-player "mpv")

(defun p/elfeed-setup ()
  (setq elfeed-feeds 
        '(("https://michalplachta.com/feed.xml")
          ;; Leeren
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6fXiuFCWAnVPwRhBMztLlQ" youtube) 
	  ;; Protesilaos Stavrou
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" youtube emacs) 
	  ;; SystemCrafters
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" youtube emacs) 
	  ;; code_report
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1kBxkk2bcG78YBX7LMl9pQ" youtube programming) 
          ("https://degoes.net/feed.xml")
          ("https://blog.softwaremill.com/feed")
          ("https://sachachua.com/blog/feed" emacs))))

(defun p/elfeed-play-with-mpv ()
  "Play the link in the region with mpv"
  (interactive)

  (let* ((entries (elfeed-search-selected))
         (link (elfeed-entry-link (car entries))))
     (start-process "elfeed-player" "*elfeed-player*" p/elfeed-player link)))

(defun p/elfeed-show-entry (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))

  (when (elfeed-entry-p entry)
    (elfeed-search-update-entry entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (elfeed-show-entry entry)))

(defun p/elfeed-browse-url (&optional use-generic-p)
  (interactive "P")

  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
	     do (elfeed-untag entry 'unread)
	     when (elfeed-entry-link entry)
	     do (if use-generic-p
		    (browse-url-generic it)
		  (browse-url it)))
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(provide 'pelfeed)
;;; pgraphql.el ends here
