;;; pgraphql.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 's)
(require 'async)
(require 'elfeed)
(require 'elfeed-show)
(require 'org-roam)


;; (async-start
;;    ;; What to do in the child process
;;    (lambda ()
;;      (message "This is a test")
;;      (sleep-for 3)
;;      222)

;;    ;; What to do when it finishes
;;    (lambda (result)
;;      (message "Async process done, result should be 222: %s" result))))

(defvar p/paper-directory "~/Dropbox/papers/")

(defun p/elfeed-show-hide-images ()
  (interactive)
  (let ((shr-inhibit-images t))
    (elfeed-show-refresh)))

(defun p/elfeed-roam-note (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))

  (let* ((article-link (elfeed-entry-link entry))
         (pdf-link (p/arxiv-pdf-link article-link))
         (location (p/article-location (elfeed-entry-title entry)))
	 (template (concat
		    "#+title: ${title}\n"
		    "#+filetags: paper\n"
		    "#+file: " location "\n"
		    "#+startup: content latexpreview\n")))

    (async-start
     (lambda () (url-copy-file pdf-link location t))
     (lambda (result) (message "Fetching %s finished with: %s" pdf-link result)))

    (org-roam-capture- :templates
                       `(("n" "note" plain "%?" :if-new
                          (file+head "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}.org\" (current-time) t)"
				     ,template)
                          :immediate-finish t
                          :unnarrowed t))
                       :node (org-roam-node-create :title (elfeed-entry-title entry))
                       :props '(:finalize find-file))))


(defun p/arxiv-pdf-link (article-link)
  (ignore-errors
    (let* ((match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" article-link))
           (matched-arxiv-number (match-string 1 article-link)))

      (when matched-arxiv-number
	(s-concat "https://arxiv.org/pdf/" matched-arxiv-number)))))


(defun p/article-location (title)
  (s-concat p/paper-directory (s-dashed-words title) ".pdf"))

(defun p/fetch-arxiv-paper (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))

  (let* ((article-link (elfeed-entry-link entry))
	 (pdf-link (p/arxiv-pdf-link article-link)))
    (when pdf-link
      (let ((location (p/article-location (elfeed-entry-title entry))))
	(if (not (file-exists-p location))
	    (url-copy-file pdf-link location t))))))

(defun p/arxiv-feed (cat)
  (s-concat
   "http://export.arxiv.org/api/query?search_query=cat:"
   cat
   "&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending"))

(defun p/elfeed-setup ()
  (setq elfeed-feeds `("https://softwaremill.com/blog.rss"))) 

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
