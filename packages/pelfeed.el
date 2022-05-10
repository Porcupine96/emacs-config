;;; pgraphql.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 's)
(require 'elfeed)
(require 'elfeed-show)
(require 'org-roam)

(defvar p/paper-directory "~/Dropbox/papers/")

(defun p/elfeed-roam-note (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))

  (org-roam-capture- :templates
                         '(("n" "note" plain "%?" :if-new
                            (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :node (org-roam-node-create :title (elfeed-entry-title entry))
                         :props '(:finalize find-file)))


(defun p/fetch-arxiv-paper (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))

  (let* ((link (elfeed-entry-link entry))
         (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
         (matched-arxiv-number (match-string 1 link)))
    (when matched-arxiv-number
      (let* ((url (s-concat "https://arxiv.org/pdf/" matched-arxiv-number))
	     (readable-name (s-dashed-words (elfeed-entry-title entry)))
	     (location (s-concat p/paper-directory readable-name ".pdf")))
	(if (not (file-exists-p location))
	    (url-copy-file url location t))))))

(defun p/arxiv-feed (cat)
  (s-concat
   "http://export.arxiv.org/api/query?search_query=cat:"
   cat
   "&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending"))

(defun p/elfeed-setup ()
  (setq elfeed-feeds `(,(p/arxiv-feed "cs.AI") ;; Artificial Intelligence
		       ,(p/arxiv-feed "cs.CL") ;; Computation and Language
		       ,(p/arxiv-feed "cs.LG"))) ;; Machine Learning


  ;; TODO: add a hook that adds a new capture template to the list
  
	) 

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
