(require 'ox-publish)
(require 'org-roam)


(defvar p/publish--html-head
  "<link rel=\"stylesheet\" href=\"./theme.css\" type=\"text/css\"/>
   <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>")

(defun p/force-publish-all ()
  (interactive)
  (org-publish-all t))

(defun p/force-publish-current-file ()
  (interactive)
  ;; make sure images are published 
  ;; TODO: it maybe necessary to optimize that someday
  (org-publish-current-file t)
  (org-publish-project "jupyter" t))

(defun p/show-current-currently-published-file ()
  (interactive)

  (let* ((prefix "file:///home/porcupine/.emacs.default/packages/publish/index.html?indexVisible=false&notes=")
	 (note (s-replace ".org" ".html" (buffer-name)))
	 (path (s-concat prefix note)))
    (call-process "firefox" nil 0 nil path)))


(defun p/org-roam--resolve-link (node-id)
  (car
   (last
    (s-split "/"
	     (caar 
	      (org-roam-db-query [:select [file] :from nodes
				  :where (= id $s1)]
				 node-id))))))

(defun org-export-resolve-id-link (link info)
  (let ((id (org-element-property :path link)))
    (or (org-element-map (plist-get info :parse-tree) 'headline
	  (lambda (headline)
	    (when (or (equal (org-element-property :ID headline) id)
		      (equal (org-element-property :CUSTOM_ID headline) id))
	      headline))
	  info 'first-match)
	(cdr (assoc id (plist-get info :id-alist)))
	(p/org-roam--resolve-link id)
	(signal 'org-link-broken (list id)))))


(defun p/publish-configure ()
  (message "Running p/publish-configure")

  (setq org-html-htmlize-output-type 'css)

  (setq org-publish-project-alist
       `(("orgfiles"
           :base-directory "/home/porcupine/Dropbox/org-roam/"
           :base-extension "org"
           :publishing-directory "/home/porcupine/kb"
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc nil
           :html-head ,p/publish--html-head
           :html-preamble t)
  
          ("images"
           :base-directory "/home/porcupine/Dropbox/org-roam/images/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "/home/porcupine/kb/images"
           :publishing-function org-publish-attachment)

          ("jupyter"
           :base-directory "/home/porcupine/Dropbox/org-roam/.ob-jupyter/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "/home/porcupine/kb/images"
           :publishing-function org-publish-attachment))))
