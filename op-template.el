(require 'mustache)

(defun op/update-default-template-parameters ()
  "Update the default template parameters. It is only needed when user did some
customization to relevant variables."
  (ht-update
   op/default-template-parameters
   (ht ("site-main-title" op/site-main-title)
       ("site-sub-title" op/site-sub-title)
       ("github" op/personal-github-link)
       ("site-domain" (if (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                        op/site-url)
                          (match-string 1 op/site-url)
                        op/site-url))
       ("disqus-shortname" op/personal-disqus-shortname)
       ("google-analytics-id" op/personal-google-analytics-id)
       ("google-analytics" (if op/personal-google-analytics-id t nil))))
  op/default-template-parameters)

(defun op/compose-template-parameters (attr-plist content)
  "Compose parameters for org file represented in current buffer.
ATTR-PLIST is the attribute plist of the buffer, read by `op/read-file-info'."
  (let* ((info
          (org-combine-plists
           (org-export--get-global-options 'html)
           ;;; the two are not needed because they are included in attr-plist
           ;; (org-export--get-buffer-attributes)
           ;; (org-export--get-inbuffer-options 'html)
           attr-plist))
         (title (org-element-interpret-data (plist-get info :title)))
         (author (org-element-interpret-data
                  (or (plist-get info :author) user-full-name)))
         (email (confound-email (or (plist-get info :email)
                                    user-mail-address)))
         (description (or (plist-get info :description) nil))
         (keywords (or (plist-get info :keywords) nil))
         (category (plist-get info :category))
         (show-meta-info (and (not (eq category 'index))
                              (not (eq category 'about))))
         (creation-date (or (plist-get info :creation-date) "N/A"))
         (mod-date (or (plist-get info :mod-date) "N/A"))
         (tag-links (mapconcat
                     #'(lambda (tag-name)
                         (mustache-render
                          "<a href=\"{{link}}\">{{name}}</a>"
                          (ht ("link" (op/generate-tag-uri tag-name))
                              ("name" tag-name))))
                     (plist-get info :tags) ", "))
         (show-comment (eq category 'blog))
         (disqus-id (plist-get info :uri))
         (disqus-url (concat (replace-regexp-in-string "/?$" "" op/site-url)
                             disqus-id))
         (param-table (ht-create)))
    (ht-update param-table op/default-template-parameters)
    (ht-update
     param-table
     (ht ("page-title"        (concat title " - " op/site-main-title))
         ("author"            author)
         ("description"       description)
         ("keywords"          keywords)
         ("title"             title)
         ("content"           content)
         ("show-meta-info"    show-meta-info)
         ("creation-date"     creation-date)
         ("modification-date" mod-date)
         ("tags"              tag-links)
         ("show-comment"      show-comment)
         ("disqus-id"         disqus-id)
         ("disqus-url"        disqus-url)
         ("email"             email)))
    param-table))


(provide 'op-template)

;;; op-template.el ends here
