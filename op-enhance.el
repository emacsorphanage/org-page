(defun op/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let* ((pub-theme-dir (concat (file-name-as-directory pub-root-dir)
                                "media/"))
         (theme-dir (file-name-as-directory
                     (concat (file-name-as-directory op/theme-directory)
                             (symbol-name op/theme)))))
    (unless (file-directory-p theme-dir)
      (message "Theme %s not found, use `default' theme instead."
               (symbol-name op/theme))
      (setq theme-dir (file-name-as-directory
                       (concat (file-name-as-directory op/theme-directory)
                               (symbol-name 'default)))))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (copy-directory theme-dir pub-theme-dir t t t)))

(defun op/generate-page-header ()
  "Generate page header, based on the templated defined by
`op/html-header-template', please see its description for more detail."
  (let* ((search-url op/site-url))
    (when (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'" op/site-url)
      (setq search-url (match-string 1 op/site-url)))
    (format-spec op/html-header-template `((?m . ,op/site-main-title)
                                           (?s . ,op/site-sub-title)
                                           (?t . "/tags/") ; TODO customization
                                           (?a . "/about/")
                                           (?g . ,op/personal-github-link)
                                           (?u . ,search-url)))))

(defun op/generate-style ()
  "Generate css style links."
  (let* ((template "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />")
         (css-list op/css-list)
         css-links)
    (unless css-list
      (setq css-list '("main.css")))
    (mapconcat '(lambda (css)
                  (format template (concat "/media/css/" css))) ;; TODO customization
               css-list "\n")))

(defun op/generate-footer (uri attr-plist hidden-meta-info hidden-comment)
  "Generate page footer, based on the template defined by
`op/html-postamble-template', please see its description for more detail."
  (let* ((footer-template op/html-postamble-template)
         (disqus-identifier uri)
         (disqus-url (concat (replace-regexp-in-string "/?$" "" op/site-url)
                             disqus-identifier))
         (disqus-shortname op/personal-disqus-shortname)
         (email (confound-email op/email))
         cdate mdate tags-list tag-links)
    (unless (or hidden-meta-info hidden-comment)
      (setq footer-template op/footer)
      (unless hidden-comment
        (setq footer-template (concat op/comment footer-template)))
      (unless hidden-meta-info
        (setq footer-template (concat op/meta-info footer-template))))
    (when attr-plist
      (setq uri (plist-get attr-plist :uri))
      (setq cdate (plist-get attr-plist :creation-date))
      (setq mdate (plist-get file-info :mod-date))
      (setq tag-list (plist-get file-info :tags)))
    (when tag-list
      (setq tag-links
            (mapconcat
             '(lambda (tag-name)
                (format-spec "<a href=\"%l\">%n</a>"
                             `((?l . ,(op/generate-tag-uri tag-name))
                               (?n . ,tag-name))))
             tag-list ", ")))
    (format-spec op/footer-template
                 `((?a . "%a") (?c . "%c") (?d . "%d") (?e . "%e") (?v . "%v")
                   (?i . ,(org-html-expand email))
                   (?h . ,(or cdate "N/A"))
                   (?m . ,(or mdate "N/A"))
                   (?t . ,(or tag-links "N/A"))
                   (?n . ,disqus-identifier)
                   (?u . ,disqus-url)
                   (?s . ,disqus-shortname)))))
