(defun op/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let* ((theme-name (symbol-name op/theme))
         (pub-theme-dir (concat (file-name-as-directory pub-root-dir)
                                "media/"))
         theme-dir)
    (unless (file-directory-p theme-dir)
      (message "Theme %s not found, use `default' theme instead."
               (symbol-name op/theme))
      (setq theme-name (symbol-name 'default)))
    (setq theme-dir (file-name-as-directory
                     (concat (file-name-as-directory op/theme-directory)
                             theme-name)))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (copy-directory theme-dir pub-theme-dir t t t)))

(defun op/generate-page-header ()
  "Generate page header, based on the templated defined by
`op/html-header-template', please see its description for more detail."
  (let* ((search-url op/site-url))
    (when (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'" op/site-url)
      (setq search-url (match-string 1 op/site-url)))
    (format-spec op/html-header-template `((?m . op/site-main-title)
                                           (?s . op/site-sub-title)
                                           (?t . "/tags/") ; TODO customization
                                           (?a . "/about/")
                                           (?g . op/personal-github-link)
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
