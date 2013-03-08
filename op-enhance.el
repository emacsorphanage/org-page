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
