;;; TODO: add some description here

(defgroup org-page nil
  "Options for generating static pages using org-page."
  :tag "Org static page generator" :group 'org)

(defconst op/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-page")

(defcustom op/repository-directory nil
  "The git repository directory, where org files stored on branch
`op/repository-org-branch', and generated html files stored on branch
`op/repository-html-branch'."
  :group 'org-page
  :type 'string)

(defcustom op/site-url nil
  "The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned."
  :group 'org-page :type 'string)

(defcustom op/site-main-title "org-page"
  "The main title of entire site."
  :group 'org-page :type 'string)

(defcustom op/site-sub-title "static site generator"
  "The subtitle of entire site."
  :group 'org-page :type 'string)

(defcustom op/repository-org-branch "source"
  "The branch where org files stored on, it is within repository presented by
`op/repository-directory'."
  :group 'org-page :type 'string)

(defcustom op/repository-html-branch "master"
  "The branch where generated html files stored on, it is within repository
presented by `op/repository-directory'."
  :group 'org-page :type 'string)

(defcustom op/html-header-template
  (file-to-string (concat (file-name-directory load-file-name)
                          "templates/html/default/header-template.html"))
  "The template used to construct page header, below parameters can be used:
%m: the main title of entire site (defined by `op/site-main-title')
%s: the subtitle of entire site (defined by `op/site-sub-title')
%b: the path to blog index
%w: the path to wiki index
%t: the path to root tag
%a: the path to about
%g: the github link (defined by `op/personal-github-link')
%u: the url of current site, used for search (defined by `op/site-url')"
  :group 'org-page :type 'string)

(defcustom op/theme-directory
  (concat (file-name-directory (or load-file-name
                                   (buffer-file-name))) "themes/")
  "The directory stores org-page styles/scripts/images."
  :group 'org-page :type 'string)

(defcustom op/theme 'default
  "The theme used for page generation."
  :group 'org-page :type 'symbol)

(defcustom op/css-list '("main.css")
  "CSS style file name list, will using uri \"/media/css/<name>\"."
  :group 'org-page :type 'list)

(defcustom op/meta-info
  (file-to-string (concat (file-name-directory load-file-name)
                          "templates/html/default/meta-info-template.html"))
  "Meta info of current post, will be used to compose
`op/html-postamble-template', please see `op/html-postamble-template' for
detailed information."
  :group 'org-page :type 'string)

(defcustom op/comment
  (file-to-string (concat (file-name-directory load-file-name)
                          "templates/html/default/comment-template.html"))
  "Comment section of current post, will be used to compose
`op/html-postamble-template', please see `op/html-postamble-template' for
detailed information."
  :group 'org-page :type 'string)

(defcustom op/footer
  (file-to-string (concat (file-name-directory load-file-name)
                          "templates/html/default/footer-template.html"))
  "Footer of current post, will be used to compose
`op/html-postamble-template', please see `op/html-postamble-template' for
detailed information."
  :group 'org-page :type 'string)

(defcustom op/html-postamble-template (concat op/meta-info op/comment op/footer)
  "Template used to construct page footer, it is composed by `op/meta-info',
`op/comment' and `op/footer', since some pages do not need comments, so
the second component should be removed from these pages.
below parameter can be used:
%a: author's name
%c: creator versions (org/emacs versions)
%d: publish date
%e: author's email
%v: validation-link, will be replaced by `org-export-html-validation-link'

%h: last changed date (this change means meta change, not content change)
%m: last modified date
%t: tags of file, it will be expanded to the following format(assume the file
has tag tag1, tag2):
<a href=\"tag1-link\">tag1</a>, <a href=\"tag2-link\">tag2</a>
%n: javascript variable 'disqus_identifier' of current page
%u: javascript variable 'disqus_url' of current page
%s: javascript variable 'disqus_shortname' of current page
%i: author's email, the difference from %e is this one will keep the email
address unchanged, but %e will expand it to html tag <a href=\"mailto:\">
automatically."
  :group 'org-page :type 'string)

(defcustom op/email user-mail-address
  "Email address will be presented at the footer."
  :group 'org-page :type 'string)

(defcustom op/personal-github-link "https://github.com/kelvinh/org-page"
  "the personal github link"
  :group 'org-page :type 'string)

(defcustom op/personal-disqus-shortname nil
  "the personal disqus shortname"
  :group 'org-page :type 'string)


(provide 'op-vars)

;;; op-vars.el ends here
