;;==============================================================================
;; The file defines the html templates used by corresponding theme, the name of
;; this file is "theme-default.el", so it will be used by the theme "default".
;;
;; The templates include:
;;   1. index page of entire site
;;   2. header, shows at head of each page
;;   3. meta info, shows at the end of each post
;;   4. comment, shows after meta info
;;   5. footer, shows at tail of each page
;;
;; Be aware that the code should be with Elisp syntax, because org-page will
;; load it directly.
;;==============================================================================


(setq op/publish-html-index-template
;; the template used to construct index page of entire site, our real site is
;; generated in a subfolder named 'blog/', so the index page is needed to act
;; as a bridge, taking visitor to the real index. below parameters can be used:
;; %t: the site title
;; %c: css links, be aware that here the links are already wrapped by tag <link>
;; %p: the path to real index page"
      (file-to-string
       (concat (file-name-directory op/load-file-name)
               "templates/html/index-template.html")))

(setq op/publish-html-header-template
;; the template used to construct page header, below parameters can be used:
;; %p: the relative path to index html file, not the site's index, but index in folder 'blog'
;; %h: the title/headline of entire site (defined by `op/publish-site-title')
;; %s: the relative path to sitemap html file
;; %c: the relative path to category root html file
;; %t: the relative path to tag root html file
;; %r: the relative path to recent posts html file
;; %a: the relative path to about html file
;; %g: the github link (defined by `op/personal-github-link')
;; %u: the url of current site, used for search (defined by `op/publish-site-url')
      (file-to-string
       (concat (file-name-directory op/load-file-name)
               "templates/html/header-template.html")))

(setq op/publish-meta-info
;; the meta info of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information.
      (file-to-string
       (concat (file-name-directory op/load-file-name)
               "templates/html/meta-info-template.html")))

(setq op/publish-comment
;; the comment section of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information."
      (file-to-string
       (concat (file-name-directory op/load-file-name)
               "templates/html/comment-template.html")))

(setq op/publish-footer
;; the footer of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information."
      (file-to-string
       (concat (file-name-directory op/load-file-name)
               "templates/html/footer-template.html")))
