;;; op-vars.el --- Variable configurations required by org-page

;; Copyright (C) 2012, 2013 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/kelvinh/org-page
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; op-vars.el contains almost all variable definitions and configurations.
;;
;; As described in `org-page.el', most variables will work with default
;; values, but you can configure them for more customization. You could
;; visit each variable's description for its usage.
;;
;; Three variables are required to configure:
;;
;; 1.`op/repository-directory'
;; 2. `op/site-url'
;; 3. `op/personal-disqus-shortname'
;;
;; Six variables are recommended to do customization:
;;
;; 1. `op/repository-org-branch'
;; 2. `op/repository-html-branch'
;; 3. `op/site-main-title'
;; 4. `op/site-sub-title'
;; 5. `op/email'
;; 6. `op/personal-github-link'
;;
;; If you perfer another theme, you may need to change `op/theme'.

;;; Code:

(defgroup org-page nil
  "Options for generating static pages using org-page."
  :tag "Org static page generator" :group 'org)

(defconst op/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-page.")

(defconst op/load-directory
  (cond
   (load-file-name (file-name-directory load-file-name))
   ((symbol-file 'op/temp-buffer-name)
    (file-name-directory (symbol-file 'op/temp-buffer-name)))
   ((string= (file-name-nondirectory buffer-file-name) "op-vars.el")
    (file-name-directory buffer-file-name))
   (t nil))
  "The directory where org-page is loaded from.")

(defcustom op/repository-directory nil
  "The git repository directory, where org files stored on branch
`op/repository-org-branch', and generated html files stored on branch
`op/repository-html-branch'."
  :group 'org-page :type 'string)

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

(defcustom op/theme 'default
  "The theme used for page generation."
  :group 'org-page :type 'symbol)

(defcustom op/html-header-template
  (file-to-string (concat op/load-directory
                          (format "templates/html/%s/header-template.html"
                                  (symbol-name (or op/theme 'default)))))
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

(defcustom op/meta-info
  (file-to-string (concat op/load-directory
                          (format "templates/html/%s/meta-info-template.html"
                                  (symbol-name (or op/theme 'default)))))
  "Meta info of current post, will be used to compose
`op/html-postamble-template', please see `op/html-postamble-template' for
detailed information."
  :group 'org-page :type 'string)

(defcustom op/comment
  (file-to-string (concat op/load-directory
                          (format "templates/html/%s/comment-template.html"
                                  (symbol-name (or op/theme 'default)))))
  "Comment section of current post, will be used to compose
`op/html-postamble-template', please see `op/html-postamble-template' for
detailed information."
  :group 'org-page :type 'string)

(defcustom op/footer
  (file-to-string (concat op/load-directory
                          (format "templates/html/%s/footer-template.html"
                                  (symbol-name (or op/theme 'default)))))
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
