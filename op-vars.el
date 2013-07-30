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
;; 1. `op/repository-directory'
;; 2. `op/site-domain'
;; 3. `op/personal-disqus-shortname'
;;
;; Six variables are recommended to do customization:
;;
;; 1. `op/repository-org-branch'
;; 2. `op/repository-html-branch'
;; 3. `op/site-main-title'
;; 4. `op/site-sub-title'
;; 5. `op/personal-github-link'
;; 6. `op/personal-google-analytics-id'
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

(defcustom op/site-domain nil
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

(defcustom op/theme 'mdo
  "The theme used for page generation."
  :group 'org-page :type 'symbol)

(defcustom op/personal-github-link "https://github.com/kelvinh/org-page"
  "The personal github link."
  :group 'org-page :type 'string)

(defcustom op/personal-disqus-shortname nil
  "The personal disqus shortname."
  :group 'org-page :type 'string)

(defcustom op/personal-google-analytics-id nil
  "Personal google analytics id."
  :group 'org-page :type 'string)

(defcustom op/page-template
  (file-to-string (concat op/load-directory
                          (format "themes/%s/templates/template.html"
                                  (symbol-name (or op/theme 'mdo)))))
  "The template used to construct pages, see the template itself for detail."
  :group 'org-page :type 'string)

(defcustom op/retrieve-category-function 'op/get-file-category
  "The function used to retrieve an org file's category, its parameter is the
org file's path, the default value is `op/get-file-category'."
  :group 'org-page :type 'function)

(defvar op/category-config-alist
  '(("blog" ;; this is the default configuration
    :show-meta t
    :show-comment t
    :uri-generator 'op/generate-uri
    :uri-template "/%y/%m/%d/%t/")
   ("index"
    :show-meta nil
    :show-comment nil
    :uri-generator 'op/generate-uri
    :uri-template "/index/")
   ("about"
    :show-meta nil
    :show-comment nil
    :uri-generator 'op/generate-uri
    :uri-template "/about/"))
  "Configurations for different categories, can and should be customized.")

(defvar op/default-template-parameters
  (ht ("blog-uri" "/blog/")
      ("wiki-uri" "/wiki/")
      ("tags-uri" "/tags/")
      ("about-uri" "/about/")
      ("site-main-title" op/site-main-title)
      ("site-sub-title" op/site-sub-title)
      ("github" op/personal-github-link)
      ("site-domain" (if (and op/site-domain
                              (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                            op/site-domain))
                         (match-string 1 op/site-domain)
                       op/site-domain))
      ("disqus-shortname" op/personal-disqus-shortname)
      ("google-analytics-id" op/personal-google-analytics-id)
      ("google-analytics" (if op/personal-google-analytics-id t nil))
      ("creator-info" org-html-creator-string))
  "Default template rendering parameters.")


(provide 'op-vars)

;;; op-vars.el ends here
