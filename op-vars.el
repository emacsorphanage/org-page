;;; op-vars.el --- Variable configurations required by org-page

;; Copyright (C) 2012, 2013, 2014 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/kelvinh/org-page

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

;;; Code:

(require 'ox)
(require 'ht)


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

(defcustom op/export-backend 'html
  "The org-export backend used for page generation"
  :group 'org-page :type 'symbol)

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

(defcustom op/theme-root-directory
  (concat op/load-directory "themes/")
  "The root directory that stores themes for page rendering. By default, it
points to the directory `themes' in org-page installation directory."
  :group 'org-page :type 'string)

(defcustom op/theme 'mdo
  "The theme used for page generation."
  :group 'org-page :type 'symbol)

(defcustom op/highlight-render 'js
  "Code highlight render."
  :group 'org-page :type 'symbol)


(defcustom op/personal-github-link "https://github.com/kelvinh/org-page"
  "The personal github link."
  :group 'org-page :type 'string)

(defcustom op/personal-avatar nil
  "The link to an avatar image."
  :group 'org-page :type 'string)

(defcustom op/personal-disqus-shortname nil
  "The personal disqus shortname."
  :group 'org-page :type 'string)

(defcustom op/personal-duoshuo-shortname nil
  "The personal duoshuo shortname."
  :group 'org-page :type 'string)

(defcustom op/personal-google-analytics-id nil
  "Personal google analytics id."
  :group 'org-page :type 'string)

(defcustom op/template-directory nil
  "The directory stores templates for page rendering. By default, org-page uses
`op/theme' and `op/theme-root-directory' to determine the template directory.
DON'T set this variable unless you know what you are doing!"
  :group 'org-page :type 'string)

(defcustom op/confound-email t
  "This variable is used to determine whether email addresses should be
confounded or not."
  :group 'org-page :type 'boolean)

(defcustom op/tag-rss nil
  "This variable is used to determine whether a rss.xml will be generated for
each tag."
  :group 'org-page :type 'boolean)

(defcustom op/organization nil
  "This variable is used to determine whether the site is used by organization or not"
  :group 'org-page :type 'boolean)

(defcustom op/retrieve-category-function 'op/get-file-category
  "The function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories, the
default value is `op/get-file-category'."
  :group 'org-page :type 'function)

(defcustom op/site-preview-directory "~/.op-tmp/"
  "Temporary directory path for site preview."
  :group 'org-page :type 'string)

(defvar op/category-config-alist
  '(("blog" ;; this is the default configuration
    :show-meta t
    :show-comment t
    :uri-generator op/generate-uri
    :uri-template "/blog/%y/%m/%d/%t/"
    :sort-by :date     ;; how to sort the posts
    :category-index t) ;; generate category index or not
   ("index"
    :show-meta nil
    :show-comment nil
    :uri-generator op/generate-uri
    :uri-template "/"
    :sort-by :date
    :category-index nil)
   ("about"
    :show-meta nil
    :show-comment nil
    :uri-generator op/generate-uri
    :uri-template "/about/"
    :sort-by :date
    :category-index nil))
  "Configurations for different categories, can and should be customized.")

(defvar op/category-ignore-list
  '("themes" "assets")
  "Ignore these subdirs/categories for navigation")

;;; this variable is deprecated
(defvar op/default-template-parameters
  (ht ("blog-uri" "/blog/")
      ("wiki-uri" "/wiki/")
      ("tags-uri" "/tags/")
      ("about-uri" "/about/")
      ("site-main-title" op/site-main-title)
      ("site-sub-title" op/site-sub-title)
      ("avatar" op/personal-avatar)
      ("github" op/personal-github-link)
      ("site-domain" (if (and op/site-domain
                              (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                            op/site-domain))
                         (match-string 1 op/site-domain)
                       op/site-domain))
      ("disqus-shortname" op/personal-disqus-shortname)
      ("disqus-comment" (if op/personal-disqus-shortname t nil))
      ("duoshuo-shortname" op/personal-duoshuo-shortname)
      ("duoshuo-comment" (if op/personal-duoshuo-shortname t nil))
      ("google-analytics-id" op/personal-google-analytics-id)
      ("google-analytics" (if op/personal-google-analytics-id t nil))
      ("creator-info" org-html-creator-string))
  "Default template rendering parameters.")

(defvar op/item-cache nil
  "The cache for general purpose.")

(defconst op/rss-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version=\"2.0\">
  <channel>
    <title>{{title}}</title>
    <link>{{link}}</link>
    <description>{{description}}</description>
    <pubDate>{{date}}</pubDate>
    <lastBuildDate>{{date}}</lastBuildDate>
    <docs>http://www.rssboard.org/rss-specification</docs>
    <generator>Org-page static site generator \
(https://github.com/kelvinh/org-page)</generator>
    {{#items}}
    <item>
      <title>{{item-title}}</title>
      <link>{{item-link}}</link>
      <description><![CDATA[{{& item-description}}]]></description>
      <pubDate>{{item-update-date}}</pubDate>
      <guid>{{item-link}}</guid>
    </item>
    {{/items}}
  </channel>
</rss>"
  "Template for RSS rendering.")

(defcustom op/html-creator-string
  (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
 (<a href=\"http://orgmode.org\">Org mode</a> %s)"
          (format "%s.x" emacs-major-version)
          (if (fboundp 'org-version)
              (replace-regexp-in-string "\\..*" ".x" (org-version))
            "Unknown Version"))
  "Information about the creator of the HTML document."
  :group 'org-page
  :type 'string)


(provide 'op-vars)

;;; op-vars.el ends here
