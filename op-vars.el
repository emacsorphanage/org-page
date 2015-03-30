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
  :tag "Org static page generator"
  :group 'org)

(defcustom op/project-config-alist nil
  "Association list to control org-page publishing behavior.

Each element of the alist is a org-page 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the publishing process.

  \(:property value :property value ... )

Most properties are optional, but some should always be set:

  `:repository-directory'

The git repository directory, where org files stored on branch
`:repository-org-branch', and generated html files stored on branch
`:repository-html-branch'.

  `:site-domain'

The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned.

  `:site-main-title'

The main title of entire site.

  `:site-sub-title'

The subtitle of entire site.

  `:repository-org-branch'

The branch where org files stored on, it is within repository presented by
`:repository-directory'.

  `:repository-html-branch'

The branch where generated html files stored on, it is within repository
presented by `:repository-directory'.

  `:theme-root-directory'

The root directory list that stores themes for page rendering. By default, it
points to the directory `themes' in org-page installation directory.

  `:theme'

The theme used for page generation.

  `:personal-github-link'

The personal github link.

  `:personal-avatar'

The link to an avatar image.

  `:personal-disqus-shortname'

The personal disqus shortname.

  `:personal-duoshuo-shortname'

The personal duoshuo shortname.

  `:personal-google-analytics-id'

Personal google analytics id.

  `:confound-email'

Determine whether email addresses should be confounded or not.

  `:category-ignore-list'

Ignore subdirs/categories for navigation

  `:get-title-function'

A function used to retrieve an org file's Title, it has no parameter and
run org file buffer.

  `:retrieve-category-function'

A function used to retrieve an org file's category, its parameter is the
org file's path, if parameter is nil, it should return all categories.

  `:html-creator-string'

Information about the creator of the HTML document.

  `:repo-files-function'

The function used to get all org files exported.

  `:web-server-docroot'

org-page can start a web server to test publish, this
set the server document root.

  `:web-server-port'

org-page can start a web server to test publish, this
set the server port.

You can see fallback value of above option in `op/config-fallback'"
  :group 'org-page
  :type 'alist)

(defcustom op/get-config-option-function
  'op/get-config-option-from-alist
  "The function used to get config option."
  :group 'org-page
  :type 'function)

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

(defvar op/current-project-name nil)

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
<description>{{item-description}}</description>
<pubDate>{{item-update-date}}</pubDate>
<guid>{{item-link}}</guid>
</item>
{{/items}}
</channel>
</rss>"
  "Template for RSS rendering.")

(defvar op/config-fallback
      `(:repository-directory nil
        :site-domain nil
        :site-main-title "org-page"
        :site-sub-title "static site generator"
        :repository-org-branch "source"
        :repository-html-branch "master"
        :theme-root-directory ,(list (concat op/load-directory "themes/"))
        :theme (default)
        :personal-github-link "https://github.com/tumashu/org-page"
        :personal-avatar nil
        :personal-disqus-shortname nil
        :personal-duoshuo-shortname nil
        :personal-google-analytics-id nil
        :category-ignore-list ("themes" "assets")
        :confound-email t
        :get-title-function op/get-title
        :retrieve-category-function op/get-file-category
        :repo-files-function op/git-all-files
        :web-server-docroot "~/.emacs.d/org-page-server/default"
        :web-server-port 9876
        :html-creator-string ,(format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
(<a href=\"http://orgmode.org\">Org mode</a> %s)"
(format "%s.x" emacs-major-version)
(if (fboundp 'org-version)
    (replace-regexp-in-string "\\..*" ".x" (org-version))
  "Unknown Version"))
"If User don't set an option, org-page will use fallback value of this option."))


(provide 'op-vars)

;;; op-vars.el ends here
