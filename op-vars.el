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

(defcustom op/config-file nil
  "The path of org-page config file"
  :group 'org-page
  :type 'file)

(defcustom op/get-config-option-function
  'op/get-config-option-from-file
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


(provide 'op-vars)

;;; op-vars.el ends here
