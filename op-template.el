;;; op-template.el --- templating system based on mustache, required by org-page

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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'mustache)

(defun op/update-default-template-parameters ()
  "Update the default template parameters. It is only needed when user did some
customization to relevant variables."
  (ht-update
   op/default-template-parameters
   (ht ("site-main-title" op/site-main-title)
       ("site-sub-title" op/site-sub-title)
       ("github" op/personal-github-link)
       ("site-domain" (if (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                        op/site-domain)
                          (match-string 1 op/site-domain)
                        op/site-domain))
       ("disqus-shortname" op/personal-disqus-shortname)
       ("google-analytics-id" op/personal-google-analytics-id)
       ("google-analytics" (if op/personal-google-analytics-id t nil))))
  op/default-template-parameters)

(defun op/compose-template-parameters (attr-plist content)
  "Compose parameters for org file represented in current buffer.
ATTR-PLIST is the attribute plist of the buffer, retrieved by the combination of
`org-export--get-inbuffer-options' and `op/get-inbuffer-extra-options'."
  (let* ((info
          (org-combine-plists
           (org-export--get-global-options 'html)
           attr-plist))
         (title (org-element-interpret-data (plist-get info :title)))
         (author (org-element-interpret-data
                  (or (plist-get info :author) user-full-name)))
         (email (confound-email (or (plist-get info :email)
                                    user-mail-address)))
         (description (or (plist-get info :description) nil))
         (keywords (or (plist-get info :keywords) nil))
         (category (plist-get info :category))
         (show-meta-info (and (not (eq category 'index))
                              (not (eq category 'about))
                              (not (eq category 'none))))
         (creation-date (if (plist-get info :date)
                            (fix-timestamp-string
                             (org-element-interpret-data
                              (plist-get info :date)))
                          "N/A"))
         (mod-date (or (plist-get info :mod-date) "N/A"))
         (tag-links (mapconcat
                     #'(lambda (tag-name)
                         (mustache-render
                          "<a href=\"{{link}}\">{{name}}</a>"
                          (ht ("link" (op/generate-tag-uri tag-name))
                              ("name" tag-name))))
                     (plist-get info :tags) ", "))
         (show-comment (eq category 'blog))
         (disqus-id (plist-get info :uri))
         (disqus-url (concat (replace-regexp-in-string "/?$" "" op/site-domain)
                             disqus-id))
         (param-table (ht-create)))
    (ht-update param-table op/default-template-parameters)
    (ht-update
     param-table
     (ht ("page-title"        (concat title " - " op/site-main-title))
         ("author"            author)
         ("description"       description)
         ("keywords"          keywords)
         ("title"             title)
         ("content"           content)
         ("show-meta-info"    show-meta-info)
         ("creation-date"     creation-date)
         ("modification-date" mod-date)
         ("tags"              tag-links)
         ("show-comment"      show-comment)
         ("disqus-id"         disqus-id)
         ("disqus-url"        disqus-url)
         ("email"             email)))
    param-table))


(provide 'op-template)

;;; op-template.el ends here
