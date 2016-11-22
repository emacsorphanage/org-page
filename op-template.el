;;; op-template.el --- templating system based on mustache, required by org-page

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

;; templating system based on mustache.el, to replace `format-spec'.

;;; Code:

(require 'ox)
(require 'cl-lib)
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'op-util)
(require 'op-vars)
(require 'op-git)


(defun op/get-template-dir ()
  "Return the template directory, it is determined by variable
`op/theme-root-directory' with `op/theme' or `op/template-directory'."
  (or op/template-directory
      (file-name-as-directory
       (expand-file-name
        (format "%s/templates" (symbol-name op/theme))
        op/theme-root-directory))))

(defun op/get-cache-item (key)
  "Get the item associated with KEY in `op/item-cache', if `op/item-cache' is
nil or there is no item associated with KEY in it, return nil."
  (and op/item-cache
       (plist-get op/item-cache key)))

(defun op/update-cache-item (key value)
  "Update the item associated with KEY in `op/item-cache', if `op/item-cache' is
nil, initialize it."
  (if op/item-cache
      (plist-put op/item-cache key value)
    (setq op/item-cache `(,key ,value)))
  value)

(defmacro op/get-cache-create (key &rest body)
  "Firstly get item from `op/item-cache' with KEY, if item not found, evaluate
BODY and push the result into cache and return it."
  `(or (op/get-cache-item ,key)
       (op/update-cache-item ,key (funcall (lambda () ,@body)))))

(defun op/get-category-name (category)
  "Return the name of the CATEGORY based on op/category-config-alist :label property. 
Default to capitalized CATEGORY name if no :label property found."
  (let* ((config (cdr (or (assoc category op/category-config-alist)
                          (assoc "blog" op/category-config-alist)))))
    (or (plist-get config :label)
        (capitalize category))))

(defun op/render-header (&optional param-table)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (op/get-cache-create
    :header-template
    (message "Read header.mustache from file")
    (file-to-string (concat (op/get-template-dir) "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (or (op/read-org-option "TITLE") "Untitled")
                                 " - " op/site-main-title))
           ("author" (or (op/read-org-option "AUTHOR")
                         user-full-name "Unknown Author"))
           ("description" (op/read-org-option "DESCRIPTION"))
           ("keywords" (op/read-org-option "KEYWORDS"))))))

(defun op/render-navigation-bar (&optional param-table)
  "Render the navigation bar on each page. it will be read firstly from
`op/item-cache', if there is no cached content, it will be rendered
and pushed into cache from template. PARAM-TABLE is the hash table for mustache
to render the template. If it is not set or nil, this function will try to
render from a default hash table."
  (op/get-cache-create
   :nav-bar-html
   (message "Render navigation bar from template")
   (mustache-render
    (op/get-cache-create
     :nav-bar-template
     (message "Read nav.mustache from file")
     (file-to-string (concat (op/get-template-dir) "nav.mustache")))
    (or param-table
        (ht-merge (ht ("site-main-title" op/site-main-title)
                      ("site-sub-title" op/site-sub-title)
                      ("nav-categories"
                       (mapcar
                        #'(lambda (cat)
                            (ht ("category-uri"
                                 (concat "/" (encode-string-to-url cat) "/"))
                                ("category-name" (op/get-category-name cat))))
                        (sort (cl-remove-if
                               #'(lambda (cat)
                                   (or (string= cat "index")
                                       (string= cat "about")))
                               (op/get-file-category nil))
                              'string-lessp)))
                      ("github" op/personal-github-link)
                      ("avatar" op/personal-avatar)
                      ("site-domain" (if (string-match
                                          "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                          op/site-domain)
                                         (match-string 1 op/site-domain)
                                       op/site-domain)))
                  (if op/organization (ht ("authors-li" t)) (ht ("avatar" op/personal-avatar))))))))

(defun op/render-content (&optional template param-table)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `op/render-header'. `op/highlight-render' is `js' or `htmlize'."
  (mustache-render
   (op/get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (file-to-string (concat (op/get-template-dir)
                            (or template "post.mustache"))))
   (or param-table
       (ht ("title" (or (op/read-org-option "TITLE") "Untitled"))
           ("content"
            (cond ((eq op/highlight-render 'js)
                   (progn
                     (cl-letf (((symbol-function'org-html-fontify-code)
                                #'(lambda (code lang)
                                    (when code
                                      (org-html-encode-plain-text code)))))
                       (org-export-as op/export-backend nil nil t nil))))
                  ((eq op/highlight-render 'htmlize)
                   (org-export-as op/export-backend nil nil t nil))))))))

(defun op/render-footer (&optional param-table)
  "Render the footer on each page. PARAM-TABLE is similar to
`op/render-header'."
  (mustache-render
   (op/get-cache-create
    :footer-template
    (message "Read footer.mustache from file")
    (file-to-string (concat (op/get-template-dir) "footer.mustache")))
   (or param-table
       (let* ((filename (buffer-file-name))
              (title (or (op/read-org-option "TITLE") "Untitled"))
              (date (fix-timestamp-string
                     (or (op/read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (op/read-org-option "TAGS"))
              (tags (if tags
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (op/generate-tag-uri tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'trim-string (split-string tags "[:,]+" t))))))
              (category (funcall (or op/retrieve-category-function
                                     #'op/get-file-category)
                                 filename))
              (config (cdr (or (assoc category op/category-config-alist)
                               (assoc "blog" op/category-config-alist))))
              (uri (funcall (plist-get config :uri-generator)
                            (plist-get config :uri-template) date title)))
         (ht ("show-meta" (plist-get config :show-meta))
             ("show-comment" (plist-get config :show-comment))
             ("date" date)
             ("mod-date" (if (not filename)
                             (format-time-string "%Y-%m-%d")
                           (or (op/git-last-change-date
                                op/repository-directory
                                filename)
                               (format-time-string
                                "%Y-%m-%d"
                                (nth 5 (file-attributes filename))))))
             ("tags" tags)
             ("tag-links" (if (not tags) "N/A"
                            (mapconcat
                             #'(lambda (tag)
                                 (mustache-render
                                  "<a href=\"{{link}}\">{{name}}</a>" tag))
                             tags ", ")))
             ("author" (or (op/read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
             ("disqus-id" uri)
             ("disqus-url" (get-full-url uri))
             ("disqus-comment" (and (boundp 'op/personal-disqus-shortname)
                                    op/personal-disqus-shortname))
             ("disqus-shortname" op/personal-disqus-shortname)
             ("duoshuo-comment" (and (boundp 'op/personal-duoshuo-shortname)
                                     op/personal-duoshuo-shortname))
             ("duoshuo-shortname" op/personal-duoshuo-shortname)
             ("google-analytics" (and (boundp 'op/personal-google-analytics-id)
                                      op/personal-google-analytics-id))
             ("google-analytics-id" op/personal-google-analytics-id)
             ("creator-info" op/html-creator-string)
             ("email" (confound-email (or (op/read-org-option "EMAIL")
                                          user-mail-address
                                          "Unknown Email"))))))))

;;; this function is deprecated
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
       ("disqus-comment" (if op/personal-disqus-shortname t nil))
       ("duoshuo-shortname" op/personal-duoshuo-shortname)
       ("duoshuo-comment" (if op/personal-duoshuo-shortname t nil))
       ("google-analytics-id" op/personal-google-analytics-id)
       ("google-analytics" (if op/personal-google-analytics-id t nil))))
  op/default-template-parameters)

;;; this function is deprecated
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
         (disqus-url (get-full-url disqus-id))
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
