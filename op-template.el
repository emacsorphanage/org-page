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
;; (require 'mustache)
(autoload 'mustache-render "mustache")
(require 'op-util)
(require 'op-vars)
(require 'op-config)
(require 'op-git)


(defun op/get-template-file (template-file-name)
  "Get path of template file which name is `template-file-name'."
  (car (remove nil (mapcar
                    #'(lambda (dir)
                        (let ((file (concat (file-name-as-directory dir)
                                            template-file-name)))
                          (when (file-exists-p file)
                            file)))
                    (op/get-theme-dirs nil nil 'templates)))))

(defun op/get-title ()
  "Get the title of org file."
  (or (op/read-org-option "TITLE")
      (file-name-sans-extension (buffer-name))))

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

(defun op/render-header (&optional param-table)
  "Render the header on each page. PARAM-TABLE is the hash table from mustache
to render the template. If it is not set or nil, this function will try to build
a hash table accordint to current buffer."
  (mustache-render
   (op/get-cache-create
    :header-template
    (message "Read header.mustache from file")
    (op/file-to-string (op/get-template-file "header.mustache")))
   (or param-table
       (ht ("page-title" (concat (funcall (op/get-config-option :get-title-function))
                                 " - " (op/get-config-option :site-main-title)))
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
  (let ((site-domain (op/get-site-domain)))
    (op/get-cache-create
     :nav-bar-html
     (message "Render navigation bar from template")
     (mustache-render
      (op/get-cache-create
       :nav-bar-template
       (message "Read nav.mustache from file")
       (op/file-to-string (op/get-template-file "nav.mustache")))
      (or param-table
          (ht ("site-main-title" (op/get-config-option :site-main-title))
              ("site-sub-title" (op/get-config-option :site-sub-title))
              ("nav-categories"
               (mapcar
                #'(lambda (cat)
                    (ht ("category-uri"
                         (concat "/" (op/encode-string-to-url cat) "/"))
                        ("category-name" (capitalize cat))))
                (sort (remove-if
                       #'(lambda (cat)
                           (or (string= cat "index")
                               (string= cat "about")))
                       (op/get-file-category nil))
                      'string-lessp)))
              ("github" (op/get-config-option :personal-github-link))
              ("avatar" (op/get-config-option :personal-avatar))
              ("site-domain" (if (string-match
                                  "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                  site-domain)
                                 (match-string 1 site-domain)
                               site-domain))))))))

(defun op/render-content (&optional template param-table)
  "Render the content on each page. TEMPLATE is the template name for rendering,
if it is not set of nil, will use default post.mustache instead. PARAM-TABLE is
similar to `op/render-header'."
  (mustache-render
   (op/get-cache-create
    (if template
        (intern (replace-regexp-in-string "\\.mustache$" "-template" template))
      :post-template)
    (message (concat "Read " (or template "post.mustache") " from file"))
    (op/file-to-string (op/get-template-file
                     (or template "post.mustache"))))
   (or param-table
       (ht ("title" (funcall (op/get-config-option :get-title-function)))
           ("content" (cl-flet ((org-html-fontify-code
                                 (code lang)
                                 (when code (org-html-encode-plain-text code))))
                        (org-export-as 'html nil nil t nil)))))))

(defun op/render-footer (&optional param-table)
  "Render the footer on each page. PARAM-TABLE is similar to
`op/render-header'."
  (mustache-render
   (op/get-cache-create
    :footer-template
    (message "Read footer.mustache from file")
    (op/file-to-string (op/get-template-file "footer.mustache")))
   (or param-table
       (let* ((filename (buffer-file-name))
              (title (funcall (op/get-config-option :get-title-function)))
              (date (op/fix-timestamp-string
                     (or (op/read-org-option "DATE")
                         (format-time-string "%Y-%m-%d"))))
              (tags (op/read-org-option "TAGS"))
              (tags (if tags
                        (mapcar
                         #'(lambda (tag-name)
                             (ht ("link" (op/generate-tag-uri tag-name))
                                 ("name" tag-name)))
                         (delete "" (mapcar 'op/trim-string (split-string tags "[:,]+" t))))))
              (category (funcall (or (op/get-config-option :retrieve-category-function)
                                     op/get-file-category)
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
                                (op/get-repository-directory)
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
                             tags " ")))
             ("author" (or (op/read-org-option "AUTHOR")
                           user-full-name
                           "Unknown Author"))
             ("disqus-id" uri)
             ("disqus-url" (op/get-full-url uri))
             ("disqus-comment" (op/get-config-option :personal-disqus-shortname))
             ("disqus-shortname" (op/get-config-option :personal-disqus-shortname))
             ("duoshuo-comment" (op/get-config-option :personal-duoshuo-shortname))
             ("duoshuo-shortname" (op/get-config-option :personal-duoshuo-shortname))
             ("google-analytics" (op/get-config-option :personal-google-analytics-id))
             ("google-analytics-id" (op/get-config-option :personal-google-analytics-id))
             ("creator-info" (op/get-html-creator-string))
             ("email" (op/confound-email-address (or (op/read-org-option "EMAIL")
                                                     user-mail-address
                                                     "Unknown Email"))))))))

;;; this function is deprecated
(defun op/update-default-template-parameters ()
  "Update the default template parameters. It is only needed when user did some
customization to relevant variables."
  (let ((site-domain (op/get-site-domain)))
    (ht-update
     op/default-template-parameters
     (ht ("site-main-title" (op/get-config-option :site-main-title))
         ("site-sub-title" (op/get-config-option :site-sub-title))
         ("github" (op/get-config-option :personal-github-link))
         ("site-domain" (if (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'"
                                          site-domain)
                            (match-string 1 site-domain)
                          site-domain))
         ("disqus-shortname" (op/get-config-option :personal-disqus-shortname))
         ("disqus-comment" (if (op/get-config-option :personal-disqus-shortname) t nil))
         ("duoshuo-shortname" (op/get-config-option :personal-duoshuo-shortname))
         ("duoshuo-comment" (if (op/get-config-option :personal-duoshuo-shortname) t nil))
         ("google-analytics-id" (op/get-config-option :personal-google-analytics-id))
         ("google-analytics" (if (op/get-config-option :personal-google-analytics-id) t nil))))))

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
         (email (op/confound-email-address (or (plist-get info :email)
                                    user-mail-address)))
         (description (or (plist-get info :description) nil))
         (keywords (or (plist-get info :keywords) nil))
         (category (plist-get info :category))
         (show-meta-info (and (not (eq category 'index))
                              (not (eq category 'about))
                              (not (eq category 'none))))
         (creation-date (if (plist-get info :date)
                            (op/fix-timestamp-string
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
         (disqus-url (op/get-full-url disqus-id))
         (param-table (ht-create)))
    (ht-update param-table op/default-template-parameters)
    (ht-update
     param-table
     (ht ("page-title"        (concat title " - " (op/get-config-option :site-main-title)))
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
