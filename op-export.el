;;; op-export.el --- Publication related functions required by org-page

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

;; org source publication related functions

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'dash)
(require 'cl-lib)
(require 'op-util)
(require 'op-vars)
(require 'op-git)
(require 'op-template)


(defun op/publish-changes (all-list change-plist pub-root-dir)
  "This function is for:
1. publish changed org files to html
2. delete html files which are relevant to deleted org files (NOT implemented)
3. update index pages
4. regenerate tag pages
ALL-LIST contains paths of all org files, CHANGE-PLIST contains two properties,
one is :update for files to be updated, another is :delete for files to be
deleted. PUB-ROOT-DIR is the root publication directory."
  (let* ((upd-list (plist-get change-plist :update))
         (del-list (plist-get change-plist :delete))
         attr-cell file-attr-list)
    (when (or upd-list del-list)
      (mapc
       #'(lambda (org-file)
           (with-temp-buffer
             (insert-file-contents org-file)
             (beginning-of-buffer)
             ;; somewhere need `buffer-file-name',make them happy
             (setq buffer-file-name org-file)
             (setq attr-cell (op/get-org-file-options
                              pub-root-dir
                              (member org-file upd-list)))
             (setq file-attr-list (cons (car attr-cell) file-attr-list))
             (when (member org-file upd-list)
               (op/publish-modified-file (cdr attr-cell)
                                         (plist-get (car attr-cell) :pub-dir)))
             (when (member org-file del-list)
               (op/handle-deleted-file org-file))
             (setq buffer-file-name nil) ;; dismiss `kill-anyway?'
             ))
       all-list)
      (unless (member
               (expand-file-name "index.org" op/repository-directory)
               all-list)
        (op/generate-default-index file-attr-list pub-root-dir))
      (unless (member
               (expand-file-name "about.org" op/repository-directory)
               all-list)
        (op/generate-default-about pub-root-dir))
      (op/update-category-index file-attr-list pub-root-dir)
      (op/update-rss file-attr-list pub-root-dir)
      (op/update-tags file-attr-list pub-root-dir)
      (when op/organization
        (op/update-authors file-attr-list pub-root-dir)))))

(defun op/get-org-file-options (pub-root-dir do-pub)
  "Retrieve all needed options for org file opened in current buffer.
PUB-ROOT-DIR is the root directory of published files, if DO-PUB is t, the
content of the buffer will be converted into html."
  (let* ((filename (buffer-file-name))
         (attr-plist `(:title ,(or (op/read-org-option "TITLE")
                                   "Untitled")
                              :date ,(fix-timestamp-string
                                      (or (op/read-org-option "DATE")
                                          (format-time-string "%Y-%m-%d")))
                              :mod-date ,(if (not filename)
                                             (format-time-string "%Y-%m-%d")
                                           (or (op/git-last-change-date
                                                op/repository-directory
                                                filename)
                                               (format-time-string
                                                "%Y-%m-%d"
                                                (nth 5 (file-attributes filename)))))
                              :thumb ,(op/read-org-option "THUMBNAIL")))
         assets-dir post-content
         asset-path asset-abs-path pub-abs-path converted-path
         component-table tags category cat-config)
    (setq tags (op/read-org-option "TAGS"))
    (when tags
      (plist-put
       attr-plist :tags (delete "" (mapcar 'trim-string
                                           (split-string tags "[:,]+" t)))))
    (when op/organization
      (plist-put
       attr-plist :authororg (delete "" (mapcar 'trim-string (split-string (or (op/read-org-option "AUTHOR")
                                                                               user-full-name
                                                                               "anonymous"
                                                                               ) "[:,]+" t)))))
    (setq category (funcall (or op/retrieve-category-function
                                #'op/get-file-category)
                            filename))
    (plist-put attr-plist :category category)
    (setq cat-config (cdr (or (assoc category op/category-config-alist)
                              (assoc "blog" op/category-config-alist))))
    (plist-put attr-plist :uri (funcall (plist-get cat-config :uri-generator)
                                        (plist-get cat-config :uri-template)
                                        (plist-get attr-plist :date)
                                        (plist-get attr-plist :title)))
    (plist-put attr-plist :pub-dir (file-name-as-directory
                                    (concat
                                     (file-name-as-directory pub-root-dir)
                                     (replace-regexp-in-string
                                      "\\`/" ""
                                      (plist-get attr-plist :uri)))))
    (when do-pub
      (princ attr-plist)
      (setq post-content (op/render-content))
      (setq assets-dir (file-name-as-directory
                        (concat (file-name-as-directory pub-root-dir)
                                "assets/"
                                (replace-regexp-in-string
                                 "\\`" "" (plist-get attr-plist :uri)))))
      (with-temp-buffer
        (insert post-content)
        (beginning-of-buffer)
        (while (re-search-forward
                ;;; TODO: not only links need to convert, but also inline
                ;;; images, may add others later
                ;; "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" nil t)
                "<[a-zA-Z]+[^/>]+\\(src\\|href\\|data\\)=\"\\([^\"]+\\)\"[^>]*>" nil t)
          (setq asset-path (match-string 2))
          (when (not (or (string-prefix-p "http://" asset-path)
                         (string-prefix-p "https://" asset-path)
                         (string-prefix-p "mailto:" asset-path)
                         (string-prefix-p "ftp://" asset-path)
                         (string-prefix-p "#" asset-path)
                         ;; TODO add more here
                         ))
            (setq asset-abs-path
                  (expand-file-name asset-path (file-name-directory filename)))
            (if (not (file-exists-p asset-abs-path))
                (message "[WARN] File %s in hyper link does not exist, org \
file: %s." asset-path filename)
              (unless (file-directory-p assets-dir)
                (mkdir assets-dir t))
              (copy-file asset-abs-path assets-dir t t t t)
              (setq pub-abs-path (concat assets-dir
                                         (file-name-nondirectory asset-path)))
              (unless (string-prefix-p pub-root-dir pub-abs-path)
                (message "[WARN] The publication root directory %s is not an \
ancestor directory of assets directory %s." pub-root-dir assets-dir))
              (setq converted-path
                    (concat "/" (file-relative-name pub-abs-path pub-root-dir)))
              (setq post-content
                    (replace-regexp-in-string
                     (regexp-quote asset-path) converted-path post-content))))))
      (setq component-table (ht ("header" (op/render-header))
                                ("nav" (op/render-navigation-bar))
                                ("content" post-content)
                                ("footer" (op/render-footer))))
      (plist-put attr-plist :description (or (op/read-org-option "DESCRIPTION")
                                             post-content)))
    (cons attr-plist component-table)))

(defun op/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun op/generate-uri (default-uri-template creation-date title)
  "Generate URI of org file opened in current buffer. It will be firstly created
by #+URI option, if it is nil, DEFAULT-URI-TEMPLATE will be used to generate the
uri. If CREATION-DATE is nil, current date will be used. The uri template option
can contain following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date
%f: base file name with suffix .html (a.org->a.html)
%t: title of current buffer"
  (let ((uri-template (or (op/read-org-option "URI")
                          default-uri-template))
        (date-list (split-string (if creation-date
                                     (fix-timestamp-string creation-date)
                                   (format-time-string "%Y-%m-%d"))
                                 "-"))
        (html-file-name (concat (file-name-base (buffer-file-name)) ".html"))
        (encoded-title (encode-string-to-url title)))
    (format-spec uri-template `((?y . ,(car date-list))
                                (?m . ,(cadr date-list))
                                (?d . ,(cl-caddr date-list))
                                (?f . ,html-file-name)
                                (?t . ,encoded-title)))))


(defun op/get-file-category (org-file)
  "Get org file category presented by ORG-FILE, return all categories if
ORG-FILE is nil. This is the default function used to get a file's category,
see `op/retrieve-category-function'. How to judge a file's category is based on
its name and its root folder name under `op/repository-directory'."
  (cond ((not org-file)
         (let ((cat-list '("index" "about" "blog"))) ;; 3 default categories
           (dolist (f (directory-files op/repository-directory))
             (when (and (not (equal f "."))
                        (not (equal f ".."))
                        (not (equal f ".git"))
                        (not (member f op/category-ignore-list))
                        (not (equal f "blog"))
                        (file-directory-p
                         (expand-file-name f op/repository-directory)))
               (setq cat-list (cons f cat-list))))
           cat-list))
        ((string= (expand-file-name "index.org" op/repository-directory)
                  (expand-file-name org-file)) "index")
        ((string= (expand-file-name "about.org" op/repository-directory)
                  (expand-file-name org-file)) "about")
        ((string= (file-name-directory (expand-file-name org-file))
                  op/repository-directory) "blog")
        (t (car (split-string (file-relative-name (expand-file-name org-file)
                                                  op/repository-directory)
                              "[/\\\\]+")))))

(defun op/publish-modified-file (component-table pub-dir)
  "Publish org file opened in current buffer. COMPONENT-TABLE is the hash table
used to render the template, PUB-DIR is the directory for published html file.
If COMPONENT-TABLE is nil, the publication will be skipped."
  (when component-table
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (string-to-file (mustache-render
                     (op/get-cache-create
                      :container-template
                      (message "Read container.mustache from file")
                      (file-to-string (concat (op/get-template-dir)
                                              "container.mustache")))
                     component-table)
                    (concat pub-dir "index.html") ;; 'html-mode ;; do NOT indent the code
                    )))

(defun op/handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )

(defun op/rearrange-category-sorted (file-attr-list)
  "Rearrange and sort attribute property lists from FILE-ATTR-LIST. Rearrange
according to category, and sort according to :sort-by property defined in
`op/category-config-alist', if category is not in `op/category-config-alist',
the default 'blog' category will be used. For sorting, later lies headmost."
  (let (cat-alist cat-list)
    (mapc
     #'(lambda (plist)
         (setq cat-list (cdr (assoc (plist-get plist :category) cat-alist)))
         (if cat-list
             (nconc cat-list (list plist))
           (setq cat-alist (cons (cons (plist-get plist :category)
                                       (list plist))
                                 cat-alist))))
     file-attr-list)
    (mapc
     #'(lambda (cell)
         (setcdr
          cell
          (sort (cdr cell)
                #'(lambda (plist1 plist2)
                    (<= (compare-standard-date
                         (fix-timestamp-string
                          (plist-get
                           plist1
                           (plist-get
                            (cdr (or (assoc (plist-get plist1 :category)
                                            op/category-config-alist)
                                     (assoc "blog"
                                            op/category-config-alist)))
                            :sort-by)))
                         (fix-timestamp-string
                          (plist-get
                           plist2
                           (plist-get
                            (cdr (or (assoc (plist-get plist2 :category)
                                            op/category-config-alist)
                                     (assoc "blog"
                                            op/category-config-alist)))
                            :sort-by))))
                        0)))))
     cat-alist)))

(defun op/update-category-index (file-attr-list pub-base-dir)
  "Update index page of different categories. FILE-ATTR-LIST is the list of all
file attribute property lists. PUB-BASE-DIR is the root publication directory."
  (let* ((sort-alist (op/rearrange-category-sorted file-attr-list))
         cat-dir)
    (mapc
     #'(lambda (cat-list)
         (unless (not (plist-get (cdr (or (assoc (car cat-list)
                                                 op/category-config-alist)
                                          (assoc "blog"
                                                 op/category-config-alist)))
                                 :category-index))
           (setq cat-dir (file-name-as-directory
                          (concat (file-name-as-directory pub-base-dir)
                                  (encode-string-to-url (car cat-list)))))
           (unless (file-directory-p cat-dir)
             (mkdir cat-dir t))
           (string-to-file
            (mustache-render
             (op/get-cache-create
              :container-template
              (message "Read container.mustache from file")
              (file-to-string (concat (op/get-template-dir)
                                      "container.mustache")))
             (ht ("header"
                  (op/render-header
                   (ht ("page-title" (concat (op/get-category-name (car cat-list))
                                             " Index - "
                                             op/site-main-title))
                       ("author" (or user-full-name "Unknown Author")))))
                 ("nav" (op/render-navigation-bar))
                 ("content"
                  (op/render-content
                   "category-index.mustache"
                   (ht ("cat-name" (op/get-category-name (car cat-list)))
                       ("posts"
                        (mapcar
                         #'(lambda (attr-plist)
                             (ht ("date"
                                  (funcall
				   op/date-final-format
				   (plist-get
				    attr-plist
				    (plist-get
				     (cdr (or (assoc
					       (plist-get attr-plist :category)
					       op/category-config-alist)
					      (assoc
					       "blog"
					       op/category-config-alist)))
				     :sort-by))))
                                 ("post-uri" (plist-get attr-plist :uri))
                                 ("post-title" (plist-get attr-plist :title))))
                         (cdr cat-list))))))
                 ("footer"
                  (op/render-footer
                   (ht ("show-meta" nil)
                       ("show-comment" nil)
                       ("author" (or user-full-name "Unknown Author"))
                       ("google-analytics" (and
                                            (boundp
                                             'op/personal-google-analytics-id)
                                            op/personal-google-analytics-id))
                       ("google-analytics-id" op/personal-google-analytics-id)
                       ("creator-info" op/html-creator-string)
                       ("email" (confound-email (or user-mail-address
                                                    "Unknown Email"))))))))
            (concat cat-dir "index.html") 'html-mode)))
     sort-alist)))

(defun op/generate-default-index (file-attr-list pub-base-dir)
  "Generate default index page, only if index.org does not exist. FILE-ATTR-LIST
is the list of all file attribute property lists. PUB-BASE-DIR is the root
publication directory."
  (let ((sort-alist (op/rearrange-category-sorted file-attr-list))
        (id 0))
    (string-to-file
     (mustache-render
      (op/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (file-to-string (concat (op/get-template-dir) "container.mustache")))
      (ht ("header"
           (op/render-header
            (ht ("page-title" (concat "Index - " op/site-main-title))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (op/render-navigation-bar))
          ("content"
           (op/render-content
            "index.mustache"
            (ht ("categories"
                 (mapcar
                  #'(lambda (cell)
                      (ht ("id" (setq id (+ id 1)))
                          ("category" (capitalize (car cell)))
                          ("posts" (mapcar
                                    #'(lambda (plist)
                                        (ht ("post-uri"
                                             (plist-get plist :uri))
                                            ("post-title"
                                             (plist-get plist :title))
                                            ("post-desc"
                                             (plist-get plist :description))
                                            ("post-date"
                                             (funcall op/date-final-format (plist-get plist :date)))
                                            ("post-thumb"
                                             (or (plist-get plist :thumb) ""))))
                                    (cdr cell)))))
                  (cl-remove-if
                   #'(lambda (cell)
                       (string= (car cell) "about"))
                   sort-alist))))))
          ("footer"
           (op/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (and (boundp
                                          'op/personal-google-analytics-id)
                                         op/personal-google-analytics-id))
                ("google-analytics-id" op/personal-google-analytics-id)
                ("creator-info" op/html-creator-string)
                ("email" (confound-email (or user-mail-address
                                             "Unknown Email"))))))))
     (concat pub-base-dir "index.html") 'html-mode)))

(defun op/generate-default-about (pub-base-dir)
  "Generate default about page, only if about.org does not exist. PUB-BASE-DIR
is the root publication directory."
  (let ((pub-dir (expand-file-name "about/" pub-base-dir)))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (string-to-file
     (mustache-render
      (op/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (file-to-string (concat (op/get-template-dir) "container.mustache")))
      (ht ("header"
           (op/render-header
            (ht ("page-title" (concat "About - " op/site-main-title))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (op/render-navigation-bar))
          ("content"
           (op/render-content
            "about.mustache"
            (ht ("author" (or user-full-name "Unknown Author")))))
          ("footer"
           (op/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (and (boundp
                                          'op/personal-google-analytics-id)
                                         op/personal-google-analytics-id))
                ("google-analytics-id" op/personal-google-analytics-id)
                ("creator-info" op/html-creator-string)
                ("email" (confound-email (or user-mail-address
                                             "Unknown Email"))))))))
     (concat pub-dir "index.html") 'html-mode)))

(defun op/generate-tag-uri (tag-name)
  "Generate tag uri based on TAG-NAME."
  (concat "/tags/" (encode-string-to-url tag-name) "/"))

(defun op/update-tags (file-attr-list pub-base-dir)
  "Update tag pages. FILE-ATTR-LIST is the list of all file attribute property
lists. PUB-BASE-DIR is the root publication directory.
TODO: improve this function."
  (let ((tag-base-dir (expand-file-name "tags/" pub-base-dir))
        tag-alist tag-list tag-dir)
    (mapc
     #'(lambda (attr-plist)
         (mapc
          #'(lambda (tag-name)
              (setq tag-list (assoc tag-name tag-alist))
              (unless tag-list
                (add-to-list 'tag-alist (setq tag-list `(,tag-name))))
              (nconc tag-list (list attr-plist)))
          (plist-get attr-plist :tags)))
     file-attr-list)
    (unless (file-directory-p tag-base-dir)
      (mkdir tag-base-dir t))
    (string-to-file
     (mustache-render
      (op/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (file-to-string (concat (op/get-template-dir) "container.mustache")))
      (ht ("header"
           (op/render-header
            (ht ("page-title" (concat "Tag Index - " op/site-main-title))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (op/render-navigation-bar))
          ("content"
           (op/render-content
            "tag-index.mustache"
            (ht ("tags"
                 (mapcar
                  #'(lambda (tag-list)
                      (ht ("tag-name" (car tag-list))
                          ("tag-uri" (op/generate-tag-uri (car tag-list)))
                          ("count" (number-to-string (length (cdr tag-list))))))
                  tag-alist)))))
          ("footer"
           (op/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (and (boundp
                                          'op/personal-google-analytics-id)
                                         op/personal-google-analytics-id))
                ("google-analytics-id" op/personal-google-analytics-id)
                ("creator-info" op/html-creator-string)
                ("email" (confound-email (or user-mail-address
                                             "Unknown Email"))))))))
     (concat tag-base-dir "index.html") 'html-mode)
    (mapc
     #'(lambda (tag-list)
         (setq tag-dir (file-name-as-directory
                        (concat tag-base-dir
                                (encode-string-to-url (car tag-list)))))
         (unless (file-directory-p tag-dir)
           (mkdir tag-dir t))
         (when op/tag-rss
           (let ((last-10-posts
                  (-take 10 (--sort (>= 0 (compare-standard-date
                                           (fix-timestamp-string
                                            (plist-get it :mod-date))
                                           (fix-timestamp-string
                                            (plist-get other :mod-date))))
                                    (cdr tag-list)))))
             (string-to-file
              (mustache-render
               op/rss-template
               (ht ("title" op/site-main-title)
                   ("link" op/site-domain)
                   ("description" op/site-sub-title)
                   ("date" (format-time-string "%a, %d %b %Y %T %Z"))
                   ("items" (--map
                             (ht
                              ("item-title" (plist-get it :title))
                              ("item-link" (get-full-url (plist-get it :uri)))
                              ("item-description" (plist-get it :description))
                              ("item-update-date" (plist-get it :mod-date)))
                             last-10-posts))))
              (concat tag-dir "rss.xml"))))
         (string-to-file
          (mustache-render
           (op/get-cache-create
            :container-template
            (message "Read container.mustache from file")
            (file-to-string (concat (op/get-template-dir)
                                    "container.mustache")))
           (ht ("header"
                (op/render-header
                 (ht ("page-title" (concat "Tag: " (car tag-list)
                                           " - " op/site-main-title))
                     ("author" (or user-full-name "Unknown Author")))))
               ("nav" (op/render-navigation-bar))
               ("content"
                (op/render-content
                 "tag.mustache"
                 (ht ("tag-name" (car tag-list))
                     ("posts"
                      (mapcar
                       #'(lambda (attr-plist)
                           (ht ("post-uri" (plist-get attr-plist :uri))
                               ("post-title" (plist-get attr-plist :title))
                               ("post-date" (plist-get attr-plist :date))))
                       (cdr tag-list))))))
               ("footer"
                (op/render-footer
                 (ht ("show-meta" nil)
                     ("show-comment" nil)
                     ("author" (or user-full-name "Unknown Author"))
                     ("google-analytics" (and (boundp
                                               'op/personal-google-analytics-id)
                                              op/personal-google-analytics-id))
                     ("google-analytics-id" op/personal-google-analytics-id)
                     ("creator-info" op/html-creator-string)
                     ("email" (confound-email (or user-mail-address
                                                  "Unknown Email"))))))))
          (concat tag-dir "index.html") 'html-mode))
     tag-alist)))

(defun op/generate-author-uri (author-name)
  "Generate author uri based on AUTHOR-NAME."
  (concat "/authors/" (encode-string-to-url author-name) "/"))

(defun op/update-authors (file-attr-list pub-base-dir)
  "Update author pages. FILE-ATTR-LIST is the list of all file attribute property
lists. PUB-BASE-DIR is the root publication directory.
TODO: improve this function."
  (let ((author-base-dir (expand-file-name "authors/" pub-base-dir))
        author-alist author-list author-dir)
    (mapc
     #'(lambda (attr-plist)
         (mapc
          #'(lambda (author-name)
              (setq author-list (assoc author-name author-alist))
              (unless author-list
                (add-to-list 'author-alist (setq author-list `(,author-name))))
              (nconc author-list (list attr-plist)))
          (plist-get attr-plist :authororg)))
     file-attr-list)
    (unless (file-directory-p author-base-dir)
      (mkdir author-base-dir t))
    (string-to-file
     (mustache-render
      (op/get-cache-create
       :container-template
       (message "Read container.mustache from file")
       (file-to-string (concat (op/get-template-dir) "container.mustache")))
      (ht ("header"
           (op/render-header
            (ht ("page-title" (concat "Author Index - " op/site-main-title))
                ("author" (or user-full-name "Unknown Author")))))
          ("nav" (op/render-navigation-bar))
          ("content"
           (op/render-content
            "author-index.mustache"
            (ht ("authors"
                 (mapcar
                  #'(lambda (author-list)
                      (ht ("author-name" (car author-list))
                          ("author-uri" (op/generate-author-uri (car author-list)))
                          ("count" (number-to-string (length (cdr author-list))))))
                  author-alist)))))
          ("footer"
           (op/render-footer
            (ht ("show-meta" nil)
                ("show-comment" nil)
                ("author" (or user-full-name "Unknown Author"))
                ("google-analytics" (and (boundp
                                          'op/personal-google-analytics-id)
                                         op/personal-google-analytics-id))
                ("google-analytics-id" op/personal-google-analytics-id)
                ("creator-info" op/html-creator-string)
                ("email" (confound-email (or user-mail-address
                                             "Unknown Email"))))))))
     (concat author-base-dir "index.html") 'html-mode)
    (mapc
     #'(lambda (author-list)
         (setq author-dir (file-name-as-directory
                           (concat author-base-dir
                                   (encode-string-to-url (car author-list)))))
         (unless (file-directory-p author-dir)
           (mkdir author-dir t))
         (string-to-file
          (mustache-render
           (op/get-cache-create
            :container-template
            (message "Read container.mustache from file")
            (file-to-string (concat (op/get-template-dir)
                                    "container.mustache")))
           (ht ("header"
                (op/render-header
                 (ht ("page-title" (concat "Author: " (car author-list)
                                           " - " op/site-main-title))
                     ("author" (or user-full-name "Unknown Author")))))
               ("nav" (op/render-navigation-bar))
               ("content"
                (op/render-content
                 "author.mustache"
                 (ht ("author-name" (car author-list))
                     ("posts"
                      (mapcar
                       #'(lambda (attr-plist)
                           (ht ("post-uri" (plist-get attr-plist :uri))
                               ("post-title" (plist-get attr-plist :title))
                               ("post-date" (plist-get attr-plist :date))))
                       (cdr author-list))))))
               ("footer"
                (op/render-footer
                 (ht ("show-meta" nil)
                     ("show-comment" nil)
                     ("author" (or user-full-name "Unknown Author"))
                     ("google-analytics" (and (boundp
                                               'op/personal-google-analytics-id)
                                              op/personal-google-analytics-id))
                     ("google-analytics-id" op/personal-google-analytics-id)
                     ("creator-info" op/html-creator-string)
                     ("email" (confound-email (or user-mail-address
                                                  "Unknown Email"))))))))
          (concat author-dir "index.html") 'html-mode))
     author-alist)))

(defun op/update-rss (file-attr-list pub-base-dir)
  "Update RSS. FILE-ATTR-LIST is the list of all file attribute property lists.
PUB-BASE-DIR is the root publication directory."
  (let ((last-10-posts
         (-take 10 (--sort (>= 0 (compare-standard-date
                                  (fix-timestamp-string
                                   (plist-get it :mod-date))
                                  (fix-timestamp-string
                                   (plist-get other :mod-date))))
                           (--filter (not (or
                                           (string= (plist-get it :category)
                                                    "index")
                                           (string= (plist-get it :category)
                                                    "about")))
                                     file-attr-list)))))
    (string-to-file
     (mustache-render
      op/rss-template
      (ht ("title" op/site-main-title)
          ("link" op/site-domain)
          ("description" op/site-sub-title)
          ("date" (format-time-string "%a, %d %b %Y %T %Z"))
          ("items" (--map (ht ("item-title" (plist-get it :title))
                              ("item-link" (get-full-url (plist-get it :uri)))
                              ("item-description" (plist-get it :description))
                              ("item-update-date" (plist-get it :mod-date)))
                          last-10-posts))))
     (concat pub-base-dir "rss.xml"))))


(provide 'op-export)

;;; op-export.el ends here
