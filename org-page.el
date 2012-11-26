;;; org-page.el --- static page generator based on org mode

;; Copyright (C) 2012 Kelvin Hu.

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/kelvinh/org-page
;; Version: 0.1

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

;; This program can generate a complete static website based on
;; org-mode, with only a little configuration.
;;
;; org-page.el provides the following features:
;;
;; + much more beautiful theme
;; + customized theme support
;; + recent posts support (auto generated)
;; + tags support (auto generated)
;; + search engine support (auto generated)
;;
;; To use org-page, you just need a little configuration,as follows:
;;
;; 1. `op/root-directory': the root directory of your project, org-page
;;    need to know this directory to do further action, and actually,
;;    you need to put your all org files(include subdirectory) into a
;;    folder named "src" under this directory, and then org-page will
;;    automatically publish them to a folder named "pub", which is also
;;    under this directory
;; 2. `op/publish-search-url': the URL search engine will base on, it
;;    should be the URL of your personal site
;;
;; So, the following simple code will make org-page work:
;;
;;    (add-to-list 'load-path "path/to/org-page")
;;    (require 'org-page)
;;    (setq op/root-directory "path/to/your/project")
;;    (setq op/publish-search-url "your.personal.site.com")
;;    (global-set-key (kbd "<f9>") 'op/publish-pages)
;;    (global-set-key (kbd "C-<f9>") (lambda () (interactive)
;;                                              (op/publish-pages t)))
;;
;; Now you can publish your pages with a press: <F9>, then your fresh
;; pages will be generated, or if you want to republish all your files
;; even they have already been published and not modified since last
;; publishing, your can just press Ctrl + <F9>.
;;
;; The two variable above are required, and the following variables
;; are optional, but also could be customized:
;;
;; 1. `op/exclude-filename-regexp': describes which org files in the
;;    project subdirectory "src" should not be published
;; 2. `op/include-filename-regexp': describes which org files in the
;;    project subdirectory "src" should always be published
;; 3. `op/theme-directory': the directory stores themes, it has a
;;    default value, you could customize it to your own value
;; 4. `op/theme': the theme will be used, default value is 'default,
;;    and sorry for that no more themes are provided at present
;; 5. `op/tag-directory': the folder stores auto-generated tags
;; 6. `op/tag-index-filename': filename of auto-generated tag index

;;; Code:

(require 'org-page-util)

(defgroup org-page nil
  "Options for generating static pages using org-page."
  :tag "Org static page generator"
  :group 'org)

(defcustom op/root-directory nil
  "the root directory of whole project"
  :group 'org-page
  :type 'string)

(defvar op/src-root-directory nil
  "the directory stores source org files")

(defvar op/pub-root-directory nil
  "the directory stores published files")

(defvar op/pub-html-directory nil
  "the directory stores published html files")

(defvar op/pub-org-directory nil
  "the directory stores published org-htmlized files")

(defcustom op/exclude-filename-regexp nil
  "regular expression, filename matches this expression will not be published.
please also see `op/include-filename-regexp'."
  :group 'org-page
  :type 'string)

(defcustom op/include-filename-regexp nil
  "regular expression, filename matches this expression will always be published.
it has higher priority than `op/exclude-filename-regexp'."
  :group 'org-page
  :type 'string)

(defconst op/load-file-name load-file-name
  "the filename that org-page.el was originally loaded from")

(defcustom op/theme-directory (cond
                               (op/load-file-name (concat (file-name-directory op/load-file-name) "themes/"))
                               ((symbol-file 'op/publish-pages) (concat (file-name-directory (symbol-file 'op/publish-pages)) "themes/"))
                               ((equal (file-name-nondirectory buffer-file-name) "org-page.el") (concat (file-name-directory buffer-file-name) "themes/"))
                               (t nil))
  "the directory stores org-page styles/scripts/images"
  :group 'org-page
  :type 'string)

(defcustom op/theme 'default
  "the theme used for page generation"
  :group 'org-page
  :type 'symbol)

(defcustom op/tag-directory "tags/"
  "the directory used to store generated tags"
  :group 'org-page
  :type 'string)

(defcustom op/tag-index-filename "index.org"
  "the index tag filename"
  :group 'org-page
  :type 'string)

(defcustom op/category-directory "categories/"
  "the directory used to store generated categories"
  :group 'org-page
  :type 'string)

(defcustom op/category-index-filename "index.org"
  "the index category filename"
  :group 'org-page
  :type 'string)

(defcustom op/publish-site-url nil
  "the domain name of entire site, it is better to assign it with prefix
http:// or https://, if not assigned, org-page will assume that the site
uses http protocol"
  :group 'org-page
  :type 'string)

(defcustom op/publish-site-title "org-page"
  "the title of entire site"
  :group 'org-page
  :type 'string)

(defcustom op/personal-github-link "https://github.com/kelvinh/org-page"
  "the personal github link"
  :group 'org-page
  :type 'string)

(defcustom op/personal-disqus-shortname nil
  "the personal disqus shortname"
  :group 'org-page
  :type 'string)

; TODO remove "TODO" in below string after rss feature implemented
(defconst op/publish-html-header-template
  "<h1><a href=\"/\">%h</a></h1>
   <nav id=\"main-nav\">
     <ul id=\"nav-list-main\">
       <li><a href=\"%s\" class=\"menu\">Sitemap</a></li>
       <li><a href=\"%c\" class=\"menu\">Categories</a></li>
       <li><a href=\"%t\" class=\"menu\">Tags</a></li>
       <li><a href=\"%r\" class=\"menu\">Recent Posts</a></li>
     </ul>
   </nav>
   <nav id=\"sub-nav\" class=\"align-right\">
     <div class=\"social\">
         <a class=\"github\" title=\"GitHub\" href=\"%g\">GitHub</a>
         <!-- TODO: change the link below to proper value  -->
         <a class=\"rss\" title=\"RSS(TODO)\" href=\"TODO\">RSS</a>
     </div>
     <form class=\"search\" target=\"_blank\" method=\"get\" action=\"http://google.com/search\">
       <input type=\"hidden\" value=\"site:%u\" name=\"q\" />
       <input type=\"text\" class=\"align-right\" results=\"0\" name=\"q\" />
     </form>
   </nav>"
  "the template used to construct page header, below parameters can be used:
%h: the title/headline of entire site (defined by `op/publish-site-title')
%s: the relative path to sitemap html file
%c: the relative path to category root html file
%t: the relative path to tag root html file
%r: the relative path to recent posts html file
%g: the github link (defined by `op/personal-github-link')
%u: the url of current site, used for search (defined by `op/publish-site-url')")

(defcustom op/publish-html-style-list '("main.css")
  "style file name list, which will be included in exported html files.
these files will be considered in folder <`:base-directory'>/media/css as default."
  :group 'org-page
  :type 'list)

;; TODO replace the tags with correct ones
(defcustom op/publish-meta-info "<div id=\"post-meta\">
  <span title=\"post date\" class=\"post-info\">%h</span>
  <span title=\"last modification date\" class=\"update-info\">%m</span>
  <span title=\"category\" class=\"category\">%g</span>
  <span title=\"tags\" class=\"tags\">%t</span>
  <span title=\"author\" class=\"author\">%a</span>
  <span title=\"htmlized org source file\" class=\"org-source\"><a href=\"%l\">htmlized org source</a></span>
</div>"
  "the meta info of current post, it will be used to compose `op/publish-html-postamble-template',
please see `op/publish-html-postamble-template' for detailed information."
  :group 'org-page
  :type 'string)

;; TODO customize the TODO items
(defcustom op/publish-comment "<section id=\"comment\">
  <h1 class=\"title\">Comments</h1>
  <div id=\"disqus_thread\"></div>
  <script type=\"text/javascript\">
    // TODO: change the following variables to proper value
    var disqus_developer = 1;
    //var disqus_title = \"testing\";
    var disqus_identifier = \"%n\";
    var disqus_url = \"%u\";
    var disqus_shortname = '%s'; // required: replace example with your forum shortname

    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function() {
      var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
      dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
      (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
  </script>
  <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
  <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
</section>"
  "the comment section of current post, it will be used to compose `op/publish-html-postamble-template',
please see `op/publish-html-postamble-template' for detailed information."
  :group 'org-page
  :type 'string)

(defcustom op/publish-footer "<div id=\"footer\">
  <p class=\"creator\">Generated by %c</p>
  <p class=\"copyright\">
    Copyright &copy; 2012 <a href=\"mailto:%i\">Kelvin Hu</a>
    &nbsp;&nbsp;-&nbsp;&nbsp;
    Powered by <a href=\"https://github.com/kelvinh/org-page\" target=\"_blank\">org-page</a>
  </p>
</div>"
  "the footer of current post, it will be used to compose `op/publish-html-postamble-template',
please see `op/publish-html-postamble-template' for detailed information."
  :group 'org-page
  :type 'string)

; TODO the copyright info in below variable should could be customized
(defcustom op/publish-html-postamble-template (concat op/publish-meta-info op/publish-comment op/publish-footer)
;; "
;; <!--<div id=\"post-meta\">
;;   <p class=\"post-info\">Posted on %h, last modified on %m. Author: %a. (view the <a href=\"%l\">htmlized org file</a>)</p>
;; </div>-->
;; "
  "the template used to construct page footer, it is composed by `op/publish-meta-info',
`op/publish-comment' and `op/publish-footer', since some pages do not need comments, so
the second component should be removed from these pages.
below parameter can be used:
%a: author's name
%c: creator versions (org/emacs versions)
%d: publish date (default is publishing time, can be customized by #+DATE in org file)
%e: author's email
%v: validation-link, will be replaced by `org-export-html-validation-link'

%h: last changed date (this change means meta change, not content change)
%m: last modified date
%g: category of current file, it will be automatically wrapped with <a> html tag, like %t
%t: tags of file, it will be expanded to the following format(assume the file has tag tag1, tag2):
<a href=\"tag1-link\">tag1</a>, <a href=\"tag2-link\">tag2</a>
%n: javascript variable 'disqus_identifier' of current page
%u: javascript variable 'disqus_url' of current page
%s: javascript variable 'disqus_shortname' of current page
%i: author's email, the difference from %e is this one will keep the email address unchanged,
but %e will expand it to html tag <a href=\"mailto:\"> automatically
%l: the link links to the corresponding htmlized org file"
  :group 'org-page
  :type 'string)

(defvar op/org-file-info-list nil
  "the list stores info of org files, each item is a plist, includes following keywords:
:path            old path before project structure reorganization
:new-path        current path after project structure reorganization
:creation-date   creation date of the file
:mod-date        last modification date of the file
:title           title of the file
:tags            tags of the file
:category        category of the file")

;-----------------------------------------------------------------------
(defun op/project-initialize ()
  "initialize the whole project"
  (unless op/root-directory
    (error "Please set the project root directory `%s' first before we can continue."
           (symbol-name 'op/root-directory)))

  (unless op/theme-directory
    (error "Well, org-page cannot detect where the theme is installed, please set the theme
directory `%s' first, usually it is <org-page directory>/themes/"
           (symbol-name 'op/theme-directory)))

  (unless op/publish-site-url
    (error "Please specify the URL(`%s'), which will be used for searching and commenting."
           (symbol-name 'op/publish-site-url)))
  (unless (or (string-prefix-p "http://" op/publish-site-url)
              (string-prefix-p "https://" op/publish-site-url))
    (setq op/publish-site-url (concat "http://" op/publish-site-url)))

  (unless op/personal-disqus-shortname
    (error "Please specify your personal disqus shortname(`%s'), which will be used for commenting."
           (symbol-name 'op/personal-disqus-shortname)))

  (unless op/theme
    (setq op/theme 'default))

  ;;; expand the root-directory path, to avoid path looks like ~/xxx
  (setq op/root-directory (expand-file-name op/root-directory))

  ; TODO the variables below may also should could be customized
  (setq op/src-root-directory (concat op/root-directory "src/"))
  (setq op/src-temp-directory (concat op/root-directory "tmp/"))
  (setq op/pub-root-directory (concat op/root-directory "pub/"))
  (setq op/pub-html-directory (concat op/pub-root-directory "blog/"))
  (setq op/pub-org-directory (concat op/pub-root-directory "org/"))

  ;;; do not use the default style
  (setq org-export-html-style-include-default nil)
  ;;; do not include the javascript
  (setq org-export-html-style-include-scripts nil)

  (setq org-publish-project-alist `(("op-whole-project"
                                     :components ("op-html" "op-static" "op-src-html")
                                     :author ,user-full-name
                                     :email ,(confound-email user-mail-address))
                                    ("op-html"
                                     :base-directory ,op/src-root-directory
                                     :publishing-directory ,op/pub-html-directory
                                     :preparation-function op/publish-preparation
                                     ;; move the completion function to op-src-html
                                     ;;:completion-function op/publish-completion
                                     :base-extension "org"
                                     :exclude ,op/exclude-filename-regexp
                                     :include ,op/include-filename-regexp
                                     :recursive t
                                     :publishing-function (op/publish-customize-style op/publish-customize-header op/publish-customize-footer org-publish-org-to-html)
                                     :html-preamble t
                                     :html-postamble t
                                     :auto-sitemap t
                                     :sitemap-function op/publish-sitemap
                                     :sitemap-title ,(concat "Sitemap of " user-full-name "'s Personal Site")
                                     :sitemap-style list
                                     :table-of-contents nil
                                     :section-numbers nil
                                     :preserve-breaks nil
                                     :tags 'not-in-toc
                                     :author ,user-full-name
                                     :email ,(confound-email user-mail-address))
                                    ("op-static"
                                     :base-directory ,op/src-root-directory
                                     :publishing-directory ,op/pub-html-directory
                                     :recursive t
                                     ; TODO add full definition here
                                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|eot\\|svg\\|ttf\\|woff\\|el"
                                     :publishing-function org-publish-attachment
                                     :author ,user-full-name
                                     :email ,(confound-email user-mail-address))
                                    ("op-src-html"
                                     :base-directory ,op/src-root-directory
                                     :publishing-directory ,op/pub-org-directory
                                     :base-extension "org"
                                     :html-extension "org.html"
                                     :exclude ,op/exclude-filename-regexp
                                     :include ,op/include-filename-regexp
                                     :recursive t
                                     :htmlized-source t
                                     :publishing-function org-publish-org-to-org
                                     :completion-function op/publish-completion
                                     :author ,user-full-name
                                     :email ,(confound-email user-mail-address)))))

(defun op/publish-pages (&optional force)
  "the main entry of whole project publishing process"
  (interactive)
  (op/project-initialize)
  (if force
      (org-publish "op-whole-project" t)
    (org-publish "op-whole-project")))

(defun op/publish-preparation ()
  "the preparation-function hook of org publishing process"
  (let* ((current-project project)
        (project-plist (cdr current-project))
        (exclude-regexp (plist-get project-plist :exclude)))

    (setq op/org-file-info-list (op/reorganize-project-structure current-project))

    (op/prepare-theme current-project)

    (op/publish-generate-tags current-project op/org-file-info-list)

    (op/publish-generate-categories current-project op/org-file-info-list)

    ; TODO the count number in below line should could be customized
    (op/publish-generate-recent-posts 30 current-project op/org-file-info-list)

    (op/publish-generate-index current-project)

    ; update the org file list
    ; "files" is defined in the "let" scope of function org-publish-projects
    (setq files (org-publish-get-base-files current-project exclude-regexp))))

(defun op/publish-completion ()
  "the completion-function hook of org publish process"
  ; TODO clear the customized `org-export-html-preamble-format' to original value

  (op/generate-site-index-html)

  ;; clear the variable
  (setq op/org-file-info-list nil)

  (let ((current-project project))
    (op/restore-project-structure current-project))

  (message "finished"))
;----------------------------------------------------------------------

(defun op/prepare-theme (project)
  "prepare theme for project"
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (theme-dir (file-name-as-directory (concat (file-name-as-directory op/theme-directory) (symbol-name op/theme)))))

    (unless (file-directory-p theme-dir)
      (message "org-page does not have a theme named %s, will use the `default' theme instead" (symbol-name op/theme))
      (setq op/theme 'default)
      (setq theme-dir (file-name-as-directory (concat (file-name-as-directory op/theme-directory) (symbol-name op/theme)))))

    (copy-directory theme-dir
                    (concat root-dir "media/")
                    t t t)))

(defun op/publish-generate-categories (project org-file-info-list)
  "The category generation function
TODO: improve the doc here"
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory (plist-get project-plist :base-directory)))
         (cat-dir (file-name-as-directory (concat root-dir (or op/category-directory "categories/"))))
         (cat-index-filename (concat cat-dir (or op/category-index-filename "index.org")))

         cat-name cat-subdir cat-file-list cat-alist
         cat-filename cat-visiting cat-title cat-buffer relative-path
         )

    (unless (file-directory-p cat-dir)
      (make-directory cat-dir t))

    (dolist (info-plist org-file-info-list)
      (setq cat-name (plist-get info-plist :category))
      (setq cat-subdir (file-name-as-directory (concat cat-dir (convert-string-to-path cat-name) "/")))
      (setq relative-path (file-relative-name (plist-get info-plist :new-path) cat-subdir))
      (unless (file-directory-p cat-subdir)
        (make-directory cat-subdir t))
      (setq cat-file-list (assoc cat-name cat-alist))
      (unless cat-file-list
        (setq cat-file-list (list cat-name))
        (add-to-list 'cat-alist cat-file-list))
      (nconc cat-file-list (list (cons relative-path (plist-get info-plist :title)))))

    ;; write single category file info
    (mapc '(lambda (cat-list)
             (setq cat-subdir (file-name-as-directory (concat cat-dir (convert-string-to-path (car cat-list)) "/")))
             (setq cat-filename (concat cat-subdir "index.org"))
             (setq cat-visiting (find-buffer-visiting cat-filename))
             (setq cat-title (concat "Category: " (car cat-list)))
             (with-current-buffer (setq cat-buffer (or cat-visiting (find-file cat-filename)))
               (erase-buffer)
               (insert (concat "#+TITLE: " cat-title "\n\n"))
               (mapc '(lambda (path-title-cell)
                        (insert (concat "+ [[file:" (get-valid-uri-path (car path-title-cell)) "][" (cdr path-title-cell) "]]" "\n")))
                     (cdr cat-list))
               (save-buffer)
               (or cat-visiting (kill-buffer cat-buffer))))
          cat-alist)

    ;; write the index category file
    (setq cat-visiting (find-buffer-visiting cat-index-filename))
    ;; TODO here may should could be customized
    (setq cat-title "Categories")
    (with-current-buffer (setq cat-buffer (or cat-visiting (find-file cat-index-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " cat-title "\n\n"))
      (mapc '(lambda (cat-list)
               (setq relative-path (concat (convert-string-to-path (car cat-list)) "/index.org"))
               (insert (concat "** [[file:"
                               (get-valid-uri-path relative-path)
                               "]["
                               (car cat-list)
                               "]] ("
                               (int-to-string (length (cdr cat-list)))
                               ")\n"))
               ;; (mapc '(lambda (path-title-cell)
               ;;          ;; here the relative-path in path-title-cell is wrong,
               ;;          ;; because it is relative to the category sub dir, not the category root dir
               ;;          (insert (concat "  - [[file:" (get-valid-uri-path (car path-title-cell)) "][" (cdr path-title-cell) "]]" "\n")))
               ;;       (cdr cat-list))
               (insert "\n"))
            cat-alist)
      (save-buffer)
      (or cat-visiting (kill-buffer cat-buffer)))))

(defun op/publish-generate-tags (project org-file-info-list)
  "The new tag generation function
TODO: improve the doc here"
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory (plist-get project-plist :base-directory)))
         (tag-dir (file-name-as-directory (concat root-dir (or op/tag-directory "tags/"))))
         (tag-index-filename (concat tag-dir (or op/tag-index-filename "index.org")))

         tag-subdir tag-file-list tags-alist tag-filename tag-visiting tag-title tag-buffer relative-path)

    (unless (file-directory-p tag-dir)
      (make-directory tag-dir t))

    (dolist (info-plist org-file-info-list)
      (if (plist-get info-plist :tags)
          (mapc '(lambda (tag-name)
                  (setq tag-subdir (file-name-as-directory (concat tag-dir (convert-string-to-path tag-name) "/")))
                  (setq relative-path (file-relative-name (plist-get info-plist :new-path) tag-subdir))
                  (unless (file-directory-p tag-subdir)
                    (make-directory tag-subdir t))
                  (setq tag-file-list (assoc tag-name tags-alist))
                  (unless tag-file-list
                    (setq tag-file-list (list tag-name))
                    (add-to-list 'tags-alist tag-file-list))
                  (nconc tag-file-list (list (cons relative-path (plist-get info-plist :title)))))
                (plist-get info-plist :tags))))

    ;; write single tag file info
    (mapc '(lambda (tag-list)
             (setq tag-subdir (file-name-as-directory (concat tag-dir (convert-string-to-path (car tag-list)) "/")))
             (setq tag-filename (concat tag-subdir "index.org"))
             (setq tag-visiting (find-buffer-visiting tag-filename))
             (setq tag-title (concat "Tag: " (car tag-list)))
             (with-current-buffer (setq tag-buffer (or tag-visiting (find-file tag-filename)))
               (erase-buffer)
               (insert (concat "#+TITLE: " tag-title "\n\n"))
               (mapc '(lambda (path-title-cell)
                        (insert (concat "+ [[file:" (get-valid-uri-path (car path-title-cell)) "][" (cdr path-title-cell) "]]" "\n")))
                     (cdr tag-list))
               (save-buffer)
               (or tag-visiting (kill-buffer tag-buffer))))
          tags-alist)

    ;; write the index tag file
    (setq tag-visiting (find-buffer-visiting tag-index-filename))
    ;; TODO here may should could be customized
    (setq tag-title "Tags")
    (with-current-buffer (setq tag-buffer (or tag-visiting (find-file tag-index-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " tag-title "\n\n"))
      (mapc '(lambda (tag-list)
               (setq relative-path (concat (convert-string-to-path (car tag-list)) "/index.org"))
               (insert (concat "** [[file:"
                               (get-valid-uri-path relative-path)
                               "]["
                               (car tag-list)
                               "]] ("
                               (int-to-string (length (cdr tag-list)))
                               ")\n"))
               ;; (mapc '(lambda (path-title-cell)
               ;;          ;; here the relative-path in path-title-cell is wrong,
               ;;          ;; because it is relative to the tag sub dir, not the tag root dir
               ;;          (insert (concat "  - [[file:" (get-valid-uri-path (car path-title-cell)) "][" (cdr path-title-cell) "]]" "\n")))
               ;;       (cdr tag-list))
               (insert "\n"))
            tags-alist)
      (save-buffer)
      (or tag-visiting (kill-buffer tag-buffer)))))

(defun op/publish-generate-recent-posts (count project org-file-info-list)
  "this function is used to generate recent posts org file, it
uses the meta data #+DATE defined in org files to compare, if it
is not defined or defined as date format, uses the files' creation
time to compare.

Note:
  on some Unix link file systems, like ext2/ext3, file creation
time is not tracked, under this condition last change time will
be used to compare, be aware that last change time does mean
content change, but file's meta data change, like attribute change.
  as a result, the suggested way is to define meta data #+DATE
in each org file, it can always work on any system.

  count: stands for how many recent posts will be counted
project: stands for org project"

  (let*
      ((project-plist (cdr project))
       (root-dir (file-name-as-directory (plist-get project-plist :base-directory)))
       ; TODO the name should be customizable
       (rp-filename (concat root-dir "recentposts.org"))
       (rp-title "Recent Posts")
       (rp-visiting (find-buffer-visiting rp-filename))
       relative-path date-alist rp-buffer)

    (dolist (info-plist org-file-info-list)
      (setq relative-path (file-relative-name (plist-get info-plist :new-path) root-dir))
      (add-to-list 'date-alist (list (plist-get info-plist :creation-date)
                                     relative-path
                                     (plist-get info-plist :title))))

    (setq date-alist (sort date-alist
                           '(lambda (list1 list2)
                              (let* ((date1 (car list1))
                                     (date2 (car list2))
                                     (result (compare-standard-date
                                              (fix-timestamp-string date1)
                                              (fix-timestamp-string date2))))
                                (cond
                                 ((<= result 0) t)
                                 (t nil))))))

    (with-current-buffer (setq rp-buffer (or rp-visiting (find-file rp-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " rp-title "\n\n"))
      (while (and (> count 0)
                  (> (length date-alist) 0))
        (setq count (1- count))
        (let ((date-path-title-list (pop date-alist)))
          (insert (concat "+ " (car date-path-title-list)
                          "  [[file:" (get-valid-uri-path (nth 1 date-path-title-list)) "]["
                          (nth 2 date-path-title-list) "]]"))
          (insert "\n\n")))
      (save-buffer)
      (or rp-visiting (kill-buffer rp-buffer)))))

(defun op/publish-customize-header (project-plist filename pub-dir)
  "this function is called before publishing process of each file.
it's purpose is to customize the page header, mainly about the relative path of tag, recent post, etc.
please see `op/publish-site-url' and `op/publish-html-header-template' for more detail.
filename: the whole name of file to publish"

   (let* ((root-dir (plist-get project-plist :base-directory))
          (html-extension (or (plist-get project-plist :html-extension) "html"))
          ;; remove the prefix http:// or https://, and the suffix slash /
          (search-url (replace-regexp-in-string "/?$" "" (replace-regexp-in-string "^https?://" "" op/publish-site-url)))
          (cat-file-path (concat (file-name-as-directory (concat root-dir (or op/category-directory "categories/")))
                                 (concat (file-name-sans-extension (or op/category-index-filename "index.org")) "." html-extension)))
          (tags-file-path (concat (file-name-as-directory (concat root-dir (or op/tag-directory "tags/")))
                                  (concat (file-name-sans-extension (or op/tag-index-filename "index.org")) "." html-extension)))
          ; TODO the variable in the below line should could be customized
          (rp-file-path (concat (file-name-as-directory root-dir) (concat "recentposts." html-extension)))

          (sitemap-file-path (concat (file-name-as-directory root-dir) (concat (file-name-sans-extension (or (plist-get project-plist :sitemap-filename)
                                                                                                             "sitemap.org")) "." html-extension)))

          (header (format-spec op/publish-html-header-template `((?h . ,(or op/publish-site-title "org-page"))
                                                                 (?s . ,(get-valid-uri-path (file-relative-name sitemap-file-path (file-name-directory filename))))
                                                                 (?c . ,(get-valid-uri-path (file-relative-name cat-file-path (file-name-directory filename))))
                                                                 (?t . ,(get-valid-uri-path (file-relative-name tags-file-path (file-name-directory filename))))
                                                                 (?r . ,(get-valid-uri-path (file-relative-name rp-file-path (file-name-directory filename))))
                                                                 (?g . ,(or op/personal-github-link "https://github.com/kelvinh/org-page"))
                                                                 (?u . ,search-url)))))
     (setq org-export-html-preamble-format `(("en" ,header)))))

(defun op/publish-customize-style (project-plist filename pub-dir)
  "this function is called before publishing process of each file.
it's purpose is to customize the css style, mainly to decide the relative path of style file(s).
please see `op/publish-html-style-list' for more detail.
filename: the whole name of file to publish"

  (let* ((root-dir (plist-get project-plist :base-directory))
         (template "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />")
         css-links)

    (unless op/publish-html-style-list
      ; TODO here the default css file name may should could be customized
      (setq op/publish-html-style-list '("main.css")))

    (dolist (css op/publish-html-style-list)
      (setq css-links (concat css-links
                              "\n"
                              ; TODO here the path may should could be customized
                              (format template (file-relative-name (concat (file-name-as-directory root-dir) "media/css/" css)
                                                                   (file-name-directory filename))))))
    (setq org-export-html-style css-links)))

(defun op/publish-customize-footer (project-plist filename pub-dir)
  "This function is called before publishing process of each file.
its purpose is to customize the footer of each generated html file.
please see `op/publish-html-postamble-template' for more detail.
filename: the whole name of file to publish"

  (let* ((root-dir (plist-get project-plist :base-directory))
         (html-extension (or (plist-get project-plist :html-extension) "html"))
         (pub-root-dir (or op/pub-root-directory (concat op/root-directory "pub/")))
         (pub-file (concat pub-dir (file-name-nondirectory (file-name-sans-extension filename)) "." html-extension))
         (disqus-identifier (get-valid-uri-path (substring pub-file (1- (length pub-root-dir)))))
         ;;; disqus-identifier has a prefix "/", so remove the suffix "/" from op/publish-site-url
         (disqus-url (concat (replace-regexp-in-string "/?$" "" op/publish-site-url) disqus-identifier))
         (disqus-shortname op/personal-disqus-shortname)
         (file-info (find-if '(lambda (plist)
                                (string= (plist-get plist :new-path) filename))
                             op/org-file-info-list))
         (pub-cat-dir (file-name-as-directory (concat (or op/pub-html-directory
                                                          (concat op/root-directory "pub/blog/"))
                                                      (or op/category-directory "categories/"))))
         (pub-tag-dir (file-name-as-directory (concat (or op/pub-html-directory
                                                          (concat op/root-directory "pub/blog/"))
                                                      (or op/tag-directory "tags/"))))
         (sitemap-filename (concat root-dir (or (plist-get project-plist :sitemap-filename) "sitemap.org")))
         ;; TODO the name should not be hard coded, should could be customized
         (rp-filename (concat root-dir "recentposts.org"))
         (index-filename (concat root-dir "index.org"))
         (cdate (plist-get file-info :creation-date))
         (mdate (plist-get file-info :mod-date))
         (category (plist-get file-info :category))
         (tags-list (plist-get file-info :tags))
         (email (or (plist-get project-plist :email)
                    (confound-email user-mail-address)))
         (org-link (op/get-htmlized-org-link filename project-plist))
         pub-tag-file cat-relative-path cat-link tag-relative-path tag-links)

    (if (or (string-prefix-p pub-cat-dir pub-file)
            (string-prefix-p pub-tag-dir pub-file)
            (string= rp-filename filename)
            (string= sitemap-filename filename)
            (string= index-filename filename))
        (setq op/publish-html-postamble-template op/publish-footer)
      (setq op/publish-html-postamble-template (concat op/publish-meta-info op/publish-comment op/publish-footer)))

    (if (not (or (string-match "%h" op/publish-html-postamble-template)
                 (string-match "%m" op/publish-html-postamble-template)
                 (string-match "%i" op/publish-html-postamble-template)
                 (string-match "%l" op/publish-html-postamble-template)
                 (string-match "%t" op/publish-html-postamble-template)
                 (string-match "%g" op/publish-html-postamble-template)
                 (string-match "%n" op/publish-html-postamble-template)
                 (string-match "%u" op/publish-html-postamble-template)
                 (string-match "%s" op/publish-html-postamble-template)))
        (setq org-export-html-postamble-format `(("en" ,op/publish-html-postamble-template)))

      (progn
        (unless (not category)
          (setq cat-relative-path (file-relative-name (concat pub-cat-dir
                                                              (convert-string-to-path category)
                                                              "/index.html")
                                                      pub-dir))
          (setq cat-link (format-spec "<a href=\"%l\">%n</a>"
                                      `((?l . ,(get-valid-uri-path cat-relative-path))
                                        (?n . ,category)))))
        (mapc '(lambda (tag)
                 (setq pub-tag-file (concat pub-tag-dir (convert-string-to-path tag) "/index.html"))
                 (setq tag-relative-path (file-relative-name pub-tag-file pub-dir))
                 (setq tag-links (concat tag-links (format-spec "<a href=\"%l\">%n</a>, "
                                                                `((?l . ,(get-valid-uri-path tag-relative-path))
                                                                  (?n . ,tag))))))
              tags-list)
        (if tag-links
            (setq tag-links (substring tag-links 0 -2))) ; remove the last two characters, comma and space
        (setq org-export-html-postamble-format `(("en" ,(format-spec op/publish-html-postamble-template `((?a . "%a") (?c . "%c") (?d . "%d") (?e . "%e") (?v . "%v")
                                                                                                          (?i . ,(org-html-expand email))
                                                                                                          (?l . ,(or org-link "javascript:void(0);"))
                                                                                                          (?h . ,(or cdate "N/A"))
                                                                                                          (?m . ,(or mdate "N/A"))
                                                                                                          (?t . ,(or tag-links "N/A"))
                                                                                                          (?g . ,(or cat-link "N/A"))
                                                                                                          (?n . ,disqus-identifier)
                                                                                                          (?u . ,disqus-url)
                                                                                                          (?s . ,disqus-shortname))))))))))

(defun op/publish-sitemap (project &optional sitemap-filename)
  "Create a sitemap of project, this function is copied and customized
from `org-publish-org-sitemap' defined in `org-publish.el'."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (localdir (file-name-directory root-dir))
         (cat-dir (file-name-as-directory (concat root-dir (or op/category-directory "categories/"))))
         (tag-dir (file-name-as-directory (concat root-dir (or op/tag-directory "tags/"))))
         ; TODO here should could be customized
         (recent-posts-filename (concat root-dir "recentposts.org"))

         (indent-str (make-string 2 ?\ ))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat root-dir (or sitemap-filename "sitemap.org")))
         (sitemap-title (or (plist-get project-plist :sitemap-title)
                            (concat "Sitemap for project " (car project))))
         (sitemap-style (or (plist-get project-plist :sitemap-style)
                            'tree))
         (sitemap-sans-extension (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         (ifn (file-name-nondirectory sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer (setq sitemap-buffer
                               (or visiting (find-file sitemap-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link (file-relative-name file root-dir))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ; do not include sitemap itself, tags and recentposts
          (unless (or (equal (file-truename sitemap-filename) (file-truename file))
                      (equal (file-truename recent-posts-filename) (file-truename file))
                      (string-prefix-p (file-truename cat-dir) (file-truename file))
                      (string-prefix-p (file-truename tag-dir) (file-truename file)))
            (if (eq sitemap-style 'list)
                (message "Generating list-style sitemap for %s" sitemap-title)
              (message "Generating tree-style sitemap for %s" sitemap-title)
              (setq localdir (concat (file-name-as-directory root-dir)
                                     (file-name-directory link)))
              (unless (string= localdir oldlocal)
                (if (string= localdir root-dir)
                    (setq indent-str (make-string 2 ?\ ))
                  (let ((subdirs
                         (split-string
                          (directory-file-name
                           (file-name-directory
                            (file-relative-name localdir root-dir))) "/"))
                        (subdir "")
                        (old-subdirs (split-string
                                      (file-relative-name oldlocal root-dir) "/")))
                    (setq indent-str (make-string 2 ?\ ))
                    (while (string= (car old-subdirs) (car subdirs))
                      (setq indent-str (concat indent-str (make-string 2 ?\ )))
                      (pop old-subdirs)
                      (pop subdirs))
                    (dolist (d subdirs)
                      (setq subdir (concat subdir d "/"))
                      (insert (concat indent-str " + " d "\n"))
                      (setq indent-str (make-string
                                        (+ (length indent-str) 2) ?\ )))))))
            ;; This is common to 'flat and 'tree
            (let ((entry
                   (org-publish-format-file-entry org-sitemap-file-entry-format
                                                  file project-plist))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              (cond ((string-match-p regexp entry)
                     (string-match regexp entry)
                     (insert (concat indent-str " + " (match-string 1 entry)
                                     "[[file:" (get-valid-uri-path link) "]["
                                     (match-string 2 entry)
                                     "]]" (match-string 3 entry) "\n")))
                    (t
                     (insert (concat indent-str " + [[file:" (get-valid-uri-path link) "]["
                                     entry
                                     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun op/get-htmlized-org-link (file project-plist)
  "returns the corresponding htmlized org file relative link"
  (let* ((root-dir (plist-get project-plist :base-directory))
         (relative-path (file-relative-name file root-dir))
         (pub-dir (or op/pub-root-directory
                      (concat (file-name-directory (directory-file-name root-dir)) "pub/")))
         (html-dir (file-name-directory (concat (or op/pub-html-directory (concat pub-dir "blog/"))
                                                relative-path)))
         (org-html-file (concat (file-name-sans-extension (concat (or op/pub-org-directory
                                                                      (concat pub-dir "org/"))
                                                                  relative-path))
                                ; TODO here should be got from the "op-src-html" project
                                ".org.html")))
    (file-relative-name org-html-file html-dir)))

(defun op/read-file-info (filename base-directory)
  "Read info of given org file (the `filename' should be full-path), include:
1. path; 2. creation date; 3. modification date; 4. tags. The creation date
info will be firstly read from #+DATE defined in the file, if no date info
found, will be read from the file's last change date. However, it is recommended
to use #+DATE to record creation date, since the file's last change date will
change if its meta info changed."
  (if (or (not (file-exists-p filename))
          (file-directory-p filename))
      nil
    (let* ((file-attrs (file-attributes filename))
           (fcdate (format-time-string "%Y-%m-%d" (nth 6 file-attrs)))
           (mdate (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
           ;; TODO here the keywords of tags and category should be customizable
           (tag-match-regexp (org-make-options-regexp '("TAGS")))
           (category-match-regexp (org-make-options-regexp '("CATEGORY")))
           (file-visiting (find-buffer-visiting filename))
           (attr-plist `(:path ,filename :creation-date ,fcdate :mod-date ,mdate))
           opt-plist category tags tag-list file-buffer cdate)
      (with-current-buffer (setq file-buffer (or file-visiting
                                                 (find-file filename)))
        (setq opt-plist (org-infile-export-plist))
        (setq cdate (plist-get opt-plist :date))
        (if (and cdate (not (string-match "%" cdate)))
            (plist-put attr-plist :creation-date (fix-timestamp-string cdate)))
        (plist-put attr-plist :title (or (plist-get opt-plist :title)
                                         (file-name-sans-extension (file-name-nondirectory filename))))
        (goto-char (point-min))
        (if (re-search-forward tag-match-regexp nil t)
            (setq tags (match-string-no-properties 2 nil))
          (setq tags nil))
        (if tags
            (mapcar '(lambda (tag)
                       (unless (equal "" (replace-regexp-in-string " +" "" tag))
                         (if (not tag-list)
                             (setq tag-list (list tag))
                           (add-to-list 'tag-list tag))))
                    ;; TODO the seperator should be customizable
                    (org-split-string tags ":"))
          (setq tag-list nil))
        (plist-put attr-plist :tags tag-list)
        (goto-char (point-min))
        (if (re-search-forward category-match-regexp nil t)
            (setq category (match-string-no-properties 2 nil))
          (setq category nil))
        (unless category
          (setq category (if (string= (file-name-directory filename) base-directory)
                             ;; TODO the default category name should be customizable
                             "default"
                           (file-name-nondirectory (directory-file-name (file-name-directory filename)))))) ; read parent folder name
        (plist-put attr-plist :category (or category nil))
        (or file-visiting (kill-buffer file-buffer)))
      attr-plist)))

(defun op/reorganize-project-structure (project)
  "This function is used to re-generate project with a well-formed structure,
for example, assume a org file with path '/path/to/sample.org' is written on
2012-11-17, then it will be re-formed to '/path/to/2012/11/17/sample/index.org.
The original project directory will be temporarily renamed to 'tmp/' under the
same folder, and it will be recovered after the entire project generation.
Since this function is destructive, so alone invocation of this function is
strongly discouraged."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (tmp-dir (or op/src-temp-directory
                      (file-name-as-directory (concat op/root-directory "tmp/"))))
         tmp-project files file file-attrs file-attr-list
         date-list new-relative-path new-path)
    (if (file-directory-p tmp-dir)
        (delete-directory tmp-dir t nil))
    (rename-file root-dir tmp-dir)
    (setq tmp-project (copy-list project))
    (setq tmp-project (nconc (list (car tmp-project))
                             (plist-put (cdr tmp-project) :base-directory tmp-dir)))
    (setq files (org-publish-get-base-files tmp-project
                                            (plist-get project-plist :exclude)))
    (while (setq file (pop files))
      (unless  (not (file-exists-p file))
        (setq file-attrs (op/read-file-info file tmp-dir))
        (add-to-list 'file-attr-list file-attrs)
        (setq date-list (split-string (plist-get file-attrs :creation-date) "-"))
        (if (string= file (concat tmp-dir "index.org"))
            ;;; do not modify paths of index.org
            ;;; TODO add other 3 files: about.org, sitemap.org and recentposts.org
            (setq new-relative-path "index.org")
          (setq new-relative-path (concat (file-name-as-directory
                                           (concat (convert-string-to-path (plist-get file-attrs :category)) "/"
                                                   (car date-list) "/"
                                                   (cadr date-list) "/"
                                                   (caddr date-list) "/"
                                                   (convert-string-to-path (plist-get file-attrs :title)) "/"))
                                          "index." (file-name-extension file))))
        (setq new-path (expand-file-name new-relative-path root-dir))
        (plist-put file-attrs :new-path new-path)
        (unless (file-directory-p (file-name-directory new-path))
          (make-directory (file-name-directory new-path) t))
        (copy-file (plist-get file-attrs :path)
                   (plist-get file-attrs :new-path)
                   t)))
    file-attr-list))

(defun op/restore-project-structure (project)
  "This function is used to restore project to its original structure, which is
modified during project generation. For more information, please see function
`op/reorganize-project-structure'. Since this function is destructive, it should
only be invoked after invocation of `op/reorganize-project-structure', alone
invocation of this function is strongly discouraged."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (tmp-dir (or op/src-temp-directory
                      (file-name-as-directory (concat op/root-directory "tmp/")))))
    (unless (file-directory-p tmp-dir)
      (error (concat "The temporary directory " tmp-dir " not found, restore process cannot continue.")))

    (if (file-directory-p root-dir)
        (delete-directory root-dir t nil))
    (rename-file tmp-dir root-dir)))

(defun op/generate-site-index-html ()
  "This function is used to generate the index.html for current site.
Note: generating html file directly, not index.org"
  (let* ((index-file (concat (or op/pub-root-directory (concat op/root-directory "pub/")) "index.html"))
         (index-visiting (find-buffer-visiting index-file))
         (index-relative-path (file-relative-name (concat (or op/pub-html-directory
                                                              (concat op/root-directory "pub/blog/"))
                                                          "index.html")
                                                  (file-name-directory index-file)))
         (css-folder (concat (or op/pub-html-directory (concat op/root-directory "pub/blog/")) "media/css/"))
         (css-template "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />")
         css-links index-buffer)

    (unless op/publish-html-style-list
      (setq op/publish-html-style-list '("main.css")))

    (dolist (css op/publish-html-style-list)
      (setq css-links (concat css-links "\n"
                              (format css-template (file-relative-name (concat css-folder css)
                                                                       (file-name-directory index-file))))))

    (with-current-buffer (setq index-buffer (or index-visiting (find-file index-file)))
      (erase-buffer)
      (insert (format-spec "<!DOCTYPE html><html><head><title>%t</title>
<meta charset=\"UTF-8\">%c</head><body>
<script type=\"text/javascript\">
    var ie = /(msie) ([\w.]+)/.exec(navigator.userAgent.toLowerCase());
    var div = document.createElement('div');
    div.className = ie ? 'fucking-ie' : 'loading-center';
    div.innerHTML = ie ? 'Sorry, this site does not support the fucking IE.' : 'Loading...';
    document.getElementsByTagName('body')[0].appendChild(div);
    ie || setTimeout(function() { window.location.replace('%p');}, 1000);
</script></body></html>" `((?t . ,(or op/publish-site-title "org-page"))
                           (?c . ,css-links)
                           (?p . ,(get-valid-uri-path index-relative-path)))))
      (save-buffer)
      (or index-visiting (kill-buffer index-buffer)))))

(defun op/publish-generate-index (project)
  "The index file generation function, be careful with `op/generate-site-index-html'"
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory (plist-get project-plist :base-directory)))
         (index-file (concat root-dir "index.org"))
         (sitemap-file (concat root-dir (or (plist-get project-plist :sitemap-filename) "sitemap.org")))
         (index-visiting (find-buffer-visiting index-file))
         index-buffer
         )

    (unless (file-exists-p index-file)
      (with-current-buffer (setq index-buffer (or index-visiting (find-file index-file)))
        (erase-buffer)
        (insert "#+TITLE: The index page")
        (insert "\n\n")
        (insert (format "You are visiting %s's personal site, generated by [[http://github.com/kelvinh/org-page][org-page]]." user-full-name))
        (insert "\n\n")
        (insert (format "This page is automatically generated by org-page, since the site's author %s did not provide a customized index page." user-full-name))
        (insert "\n\n")
        (insert "It is recommanded to provide a customzied index page if you are the site's owner.")
        (insert "\n\n")
        (insert "Or, you may prefer to visit the [[file:./sitemap.org][sitemap]], it will provide more info than this page.")
        (save-buffer)
        (or index-visiting (kill-buffer index-buffer))))))


(provide 'org-page)

;;; org-page.el ends here
