;;; org-page.el --- static page generator based on org mode

;; Copyright (C) 2012 Kelvin Hu.

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/kelvinh/org-page
;; Version: 0.2

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
;; + categories support (auto generated)
;; + search engine support (auto generated)
;; + commenting (implemented using disqus)
;;
;; To use org-page, you just need a little configuration,as follows:
;;
;; 1. `op/root-directory': the root directory of your project, org-page
;;    need to know this directory to do further action, and actually,
;;    you need to put your all org files(include subdirectory) into a
;;    folder named "src" under this directory, and then org-page will
;;    automatically publish them to a folder named "pub", which is also
;;    under this directory
;; 2. `op/publish-site-url': the URL search engine will base on, and
;;    also disqus commenting will use this url, it should be the URL of
;;    your personal site
;; 3. `op/personal-disqus-shortname': the disqus shortname you own on
;;    disqus website, will be used for commenting
;;
;; So, the following simple code will make org-page work:
;;
;;    (add-to-list 'load-path "path/to/org-page")
;;    (require 'org-page)
;;    (setq op/root-directory "path/to/your/project")
;;    (setq op/publish-site-url "http://your.personal.site.com/")
;;    (setq op/personal-disqus-shortname "your_disqus_shortname")
;;    (global-set-key (kbd "<f9>") 'op/publish-pages)
;;    (global-set-key (kbd "C-<f9>") (lambda () (interactive)
;;                                              (op/publish-pages t)))
;;
;; Now you can publish your pages with a press: <F9>, then your fresh
;; pages will be generated, or if you want to republish all your files
;; even they have already been published and not modified since last
;; publishing, your can just press Ctrl + <F9>.
;;
;; The three variables above are required, and the following variables
;; are optional, but also could be customized:
;;
;;  1. `op/exclude-filename-regexp': describes which org files in the
;;     project subdirectory "src" should not be published
;;  2. `op/include-filename-regexp': describes which org files in the
;;     project subdirectory "src" should always be published
;;  3. `op/theme-directory': the directory stores themes, it has a
;;     default value, you could customize it to your own value
;;  4. `op/theme': the theme will be used, default value is 'default,
;;     and sorry for that no more themes are provided at present
;;  5. `op/tag-directory': the folder stores auto-generated tags
;;  6. `op/tag-index-filename': filename of auto-generated tag index
;;  7. `op/category-directory': the folder stores auto-generated
;;     categories
;;  8. `op/category-index-filename': filename of auto-generated tag
;;     index
;;  9. `op/publish-site-title': the title of your site
;; 10. `op/personal-github-link': your personal github link if you
;;     do have one github account
;; 11. `op/publish-meta-info': the meta info of current post, it is
;;     discouraged to change this variable unless you have strong
;;     knowledge in css and html
;; 12. `op/publish-comment': the comment section of current post, it
;;     is also discouraged to change this variable
;; 13. `op/publish-footer': the footer section of current post, also
;;     discouraged to change

;;; Code:

(require 'org-page-util)

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

(defcustom op/personal-github-link "https://github.com/kelvinh/org-page"
  "the personal github link"
  :group 'org-page
  :type 'string)

(defcustom op/personal-disqus-shortname nil
  "the personal disqus shortname"
  :group 'org-page
  :type 'string)

(defcustom op/publish-html-index-template
  "<!DOCTYPE html><html><head><title>%t</title>
<meta charset=\"UTF-8\">%c</head><body>
<script type=\"text/javascript\">
    var ie = /(msie) ([\\w.]+)/.exec(navigator.userAgent.toLowerCase());
    var div = document.createElement('div');
    div.className = ie ? 'fucking-ie' : 'loading-center';
    div.innerHTML = ie ? 'Sorry, this site does not support the fucking IE.' : 'Loading...';
    document.getElementsByTagName('body')[0].appendChild(div);
    ie || setTimeout(function() { window.location.replace('%p');}, 1000);
</script></body></html>"
  "the template used to construct index page of entire site, our real site is
generated in a subfolder named 'blog/', so the index page is needed to act
as a bridge, taking visitor to the real index. below parameters can be used:
%t: the site title
%c: css links, be aware that here the links are already wrapped by tag <link>
%p: the path to real index page"
  :group 'org-page
  :type 'string)

;; TODO replace the tags with correct ones
(defcustom op/publish-meta-info "<div id=\"post-meta\">
  <span title=\"post date\" class=\"post-info\">%h</span>
  <span title=\"last modification date\" class=\"update-info\">%m</span>
  <span title=\"category\" class=\"category\">%g</span>
  <span title=\"tags\" class=\"tags\">%t</span>
  <span title=\"author\" class=\"author\">%a</span>
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
    //var disqus_developer = 1;
    var disqus_identifier = \"%n\";
    var disqus_url = \"%u\";
    var disqus_shortname = '%s';

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
%t: tags of file, it will be expanded to the following format(assume the file has tag tag1, tag2):
<a href=\"tag1-link\">tag1</a>, <a href=\"tag2-link\">tag2</a>
%n: javascript variable 'disqus_identifier' of current page
%u: javascript variable 'disqus_url' of current page
%s: javascript variable 'disqus_shortname' of current page
%i: author's email, the difference from %e is this one will keep the email address unchanged,
but %e will expand it to html tag <a href=\"mailto:\"> automatically"
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

  ;;; after publishing, the newline character between html tag <p> will be shown
  ;;; as space, it is normal for languages like English, but for Chinese and
  ;;; other DBCS languages, the extra spaces are really annoying, so, use the
  ;;; following workaround:
  ;;; change fill-column to a very large number, and then fill the whole buffer,
  ;;; so one paragraph will be in one line, the extra spaces are avoid.
  ;;; and, do NOT fill #+begin_src/#+end_src sections, it will mess up the code.
  (setq org-export-first-hook '(lambda ()
                                 (set-fill-column 9999)
                                 (let ((regions (op/non-src-regions)))
                                   (dolist (region regions)
                                     (fill-region (car region) (cdr region))))))

  ;;; fix bug: if the org file is encoded with iso-8859-1, but the title set
  ;;; manually is not iso-8859-1, the title will show as garbage characters
  ;;; solution: set the coding-system to utf-8
  (setq org-export-html-coding-system 'utf-8)

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

    (op/publish-generate-about current-project)

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
         (theme-dir (file-name-as-directory (concat (file-name-as-directory op/theme-directory) (symbol-name op/theme))))
         ;; TODO may need to be customized here
         (html-template (concat (if op/load-file-name
                                    (file-name-directory op/load-file-name)
                                  (file-name-directory (directory-file-name op/theme-directory)))
                                "templates/html/theme-" (symbol-name op/theme) ".el")))

    (unless (file-directory-p theme-dir)
      (message "org-page does not have a theme named %s, will use the `default' theme instead" (symbol-name op/theme))
      (setq op/theme 'default)
      (setq theme-dir (file-name-as-directory (concat (file-name-as-directory op/theme-directory) (symbol-name op/theme)))))

    (if (file-exists-p html-template)
        (load-file html-template))

    (copy-directory theme-dir
                    (concat root-dir "media/")
                    t t t)))

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
         (index-filename (concat root-dir "index.org"))
         (about-filename (concat root-dir "about.org"))

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
                      (equal (file-truename index-filename) (file-truename file))
                      (equal (file-truename about-filename) (file-truename file))
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
      (insert (format-spec op/publish-html-index-template `((?t . ,(or op/publish-site-title "org-page"))
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

(defun op/publish-generate-about (project)
  "The about file generation function"
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory (plist-get project-plist :base-directory)))
         (about-file (concat root-dir "about.org"))
         (about-visiting (find-buffer-visiting about-file))
         about-buffer)

    (unless (file-exists-p about-file)
      (with-current-buffer (setq about-buffer (or about-visiting (find-file about-file)))
        (erase-buffer)
        (insert "#+TITLE: About")
        (insert "\n\n")
        (insert (format "* About %s" user-full-name))
        (insert "\n\n")
        (insert (format "I am org-page, [[http://github.com/kelvinh/org-page][here]] is my home, this site is generated by %s, and I provided a little help." user-full-name))
        (insert "\n\n")
        (insert (format "Since %s is a little lazy, he/she did not provide an about page, so I generated this page myself." user-full-name))
        (insert "\n\n")
        (insert (format "As a result, I did not know much about %s, I just know his/her [[mailto:%s][email]], you may contact him/her, and please tell him/her to improve this page." user-full-name (or (plist-get project-plist :email) (confound-email user-mail-address))))
        (insert "\n\n")
        (insert "* About me(org-page)")
        (insert "\n\n")
        (insert (format "I was created by [[http://github.com/kelvinh][Kelvin Hu]], in his thought, I am pretty enough, but if you think there is something can be done to make me much more beautiful, please [[mailto:%s][contact him]] to improve me, many thanks. :-)" (confound-email "ini.kelvin@gmail.com")))
        (save-buffer)
        (or about-visiting (kill-buffer about-buffer))))))

(provide 'org-page)

;;; org-page.el ends here
