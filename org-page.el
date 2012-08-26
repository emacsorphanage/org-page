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

(defcustom op/publish-search-url nil
  "searching using google will based on this url"
  :group 'org-page
  :type 'string)

; TODO remove "TODO" in below string after rss feature implemented
(defconst op/publish-html-header-template
  "<ul id=\"nav-list\">
    <li><a href=\"%s\" class=\"menu\">Sitemap</a></li>
    <li><a href=\"%t\" class=\"menu\">Tags</a></li>
    <li><a href=\"%r\" class=\"menu\">Recent Posts</a></li>
    <li><a href=\"javascript:void(0)\" class=\"menu\">RSS (TODO)</a>
    <li>
      <form target=\"_blank\" method=\"get\" action=\"http://google.com/search\">
        <input type=\"hidden\" value=\"site:%u\" name=\"q\">
        <input type=\"text\" x-webkit-speech=\"\" placeholder=\"Search\" class=\"search\" name=\"q\">
      </form>
    </li>
  </ul>"
  "the template used to construct page header, below parameters can be used:
%s: the relative path to sitemap.html
%t: the relative path to tags.html
%r: the relative path to recentposts.html
%u: the url of current site, used for search")

(defcustom op/publish-html-style-list '("main.css")
  "style file name list, which will be included in exported html files.
these files will be considered in folder <`:base-directory'>/media/css as default."
  :group 'org-page
  :type 'list)

; TODO the copyright info in below variable should could be customized
(defcustom op/publish-html-postamble-template "
<div id=\"post-meta\">
  <p class=\"post-info\">Posted on %h, last modified on %m. Author: %a.</p>
  <!--<p class=\"author\">Author: %a (%e)</p>
  <p class=\"date\">Date: %d</p>-->
</div>
<div id=\"footer\">
<p class=\"creator\">Generated by %c</p>
<!--<p class=\"xhtml-validation\">%v</p>-->
<p class=\"copyright\">
  Copyright &copy; 2012 <a href=\"mailto:%i\">Kelvin Hu</a>
  &nbsp;&nbsp;-&nbsp;&nbsp;
  Powered by <a href=\"https://github.com/kelvinh/org-page\" target=\"_blank\">org-page</a>
</p>
<a class=\"github\" target=\"_blank\" href=\"https://github.com/kelvinh\">Fork me on github</a>
</div>"
  "the template used to construct page footer, below parameter can be used:
%a: author's name
%c: creator versions (org/emacs versions)
%d: publish date (default is publishing time, can be customized by #+DATE in org file)
%e: author's email
%v: validation-link, will be replaced by `org-export-html-validation-link'

%h: last changed date (this change means meta change, not content change)
%m: last modified date
%i: author's email, the difference from %e is this one will keep the email address unchanged,
but %e will expand it to html tag <a href=\"mailto:\"> automatically"
  :group 'org-page
  :type 'string)

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

  (unless op/theme
    (setq op/theme 'default))

  ; TODO the variables below may also should could be customized
  (setq op/src-root-directory (concat op/root-directory "src/"))
  (setq op/pub-root-directory (concat op/root-directory "pub/"))
  (setq op/pub-html-directory (concat op/pub-root-directory "html/"))
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
                                     :completion-function op/publish-completion
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
                                     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|el"
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

    (op/prepare-theme project)

    (op/publish-generate-tags current-project)

    ; TODO the count number in below line should could be customized
    (op/publish-generate-recent-posts 7 project)

    ; update the org file list
    ; "files" is defined in the "let" scope of function org-publish-projects
    (setq files (org-publish-get-base-files current-project exclude-regexp))))

(defun op/publish-completion ()
  "the completion-function hook of org publish process"
  ; TODO clear the customized `org-export-html-preamble-format' to original value
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

(defun op/publish-generate-tags (project)
  "the preparation-function hook of org publish process
TODO: update the doc string here"
  (let*
      ((project-plist (cdr project))
       (exclude-regexp (plist-get project-plist :exclude))
       (root-dir (file-name-as-directory
                  (plist-get project-plist :base-directory)))
       (tag-dir (file-name-as-directory (concat root-dir (or op/tag-directory
                                                             "tags/"))))
       (tag-index-filename (concat tag-dir (or op/tag-index-filename
                                               "index.org")))

       (tag-match-regexp (org-make-options-regexp '("FILETAGS")))
       (files (org-publish-get-base-files project exclude-regexp))
       tag-title file tag-visiting file-visiting tags-alist tags
       tag-filename file-buffer tag-buffer)

    (unless (file-exists-p tag-dir)
      (make-directory tag-dir t))

    (while (setq file (pop files))
      (setq file-visiting (find-buffer-visiting file))
      (with-current-buffer (setq file-buffer (or file-visiting
                                                 (find-file file)))
        (let* ((fn (file-name-nondirectory file))
               (relative-path (file-relative-name file tag-dir))
               (title (or (plist-get (org-infile-export-plist) :title)
                          fn)))

          ; TODO here should exclude "sitemap.org" and "tags.org"
          ;(unless (or (equal (file-truename tag-filename) (file-truename file))
          ;            (equal (file-truename <"sitemap.org">) (file-truename file)))
          ;  add normal logic here)

          (unless (or (equal (file-truename tag-index-filename) (file-truename file))
                      (equal (file-truename tag-dir) (file-name-directory (file-truename file))))
            (goto-char (point-min))
            (if (re-search-forward tag-match-regexp nil t)
                (setq tags (match-string-no-properties 2 nil))
              (setq tags nil))
            (when tags
              (mapcar '(lambda (tag)
                         (unless (equal "" (replace-regexp-in-string " +" "" tag))
                           (let* ((tag-list (assoc tag tags-alist)))
                             (unless tag-list
                               (setq tag-list (list tag))
                               (add-to-list 'tags-alist tag-list))
                             (nconc tag-list (list (cons relative-path title))))))
                      (org-split-string tags ":")))))
        (or file-visiting (kill-buffer file-buffer))))

    (mapc '(lambda (tag-list)
             (setq tag-filename (concat tag-dir (car tag-list) ".org"))
             (setq tag-visiting (find-buffer-visiting tag-filename))
             (setq tag-title (concat "Tags: " (car tag-list)))
             (with-current-buffer (setq tag-buffer (or tag-visiting
                                                       (find-file tag-filename)))
               (erase-buffer)
               (insert (concat "#+TITLE: " tag-title "\n\n"))
               (mapc '(lambda (path-title-cell)
                        (insert (concat "* [[file:" (car path-title-cell) "][" (cdr path-title-cell) "]]" "\n")))
                     (cdr tag-list))
               (save-buffer)
               (or tag-visiting (kill-buffer tag-buffer))))
          tags-alist)

    (setq tag-visiting (find-buffer-visiting tag-index-filename))
    ; TODO here may should could be customized
    (setq tag-title "Tags")
    (with-current-buffer (setq tag-buffer (or tag-visiting
                                              (find-file tag-index-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " tag-title "\n\n"))
      (mapc '(lambda (tag-list)
               ; TODO here it is better to convert to relative path using related functions
               ; than just hard code the path
               (insert (concat "* [[file:./" (car tag-list) ".org][" (car tag-list) "]]" "\n\n"))
               (mapc '(lambda (path-title-cell)
                        (insert (concat "  - [[file:" (car path-title-cell) "][" (cdr path-title-cell) "]]" "\n")))
                     (cdr tag-list))
               (insert "\n")) tags-alist)
      (save-buffer)
      (or tag-visiting (kill-buffer tag-buffer)))))

(defun op/publish-generate-recent-posts (count project)
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
       (exclude-regexp (plist-get project-plist :exclude))
       (sitemap-filename (or (plist-get project-plist :sitemap-filename)
                             "sitemap.org"))
       (root-dir (file-name-as-directory
                  (plist-get project-plist :base-directory)))
       (tag-dir (file-name-as-directory (concat root-dir (or op/tag-directory
                                                             "tags/"))))

       ; TODO to be refined
       (rp-filename (concat root-dir "recentposts.org"))
       (rp-title "recent posts")

       (rp-visiting (find-buffer-visiting rp-filename))
       (files (org-publish-get-base-files project exclude-regexp))
       date-alist file file-visiting file-buffer rp-buffer)

    (while (setq file (pop files))
      (setq file-visiting (find-buffer-visiting file))
      (with-current-buffer (setq file-buffer (or file-visiting
                                                 (find-file file)))

        ; TODO here should also exclude tags related files
        ; but because the tags feature implementation may need to change, so
        ; complete the logic here later
        ;(unless (equal (file-truename tag-filename) (file-truename file))
        ;  add normal logic here)

        (unless (or (equal (file-truename rp-filename) (file-truename file))
                    (equal (file-truename sitemap-filename) (file-truename file))
                    (equal (file-truename tag-dir) (file-name-directory (file-truename file))))
          (let* ((fn (file-name-nondirectory file))
                 (opt-plist (org-infile-export-plist))
                 (date (plist-get opt-plist :date))
                 (relative-path (file-relative-name file root-dir))
                 (title (or (plist-get opt-plist :title)
                            fn)))
            (cond
             ((and date (string-match "%" date))
              ; set date with last change time of this file
              ;(setq date (format-time-string date (nth 6 (file-attributes file)))))
              (setq date (format-time-string "%Y-%m-%d" (nth 6 (file-attributes file)))))
             (date (setq date (fix-timestamp-string date))) ;(setq date (format-time-string "%m/%d/%Y" date)))
             (t (setq date (format-time-string "%Y-%m-%d" (nth 6 (file-attributes file))))))

            (add-to-list 'date-alist (list date relative-path title) 'eq)))
        (or file-visiting (kill-buffer file-buffer))))

    (setq date-alist (sort date-alist '(lambda (list1 list2)
                                         (let* ((date1 (car list1))
                                                (date2 (car list2))
                                                (result (compare-standard-date
                                                         (fix-timestamp-string date1)
                                                         (fix-timestamp-string date2))))
                                           (cond
                                            ((<= result 0) t)
                                            (t nil))))))

    (with-current-buffer (setq rp-buffer (or rp-visiting
                                             (find-file rp-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " rp-title "\n\n"))
      (while (and (> count 0)
                  (> (length date-alist) 0))
        (setq count (1- count))
        (let ((date-path-title-list (pop date-alist)))
          (insert (concat "* " (car date-path-title-list)
                          "  [[file:" (nth 1 date-path-title-list) "]["
                          (nth 2 date-path-title-list) "]]"))
          (insert "\n\n")))
      (save-buffer)
      (or rp-visiting (kill-buffer rp-buffer)))))

(defun op/publish-customize-header (project-plist filename pub-dir)
  "this function is called before publishing process of each file.
it's purpose is to customize the page header, mainly about the relative path of tag, recent post, etc.
please see `op/publish-search-url' and `op/publish-html-header-template' for more detail.
filename: the whole name of file to publish"

  (unless op/publish-search-url
       (error "Please firstly specify the URL(`op/publish-search-url') Google searching will based on."))
   (let* ((root-dir (plist-get project-plist :base-directory))
          (html-extension (or (plist-get project-plist :html-extension)
                              "html"))
          (tags-file-path (concat (file-name-as-directory (concat root-dir (or op/tag-directory "tags/")))
                                  (concat (file-name-sans-extension (or op/tag-index-filename "index.org")) "." html-extension)))
          ; TODO the variable in the below line should could be customized
          (rp-file-path (concat (file-name-as-directory root-dir) (concat "recentposts." html-extension)))

          (sitemap-file-path (concat (file-name-as-directory root-dir) (concat (file-name-sans-extension (or (plist-get project-plist :sitemap-filename)
                                                                                                             "sitemap.org")) "." html-extension)))

          (header (format-spec op/publish-html-header-template `((?s . ,(file-relative-name sitemap-file-path (file-name-directory filename)))
                                                                  (?t . ,(file-relative-name tags-file-path (file-name-directory filename)))
                                                                  (?r . ,(file-relative-name rp-file-path (file-name-directory filename)))
                                                                  (?u . ,op/publish-search-url)))))
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
  "this function is called before publishing process of each file.
it's purpose is to customize the footer of each generated html file.
please see `op/publish-html-postamble-template' for more detail.
filename: the whole name of file to publish"

  (if (not (or (string-match "%h" op/publish-html-postamble-template)
               (string-match "%m" op/publish-html-postamble-template)))
      (setq org-export-html-postamble-format `(("en" ,op/publish-html-postamble-template)))

    (let* ((root-dir (plist-get project-plist :base-directory))
           (file-visiting (find-buffer-visiting filename))
           (file-attrs (file-attributes filename))
           (fcdate (format-time-string "%Y-%m-%d" (nth 6 file-attrs)))
           (mdate (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
           (email (or (plist-get project-plist :email)
                      (confound-email user-mail-address)))
           opt-plist cdate template file-buffer)

      (with-current-buffer (setq file-buffer (or file-visiting
                                                 (find-file file)))
        (setq opt-plist (org-infile-export-plist))
        (setq cdate (plist-get opt-plist :date))
        (cond
         ((and cdate (string-match "%" cdate))
          (setq cdate fcdate))
         (cdate (setq cdate (fix-timestamp-string cdate)))
         (t (setq cdate fcdate)))

        (setq org-export-html-postamble-format `(("en" ,(format-spec op/publish-html-postamble-template `((?a . "%a") (?c . "%c") (?d . "%d") (?e . "%e") (?v . "%v")
                                                                                                           (?i . ,(org-html-expand email))
                                                                                                           (?h . ,cdate) (?m . ,mdate))))))
        (or file-visiting (kill-buffer file-buffer))))))

(defun op/publish-sitemap (project &optional sitemap-filename)
  "Create a sitemap of project, this function is copied and customized
from `org-publish-org-sitemap' defined in `org-publish.el'."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (localdir (file-name-directory root-dir))
         (tag-dir (file-name-as-directory (concat root-dir (or op/tag-directory
                                                               "tags/"))))
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
                      (equal (file-truename tag-dir) (file-name-directory (file-truename file))))
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
                                     "[[file:" link "]["
                                     (match-string 2 entry)
                                     "]]" (match-string 3 entry) "\n")))
                    (t
                     (insert (concat indent-str " + [[file:" link "]["
                                     entry
                                     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))



(provide 'org-page)

;;; org-page.el ends here
