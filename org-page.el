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

(require 'op-vars)
(require 'op-util)
(require 'op-git)
(require 'op-enhance)
(require 'op-export)
(require 'op-hack)


(defun op/do-publication (pub-base-dir &optional base-git-commit)
  "The main entrance of org-page, PUB-BASE-DIR is the directory where published
files will be stored, BASE-GIT-COMMIT is the commit that the publication will
base on, if omitted, will use previous commit instead."
  (op/verify-configuration)
  (op/prepare-theme pub-base-dir)
  (op/publish-changes
   (op/git-all-files op/repository-directory)
   (op/git-files-changed op/repository-directory (or base-git-commit "HEAD^1"))
   pub-base-dir)
  (message "Org-page publication finished."))

(defun op/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
`op/repository-directory': <required>
`op/theme-directory': <required> (but do not need user to configure)
`op/site-url': <required>
`op/personal-disqus-shortname': <required>
`op/site-main-title': [optional] (but customization recommanded)
`op/site-sub-title': [optional] (but customization recommanded)
`op/email': [optional] (but customization recommanded)
`op/personal-github-link': [optional] (but customization recommanded)
`op/theme': [optional]
`op/css-list': [optional]"
  (unless (and op/repository-directory
               (file-directory-p op/repository-directory))
    (error "Directory `%s' is not properly configured."
           (symbol-name 'op/repository-directory)))
  (unless (and op/theme-directory
               (file-directory-p op/theme-directory))
    (error "Org-page cannot detect theme directory `%s' automatically, please \
help configure it manually, usually it should be <org-page directory>/themes/."
           (symbol-name 'op/theme-directory)))
  (unless op/site-url
    (error "Site url `%s' is not properly configured."
           (symbol-name 'op/site-url)))
  (unless op/personal-disqus-shortname
    (error "Disqus shortname `%s' is not properly configured."
           (symbol-name 'op/personal-disqus-shortname)))

  (unless (or (string-prefix-p "http://" op/site-url)
              (string-prefix-p "https://" op/site-url))
    (setq op/site-url (concat "http://" op/site-url)))
  (unless op/theme
    (setq op/theme 'default)))


(provide 'org-page)

;;; org-page.el ends here
