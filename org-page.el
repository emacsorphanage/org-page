;;; org-page.el --- static page generator based on org mode

;; Copyright (C) 2012, 2013 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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

;; org-page.el contains the main entrance of entire program.
;;
;; This program can generate a complete static website based on
;; org-mode, with only a little configuration. It provides the following
;; features:
;;
;; + manage org source using git
;; + much more beautiful theme
;; + customized theme support
;; + tags support (auto generated)
;; + search engine support (auto generated)
;; + commenting (implemented using disqus)
;;
;; How it works:
;;
;; Read org file changes from the org source branch in the git repository,
;; and then publish the changes, to a directory if specified, or to the
;; html branch in that repository if no out directory specified.
;;
;; The minimal configuration that makes org-page work:
;;
;; 1. `op/repository-directory': the git repository directory, where your
;;    org files stored, and managed by git
;; 2. `op/site-domain': the URL that search engine and disqus commenting
;;    will base on, it should be your personal site's URL
;; 3. `op/personal-disqus-shortname': the disqus shortname you own on
;;    disqus website, will be used for commenting
;;
;; Then, the following simple code will make org-page work:
;;
;;    (add-to-list 'load-path "path/to/org-page")
;;    (require 'org-page)
;;    (setq op/repository-directory "path/to/your/repo")
;;    (setq op/publish-site-url "http://your.personal.site.com/")
;;    (setq op/personal-disqus-shortname "your_disqus_shortname")
;;
;;    (op/do-publication)
;;
;; The code above makes it work, but, it does only "make it work", to
;; make org-page works as you want, maybe you need more configuration.
;; Please visit file `op-vars.el' for other variable configurations, and
;; visit description of `op/do-publication' for how org-page does the
;; publication and its parameter usage.
;;

;;; Code:

(require 'ox)
(require 'mustache)
(require 'op-util)
(require 'op-vars)
(require 'op-template)
(require 'op-git)
(require 'op-enhance)
(require 'op-export)
;; TODO remove this (require 'op-hack)


(defun op/do-publication (&optional force-all
                                    base-git-commit pub-base-dir auto-commit)
  "The main entrance of org-page. The entire procedure is:
1) verify configuration
2) read changed files on branch `op/repository-org-branch' of repository
`op/repository-directory', the definition of 'changed files' is:
   1. if FORCE-ALL is non-nil, then all files will be published
   2. if FORCE-ALL is nil, the changed files will be obtained based on
BASE-GIT-COMMIT
   3. if BASE-GIT-COMMIT is nil or omitted, the changed files will be obtained
based on previous commit
3) publish org files to html, if PUB-BASE-DIR is specified, use that directory
to store the generated html files, otherwise html files will be stored on branch
`op/repository-html-branch' of repository `op/repository-directory'
4) if PUB-BASE-DIR is nil, and auto-commit is non-nil, then the changes stored
on branch `op/repository-html-branch' will be automatically committed, but be
careful, this feature is NOT recommended, and a manual commit is much better"
  (interactive
   (let* ((f (y-or-n-p "Publish all org files? "))
          (b (unless f (read-string "Base git commit: " "HEAD~1")))
          (p (when (y-or-n-p
                    "Publish to a directory? (to original repo if not) ")
               (read-directory-name "Publication directory: ")))
          (a (y-or-n-p "Auto commit to repo? ")))
     (list f b p a)))
  (op/verify-configuration)
  (let* ((orig-branch (op/git-branch-name op/repository-directory))
         (to-repo (not (stringp pub-base-dir)))
         (store-dir (if to-repo "~/.op-tmp/" pub-base-dir)) ; TODO customization
         changed-files all-files)
    (op/git-change-branch op/repository-directory op/repository-org-branch)
    (op/prepare-theme store-dir)
    (setq all-files (op/git-all-files op/repository-directory))
    (setq changed-files (if force-all
                            `(:update ,all-files :delete nil)
                          (op/git-files-changed op/repository-directory
                                                (or base-git-commit "HEAD~1"))))
    (op/publish-changes all-files changed-files store-dir)
    (when to-repo
      (op/git-change-branch op/repository-directory op/repository-html-branch)
      (copy-directory store-dir op/repository-directory t t t)
      (delete-directory store-dir t))
    (when (and to-repo auto-commit)
      (op/git-commit-changes op/repository-directory "Update published html \
files, committed by org-page.")
      (op/git-change-branch op/repository-directory orig-branch))
    (message "Publication finished: on branch '%s' of repository '%s'."
             op/repository-html-branch op/repository-directory)))

(defun op/new-repository (repo-dir)
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by org-page."
  (interactive
   (list (read-directory-name
          "Specify a directory to become the repository: " nil nil nil)))
  (op/git-init-repo repo-dir)
  (op/generate-readme repo-dir)
  (op/git-commit-changes repo-dir "initial commit")
  (op/git-new-branch repo-dir op/repository-org-branch)
  (op/generate-index repo-dir)
  (op/git-commit-changes repo-dir "add source index.org")
  (op/generate-about repo-dir)
  (op/git-commit-changes repo-dir "add source about.org")
  (mkdir (expand-file-name "blog/" repo-dir) t)
  (mkdir (expand-file-name "wiki/" repo-dir) t))

(defun op/verify-configuration ()
  "Ensure all required configuration fields are properly configured, include:
`op/repository-directory': <required>
`op/site-domain': <required>
`op/personal-disqus-shortname': <required>
`op/repository-org-branch': [optional] (but customization recommended)
`op/repository-html-branch': [optional] (but customization recommended)
`op/site-main-title': [optional] (but customization recommanded)
`op/site-sub-title': [optional] (but customization recommanded)
`op/personal-github-link': [optional] (but customization recommended)
`op/personal-google-analytics-id': [optional] (but customization recommended)
`op/theme': [optional]"
  (unless (and op/repository-directory
               (file-directory-p op/repository-directory))
    (error "Directory `%s' is not properly configured."
           (symbol-name 'op/repository-directory)))
  (unless (file-directory-p (op/get-theme-dir op/theme))
    (error "Org-page cannot detect theme directory `%s' automatically, please \
help configure it manually, usually it should be <org-page directory>/themes/."
           (symbol-name 'op/theme)))
  (unless op/site-domain
    (error "Site domain `%s' is not properly configured."
           (symbol-name 'op/site-domain)))
  (unless op/personal-disqus-shortname
    (error "Disqus shortname `%s' is not properly configured."
           (symbol-name 'op/personal-disqus-shortname)))

  (setq op/repository-directory (expand-file-name op/repository-directory))
  (unless (or (string-prefix-p "http://" op/site-domain)
              (string-prefix-p "https://" op/site-domain))
    (setq op/site-domain (concat "http://" op/site-domain)))
  (unless op/theme
    (setq op/theme 'default)))

(defun op/generate-readme (save-dir)
  "Generate README for `op/new-repository'. SAVE-DIR is the directory where to
save generated README."
  (string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and org-page."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by org-page \"op/new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by org-page.")
   (expand-file-name "README" save-dir)))

(defun op/generate-index (save-dir)
  "Generate index.org for `op/new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun op/generate-about (save-dir)
  "Generate about.org for `op/new-repository'. SAVE-DIR is the directory where
to save generated about.org."
  (string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by org-page.")
   (expand-file-name "about.org" save-dir)))


(provide 'org-page)

;;; org-page.el ends here
