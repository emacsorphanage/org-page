;;; org-page.el --- static site generator based on org mode

;; Copyright (C) 2012, 2013 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: org-mode, convenience, beautify
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

;; See documentation at https://github.com/kelvinh/org-page

;; Org-page is a static site generator based on org mode.

;; Org-page provides following features:

;; 1) org sources and html files managed by git
;; 2) incremental publication
;; 3) category support
;; 4) tags support (auto generated)
;; 5) search engine support (auto generated)
;; 6) a beautiful theme
;; 7) theme customization support
;; 8) commenting (implemented using disqus)
;; 9) site visiting tracking (implemented using google analytics)
;; 10) index/about page support (auto generated if no default provided)
;; 11) highly customizable

;;; Code:

(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)
(require 'op-git)
(require 'op-enhance)
(require 'op-export)


(defconst org-page-version "0.41")

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
  (mkdir (expand-file-name "blog/" repo-dir) t))

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
    (setq op/theme 'mdo)))

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

(defun op/insert-options-template ()
  "Insert a template into current buffer with information for exporting."
  (interactive)
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         <TODO: insert your uri here>
#+KEYWORDS:    <TODO: insert your keywords here>
#+TAGS:        <TODO: insert your tags here>
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: <TODO: insert your description here>
"
           (buffer-name)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps)))


(provide 'org-page)

;;; org-page.el ends here
