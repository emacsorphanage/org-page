;;; op-git.el --- git related functions required by org-page

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

;; git repository operation functions

;;; Code:

(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)
(require 'git)
(require 'dash)

(defun op/git-all-files (repo-dir &optional branch)
  "Return a list of all org files in git repository presented by REPO-DIR.
When optional BRANCH is offered, check that branch instead of pointer HEAD."
  (let* ((org-file-ext ".org")
         (git-repo repo-dir)
         (output (git-run "ls-tree" "-r" "--name-only"
                          (or branch "HEAD"))))
    (--map (expand-file-name it repo-dir)
	   (--filter (not (validate-ignore-categories it))
           (--filter (string-suffix-p org-file-ext it t)
                     (split-string output "\n"))))))

(defun op/git-branch-name (repo-dir)
  "Return name of current branch of git repository presented by REPO-DIR."
  (let ((git-repo repo-dir))
    (git-on-branch)))

(defun op/git-new-branch (repo-dir branch-name)
  "Create a new branch with BRANCH-NAME in REPO-DIR, and checkout the branch."
  (let ((git-repo repo-dir))
    (unless (git-branch? branch-name)
      (git-branch branch-name))
    (git-checkout branch-name)))

(defun op/git-change-branch (repo-dir branch-name)
  "Change branch to BRANCH-NAME in git repository REPO-DIR.
Do nothing if it is the current branch."
  (let ((git-repo repo-dir))
    (unless (git-on-branch? branch-name)
      (git-checkout branch-name))))

(defun op/git-init-repo (repo-dir)
  "Initialize a new empty git repository in REPO-DIR."
  (unless (file-directory-p repo-dir)
    (mkdir repo-dir t))
  (git-init repo-dir))

(defun op/git-commit-changes (repo-dir message)
  "Commit uncommitted changes to git repository in REPO-DIR.
MESSAGE is the commit message."
  (let ((git-repo repo-dir))
    (git-add)
    (git-commit message)))

(defun op/git-files-changed (repo-dir base-commit)
  "Get modified/deleted org files from git repositoryin REPO-DIR.
Diff based on BASE-COMMIT.
The return value is a property list, property :update maps a list of
updated/added files, property :delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for
org-page, only two types will work well: need to publish or need to
delete. <TODO>: robust enhance, branch check, etc."
  (let* ((org-file-ext ".org")
         (git-repo (file-name-as-directory repo-dir))
         (output (git-run "diff" "--name-status" base-commit "HEAD"))
         upd-list del-list)

    (--each (split-string output "\n")
      (when (string-match "\\`[A|M]\t\\(.*\.org\\)\\'" it)
        (!cons (concat git-repo (match-string 1 it)) upd-list))
      (when (string-match "\\`D\t\\(.*\.org\\)\\'" it)
        (!cons (concat git-repo (match-string 1 it)) del-list)))

    (list :update upd-list :delete del-list)))

(defun op/git-last-change-date (repo-dir filepath)
  "Return the last commit date of a file in git repository REPO-DIR.
FILEPATH is the path of target file, can be absolute or relative."
  (let* ((git-repo repo-dir)
         (output (git-run "log" "-1" "--format=\"%ci\"" "--" filepath)))
    (when (string-match "\\`\"\\([0-9]+-[0-9]+-[0-9]+\\) .*\"\n\\'" output)
      (match-string 1 output))))

(defun op/git-remote-name (repo-dir)
  "Return all remote repository names of git repository REPO-DIR.
Return nil if there is no remote repository."
  (let ((git-repo repo-dir))
    (git-remotes)))

(defun op/git-push-remote (repo-dir remote-repo branch)
  "Push local branch to remote repository.
REPO-DIR is the local git repository.  REMOTE-REPO is the remote
repository.  BRANCH is the name of branch will be pushed (the branch
name will be the same both in local and remote repository), and if
there is no branch named BRANCH in remote repository, it will be
created."
  (let ((git-repo repo-dir))
    (git-push remote-repo branch)))


(provide 'op-git)

;;; op-git.el ends here
