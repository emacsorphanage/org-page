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


(defun op/verify-git-repository (repo-dir)
  "This function will verify whether REPO-DIR is a valid git repository.
TODO: may add branch/commit verification later."
  (unless (and (file-directory-p repo-dir)
               (file-directory-p (expand-file-name ".git/" repo-dir)))
    (error "Fatal: `%s' is not a valid git repository." repo-dir)))

(defun op/shell-command (dir command &optional need-git)
  "This function execute shell commands in a specified directory.
If NEED-GIT is non-nil, then DIR must be a git repository. COMMAND is the
command to be executed."
  (if need-git
      (op/verify-git-repository dir))
  (with-current-buffer (get-buffer-create op/temp-buffer-name)
    (erase-buffer)
    (setq default-directory (file-name-as-directory dir))
    (shell-command command t nil)
    (buffer-string)))

(defun op/git-all-files (repo-dir &optional branch)
  "This function will return a list contains all org files in git repository
presented by REPO-DIR, if optional BRANCH is offered, will check that branch
instead of pointer HEAD."
  (let ((org-file-ext ".org")
        (output (op/shell-command
                 repo-dir
                 (concat "git ls-tree -r --name-only "
                         (or branch "HEAD"))
                 t)))
    (delq nil (mapcar #'(lambda (line)
                          (when (string-suffix-p org-file-ext line t)
                            (expand-file-name line repo-dir)))
                      (split-string output "\n")))))

(defun op/git-branch-name (repo-dir)
  "Return name of current branch of git repository presented by REPO-DIR."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 "git rev-parse --abbrev-ref HEAD"
                 t)))
    (replace-regexp-in-string "[\n\r]" "" output)))

(defun op/git-new-branch (repo-dir branch-name)
  "This function will create a new branch with BRANCH-NAME, and checkout it.
TODO: verify if the branch exists."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 (concat "git checkout -b " branch-name)
                 t)))
    (unless (string-match "Switched to a new branch" output)
      (error "Fatal: Failed to create a new branch with name '%s'."
             branch-name))))

(defun op/git-change-branch (repo-dir branch-name)
  "This function will change branch to BRANCH-NAME of git repository presented
by REPO-DIR. Do nothing if it is current branch."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 (concat "git checkout " branch-name)
                 t)))
    (when (string-match "\\`error" output)
      (error "Failed to change branch to '%s' of repository '%s'."
             branch-name repo-dir))))

(defun op/git-init-repo (repo-dir)
  "This function will initialize a new empty git repository. REPO-DIR is the
directory where repository will be initialized."
  (unless (file-directory-p repo-dir)
    (mkdir repo-dir t))
  (unless (string-prefix-p "Initialized empty Git repository"
                           (op/shell-command repo-dir "git init" nil))
    (error "Fatal: Failed to initialize new git repository '%s'." repo-dir)))

(defun op/git-commit-changes (repo-dir message)
  "This function will commit uncommitted changes to git repository presented by
REPO-DIR, MESSAGE is the commit message."
  (let ((repo-dir (file-name-as-directory repo-dir)) output)
    (op/shell-command repo-dir "git add ." t)
    (setq output
          (op/shell-command repo-dir
                            (format "git commit -m \"%s\"" message)
                            t))
    (when (not (string-match "\\[.* .*\\]" output))
      (error "Failed to commit changes on current branch of repository '%s'."
             repo-dir))))

(defun op/git-files-changed (repo-dir base-commit)
  "This function can get modified/deleted org files from git repository
presented by REPO-DIR, diff based on BASE-COMMIT. The return value is a
property list, property :update maps a list of updated/added files, property
:delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for org-page,
only two types will work well: need to publish or need to delete.
<TODO>: robust enhance, branch check, etc."
  (let ((org-file-ext ".org")
        (repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 (concat "git diff --name-status "
                         base-commit " HEAD")
                 t))
        upd-list del-list)
    (mapc #'(lambda (line)
              (if (string-match "\\`[A|M]\t\\(.*\.org\\)\\'" line)
                  (setq upd-list (cons (concat repo-dir (match-string 1 line))
                                       upd-list)))
              (if (string-match "\\`D\t\\(.*\.org\\)\\'" line)
                  (setq del-list (cons (concat repo-dir (match-string 1 line))
                                       del-list))))
          (split-string output "\n"))
    (list :update upd-list :delete del-list)))

(defun op/git-last-change-date (repo-dir filepath)
  "This function will return the last commit date of a file in git repository
presented by REPO-DIR, FILEPATH is the path of target file, can be absolute or
relative."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 (concat "git log -1 --format=\"%ci\" -- \"" filepath "\"")
                 t)))
    (when (string-match "\\`\\([0-9]+-[0-9]+-[0-9]+\\) .*\n\\'" output)
      (match-string 1 output))))

(defun op/git-remote-name (repo-dir)
  "This function will return all remote repository names of git repository
presented by REPO-DIR, return nil if there is no remote repository."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 "git remote"
                 t)))
    (delete "" (split-string output "\n"))))

(defun op/git-push-remote (repo-dir remote-repo branch)
  "This function will push local branch to remote repository, REPO-DIR is the
local git repository, REMOTE-REPO is the remote repository, BRANCH is the name
of branch will be pushed (the branch name will be the same both in local and
remote repository), and if there is no branch named BRANCH in remote repository,
it will be created."
  (let ((repo-dir (file-name-as-directory repo-dir))
        (output (op/shell-command
                 repo-dir
                 (concat "git push " remote-repo " " branch ":" branch)
                 t)))
    (when (or (string-match "fatal" output)
              (string-match "error" output))
      (error "Failed to push branch '%s' to remote repository '%s'."
             branch remote-repo))))


(provide 'op-git)

;;; op-git.el ends here
