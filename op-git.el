(require 'org-page-util)

(defconst op/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-page")

(defcustom op/repository-directory nil
  "The directory where org files stored, should be a git repository"
  :group 'org-page
  :type 'string)

(defun op/verify-git-repository (repo-dir)
  "This function will verify whether REPO-DIR is a valid git repository.
TODO: may add branch/commit verification later."
  (unless (and (file-directory-p repo-dir)
               (file-directory-p (concat (file-name-as-directory repo-dir)
                                         ".git/")))
    (error "Fatal: `%s' is not a valid git repository." repo-dir)))

(defun op/git-all-files (repo-dir &optional branch)
  "This function will return a list contains all org files in git repository
presented by REPO-DIR, if optional BRANCH is offered, will check that branch
instead of pointer HEAD."
  (let ((org-file-ext ".org") output)
    (op/verify-git-repository repo-dir)
    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (setq default-directory (file-name-as-directory repo-dir))
      (shell-command (concat "git ls-tree -r --name-only "
                             (or branch "HEAD")) t nil)
      (setq output (buffer-string)))
    (delq nil (mapcar '(lambda (line)
                         (if (string-suffix-p org-file-ext line t)
                             (concat (file-name-as-directory repo-dir) line)))
                      (split-string output "\n")))))

(defun op/git-files-changed (repo-dir base-commit)
  "This function can get modified/deleted org files from a git repository, other
files will be ignored. The return value is a list, each element is a con cell,
whose car is the complete file path, cdr is status: publish, or delete.

For git, there are three types: Added, Modified, Deleted, but for org-page,
only two types will work well: need to publish or need to delete.

Function will raise error if repo-dir is invalid, but cannot verify base-commit,
so be careful with the second parameter.

REPO-DIR: git repository directory
BASE-COMMIT: the commit that diff operation will be based on
<TODO>: robust enhance, branch check, etc.
"
  (let ((org-file-ext ".org")
        output kv)
    (op/verify-git-repository repo-dir)
    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (setq default-directory (file-name-as-directory repo-dir))
      (shell-command (concat "git diff --name-status "
                             base-commit " HEAD") t nil)
      (setq output (buffer-string)))
    (delq nil (mapcar
               '(lambda (line)
                  (if (string-suffix-p org-file-ext line t)
                      (progn
                        (setq kv (split-string line "\t"))
                        (cons
                         (concat (file-name-as-directory repo-dir) (cadr kv))
                         (if (member (car kv) '("M" "A")) 'update 'delete)))))
               (split-string output "\n")))))
