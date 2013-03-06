(require 'org-page-util)

(defconst op/temp-buffer-name "*Org Page Output*"
  "Name of the temporary buffer used by org-page")

(defcustom op/repository-directory nil
  "The directory where org files stored, should be a git repository"
  :group 'org-page
  :type 'string)

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
        output ret-list kv mod-flag file-path)
    (unless (and (file-directory-p repo-dir)
                 (file-directory-p (concat (file-name-as-directory repo-dir)
                                           ".git/")))
      (error "Fatal: `%s' is not a valid git repository." repo-dir))

    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (setq default-directory (file-name-as-directory repo-dir))
      (shell-command (concat "git diff --name-status"
                             " "
                             base-commit
                             " "
                             "HEAD") t nil)
      (setq output (buffer-string)))
    (mapc
     (lambda (line)
       (setq kv (split-string line "\t"))
       (setq mod-flag (if (member (car kv) '("M" "A"))
                          'update
                        'delete))
       (setq file-path (concat (file-name-as-directory repo-dir)
                               (cadr kv)))
       (if (string-suffix-p org-file-ext file-path t)
           (if (not ret-list)
               (setq ret-list (list (cons file-path mod-flag)))
             (add-to-list 'ret-list (cons file-path mod-flag)))))
     (split-string output "\n"))
    ret-list))
