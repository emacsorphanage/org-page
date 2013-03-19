(require 'org-page-util)

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
  "This function can get modified/deleted org files from git repository
presented by REPO-DIR, diff based on BASE-COMMIT. The return value is a
property list, property :update maps a list of updated/added files, property
:delete maps a list of deleted files.
For git, there are three types: Added, Modified, Deleted, but for org-page,
only two types will work well: need to publish or need to delete.
<TODO>: robust enhance, branch check, etc."
  (let ((org-file-ext ".org")
        (repo-dir (file-name-as-directory repo-dir))
        output upd-list del-list)
    (op/verify-git-repository repo-dir)
    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (setq default-directory repo-dir)
      (shell-command (concat "git diff --name-status "
                             base-commit " HEAD") t nil)
      (setq output (buffer-string)))
    (mapc '(lambda (line)
             (if (string-match "\\`[A|M]\t\\(.*\.org\\)\\'" line)
                 (setq upd-list (cons (concat repo-dir (match-string 1 line))
                                      upd-list)))
             (if (string-match "\\`D\t\\(.*\.org\\)\\'" line)
                 (setq del-list (cons (concat repo-dir (match-string 1 line))
                                      del-list))))
          (split-string output "\n"))
    (list :update upd-list :delete del-list)))
