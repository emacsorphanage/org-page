;; TODO merge this file to the main file after the feature is finished.
(defun op/read-file-info (filename)
  "Read info of given org file (the `filename' should be full-path), include:
1. path; 2. creation date; 3. modification date; 4. tags. The creation date
info will be firstly read from #+DATE defined in the file, if no date info
found, will be read from the file's last change date. However, it is recommended
to use #+DATE to record creation date, since the file's last change date will
change if its meta info changed."
  (if (or (not (file-exists-p filename))
          (file-directory-p filename))
      nil
    (let* ((file-attrs (file-attributes filename))
           (fcdate (format-time-string "%Y-%m-%d" (nth 6 file-attrs)))
           (mdate (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
           ;; TODO here the keywords of tags and category should be customizable
           (tag-match-regexp (org-make-options-regexp '("TAGS")))
           (category-match-regexp (org-make-options-regexp '("CATEGORY")))
           (file-visiting (find-buffer-visiting filename))
           (attr-plist `(:path ,filename :creation-date ,fcdate :mod-date ,mdate))
           opt-plist category tags tag-list file-buffer cdate)
      (with-current-buffer (setq file-buffer (or file-visiting
                                                 (find-file filename)))
        (setq opt-plist (org-infile-export-plist))
        (setq cdate (plist-get opt-plist :date))
        (if (and cdate (not (string-match "%" cdate)))
            (plist-put attr-plist :creation-date cdate))
        (plist-put attr-plist :title (or (plist-get opt-plist :title)
                                         (file-name-sans-extension (file-name-nondirectory filename))))
        (goto-char (point-min))
        (if (re-search-forward tag-match-regexp nil t)
            (setq tags (match-string-no-properties 2 nil))
          (setq tags nil))
        (if tags
            (mapcar '(lambda (tag)
                       (unless (equal "" (replace-regexp-in-string " +" "" tag))
                         (if (not tag-list)
                             (setq tag-list (list tag))
                           (add-to-list 'tag-list tag))))
                    ;; TODO the seperator should be customizable
                    (org-split-string tags ":"))
          (setq tag-list nil))
        (plist-put attr-plist :tags tag-list)
        (goto-char (point-min))
        (if (re-search-forward tag-match-regexp nil t)
            (setq category (match-string-no-properties 2 nil))
          (setq category nil))
        (plist-put attr-plist :category (or category nil))
        (or file-visiting (kill-buffer file-buffer)))
      attr-plist)))


(defun op/reorganize-project-structure (project)
  "This function is used to re-generate project with a well-formed structure,
for example, assume a org file with path '/path/to/sample.org' is written on
2012-11-17, then it will be re-formed to '/path/to/2012/11/17/sample/index.org.
The original project directory will be temporarily renamed to 'tmp/' under the
same folder, and it will be recovered after the entire project generation.
Since this function is destructive, so alone invocation of this function is
strongly discouraged."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (tmp-dir (or op/src-temp-directory
                      (file-name-as-directory (concat op/root-directory "tmp/"))))
         tmp-project files file file-attrs file-attr-list
         date-list old-relative-path new-relative-path new-path)
    (if (file-directory-p tmp-dir)
        (delete-directory tmp-dir t nil))
    (rename-file root-dir tmp-dir)
    (setq tmp-project (copy-list project))
    (setq tmp-project (nconc (list (car tmp-project))
                             (plist-put (cdr tmp-project) :base-directory tmp-dir)))
    (setq files (org-publish-get-base-files tmp-project
                                            (plist-get project-plist :exclude)))
    (while (setq file (pop files))
      (unless  (not (file-exists-p file))
        (setq file-attrs (op/read-file-info file))
        (add-to-list 'file-attr-list file-attrs)
        (setq date-list (split-string (fix-timestamp-string (plist-get file-attrs :creation-date)) "-"))
        (setq old-relative-path (file-relative-name file tmp-dir))
        (setq new-relative-path (concat (file-name-as-directory
                                         (concat (file-name-directory old-relative-path)
                                                 (car date-list) "/"
                                                 (cadr date-list) "/"
                                                 (caddr date-list) "/"
                                                 ;;(file-name-sans-extension (file-name-nondirectory old-relative-path)) "/"))
                                                 (replace-regexp-in-string "[ :/\\]+" "-" (plist-get file-attrs :title)) "/"))
                                        "index." (file-name-extension old-relative-path)))
        (setq new-path (expand-file-name new-relative-path root-dir))
        (plist-put file-attrs :new-path new-path)
        (unless (file-directory-p (file-name-directory new-path))
          (make-directory (file-name-directory new-path) t))
        (copy-file (plist-get file-attrs :path)
                   (plist-get file-attrs :new-path)
                   t)))
    file-attr-list))

(defun op/restore-project-structure (project)
  "This function is used to restore project to its original structure, which is
modified during project generation. For more information, please see function
`op/reorganize-project-structure'. Since this function is destructive, it should
only be invoked after invocation of `op/reorganize-project-structure', alone
invocation of this function is strongly discouraged."
  (let* ((project-plist (cdr project))
         (root-dir (file-name-as-directory
                    (plist-get project-plist :base-directory)))
         (tmp-dir (or op/src-temp-directory
                      (file-name-as-directory (concat op/root-directory "tmp/")))))
    (unless (file-directory-p tmp-dir)
      (error (concat "The temporary directory " tmp-dir " not found, restore process cannot continue.")))

    (if (file-directory-p root-dir)
        (delete-directory root-dir t nil))
    (rename-file tmp-dir root-dir)))
