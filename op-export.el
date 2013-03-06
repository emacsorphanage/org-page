(require 'org-page-util)

(defun op/handle-changes (status-list, pub-root-dir)
  "TODO: doc"
  (if status-list
      (mapc (lambda (cell)
              (if (eq (cdr cell) 'update)
                  (op/handle-modified-file (car cell) pub-root-dir) ; update
                (op/handle-deleted-file (car cell))))  ; deletion
            status-list)))

(defun op/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (match-string-no-properties 2 nil)))))

(defun op/read-file-info ()
  "Read info of org file opened in current buffer, include:
<TODO>: seems other values are not used except tags and uri
1. creation date
2. modification date
3. tags (read from #+TAGS property)
4. uri (read from #+URI property)
5. title
Creation date will be firstly read from #+DATE defined in the file, if no date
info found, will be read from the file's last change date. However, it is
recommended to use #+DATE."

  (let* ((filename (buffer-file-name))
         (file-attrs (file-attributes filename))
         (fcdate (format-time-string "%Y-%m-%d" (nth 6 file-attrs)))
         (mdate (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
         (attr-plist `(:creation-date ,fcdate :mod-date ,mdate :tags nil))
         (date-list (split-string fcdate "-"))
         opt-plist tags cdate uri)

    (setq opt-plist (org-infile-export-plist))
    (setq cdate (plist-get opt-plist :date))
    (if (and cdate (not (string-match "%" cdate)))
        (progn
          (setq cdate (fix-timestamp-string cdate))
          (plist-put attr-plist :creation-date cdate)
          (setq date-list (split-string cdate "-"))))
    (plist-put attr-plist :title (or (plist-get opt-plist :title)
                                     (file-name-sans-extension
                                      (file-name-nondirectory filename))))
    (setq tags (op/read-org-option "TAGS")) ; TODO customization
    (if tags
        (plist-put
         attr-plist :tags (delete "" (mapcar 'trim-string
                                             (split-string tags ":" t))))) ;; TODO customization
    (setq uri (op/read-org-option "URI"))
    (unless uri
      (setq uri (concat "/%Y/%m/%d/"
                        (convert-string-to-path
                         (plist-get attr-plist :title)))))
    (setq uri (format-spec uri `((?Y . ,(car date-list))
                                 (?m . ,(cadr date-list))
                                 (?d . ,(caddr date-list)))))
    (plist-put attr-plist :uri uri))) ; TODO customization

(defun op/handle-modified-file (org-file-path pub-base-dir)
  "TODO: doc"

  (let* ((visiting (find-buffer-visiting org-file-path))
         file-buffer attr-plist pub-dir title tags uri pub-dir)

    (with-current-buffer (setq file-buffer
                               (or visiting (find-file org-file-path)))
      (setq attr-plist (op/read-file-info))
      (setq uri (plist-get attr-plist :uri))
      (setq pub-dir (file-name-as-directory
                     (concat (file-name-as-directory pub-base-dir)
                             (replace-regexp-in-string "\\`/" "" uri))))
      (unless (file-directory-p pub-dir)
        (mkdir pub-dir t))
      (op/export-as-html nil nil nil nil nil pub-dir))
    (or visiting (kill-buffer file-buffer))))

(defun op/handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )
