(require 'org-page-util)

(defun op/publish-changes (all-list change-plist pub-root-dir)
  "This function is for:
1. publish changed org files to html
2. delete html files which are relevant to deleted org files
3. update index page
4. regenerate tag pages.
ALL-LIST contains paths of all org files, CHANGE-PLIST contains two properties,
one is :update for files to be updated, another is :delete for files to be
deleted. PUB-ROOT-DIR is the root publication directory."
  (let* ((upd-list (plist-get change-plist :update))
         (del-list (plist-get change-plist :delete))
         (header (op/generate-page-header))
         (style (op/generate-style))
         (ext-plist `(:style ,style :html-preamble ,header))
         visiting file-buffer file-attr-list)
    (when (or upd-list del-list)
      (mapc
       '(lambda (org-file)
          (setq visiting (find-buffer-visiting org-file))
          (with-current-buffer (setq file-buffer
                                     (or visiting (find-file org-file)))
            (setq file-attr-list (cons (op/read-file-info) file-attr-list))
            (when (member org-file upd-list)
              (op/publish-modified-file
               (car file-attr-list) pub-root-dir ext-plist))
            (when (member org-file del-list)
              (op/handle-deleted-file org-file)))
          (or visiting (kill-buffer file-buffer)))
       all-list)
      (op/generate-index file-attr-list pub-root-dir ext-plist)
      (op/generate-tags file-attr-list pub-root-dir ext-plist))))

(defun op/read-org-option (option)
  "Read option value of org file opened in current buffer.
e.g:
#+TITLE: this is title
will return \"this is title\" if OPTION is \"TITLE\""
  (let ((match-regexp (org-make-options-regexp `(,option))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))

(defun op/generate-uri (creation-date title file-type)
  "Generate URI of org file opened in current buffer. It will be firstly read
from #+URI property, if it is not presented, will be created by CREATION-DATE,
TITLE and FILE-TYPE, if FILE-TYPE is 'blog, the uri will be like:
/blog/2013/03/07/this-is-post-title, if FILE-TYPE is 'wiki, the uri will be
like: /wiki/compare-and-swap
The #+URI property can be defined containing following parameters:
%y: year of creation date
%m: month of creation date
%d: day of creation date"

  (let* ((date-list (split-string creation-date "-"))
         uri)
    (setq uri (or (op/read-org-option "URI")
                  (if (eq file-type 'blog)
                      (concat "/blog/%y/%m/%d/" (convert-string-to-path title))
                    (concat "/wiki/" (convert-string-to-path title))))) ; TODO customization
    (format-spec uri `((?y . ,(car date-list))
                       (?m . ,(cadr date-list))
                       (?d . ,(caddr date-list))))))

(defun op/get-file-type (org-file)
  "Get org file type presented by ORG-FILE, will return 'blog or 'wiki.
How to judge a file is belong to blog or wiki type is based on its root folder
name under `op/repository-directory'.
TODO: This function may be improved to have a better type determination system."
  (let ((full-path (expand-file-name org-file))
        (wiki-prefix-path (expand-file-name
                           (concat
                            (file-name-as-directory op/repository-directory)
                            "wiki/")))) ; TODO customization
    (if (string-prefix-p wiki-prefix-path full-path t) 'wiki 'blog)))

(defun op/read-file-info ()
  "Read info of org file opened in current buffer, include:
1. creation date
2. modification date
3. tags (read from #+TAGS property)
4. uri (read from #+URI property)
5. title
6. type ('blog or 'wiki, distinguished by their root folder name under
`op/repository-directory')
Creation date will be firstly read from #+DATE defined in the file, if no date
info found, will be read from the file's last change date. However, it is
recommended to use #+DATE."
  (let* ((filename (buffer-file-name))
         (file-attrs (file-attributes filename))
         (fcdate (format-time-string "%Y-%m-%d" (nth 6 file-attrs)))
         (mdate (format-time-string "%Y-%m-%d" (nth 5 file-attrs)))
         (attr-plist `(:creation-date ,fcdate :mod-date ,mdate :tags nil))
         opt-plist tags cdate)

    (setq opt-plist (org-infile-export-plist))
    (setq cdate (plist-get opt-plist :date))
    (when (and cdate (not (string-match "%" cdate)))
      (plist-put attr-plist :creation-date (fix-timestamp-string cdate)))
    (plist-put attr-plist :title (or (plist-get opt-plist :title)
                                     (file-name-sans-extension
                                      (file-name-nondirectory filename))))
    (setq tags (op/read-org-option "TAGS")) ; TODO customization
    (when tags
      (plist-put
       attr-plist :tags (delete "" (mapcar 'trim-string
                                           (split-string tags ":" t))))) ;; TODO customization
    (plist-put attr-plist :type (op/get-file-type filename))
    (plist-put attr-plist :uri (op/generate-uri
                                (plist-get attr-plist :creation-date)
                                (plist-get attr-plist :title)
                                (plist-get attr-plist :type))))) ; TODO customization

(defun op/publish-modified-file (attr-plist pub-base-dir ext-plist)
  "Publish org file opened in current buffer. ATTR-PLIST is the attribute
property list of current file. PUB-BASE-DIR is the root publication directory.
EXT-PLIST is the property list will be passed to `op/export-as-html'."
  (let* (title tags uri hidden-comment pub-dir)
    (setq uri (plist-get attr-plist :uri))
    (setq hidden-comment (if (eq (plist-get attr-plist :type) 'wiki) t nil))
    (setq pub-dir (file-name-as-directory
                   (concat (file-name-as-directory pub-base-dir)
                           (replace-regexp-in-string "\\`/" "" uri))))
    (unless (file-directory-p pub-dir)
      (mkdir pub-dir t))
    (plist-put ext-plist :html-postamble
               (op/generate-footer uri attr-plist nil hidden-comment))
    (op/kill-exported-buffer
     (op/export-as-html nil nil ext-plist nil nil pub-dir))))

(defun op/handle-deleted-file (org-file-path)
  "TODO: add logic for this function, maybe a little complex."
  )

(defun op/generate-index (file-attr-list pub-base-dir ext-plist)
  "Generate index page, FILE-ATTR-LIST is the list of all file attribute
property lists. PUB-BASE-DIR is the root publication directory. EXT-PLIST is the
property list will be passed to `op/export-as-html'."
  (let ((blog-list (remove-if-not
                    '(lambda (attr-plist)
                       (eq 'blog (plist-get attr-plist :type)))
                    file-attr-list))
        (wiki-list (remove-if-not
                    '(lambda (attr-plist)
                       (eq 'wiki (plist-get attr-plist :type)))
                    file-attr-list))
        (other-list (remove-if
                     '(lambda (attr-plist)
                        (memq (plist-get attr-plist :type) '(wiki blog)))
                     file-attr-list)))
    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (insert "#+TITLE: Index" "\n")
      (insert "#+OPTIONS: *:nil" "\n\n")
      (insert " - blog" "\n")
      (mapc '(lambda (attr-plist)
               (insert "   - ")
               (insert (plist-get attr-plist :creation-date)
                       "\\nbsp\\nbsp\\nbsp"
                       "@<a href=\"" (plist-get attr-plist :uri) "\">"
                       (plist-get attr-plist :title) "@</a>" "\n"))
            blog-list)
      (insert " - wiki" "\n")
      (mapc '(lambda (attr-plist)
               (insert "   - ")
               (insert (plist-get attr-plist :mod-date)
                       "\\nbsp\\nbsp\\nbsp"
                       "@<a href=\"" (plist-get attr-plist :uri) "\">"
                       (plist-get attr-plist :title) "@</a>" "\n"))
            wiki-list)
      (mapc '(lambda (attr-plist)
               (insert " - ")
               (insert (plist-get attr-plist :creation-date)
                       "\\nbsp\\nbsp\\nbsp"
                       "@<a href=\"" (plist-get attr-plist :uri) "\">"
                       (plist-get attr-plist :title) "@</a>" "\n"))
            other-list)
      (plist-put ext-plist :html-postamble
                 (op/generate-footer "/" nil t t))
      (op/kill-exported-buffer
       (op/export-as-html nil nil ext-plist nil nil pub-base-dir)))))

(defun op/generate-tag-uri (tag-name)
  "Generate tag uri based on TAG-NAME."
  (concat "/tags/" (convert-string-to-path tag-name) "/"))

(defun op/generate-tags (file-attr-list pub-base-dir ext-plist)
  "Generate tag pages. FILE-ATTR-LIST is the list of all file attribute property
lists. PUB-BASE-DIR is the root publication directory. EXT-PLIST is the property
list will be passed to `op/export-as-html'.
TODO: improve this function."
  (let ((tag-base-dir (concat (file-name-as-directory pub-base-dir) "tags/"))
        tag-alist tag-list tag-dir)
    (mapc
     '(lambda (attr-plist)
        (mapc
         '(lambda (tag-name)
            (setq tag-list (assoc tag-name tag-alist))
            (unless tag-list
              (add-to-list 'tag-alist (setq tag-list `(,tag-name))))
            (nconc tag-list (list attr-plist)))
         (plist-get attr-plist :tags)))
     file-attr-list)
    (with-current-buffer (get-buffer-create op/temp-buffer-name)
      (erase-buffer)
      (insert "#+TITLE: Tag Index" "\n")
      (insert "#+OPTIONS: *:nil" "\n\n")
      (mapc '(lambda (tag-list)
               (insert " - ")
               (insert "@<a href=\""
                       (op/generate-tag-uri (car tag-list))
                       "\">" (car tag-list)
                       " (" (number-to-string (length (cdr tag-list))) ")"
                       "@</a>" "\n"))
            tag-alist)
      (unless (file-directory-p tag-base-dir)
        (mkdir tag-base-dir t))
      (plist-put ext-plist :html-postamble
                 (op/generate-footer "/tags/" nil t t)) ;; TODO customization
      (op/kill-exported-buffer
       (op/export-as-html nil nil ext-plist nil nil tag-base-dir)))
    (mapc
     '(lambda (tag-list)
        (with-current-buffer (get-buffer-create op/temp-buffer-name)
          (erase-buffer)
          (insert "#+TITLE: Tag " (car tag-list) "\n")
          (insert "#+OPTIONS: *:nil" "\n\n")
          (mapc '(lambda (attr-plist)
                   (insert " - ")
                   (insert "@<a href=\"" (plist-get attr-plist :uri) "\">"
                           (plist-get attr-plist :title) "@</a>" "\n"))
                (cdr tag-list))
          (setq tag-dir (concat tag-base-dir
                                (convert-string-to-path (car tag-list))))
          (unless (file-directory-p tag-dir)
            (mkdir tag-dir t))
          (plist-put ext-plist :html-postamble
                     (op/generate-footer (op/generate-tag-uri (car tag-list))
                                         nil t t))
          (op/kill-exported-buffer
           (op/export-as-html nil nil ext-plist nil nil tag-dir))))
     tag-alist)))

(defun op/kill-exported-buffer (export-buf-or-file)
  "Kill the exported buffer. This function is a snippet copied from
`org-publish-org-to'."
  (when (and (bufferp export-buf-or-file)
             (buffer-live-p export-buf-or-file))
    (set-buffer export-buf-or-file)
    (when (buffer-modified-p) (save-buffer))
    (kill-buffer export-buf-or-file)))
