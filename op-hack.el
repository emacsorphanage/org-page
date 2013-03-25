;;; op-hack.el --- general hack base on org mode, required by org-page

;; Copyright (C) 2012, 2013 Kelvin Hu

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
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

;; Hack of org mode utility function, to make it work with org-page.

;;; Code:

(defun op/export-as-html (arg &optional hidden ext-plist
                              to-buffer body-only pub-dir)
  "This function is the hacked version of `org-export-as-html', to make it meet
the requirement of org-page. Below is the description of original function:

Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
             (not buffer-file-name)
             ;; HACK: ignore the error when the buffer is temp buffer
             (not (string-equal (buffer-name) op/temp-buffer-name)))
    (if (buffer-base-buffer)
        (org-set-local 'buffer-file-name
                       (with-current-buffer (buffer-base-buffer)
                         buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* ((opt-plist
          (org-export-process-option-filters
           (org-combine-plists (org-default-export-plist)
                               ext-plist
                               (org-infile-export-plist))))
         (body-only (or body-only (plist-get opt-plist :body-only)))
         (style (concat (if (plist-get opt-plist :style-include-default)
                            org-export-html-style-default)
                        (plist-get opt-plist :style)
                        (plist-get opt-plist :style-extra)
                        "\n"
                        (if (plist-get opt-plist :style-include-scripts)
                            org-export-html-scripts)))
         (html-extension (plist-get opt-plist :html-extension))
         valid thetoc have-headings first-heading-pos
         (odd org-odd-levels-only)
         (region-p (org-region-active-p))
         (rbeg (and region-p (region-beginning)))
         (rend (and region-p (region-end)))
         (subtree-p
          (if (plist-get opt-plist :ignore-subtree-p)
              nil
            (when region-p
              (save-excursion
                (goto-char rbeg)
                (and (org-at-heading-p)
                     (>= (org-end-of-subtree t t) rend))))))
         (level-offset (if subtree-p
                           (save-excursion
                             (goto-char rbeg)
                             (+ (funcall outline-level)
                                (if org-odd-levels-only 1 0)))
                         0))
         (opt-plist (setq org-export-opt-plist
                          (if subtree-p
                              (org-export-add-subtree-options opt-plist rbeg)
                            opt-plist)))
         ;; The following two are dynamically scoped into other
         ;; routines below.
         (org-current-export-dir
          (or pub-dir (org-export-directory :html opt-plist)))
         (org-current-export-file buffer-file-name)
         (level 0) (line "") (origline "") txt todo
         (umax nil)
         (umax-toc nil)
         (filename (if to-buffer nil
                     (expand-file-name
                      (concat
                       (file-name-sans-extension
                        (or (and subtree-p
                                 (org-entry-get (region-beginning)
                                                "EXPORT_FILE_NAME" t))
                            ;; HACK: do not use org file name, always use "index" instead
                            ;(file-name-nondirectory buffer-file-name)
                            "index"))
                       "." html-extension)
                      (file-name-as-directory
                       (or pub-dir (org-export-directory :html opt-plist))))))
         (current-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
         (auto-insert nil); Avoid any auto-insert stuff for the new file
         (buffer (if to-buffer
                     (cond
                      ((eq to-buffer 'string) (get-buffer-create "*Org HTML Export*"))
                      (t (get-buffer-create to-buffer)))
                   (find-file-noselect filename)))
         (org-levels-open (make-vector org-level-max nil))
         (date        (org-html-expand (plist-get opt-plist :date)))
         (author      (org-html-expand (plist-get opt-plist :author)))
         (html-validation-link (or org-export-html-validation-link ""))
         (title       (org-html-expand
                       (or (and subtree-p (org-export-get-title-from-subtree))
                           (plist-get opt-plist :title)
                           (and (not body-only)
                                (not
                                 (plist-get opt-plist :skip-before-1st-heading))
                                (org-export-grab-title-from-buffer))
                           (and buffer-file-name
                                (file-name-sans-extension
                                 (file-name-nondirectory buffer-file-name)))
                           "UNTITLED")))
         (link-up (and (plist-get opt-plist :link-up)
                       (string-match "\\S-" (plist-get opt-plist :link-up))
                       (plist-get opt-plist :link-up)))
         (link-home (and (plist-get opt-plist :link-home)
                         (string-match "\\S-" (plist-get opt-plist :link-home))
                         (plist-get opt-plist :link-home)))
         (dummy (setq opt-plist (plist-put opt-plist :title title)))
         (html-table-tag (plist-get opt-plist :html-table-tag))
         (quote-re0   (concat "^ *" org-quote-string "\\( +\\|[ \t]*$\\)"))
         (quote-re    (format org-heading-keyword-regexp-format
                              org-quote-string))
         (inquote     nil)
         (infixed     nil)
         (inverse     nil)
         (email       (plist-get opt-plist :email))
         (language    (plist-get opt-plist :language))
         (keywords    (org-html-expand (plist-get opt-plist :keywords)))
         (description (org-html-expand (plist-get opt-plist :description)))
         (num         (plist-get opt-plist :section-numbers))
         (lang-words  nil)
         (head-count  0) cnt
         (start       0)
         (coding-system (and (boundp 'buffer-file-coding-system)
                             buffer-file-coding-system))
         (coding-system-for-write (or org-export-html-coding-system
                                      coding-system))
         (save-buffer-coding-system (or org-export-html-coding-system
                                        coding-system))
         (charset (and coding-system-for-write
                       (fboundp 'coding-system-get)
                       (coding-system-get coding-system-for-write
                                          'mime-charset)))
         (region
          (buffer-substring
           (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))))
         (org-export-have-math nil)
         (org-export-footnotes-seen nil)
         (org-export-footnotes-data (org-footnote-all-labels 'with-defs))
         (lines
          (org-split-string
           (org-export-preprocess-string
            region
            :emph-multiline t
            :for-backend 'html
            :skip-before-1st-heading
            (plist-get opt-plist :skip-before-1st-heading)
            :drawers (plist-get opt-plist :drawers)
            :todo-keywords (plist-get opt-plist :todo-keywords)
            :tasks (plist-get opt-plist :tasks)
            :tags (plist-get opt-plist :tags)
            :priority (plist-get opt-plist :priority)
            :footnotes (plist-get opt-plist :footnotes)
            :timestamps (plist-get opt-plist :timestamps)
            :archived-trees
            (plist-get opt-plist :archived-trees)
            :select-tags (plist-get opt-plist :select-tags)
            :exclude-tags (plist-get opt-plist :exclude-tags)
            :add-text
            (plist-get opt-plist :text)
            :LaTeX-fragments
            (plist-get opt-plist :LaTeX-fragments))
           "[\r\n]"))
         (mathjax
          (if (or (eq (plist-get opt-plist :LaTeX-fragments) 'mathjax)
                  (and org-export-have-math
                       (eq (plist-get opt-plist :LaTeX-fragments) t)))

              (org-export-html-mathjax-config
               org-export-html-mathjax-template
               org-export-html-mathjax-options
               (or (plist-get opt-plist :mathjax) ""))
            ""))
         table-open
         table-buffer table-orig-buffer
         ind
         rpl path attr desc descp desc1 desc2 link
         snumber fnc
         footnotes footref-seen
         href
         )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
                               '(:org-license-to-kill t))))

    (message "Exporting...")

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    ;; Get the language-dependent settings
    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
          (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
              (set (make-local-variable (nth 2 x))
                   (plist-get opt-plist (car x))))
            org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
                   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
                         (min org-export-with-toc umax)
                       umax))
      (unless body-only
        ;; File header
        (insert (format
                 "%s
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"title\" content=\"%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
%s
</head>
<body>
%s
"
                 (format
                  (or (and (stringp org-export-html-xml-declaration)
                           org-export-html-xml-declaration)
                      (cdr (assoc html-extension org-export-html-xml-declaration))
                      (cdr (assoc "html" org-export-html-xml-declaration))

                      "")
                  ;; HACK: use utf-8 instead of iso-8859-1
                  ;(or charset "iso-8859-1"))
                  (or charset "utf-8"))
                 language language
                 title
                 ;; HACK: use utf-8 instead of iso-8859-1
                 ;(or charset "iso-8859-1")
                 (or charset "utf-8")
                 title date author description keywords
                 style
                 mathjax
                 (if (or link-up link-home)
                     (concat
                      (format org-export-html-home/up-format
                              (or link-up link-home)
                              (or link-home link-up))
                      "\n")
                   "")))

        ;; insert html preamble
        (when (plist-get opt-plist :html-preamble)
          (let ((html-pre (plist-get opt-plist :html-preamble))
                (html-pre-real-contents ""))
            (cond ((stringp html-pre)
                   (setq html-pre-real-contents
                         (format-spec html-pre `((?t . ,title) (?a . ,author)
                                                 (?d . ,date) (?e . ,email)))))
                  ((functionp html-pre)
                   (insert "<div id=\"" (nth 0 org-export-html-divs) "\">\n")
                   (if (stringp (funcall html-pre)) (insert (funcall html-pre)))
                   (insert "\n</div>\n"))
                  (t
                   (setq html-pre-real-contents
                         (format-spec
                          (or (cadr (assoc (nth 0 lang-words)
                                           org-export-html-preamble-format))
                              (cadr (assoc "en" org-export-html-preamble-format)))
                          `((?t . ,title) (?a . ,author)
                            (?d . ,date) (?e . ,email))))))
            ;; don't output an empty preamble DIV
            (unless (and (functionp html-pre)
                         (equal html-pre-real-contents ""))
              (insert "<div id=\"" (nth 0 org-export-html-divs) "\">\n")
              (insert html-pre-real-contents)
              (insert "\n</div>\n"))))

        ;; begin wrap around body
        (insert (format "\n<div id=\"%s\">"
                        ;; FIXME org-export-html-content-div is obsolete since 7.7
                        (or org-export-html-content-div
                            (nth 1 org-export-html-divs)))
                ;; FIXME this should go in the preamble but is here so
                ;; that org-infojs can still find it
                "\n<h1 class=\"title\">" title "</h1>\n"))

      ;; insert body
      (if (and org-export-with-toc (not body-only))
          (progn
            (push (format "<h%d>%s</h%d>\n"
                          org-export-html-toplevel-hlevel
                          (nth 3 lang-words)
                          org-export-html-toplevel-hlevel)
                  thetoc)
            (push "<div id=\"text-table-of-contents\">\n" thetoc)
            (push "<ul>\n<li>" thetoc)
            (setq lines
                  (mapcar
                   #'(lambda (line)
                       (if (and (string-match org-todo-line-regexp line)
                                (not (get-text-property 0 'org-protected line)))
                           ;; This is a headline
                           (progn
                             (setq have-headings t)
                             (setq level (- (match-end 1) (match-beginning 1)
                                            level-offset)
                                   level (org-tr-level level)
                                   txt (save-match-data
                                         (org-html-expand
                                          (org-export-cleanup-toc-line
                                           (match-string 3 line))))
                                   todo
                                   (or (and org-export-mark-todo-in-toc
                                            (match-beginning 2)
                                            (not (member (match-string 2 line)
                                                         org-done-keywords)))
                                        ; TODO, not DONE
                                       (and org-export-mark-todo-in-toc
                                            (= level umax-toc)
                                            (org-search-todo-below
                                             line lines level))))
                             (if (string-match
                                  (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") txt)
                                 (setq txt (replace-match
                                            "&nbsp;&nbsp;&nbsp;<span class=\"tag\">\\1</span>" t nil txt)))
                             (if (string-match quote-re0 txt)
                                 (setq txt (replace-match "" t t txt)))
                             (setq snumber (org-section-number level))
                             (if (and num (if (integerp num)
                                              (>= num level)
                                            num))
                                 (setq txt (concat snumber " " txt)))
                             (if (<= level (max umax umax-toc))
                                 (setq head-count (+ head-count 1)))
                             (if (<= level umax-toc)
                                 (progn
                                   (if (> level org-last-level)
                                       (progn
                                         (setq cnt (- level org-last-level))
                                         (while (>= (setq cnt (1- cnt)) 0)
                                           (push "\n<ul>\n<li>" thetoc))
                                         (push "\n" thetoc)))
                                   (if (< level org-last-level)
                                       (progn
                                         (setq cnt (- org-last-level level))
                                         (while (>= (setq cnt (1- cnt)) 0)
                                           (push "</li>\n</ul>" thetoc))
                                         (push "\n" thetoc)))
                                   ;; Check for targets
                                   (while (string-match org-any-target-regexp line)
                                     (setq line (replace-match
                                                 (concat "@<span class=\"target\">"
                                                         (match-string 1 line) "@</span> ")
                                                 t t line)))
                                   (while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
                                     (setq txt (replace-match "" t t txt)))
                                   (setq href
                                         (replace-regexp-in-string
                                          "\\." "-" (format "sec-%s" snumber)))
                                   (setq href (org-solidify-link-text
                                               (or (cdr (assoc href
                                                               org-export-preferred-target-alist)) href)))
                                   (push
                                    (format
                                     (if todo
                                         "</li>\n<li><a href=\"#%s\"><span class=\"todo\">%s</span></a>"
                                       "</li>\n<li><a href=\"#%s\">%s</a>")
                                     href txt) thetoc)

                                   (setq org-last-level level)))))
                       line)
                   lines))
            (while (> org-last-level (1- org-min-level))
              (setq org-last-level (1- org-last-level))
              (push "</li>\n</ul>\n" thetoc))
            (push "</div>\n" thetoc)
            (setq thetoc (if have-headings (nreverse thetoc) nil))))

      (setq head-count 0)
      (org-init-section-numbers)

      (org-open-par)

      (while (setq line (pop lines) origline line)
        (catch 'nextline

          ;; end of quote section?
          (when (and inquote (string-match org-outline-regexp-bol line))
            (insert "</pre>\n")
            (org-open-par)
            (setq inquote nil))
          ;; inside a quote section?
          (when inquote
            (insert (org-html-protect line) "\n")
            (throw 'nextline nil))

          ;; Fixed-width, verbatim lines (examples)
          (when (and org-export-with-fixed-width
                     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
            (when (not infixed)
              (setq infixed t)
              (org-close-par-maybe)

              (insert "<pre class=\"example\">\n"))
            (insert (org-html-protect (match-string 3 line)) "\n")
            (when (or (not lines)
                      (not (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)"
                                         (car lines))))
              (setq infixed nil)
              (insert "</pre>\n")
              (org-open-par))
            (throw 'nextline nil))

          ;; Protected HTML
          (when (and (get-text-property 0 'org-protected line)
                     ;; Make sure it is the entire line that is protected
                     (not (< (or (next-single-property-change
                                  0 'org-protected line) 10000)
                             (length line))))
            (let (par (ind (get-text-property 0 'original-indentation line)))
              (when (re-search-backward
                     "\\(<p>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
                (setq par (match-string 1))
                (replace-match "\\2\n"))
              (insert line "\n")
              (while (and lines
                          (or (= (length (car lines)) 0)
                              (not ind)
                              (equal ind (get-text-property 0 'original-indentation (car lines))))
                          (or (= (length (car lines)) 0)
                              (get-text-property 0 'org-protected (car lines))))
                (insert (pop lines) "\n"))
              (and par (insert "<p>\n")))
            (throw 'nextline nil))

          ;; Blockquotes, verse, and center
          (when (equal "ORG-BLOCKQUOTE-START" line)
            (org-close-par-maybe)
            (insert "<blockquote>\n")
            (org-open-par)
            (throw 'nextline nil))
          (when (equal "ORG-BLOCKQUOTE-END" line)
            (org-close-par-maybe)
            (insert "\n</blockquote>\n")
            (org-open-par)
            (throw 'nextline nil))
          (when (equal "ORG-VERSE-START" line)
            (org-close-par-maybe)
            (insert "\n<p class=\"verse\">\n")
            (setq org-par-open t)
            (setq inverse t)
            (throw 'nextline nil))
          (when (equal "ORG-VERSE-END" line)
            (insert "</p>\n")
            (setq org-par-open nil)
            (org-open-par)
            (setq inverse nil)
            (throw 'nextline nil))
          (when (equal "ORG-CENTER-START" line)
            (org-close-par-maybe)
            (insert "\n<div style=\"text-align: center\">")
            (org-open-par)
            (throw 'nextline nil))
          (when (equal "ORG-CENTER-END" line)
            (org-close-par-maybe)
            (insert "\n</div>")
            (org-open-par)
            (throw 'nextline nil))
          (run-hooks 'org-export-html-after-blockquotes-hook)
          (when inverse
            (let ((i (org-get-string-indentation line)))
              (if (> i 0)
                  (setq line (concat (mapconcat 'identity
                                                (make-list (* 2 i) "\\nbsp") "")
                                     " " (org-trim line))))
              (unless (string-match "\\\\\\\\[ \t]*$" line)
                (setq line (concat line "\\\\")))))

          ;; make targets to anchors
          (setq start 0)
          (while (string-match
                  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
            (cond
             ((get-text-property (match-beginning 1) 'org-protected line)
              (setq start (match-end 1)))
             ((match-end 2)
              (setq line (replace-match
                          (format
                           "@<a name=\"%s\" id=\"%s\">@</a>"
                           (org-solidify-link-text (match-string 1 line))
                           (org-solidify-link-text (match-string 1 line)))
                          t t line)))
             ((and org-export-with-toc (equal (string-to-char line) ?*))
              ;; FIXME: NOT DEPENDENT on TOC?????????????????????
              (setq line (replace-match
                          (concat "@<span class=\"target\">"
                                  (match-string 1 line) "@</span> ")
                          ;; (concat "@<i>" (match-string 1 line) "@</i> ")
                          t t line)))
             (t
              (setq line (replace-match
                          (concat "@<a name=\""
                                  (org-solidify-link-text (match-string 1 line))
                                  "\" class=\"target\">" (match-string 1 line)
                                  "@</a> ")
                          t t line)))))

          (setq line (org-html-handle-time-stamps line))

          ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
          ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
          ;; Also handle sub_superscripts and checkboxes
          (or (string-match org-table-hline-regexp line)
              (string-match "^[ \t]*\\([+]-\\||[ ]\\)[-+ |]*[+|][ \t]*$" line)
              (setq line (org-html-expand line)))

          ;; Format the links
          (setq line (org-html-handle-links line opt-plist))

          ;; TODO items
          (if (and org-todo-line-regexp
                   (string-match org-todo-line-regexp line)
                   (match-beginning 2))

              (setq line
                    (concat (substring line 0 (match-beginning 2))
                            "<span class=\""
                            (if (member (match-string 2 line)
                                        org-done-keywords)
                                "done" "todo")
                            " " (org-export-html-get-todo-kwd-class-name
                                 (match-string 2 line))
                            "\">" (match-string 2 line)
                            "</span>" (substring line (match-end 2)))))

          ;; Does this contain a reference to a footnote?
          (when org-export-with-footnotes
            (setq start 0)
            (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line start)
              ;; Discard protected matches not clearly identified as
              ;; footnote markers.
              (if (or (get-text-property (match-beginning 2) 'org-protected line)
                      (not (get-text-property (match-beginning 2) 'org-footnote line)))
                  (setq start (match-end 2))
                (let ((n (match-string 2 line)) extra a)
                  (if (setq a (assoc n footref-seen))
                      (progn
                        (setcdr a (1+ (cdr a)))
                        (setq extra (format ".%d" (cdr a))))
                    (setq extra "")
                    (push (cons n 1) footref-seen))
                  (setq line
                        (replace-match
                         (concat
                          (format
                           (concat "%s"
                                   (format org-export-html-footnote-format
                                           (concat "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>")))
                           (or (match-string 1 line) "") n extra n n)
                          ;; If another footnote is following the
                          ;; current one, add a separator.
                          (if (save-match-data
                                (string-match "\\`\\[[0-9]+\\]"
                                              (substring line (match-end 0))))
                              org-export-html-footnote-separator
                            ""))
                         t t line))))))

          (cond
           ((string-match "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$" line)
            ;; This is a headline
            (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
                                         level-offset))
                  txt (match-string 2 line))
            (if (string-match quote-re0 txt)
                (setq txt (replace-match "" t t txt)))
            (if (<= level (max umax umax-toc))
                (setq head-count (+ head-count 1)))
            (setq first-heading-pos (or first-heading-pos (point)))
            (org-html-level-start level txt umax
                                  (and org-export-with-toc (<= level umax))
                                  head-count opt-plist)

            ;; QUOTES
            (when (string-match quote-re line)
              (org-close-par-maybe)
              (insert "<pre>")
              (setq inquote t)))

           ((and org-export-with-tables
                 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
            (when (not table-open)
              ;; New table starts
              (setq table-open t table-buffer nil table-orig-buffer nil))

            ;; Accumulate lines
            (setq table-buffer (cons line table-buffer)
                  table-orig-buffer (cons origline table-orig-buffer))
            (when (or (not lines)
                      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
                                         (car lines))))
              (setq table-open nil
                    table-buffer (nreverse table-buffer)
                    table-orig-buffer (nreverse table-orig-buffer))
              (org-close-par-maybe)
              (insert (org-format-table-html table-buffer table-orig-buffer))))

           ;; Normal lines

           (t
            ;; This line either is list item or end a list.
            (when (get-text-property 0 'list-item line)
              (setq line (org-html-export-list-line
                          line
                          (get-text-property 0 'list-item line)
                          (get-text-property 0 'list-struct line)
                          (get-text-property 0 'list-prevs line))))

            ;; Horizontal line
            (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
              (if org-par-open
                  (insert "\n</p>\n<hr/>\n<p>\n")
                (insert "\n<hr/>\n"))
              (throw 'nextline nil))

            ;; Empty lines start a new paragraph.  If hand-formatted lists
            ;; are not fully interpreted, lines starting with "-", "+", "*"
            ;; also start a new paragraph.
            (if (string-match "^ [-+*]-\\|^[ \t]*$" line) (org-open-par))

            ;; Is this the start of a footnote?
            (when org-export-with-footnotes
              (when (and (boundp 'footnote-section-tag-regexp)
                         (string-match (concat "^" footnote-section-tag-regexp)
                                       line))
                ;; ignore this line
                (throw 'nextline nil))
              (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
                (org-close-par-maybe)
                (let ((n (match-string 1 line)))
                  (setq org-par-open t
                        line (replace-match
                              (format
                               (concat "<p class=\"footnote\">"
                                       (format org-export-html-footnote-format
                                               "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>"))
                               n n n) t t line)))))
            ;; Check if the line break needs to be conserved
            (cond
             ((string-match "\\\\\\\\[ \t]*$" line)
              (setq line (replace-match "<br/>" t t line)))
             (org-export-preserve-breaks
              (setq line (concat line "<br/>"))))

            ;; Check if a paragraph should be started
            (let ((start 0))
              (while (and org-par-open
                          (string-match "\\\\par\\>" line start))
                ;; Leave a space in the </p> so that the footnote matcher
                ;; does not see this.
                (if (not (get-text-property (match-beginning 0)
                                            'org-protected line))
                    (setq line (replace-match "</p ><p >" t t line)))
                (setq start (match-end 0))))

            (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote
        (insert "</pre>\n")
        (org-open-par))

      (org-html-level-start 1 nil umax
                            (and org-export-with-toc (<= level umax))
                            head-count opt-plist)
      ;; the </div> to close the last text-... div.
      (when (and (> umax 0) first-heading-pos) (insert "</div>\n"))

      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "\\(\\(<p class=\"footnote\">\\)[^\000]*?\\)\\(\\(\\2\\)\\|\\'\\)"
                nil t)
          (push (match-string 1) footnotes)
          (replace-match "\\4" t nil)
          (goto-char (match-beginning 0))))
      (when footnotes
        (insert (format org-export-html-footnotes-section
                        (nth 4 lang-words)
                        (mapconcat 'identity (nreverse footnotes) "\n"))
                "\n"))
      (let ((bib (org-export-html-get-bibliography)))
        (when bib
          (insert "\n" bib "\n")))

      (unless body-only
        ;; end wrap around body
        (insert "</div>\n")

        ;; export html postamble
        (let ((html-post (plist-get opt-plist :html-postamble))
              (email
               (mapconcat (lambda(e)
                            (format "<a href=\"mailto:%s\">%s</a>" e e))
                          (split-string email ",+ *")
                          ", "))
              (creator-info
               (concat "Org version " org-version " with Emacs version "
                       (number-to-string emacs-major-version))))

          (when (plist-get opt-plist :html-postamble)
            (insert "\n<div id=\"" (nth 2 org-export-html-divs) "\">\n")
            (cond ((stringp html-post)
                   (insert (format-spec html-post
                                        `((?a . ,author) (?e . ,email)
                                          (?d . ,date)   (?c . ,creator-info)
                                          (?v . ,html-validation-link)))))
                  ((functionp html-post)
                   (if (stringp (funcall html-post)) (insert (funcall html-post))))
                  ((eq html-post 'auto)
                   ;; fall back on default postamble
                   (when (plist-get opt-plist :time-stamp-file)
                     (insert "<p class=\"date\">" (nth 2 lang-words) ": " date "</p>\n"))
                   (when (and (plist-get opt-plist :author-info) author)
                     (insert "<p class=\"author\">" (nth 1 lang-words) ": " author "</p>\n"))
                   (when (and (plist-get opt-plist :email-info) email)
                     (insert "<p class=\"email\">" email "</p>\n"))
                   (when (plist-get opt-plist :creator-info)
                     (insert "<p class=\"creator\">"
                             (concat "Org version " org-version " with Emacs version "
                                     (number-to-string emacs-major-version) "</p>\n")))
                   (insert html-validation-link "\n"))
                  (t
                   (insert (format-spec
                            (or (cadr (assoc (nth 0 lang-words)
                                             org-export-html-postamble-format))
                                (cadr (assoc "en" org-export-html-postamble-format)))
                            `((?a . ,author) (?e . ,email)
                              (?d . ,date)   (?c . ,creator-info)
                              (?v . ,html-validation-link))))))
            (insert "\n</div>"))))

      ;; FIXME `org-export-html-with-timestamp' has been declared
      ;; obsolete since Org 7.7 -- don't forget to remove this.
      (if org-export-html-with-timestamp
          (insert org-export-html-html-helper-timestamp))

      (unless body-only (insert "\n</body>\n</html>\n"))

      (unless (plist-get opt-plist :buffer-will-be-killed)
        (normal-mode)
        (if (eq major-mode (default-value 'major-mode))
            (html-mode)))

      ;; insert the table of contents
      (goto-char (point-min))
      (when thetoc
        (if (or (re-search-forward
                 "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
                (re-search-forward
                 "\\[TABLE-OF-CONTENTS\\]" nil t))
            (progn
              (goto-char (match-beginning 0))
              (replace-match ""))
          (goto-char first-heading-pos)
          (when (looking-at "\\s-*</p>")
            (goto-char (match-end 0))
            (insert "\n")))
        (insert "<div id=\"table-of-contents\">\n")
        (let ((beg (point)))
          (mapc 'insert thetoc)
          (insert "</div>\n")
          (while (re-search-backward "<li>[ \r\n\t]*</li>\n?" beg t)
            (replace-match ""))))
      ;; remove empty paragraphs
      (goto-char (point-min))
      (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
        (replace-match ""))
      (goto-char (point-min))
      ;; Convert whitespace place holders
      (goto-char (point-min))
      (let (beg end n)
        (while (setq beg (next-single-property-change (point) 'org-whitespace))
          (setq n (get-text-property beg 'org-whitespace)
                end (next-single-property-change beg 'org-whitespace))
          (goto-char beg)
          (delete-region beg end)
          (insert (format "<span style=\"visibility:hidden;\">%s</span>"
                          (make-string n ?x)))))
      ;; Remove empty lines at the beginning of the file.
      (goto-char (point-min))
      (when (looking-at "\\s-+\n") (replace-match ""))
      ;; Remove display properties
      (remove-text-properties (point-min) (point-max) '(display t))
      ;; Run the hook
      (run-hooks 'org-export-html-final-hook)
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (or (org-export-push-to-kill-ring "HTML")
          (message "Exporting... done"))
      (if (eq to-buffer 'string)
          (prog1 (buffer-substring (point-min) (point-max))
            (kill-buffer (current-buffer)))
        (current-buffer)))))


(provide 'op-hack)

;;; op-hack.el ends here
