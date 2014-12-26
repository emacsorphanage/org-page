;;; op-util.el --- Common utility functions required by org-page

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

;; several utility functions

;;; Code:

(require 'ht)


(defun compare-standard-date (date1 date2)
  "Compare two standard ISO 8601 format dates, format is as below:
2012-08-17
1. if date1 is earlier than date2, returns 1
2. if equal, returns 0
3. if date2 is earlier than date1, returns -1"
  (let* ((date-list1 (parse-time-string date1))
         (year1 (nth 5 date-list1))
         (month1 (nth 4 date-list1))
         (day1 (nth 3 date-list1))
         (date-list2 (parse-time-string date2))
         (year2 (nth 5 date-list2))
         (month2 (nth 4 date-list2))
         (day2 (nth 3 date-list2)))
    (cond ((< year1 year2) 1)
          ((> year1 year2) -1)
          (t (cond ((< month1 month2) 1)
                   ((> month1 month2) -1)
                   (t (cond ((< day1 day2) 1)
                            ((> day1 day2) -1)
                            (t 0))))))))

(defun fix-timestamp-string (date-string)
  "This is a piece of code copied from Xah Lee (I modified a little):
Returns yyyy-mm-dd format of date-string
For examples:
   [Nov. 28, 1994]     => [1994-11-28]
   [November 28, 1994] => [1994-11-28]
   [11/28/1994]        => [1994-11-28]
Any \"day of week\", or \"time\" info, or any other parts of the string, are
discarded.
Code detail: URL `http://xahlee.org/emacs/elisp_parse_time.html'"
  (let ((date-str date-string)
        date-list year month date yyyy mm dd)
    (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str))
    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match
       "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ((string-match
       "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-"
              (match-string 2 date-str)))
     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$\
T[0-9][0-9]:[0-9][0-9]" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
       date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-"
              (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (match-string 1 date-str))
     (t (progn
          (setq date-str
                (replace-regexp-in-string "January " "Jan. " date-str))
          (setq date-str
                (replace-regexp-in-string "February " "Feb. " date-str))
          (setq date-str
                (replace-regexp-in-string "March " "Mar. " date-str))
          (setq date-str
                (replace-regexp-in-string "April " "Apr. " date-str))
          (setq date-str
                (replace-regexp-in-string "May " "May. " date-str))
          (setq date-str
                (replace-regexp-in-string "June " "Jun. " date-str))
          (setq date-str
                (replace-regexp-in-string "July " "Jul. " date-str))
          (setq date-str
                (replace-regexp-in-string "August " "Aug. " date-str))
          (setq date-str
                (replace-regexp-in-string "September " "Sep. " date-str))
          (setq date-str
                (replace-regexp-in-string "October " "Oct. " date-str))
          (setq date-str
                (replace-regexp-in-string "November " "Nov. " date-str))
          (setq date-str
                (replace-regexp-in-string "December " "Dec. " date-str))
          (setq date-str
                (replace-regexp-in-string " 1st," " 1" date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd," " 2" date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd," " 3" date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-str))
          (setq date-str
                (replace-regexp-in-string " 1st " " 1 " date-str))
          (setq date-str
                (replace-regexp-in-string " 2nd " " 2 " date-str))
          (setq date-str
                (replace-regexp-in-string " 3rd " " 3 " date-str))
          (setq date-str
                (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-str))
          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))
          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "-" mm "-" dd))))))

(defun confound-email (email)
  "Confound email to prevent spams using simple rule:
replace . with <dot>, @ with <at>, e.g.
name@domain.com => name <at> domain <dot> com"
  (if (not op/confound-email) email
    (replace-regexp-in-string
     " +" " " (replace-regexp-in-string
               "@" " <at> " (replace-regexp-in-string "\\." " <dot> " email)))))

(defun string-suffix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a suffix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (let ((pos (- (length str2) (length str1))))
    (if (< pos 0) nil (eq t (compare-strings str1 nil nil
                                             str2 pos nil ignore-case)))))

(defun trim-string-left (str)
  "Remove whitespace at the beginning of STR."
  (if (string-match "\\`[ \t\n\r]+" str)
      (replace-match "" t t str)
    str))

(defun trim-string-right (str)
  "Remove whitespace at the end of STR."
  (if (string-match "[ \t\n\r]+\\'" str)
      (replace-match "" t t str)
    str))

(defun trim-string (str)
  "Remove whitespace at the beginning and end of STR.
The function is copied from https://github.com/magnars/s.el, because I do not
want to make org-page depend on other libraries, so I copied the function here,
so do `trim-string-left' and `trim-string-right'."
  (trim-string-left (trim-string-right str)))

(defun encode-string-to-url (string)
  "Encode STRING to legal URL. Why we do not use `url-encode-url' to encode the
string, is that `url-encode-url' will convert all not allowed characters into
encoded ones, like %3E, but we do NOT want this kind of url."
  (downcase (replace-regexp-in-string "[ :/\\]+" "-" string)))

(defun get-full-url (uri)
  "Get the full url of URI, by joining `op/site-domain' with URI."
  (concat (replace-regexp-in-string "/?$" "" op/site-domain) uri))

(defun file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun string-to-file (string file &optional mode)
  "Write STRING into FILE, only when FILE is writable. If MODE is a valid major
mode, format the string with MODE's format settings."
  (with-temp-buffer
    (insert string)
    (set-buffer-file-coding-system 'utf-8-unix)
    (when (and mode (functionp mode))
      (funcall mode)
      (flush-lines "^[ \\t]*$" (point-min) (point-max))
      (delete-trailing-whitespace (point-min) (point-max))
      (indent-region (point-min) (point-max)))
    (when (file-writable-p file)
      (write-region (point-min) (point-max) file))))

(defun convert-plist-to-hashtable (plist)
  "Convert normal property list PLIST into hash table, keys of PLIST should be
in format :key, and it will be converted into \"key\" in hash table. This is an
alternative to `ht-from-plist'."
  (let ((h (ht-create)))
    (dolist (pair (ht/group-pairs plist) h)
      (let ((key (substring (symbol-name (car pair)) 1))
            (value (cadr pair)))
        (ht-set h key value)))))


(provide 'op-util)

;;; op-util.el ends here
