;;; org-page-util.el --- common utility functions required by org-page

;; Copyright (C) 2012 Kelvin Hu.

;; Author: Kelvin Hu <ini DOT kelvin AT gmail DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/kelvinh/org-page
;; Version: 0.1

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

(defun compare-standard-date (date1 date2)
  "this function compares two standard ISO 8601 format dates, format is as below:
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
    (cond
     ((< year1 year2) 1)
     ((> year1 year2) -1)
     (t (cond
         ((< month1 month2) 1)
         ((> month1 month2) -1)
         (t (cond
             ((< day1 day2) 1)
             ((> day1 day2) -1)
             (t 0)
             )))))))

(defun fix-timestamp-string (date-string)
  "this is a piece code copied from Xah Lee (I modified a little):
Returns yyyy-mm-dd format of date-string

For examples:
   [Nov. 28, 1994]     => [1994-11-28]
   [November 28, 1994] => [1994-11-28]
   [11/28/1994]        => [1994-11-28]

Any \"day of week\", or \"time\" info, or any other parts of the string, are discarded.

Code detail: URL `http://xahlee.org/emacs/elisp_parse_time.html'"

  (let ((date-str date-string)
        date-list year month date yyyy mm dd)
    (setq date-str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-str)) ; remove white spaces

    (cond
     ;; USA convention of mm/dd/yyyy
     ((string-match "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (concat (match-string 3 date-str) "-" (match-string 1 date-str) "-" (match-string 2 date-str)))

     ;; some ISO 8601. yyyy-mm-dd
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$T[0-9][0-9]:[0-9][0-9]" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-" (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str) "-" (match-string 3 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-str)
      (concat (match-string 1 date-str) "-" (match-string 2 date-str)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-str)
      (match-string 1 date-str))

     (t (progn
          (setq date-str (replace-regexp-in-string "January " "Jan. " date-str))
          (setq date-str (replace-regexp-in-string "February " "Feb. " date-str))
          (setq date-str (replace-regexp-in-string "March " "Mar. " date-str))
          (setq date-str (replace-regexp-in-string "April " "Apr. " date-str))
          (setq date-str (replace-regexp-in-string "May " "May. " date-str))
          (setq date-str (replace-regexp-in-string "June " "Jun. " date-str))
          (setq date-str (replace-regexp-in-string "July " "Jul. " date-str))
          (setq date-str (replace-regexp-in-string "August " "Aug. " date-str))
          (setq date-str (replace-regexp-in-string "September " "Sep. " date-str))
          (setq date-str (replace-regexp-in-string "October " "Oct. " date-str))
          (setq date-str (replace-regexp-in-string "November " "Nov. " date-str))
          (setq date-str (replace-regexp-in-string "December " "Dec. " date-str))

          (setq date-str (replace-regexp-in-string " 1st," " 1" date-str))
          (setq date-str (replace-regexp-in-string " 2nd," " 2" date-str))
          (setq date-str (replace-regexp-in-string " 3rd," " 3" date-str))
          (setq date-str (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-str))

          (setq date-str (replace-regexp-in-string " 1st " " 1 " date-str))
          (setq date-str (replace-regexp-in-string " 2nd " " 2 " date-str))
          (setq date-str (replace-regexp-in-string " 3rd " " 3 " date-str))
          (setq date-str (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-str))

          (setq date-list (parse-time-string date-str))
          (setq year (nth 5 date-list))
          (setq month (nth 4 date-list))
          (setq date (nth 3 date-list))

          (setq yyyy (number-to-string year))
          (setq mm (if month (format "%02d" month) ""))
          (setq dd (if date (format "%02d" date) ""))
          (concat yyyy "-" mm "-" dd))))))

(defun confound-email (email)
  "confound email to prevent spams sent by robots using simple rule:
replace . with <dot>, @ with <at>, e.g.
name@domain.com => name <at> domain <dot> com"

  (replace-regexp-in-string " +" " "
                            (replace-regexp-in-string "@" " <at> "
                                                      (replace-regexp-in-string "\\." " <dot> " email))))

(defun convert-string-to-path (string)
  "convert a string to legal URL path
TODO: improve doc here
TODO2: maybe DBCS strings should also be converted into ASCII URL path"
  (downcase (replace-regexp-in-string "[ :/\\]+" "-" string)))

(defun get-valid-uri-path (path)
  "This function is used to remove the html file name from the path,
e.g. path '../blog/index.org' will be converted into '../blog/', since
'index.html' will be the default request page under a folder."
  (let ((name (file-name-nondirectory path)))
    (cond
     ((or (string= path "index.org")
          (string= path "index.html")
          (string= path "index.htm"))
      "./")
     ((or (string= name "index.org")
          (string= name "index.html")
          (string= name "index.htm"))
      (file-name-directory path))
     (t path))))

(defun file-to-string (file)
  "Read the file contents and return it as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(provide 'org-page-util)

;;; org-page-util.el ends here
