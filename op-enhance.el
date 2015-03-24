;;; op-enhance.el --- HTML page customization required by org-page

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

;; Improve generated html page display effect

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)

(defun op/join-to-list (a1 &optional a2)
  "Conbine `a1' and `a2' to a list."
  (let ((list1 (if (listp a1) a1 (list a1)))
        (list2 (if (listp a2) a2 (list a2))))
    (append list1 list2)))

(defun op/get-theme-dirs (&optional root-dir theme type)
  "Get org-page theme type path.

org-page organizes its themes by directory:

| Directory           |  Argument   |  Value                 |
+---------------------+-------------+------------------------+
| /path/to/directory  |  <root-dir> | \"/path/to/directory\" |
|  \--mdo             |  <theme>    | 'mdo                   |
|      |-- templates  |  <type>     | 'templates             |
|       \- resources  |  <type>     | 'resources             |

`root-dir' and `theme' can be lists, for example:

  `(\"path/to/dir1\" \"path/to/dir2\" \"path/to/dir3\")'
  `(theme1 theme2 theme3)'

At this time, `op/get-theme-dirs' will find *all possible*
<type> directorys by permutation way and return a list with
multi path."
  (let* ((fallback-theme 'mdo)
         (fallback-theme-root (concat op/load-directory "themes/"))
         (themes (delete-dups
                  (or (op/join-to-list theme)
                      (op/join-to-list op/theme fallback-theme))))
         (theme-root-dirs
          (delete-dups
           (or (op/join-to-list root-dir)
               (op/join-to-list op/theme-root-directory
                                fallback-theme-root))))
         theme-dir theme-dirs)
    (dolist (theme themes)
      (dolist (root-dir theme-root-dirs)
        (setq theme-dir
              (file-name-as-directory
               (expand-file-name
                (format "%s/%s" (symbol-name theme)
                        (if type (symbol-name type) ""))
                root-dir)))
        (when (file-directory-p theme-dir)
          (push theme-dir theme-dirs))))
    (reverse theme-dirs)))

(defun op/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let ((pub-theme-dir (expand-file-name "media/" pub-root-dir))
        (theme-dirs (reverse (op/get-theme-dirs nil nil 'resources))))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (dolist (theme-dir theme-dirs)
      (copy-directory theme-dir pub-theme-dir t t t))))


(provide 'op-enhance)

;;; op-enhance.el ends here
