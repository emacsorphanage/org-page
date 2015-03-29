;;; op-resource.el --- Functions dealing with org-page theme resources

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

;; This file include functions which deal with org-page theme resources

;;; Code:

(require 'format-spec)
(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)
(require 'op-config)

(defun op/prepare-theme-resources (pub-root-dir)
  "Copy theme resources files to PUB-ROOT-DIR."
  (let ((pub-theme-dir (expand-file-name "media/" pub-root-dir))
        (theme-dirs (reverse (op/get-theme-dirs nil nil 'resources))))
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (dolist (theme-dir theme-dirs)
      (copy-directory theme-dir pub-theme-dir t t t))))


(provide 'op-resource)

;;; op-resource.el ends here
