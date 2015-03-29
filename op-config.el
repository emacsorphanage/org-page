;;; op-config.el --- Functions dealing with org-page configure

;; Copyright (C)  2015 Feng Shu

;; Author: Feng Shu <tumashu AT 163 DOT com>
;; Keywords: convenience
;; Homepage: https://github.com/tumashu/org-page

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

;; op-config.el contains functions used to deal with org-page configure.

;;; Code:

(require 'op-vars)

(defun op/get-config-option (option)
  (when (functionp op/get-config-option-function)
    (funcall op/get-config-option-function option)))

(defun op/get-config-option-from-file (option)
  (eval (plist-get (op/read-config-file) option)))

(defun op/read-config-file ()
  "Read and return the Lisp data stored in FILE-NAME, or nil if no such file exists."
  (when (and op/config-file
             (file-exists-p op/config-file))
    (cdr (car (read-from-string
               (with-temp-buffer
                 (insert-file-contents op/config-file)
                 (buffer-substring-no-properties (point-min) (point-max))))))))

(defun op/get-repository-directory ()
  (let ((dir (op/get-config-option :repository-directory)))
    (when dir
      (expand-file-name dir))))

(defun op/get-site-domain ()
  (let ((site-domain (op/get-config-option :site-domain)))
    (when site-domain
      (if (or (string-prefix-p "http://"  site-domain)
              (string-prefix-p "https://" site-domain))
          site-domain
        (concat "http://" site-domain)))))

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
  (let* ((fallback-theme '(default))
         (fallback-theme-root (list (concat op/load-directory "themes/")))
         (themes (delete-dups
                  (or (op/join-to-list theme)
                      (op/join-to-list (op/get-config-option :theme) fallback-theme))))
         (theme-root-dirs
          (delete-dups
           (or (op/join-to-list root-dir)
               (op/join-to-list (op/get-config-option :theme-root-directory)
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

(defun op/get-html-creator-string ()
  (or (op/get-config-option :html-creator-string)
      (format "<a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> %s\
 (<a href=\"http://orgmode.org\">Org mode</a> %s)"
              (format "%s.x" emacs-major-version)
              (if (fboundp 'org-version)
                  (replace-regexp-in-string "\\..*" ".x" (org-version))
                "Unknown Version"))))

(provide 'op-config)

;;; op-config.el ends here
