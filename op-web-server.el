;;; op-web-server.el --- Test web server required by org-page

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

;; op-web-server.el is a web server used to test org-page.

;;; Code:
(require 'web-server)
(require 'op-vars)
(require 'op-config)

(defvar op/web-server nil)

(defun op/web-server-get-url ()
  (file-name-as-directory
   (format "http://localhost:%s"
           (number-to-string
            (op/get-config-option :web-server-port)))))

(defun op/web-server-start ()
  (interactive)
  (lexical-let ((docroot
                 (expand-file-name
                  (op/get-config-option :web-server-docroot)))
                (port (op/get-config-option :web-server-port)))
    (when (and (not op/web-server)
               docroot port)
      (setq op/web-server
            (ws-start
             (lambda (request)
               (with-slots (process headers) request
                 (let* ((path (substring (cdr (assoc :GET headers)) 1)) ;; Can't deal chinese char
                        (path-expand (expand-file-name path docroot))
                        (path-index-file (concat (file-name-as-directory path-expand)
                                                 "index.html")))
                   (if (or (ws-in-directory-p docroot path-expand)
                           (< (length path) 1))
                       (cond
                        ((file-exists-p path-index-file)
                         (ws-send-file process path-index-file))
                        ((and (file-exists-p path-expand)
                              (not (file-directory-p path-expand)))
                         (ws-send-file process path-expand))
                        (t (ws-send-404 process)))
                     (ws-send-404 process)))))
             (op/get-config-option :web-server-port))))))

(defun op/web-server-stop ()
  (interactive)
  (when op/web-server
    (ws-stop op/web-server)
    (setq op/web-server nil)))

(defun op/web-server-browse ()
  (interactive)
  (op/web-server-stop)
  (op/web-server-start)
  (when op/web-server
    (browse-url-default-browser
     (op/web-server-get-url))))

(provide 'op-web-server)

;;; op-web-server.el ends here
