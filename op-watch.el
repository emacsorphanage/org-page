;;; op-watch.el --- Watch filesystem for changes in .org files. -*- lexical-binding: t -*-

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

;; Re-publish .org files when they change.

;;; Code:

(require 'op-export)
(require 'op-git)
(require 'op-vars)

(require 'dash)
(require 'ht)

(require 'filenotify nil t)

(defvar op/watch-descriptors (ht-create)
  "A mapping from watch descriptors to timers.")

(defun op/callback-for (path all)
  "Return a callback to handle change event."
  (lambda (event)
    (destructuring-bind (descriptor action file . rest) event
      (when (eq action 'changed)

        ;; One save of a file can trigger several `changed'
        ;; events in quick succession, so we debounce them.
        (let ((debounce-timer (ht-get op/watch-descriptors descriptor)))
          (when (timerp debounce-timer)
            (cancel-timer debounce-timer)))

        (ht-set op/watch-descriptors descriptor
                (run-with-timer 1 nil #'op/file-change-handler
                                file path all))))))

(defun op/add-watch (file path all)
  "Add a watch to FILE."
  (-if-let (descriptor (file-notify-add-watch file '(change)
                                              (op/callback-for path all)))
      (ht-set! op/watch-descriptors descriptor nil)
    (error "Cannot add watch to file `%s'" file)))

(defun op/watch-files (all path)
  "Watch the repository for changes of files.

When any file of ALL changes, publish it to PATH."
  (--each all
    (op/add-watch it path all)))

(defun op/stop-all-watches ()
  "Remove existing file watches and cancel timers."
  (interactive)

  (ht-each (lambda (descriptor debounce-timer)
             (file-notify-rm-watch descriptor)
             (when (timerp debounce-timer)
               (cancel-timer debounce-timer)))
           op/watch-descriptors)
  (setf op/watch-descriptors (ht-create)))

(defun op/file-change-handler (file path all)
  "Handle change event for FILE."
  (message "CHANGED %s [%s]" file path)
  (let ((changed-files `(:update (,file) :delete nil)))
    (op/publish-changes all changed-files path)))

(provide 'op-watch)
;;; op-watch.el ends here
