;;; op-enhance.el --- HTML page customization required by org-page

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

;; Improve generated html page display effect

;;; Code:

(defun op/prepare-theme (pub-root-dir)
  "Copy theme files to PUB-ROOT-DIR."
  (let* ((pub-theme-dir (concat (file-name-as-directory pub-root-dir)
                                "media/"))
         (theme-dir (file-name-as-directory
                     (concat (file-name-as-directory op/theme-directory)
                             (symbol-name op/theme)))))
    (unless (file-directory-p theme-dir)
      (message "Theme %s not found, use `default' theme instead."
               (symbol-name op/theme))
      (setq op/theme 'default)
      (setq theme-dir (file-name-as-directory
                       (concat (file-name-as-directory op/theme-directory)
                               (symbol-name op/theme)))))
    (op/update-theme op/theme)
    (when (file-directory-p pub-theme-dir)
      (delete-directory pub-theme-dir t))
    (copy-directory theme-dir pub-theme-dir t t t)))

(defun op/update-theme (theme)
  "Update theme related variables, to make them take effect after user used a
new theme."
  (unless theme
    (setq theme 'default))
  (setq theme (symbol-name theme))
  (setq op/html-header-template
        (file-to-string
         (concat op/load-directory
                 (format "templates/html/%s/header-template.html" theme))))
  (setq op/meta-info
        (file-to-string
         (concat op/load-directory
                 (format "templates/html/%s/meta-info-template.html" theme))))
  (setq op/comment
        (file-to-string
         (concat op/load-directory
                 (format "templates/html/%s/comment-template.html" theme))))
  (setq op/footer
        (file-to-string
         (concat op/load-directory
                 (format "templates/html/%s/footer-template.html" theme))))
  (setq op/html-postamble-template
        (concat op/meta-info op/comment op/footer)))

(defun op/generate-page-header ()
  "Generate page header, based on the template defined by
`op/html-header-template', please see its description for more detail."
  (let* ((search-url op/site-url))
    (when (string-match "\\`https?://\\(.*[a-zA-Z]\\)/?\\'" op/site-url)
      (setq search-url (match-string 1 op/site-url)))
    (format-spec op/html-header-template `((?m . ,op/site-main-title)
                                           (?s . ,op/site-sub-title)
                                           (?b . "/blog/") ; TODO customization
                                           (?w . "/wiki/")
                                           (?t . "/tags/")
                                           (?a . "/about/")
                                           (?g . ,op/personal-github-link)
                                           (?u . ,search-url)))))

(defun op/generate-style ()
  "Generate css style links."
  (let* ((template "<link href=\"%s\" rel=\"stylesheet\" type=\"text/css\" />")
         (css-list op/css-list)
         css-links)
    (unless css-list
      (setq css-list '("main.css")))
    (mapconcat '(lambda (css)
                  (format template (concat "/media/css/" css))) ;; TODO customization
               css-list "\n")))

(defun op/generate-footer (uri attr-plist hide-meta-info hide-comment)
  "Generate page footer, based on the template defined by
`op/html-postamble-template', please see its description for more detail."
  (let* ((footer-template op/html-postamble-template)
         (disqus-identifier uri)
         (disqus-url (concat (replace-regexp-in-string "/?$" "" op/site-url)
                             disqus-identifier))
         (disqus-shortname op/personal-disqus-shortname)
         (email (confound-email op/email))
         cdate mdate tag-list tag-links)
    (when (or hide-meta-info hide-comment)
      (setq footer-template op/footer)
      (unless hide-comment
        (setq footer-template (concat op/comment footer-template)))
      (unless hide-meta-info
        (setq footer-template (concat op/meta-info footer-template))))
    (when attr-plist
      (setq uri (plist-get attr-plist :uri))
      (setq cdate (plist-get attr-plist :creation-date))
      (setq mdate (plist-get attr-plist :mod-date))
      (setq tag-list (plist-get attr-plist :tags)))
    (when tag-list
      (setq tag-links
            (mapconcat
             '(lambda (tag-name)
                (format-spec "<a href=\"%l\">%n</a>"
                             `((?l . ,(op/generate-tag-uri tag-name))
                               (?n . ,tag-name))))
             tag-list ", ")))
    (format-spec footer-template
                 `((?a . "%a") (?c . "%c") (?d . "%d") (?e . "%e") (?v . "%v")
                   (?i . ,(org-html-expand email))
                   (?h . ,(or cdate "N/A"))
                   (?m . ,(or mdate "N/A"))
                   (?t . ,(or tag-links "N/A"))
                   (?n . ,disqus-identifier)
                   (?u . ,disqus-url)
                   (?s . ,disqus-shortname)))))


(provide 'op-enhance)

;;; op-enhance.el ends here
