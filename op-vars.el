(defgroup org-page nil
  "Options for generating static pages using org-page."
  :tag "Org static page generator" :group 'org)

(defcustom op/site-url nil
  "The domain name of entire site, it is recommended to assign with prefix
http:// or https://, http will be considered if not assigned"
  :group 'org-page :type 'string)

(defcustom op/site-main-title "org-page"
  "The main title of entire site"
  :group 'org-page :type 'string)

(defcustom op/site-sub-title "static site generator"
  "The subtitle of entire site"
  :group 'org-page :type 'string)

; TODO remove "TODO" in below string after rss feature implemented
(defcustom op/html-header-template
  "<h1><a href=\"/\">%m</a><a href=\"\">%s</a></h1>
   <nav id=\"main-nav\">
     <ul id=\"nav-list-main\">
       <li><a href=\"%t\" class=\"menu\">Tags</a></li>
       <li><a href=\"%a\" class=\"menu\">About</a></li>
     </ul>
   </nav>
   <nav id=\"sub-nav\" class=\"align-right\">
     <div class=\"social\">
         <a class=\"github\" title=\"GitHub\" href=\"%g\">GitHub</a>
         <!-- TODO: change the link below to proper value  -->
         <a class=\"rss\" title=\"RSS(TODO)\" href=\"TODO\">RSS</a>
     </div>
     <form class=\"search\" target=\"_blank\" method=\"get\" action=\"http://google.com/search\">
       <input type=\"hidden\" value=\"site:%u\" name=\"q\" />
       <input type=\"text\" class=\"align-right\" results=\"0\" name=\"q\" />
     </form>
   </nav>"
  "The template used to construct page header, below parameters can be used:
%m: the main title of entire site (defined by `op/site-main-title')
%s: the subtitle of entire site (defined by `op/site-sub-title')
%t: the path to root tag
%a: the path to about
%g: the github link (defined by `op/personal-github-link')
%u: the url of current site, used for search (defined by `op/site-url')"
  :group 'org-page :type 'string)

(defcustom op/theme-directory
  (concat (file-name-directory load-file-name) "themes/")
  "The directory stores org-page styles/scripts/images"
  :group 'org-page :type 'string)

(defcustom op/theme 'default
  "The theme used for page generation"
  :group 'org-page :type 'symbol)

(defcustom op/css-list '("main.css")
  "CSS style file name list, will using uri \"/media/css/<name>\""
  :group 'org-page
  :type 'list)
