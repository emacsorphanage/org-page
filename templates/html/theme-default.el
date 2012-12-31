;;==============================================================================
;; The file defines the html templates used by corresponding theme, the name of
;; this file is "theme-default.el", so it will be used by the theme "default".
;;
;; The templates include:
;;   1. index page of entire site
;;   2. header, shows at head of each page
;;   3. meta info, shows at the end of each post
;;   4. comment, shows after meta info
;;   5. footer, shows at tail of each page
;;
;; Be aware that the code should be with Elisp syntax, because org-page will
;; load it directly.
;;==============================================================================


(setq op/publish-html-index-template
;; the template used to construct index page of entire site, our real site is
;; generated in a subfolder named 'blog/', so the index page is needed to act
;; as a bridge, taking visitor to the real index. below parameters can be used:
;; %t: the site title
;; %c: css links, be aware that here the links are already wrapped by tag <link>
;; %p: the path to real index page"
      "<!DOCTYPE html><html><head><title>%t</title>
<meta charset=\"UTF-8\">%c</head><body>
<script type=\"text/javascript\">
    var ie = /(msie) ([\\w.]+)/.exec(navigator.userAgent.toLowerCase());
    var div = document.createElement('div');
    div.className = ie ? 'fucking-ie' : 'loading-center';
    div.innerHTML = ie ? 'Sorry, this site does not support the fucking IE.' : 'Loading...';
    document.getElementsByTagName('body')[0].appendChild(div);
    ie || setTimeout(function() { window.location.replace('%p');}, 1000);
</script></body></html>")


(setq op/publish-html-header-template
;; the template used to construct page header, below parameters can be used:
;; %p: the relative path to index html file, not the site's index, but index in folder 'blog'
;; %h: the title/headline of entire site (defined by `op/publish-site-title')
;; %s: the relative path to sitemap html file
;; %c: the relative path to category root html file
;; %t: the relative path to tag root html file
;; %r: the relative path to recent posts html file
;; %a: the relative path to about html file
;; %g: the github link (defined by `op/personal-github-link')
;; %u: the url of current site, used for search (defined by `op/publish-site-url')
      "<h1><a href=\"%p\">%h</a></h1>
   <nav id=\"main-nav\">
     <ul id=\"nav-list-main\">
       <li><a href=\"%s\" class=\"menu\">Sitemap</a></li>
       <li><a href=\"%c\" class=\"menu\">Categories</a></li>
       <li><a href=\"%t\" class=\"menu\">Tags</a></li>
       <li><a href=\"%r\" class=\"menu\">Recent Posts</a></li>
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
   </nav>")


(setq op/publish-meta-info
;; the meta info of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information.
      "<div id=\"post-meta\">
  <span title=\"post date\" class=\"post-info\">%h</span>
  <span title=\"last modification date\" class=\"update-info\">%m</span>
  <span title=\"category\" class=\"category\">%g</span>
  <span title=\"tags\" class=\"tags\">%t</span>
  <span ntitle=\"author\" class=\"author\">%a</span>
  <span title=\"htmlized org source file\" class=\"org-source\"><a href=\"%l\">htmlized org source</a></span>
</div>")


(setq op/publish-comment
;; the comment section of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information."
      "<section id=\"comment\">
  <h1 class=\"title\">Comments</h1>
  <div id=\"disqus_thread\"></div>
  <script type=\"text/javascript\">
    //var disqus_developer = 1;
    var disqus_identifier = \"%n\";
    var disqus_url = \"%u\";p
    var disqus_shortname = '%s';

    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function() {
      var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
      dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
      (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    })();
  </script>
  <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
  <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
</section>")

(setq op/publish-footer
;; the footer of current post, it will be used to compose `op/publish-html-postamble-template',
;; please see `op/publish-html-postamble-template' for detailed information."
      "<div id=\"footer\">
  <p class=\"creator\">Generated by %c</p>
  <p class=\"copyright\">
    Copyright &copy; 2012 <a href=\"mailto:%i\">Kelvin Hu</a>
    &nbsp;&nbsp;-&nbsp;&nbsp;
    Powered by <a href=\"https://github.com/kelvinh/org-page\" target=\"_blank\">org-page</a>
  </p>
</div>")
