
from https://github.com/nickbalestra/kactus

delete your index.org

git clone https://github.com/venmos/org-page-theme-kactus.git

cp -r org-page-theme-kactus ~/.emacs.d/elpa/org-page-xxxx.xx/themes/ or ln -s ~/Github/org-page-theme-kactus ~/.emacs.d/elpa/org-page-xxxx.xx/themes/org-page-theme-kactus

add your logo.png to org-page-theme-kactus/resources/images/

edit org-page-theme-kactus/templates/index.mustache , find "Your Name" and "Your Sub Title", modify them.

change org-page config

add (setq op/theme 'org-page-theme-kactus)

![image](https://dn-assets-gitcafe-com.qbox.me/venmos/org-page-theme-kactus/raw/master/1.png)

![image](https://dn-assets-gitcafe-com.qbox.me/venmos/org-page-theme-kactus/raw/master/2.png)

![image](https://dn-assets-gitcafe-com.qbox.me/venmos/org-page-theme-kactus/raw/master/3.png)
